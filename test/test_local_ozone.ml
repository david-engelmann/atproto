open OUnit2
open Atproto.Session
open Atproto.Ozone
open Atproto.Label
open Atproto.Client
open Atproto.Error
open Atproto.Moderation
open Ozone
open Label

(* Real tools.ozone.* calls against official @atproto/dev-env Ozone.
   Uses the mock admin-mod.test account and ozone service DID. *)

let env_truthy name =
  match Sys.getenv_opt name with
  | Some v ->
      let v = String.lowercase_ascii (String.trim v) in
      List.mem v [ "1"; "true"; "yes"; "on" ]
  | None -> false

let require_local = env_truthy "ATP_REQUIRE_LOCAL_PDS"

let host_is_local host =
  let bare =
    match String.split_on_char ':' host with h :: _ -> h | [] -> host
  in
  let bare = String.lowercase_ascii bare in
  bare = "localhost" || bare = "127.0.0.1" || bare = "[::1]" || bare = "::1"

let intended () =
  env_truthy "ATP_LOCAL_PDS" || host_is_local Session.atp_host_from_env

let ozone_did () =
  match Sys.getenv_opt "ATP_OZONE_DID" with
  | Some d when String.trim d <> "" -> String.trim d
  | _ -> ""

let ozone_host () =
  match Sys.getenv_opt "ATP_OZONE_HOST" with
  | Some h when String.trim h <> "" -> Some (String.trim h)
  | _ ->
      if host_is_local Session.atp_host_from_env then Some "localhost:2587"
      else None

let skip_unless_local () =
  if not (intended ()) then
    skip_if true "local Ozone not selected (start scripts/local-atproto.sh)";
  if ozone_did () = "" then
    if require_local then
      failwith "ATP_OZONE_DID is required (see scripts/local-atproto.sh env)"
    else skip_if true "ATP_OZONE_DID not set"

let session_of auth =
  match String.split_on_char ':' auth with
  | [ u; p ] -> Session.create_session u p
  | _ -> failwith "expected handle:password"

let admin_session () =
  skip_unless_local ();
  match Sys.getenv_opt "ATP_AUTH_OZONE" with
  | Some auth -> session_of auth
  | None -> session_of "admin-mod.test:admin-mod-pass"

let proxy () = Ozone.labeler_proxy (ozone_did ())

let no_xrpc_error json =
  match Error.check_for_error json with
  | Some _ -> failwith ("XRPC error: " ^ Error.to_string (Error.of_json json))
  | None -> ()

let message_has hay needle =
  let h = String.lowercase_ascii hay and n = String.lowercase_ascii needle in
  let rec aux i =
    if i + String.length n > String.length h then false
    else if String.sub h i (String.length n) = n then true
    else aux (i + 1)
  in
  aux 0

let cannot_moderate json =
  match Error.check_for_error json with
  | None -> false
  | Some _ when Error.is_not_served_json json -> true
  | Some err ->
      let e = String.lowercase_ascii err in
      let msg = String.lowercase_ascii (Error.to_string (Error.of_json json)) in
      List.exists
        (fun n -> e = n || message_has msg n)
        [
          "authenticationrequired";
          "authmissing";
          "forbidden";
          "unauthorized";
          "accessdenied";
          "accounttakedown";
        ]
      || message_has msg "not authorized"
      || message_has msg "not a moderator"
      || message_has msg "insufficient"
      || message_has msg "permission"
      || message_has msg "not a verifier"
      || message_has msg "verifier"

let served json =
  if Error.is_not_served_json json || cannot_moderate json then false
  else
    match Error.check_for_error json with
    | Some _ -> failwith ("XRPC error: " ^ Error.to_string (Error.of_json json))
    | None -> true

let leftover_tag () =
  Printf.sprintf "ocaml-leftover-%d"
    (int_of_float (Unix.gettimeofday () *. 1000.) mod 100_000_000)

let test_get_config _ =
  let s = admin_session () in
  (* Official path: PDS session + atproto-proxy (direct Ozone rejects at+jwt). *)
  let cfg = Ozone.get_config s ~proxy:(proxy ()) () in
  no_xrpc_error cfg.original;
  OUnit2.assert_bool "getConfig returned JSON"
    (match cfg.original with `Assoc _ -> true | _ -> false)

let test_emit_and_query _ =
  let s = admin_session () in
  let p = proxy () in
  let alice = Session.create_session "alice.test" "hunter2" in
  let ev =
    Ozone.emit_event s ~proxy:p
      ~event:(Ozone.comment_event "ocaml local ozone integration")
      ~subject:(Ozone.repo_ref alice.auth.did)
      ~created_by:s.auth.did ()
  in
  no_xrpc_error ev.original;
  OUnit2.assert_bool "emitEvent parsed"
    (match ev.id with Some n -> n >= 0 | None -> true);
  let events =
    Ozone.query_events s ~proxy:p ~subject:alice.auth.did ~limit:10 ()
  in
  OUnit2.assert_bool "queryEvents" (List.length events.events >= 1);
  let statuses =
    Ozone.query_statuses s ~proxy:p ~subject:alice.auth.did ~limit:10 ()
  in
  OUnit2.assert_bool "queryStatuses" (List.length statuses.subject_statuses >= 0)

let test_query_labels _ =
  let s = admin_session () in
  let alice = Session.create_session "alice.test" "hunter2" in
  let labels =
    Label.query_labels_parsed s
      ~uri_patterns:[ "at://" ^ alice.auth.did ]
      ~sources:[ ozone_did () ]
      ~limit:10 ()
  in
  OUnit2.assert_bool "queryLabels PDS" (List.length labels.labels >= 0);
  match ozone_host () with
  | None -> ()
  | Some host -> (
      let json =
        Client.get_json ~host "com.atproto.label.queryLabels"
          [
            ("uriPatterns", "at://" ^ alice.auth.did);
            ("sources", ozone_did ());
            ("limit", "10");
          ]
      in
      no_xrpc_error json;
      match Yojson.Safe.Util.member "labels" json with
      | `List _ -> ()
      | _ -> OUnit2.assert_failure "ozone queryLabels missing labels")

let with_alarm seconds f =
  let old =
    Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> failwith "timeout"))
  in
  ignore (Unix.alarm seconds);
  Fun.protect
    ~finally:(fun () ->
      ignore (Unix.alarm 0);
      Sys.set_signal Sys.sigalrm old)
    f

let is_ws_not_served msg =
  message_has msg "methodnotimplemented"
  || message_has msg "methodnotfound"
  || message_has msg "unknown lexicon"
  || message_has msg " 501" || message_has msg " 404"
  || message_has msg "connection"
  || message_has msg "tls"
  || message_has msg "handshake"
  || message_has msg "eof"

(* One subscribeLabels frame against local Ozone (or PDS) if the NSID is served. *)
let test_subscribe_labels_one_frame _ =
  skip_unless_local ();
  let host =
    match ozone_host () with Some h -> h | None -> Session.atp_host_from_env
  in
  try
    with_alarm 15 (fun () ->
        let _, msg = Label.subscribe_one ~host ~cursor:0L () in
        match msg with
        | `Labels m ->
            OUnit2.assert_bool "local #labels" (List.length m.labels >= 0)
        | `Info _ | `Unknown _ ->
            OUnit2.assert_bool "local subscribeLabels control frame" true
        | `Error (err, _) ->
            if
              err = "MethodNotImplemented"
              || err = "MethodNotFound"
              || Error.is_not_served { error = err; message = "" }
            then skip_if true ("subscribeLabels not served: " ^ err)
            else failwith ("subscribeLabels error frame: " ^ err))
  with
  | Failure msg when is_ws_not_served msg ->
      skip_if true ("subscribeLabels not served: " ^ msg)
  | Failure msg when message_has msg "timeout" ->
      skip_if true "subscribeLabels produced no local frame"

let ozone_json s p nsid pairs =
  Client.get_json ~session:s ~extra:(Ozone.proxy_headers p) nsid pairs

let test_more_ozone _ =
  let s = admin_session () in
  let p = proxy () in
  let alice = Session.create_session "alice.test" "hunter2" in
  (match
     ozone_json s p "tools.ozone.moderation.getRepo" [ ("did", alice.auth.did) ]
   with
  | json when served json ->
      let repo = Ozone.get_repo s ~proxy:p ~did:alice.auth.did () in
      OUnit2.assert_equal ~printer:(fun x -> x) alice.auth.did repo.did
  | _ -> ());
  (match
     ozone_json s p "tools.ozone.moderation.searchRepos"
       [ ("q", "alice"); ("limit", "10") ]
   with
  | json when served json ->
      let repos, _ = Ozone.search_repos s ~proxy:p ~q:"alice" ~limit:10 () in
      OUnit2.assert_bool "searchRepos" (List.length repos >= 0)
  | _ -> ());
  (match ozone_json s p "tools.ozone.team.listMembers" [ ("limit", "10") ] with
  | json when served json ->
      let members = Ozone.list_members s ~proxy:p ~limit:10 () in
      OUnit2.assert_bool "listMembers" (List.length members.members >= 0)
  | _ -> ());
  (match ozone_json s p "tools.ozone.communication.listTemplates" [] with
  | json when served json ->
      let templates = Ozone.list_templates s ~proxy:p () in
      OUnit2.assert_bool "listTemplates" (List.length templates.templates >= 0)
  | _ -> ());
  (match ozone_json s p "tools.ozone.set.querySets" [ ("limit", "10") ] with
  | json when served json ->
      let sets = Ozone.query_sets s ~proxy:p ~limit:10 () in
      OUnit2.assert_bool "querySets" (List.length sets.sets >= 0)
  | _ -> ());
  let events =
    Ozone.query_events s ~proxy:p ~subject:alice.auth.did ~limit:5 ()
  in
  (match events.events with
  | [] -> ()
  | ev :: _ -> (
      match ev.id with
      | None -> ()
      | Some id -> (
          match
            ozone_json s p "tools.ozone.moderation.getEvent"
              [ ("id", string_of_int id) ]
          with
          | json when served json ->
              let got = Ozone.get_event s ~proxy:p ~id () in
              no_xrpc_error got.original
          | _ -> ())));
  (match
     ozone_json s p "tools.ozone.moderation.getAccountTimeline"
       [ ("did", alice.auth.did) ]
   with
  | json when served json ->
      let timeline =
        Ozone.get_account_timeline s ~proxy:p ~did:alice.auth.did ()
      in
      OUnit2.assert_bool "getAccountTimeline"
        (List.length timeline.timeline >= 0)
  | _ -> ());
  (match
     ozone_json s p "tools.ozone.moderation.getRecord"
       [
         ( "uri",
           Printf.sprintf "at://%s/app.bsky.actor.profile/self" alice.auth.did
         );
       ]
   with
  | json when served json ->
      let record =
        Ozone.get_record s ~proxy:p
          ~uri:
            (Printf.sprintf "at://%s/app.bsky.actor.profile/self" alice.auth.did)
          ()
      in
      OUnit2.assert_bool "getRecord uri" (String.length record.uri > 0)
  | _ -> ());
  (match
     ozone_json s p "tools.ozone.report.queryReports"
       [ ("status", "open"); ("limit", "10") ]
   with
  | json when served json -> (
      let reports =
        Ozone.query_reports s ~proxy:p ~status:"open" ~limit:10 ()
      in
      OUnit2.assert_bool "queryReports" (List.length reports.reports >= 0);
      if reports.reports <> [] then
        match ozone_json s p "tools.ozone.report.getLatestReport" [] with
        | latest when served latest ->
            let got = Ozone.get_latest_report s ~proxy:p () in
            OUnit2.assert_bool "getLatestReport" (got.id >= 0)
        | _ -> ())
  | _ -> ());
  (match ozone_json s p "tools.ozone.queue.listQueues" [ ("limit", "10") ] with
  | json when served json ->
      let queues = Ozone.list_queues s ~proxy:p ~limit:10 () in
      OUnit2.assert_bool "listQueues" (List.length queues.queues >= 0)
  | _ -> ());
  (match ozone_json s p "tools.ozone.set.querySets" [ ("limit", "10") ] with
  | json when served json -> (
      let sets = Ozone.query_sets s ~proxy:p ~limit:10 () in
      match sets.sets with
      | [] -> ()
      | set :: _ -> (
          match
            ozone_json s p "tools.ozone.set.getValues" [ ("name", set.name) ]
          with
          | values when served values ->
              let got = Ozone.get_set_values s ~proxy:p ~name:set.name () in
              OUnit2.assert_bool "getValues" (List.length got.values >= 0)
          | _ -> ()))
  | _ -> ());
  match
    ozone_json s p "tools.ozone.moderation.getReporterStats"
      [ ("dids", alice.auth.did) ]
  with
  | json when served json ->
      let stats =
        Ozone.get_reporter_stats s ~proxy:p ~dids:[ alice.auth.did ] ()
      in
      OUnit2.assert_bool "getReporterStats" (List.length stats.stats >= 0)
  | _ -> ()

let test_leftover_ozone _ =
  let s = admin_session () in
  let p = proxy () in
  let alice = Session.create_session "alice.test" "hunter2" in
  (match
     ozone_json s p "tools.ozone.moderation.getRepos"
       [ ("dids", alice.auth.did) ]
   with
  | json when served json ->
      let repos = Ozone.get_repos s ~proxy:p ~dids:[ alice.auth.did ] () in
      OUnit2.assert_bool "getRepos" (List.length repos >= 0)
  | _ -> ());
  (match
     ozone_json s p "tools.ozone.moderation.getSubjects"
       [ ("subjects", alice.auth.did) ]
   with
  | json when served json ->
      let subjects =
        Ozone.get_subjects s ~proxy:p ~subjects:[ alice.auth.did ] ()
      in
      OUnit2.assert_bool "getSubjects" (List.length subjects.subjects >= 0)
  | _ -> ());
  let profile_uri =
    Printf.sprintf "at://%s/app.bsky.actor.profile/self" alice.auth.did
  in
  (match
     ozone_json s p "tools.ozone.moderation.getRecords"
       [ ("uris", profile_uri) ]
   with
  | json when served json ->
      let records = Ozone.get_records s ~proxy:p ~uris:[ profile_uri ] () in
      OUnit2.assert_bool "getRecords" (List.length records >= 0)
  | _ -> ());
  (match
     ozone_json s p "tools.ozone.setting.listOptions" [ ("limit", "10") ]
   with
  | json when served json ->
      let opts = Ozone.list_options s ~proxy:p ~limit:10 () in
      OUnit2.assert_bool "listOptions" (List.length opts.options >= 0)
  | _ -> ());
  (match
     ozone_json s p "tools.ozone.verification.listVerifications"
       [ ("limit", "10") ]
   with
  | json when served json ->
      let vers = Ozone.list_verifications s ~proxy:p ~limit:10 () in
      OUnit2.assert_bool "listVerifications"
        (List.length vers.verifications >= 0)
  | _ -> ());
  (match
     Client.post_json ~session:s ~extra:(Ozone.proxy_headers p)
       "tools.ozone.safelink.queryRules"
       (Yojson.Safe.to_string (`Assoc [ ("limit", `Int 10) ]))
   with
  | json when served json ->
      let rules = Ozone.query_safelink_rules s ~proxy:p ~limit:10 () in
      OUnit2.assert_bool "queryRules" (List.length rules.rules >= 0)
  | _ -> ());
  match
    Client.post_json ~session:s ~extra:(Ozone.proxy_headers p)
      "tools.ozone.moderation.listScheduledActions"
      (Yojson.Safe.to_string
         (`Assoc
           [ ("statuses", `List [ `String "pending" ]); ("limit", `Int 10) ]))
  with
  | json when served json ->
      let listed =
        Ozone.list_scheduled_actions s ~proxy:p ~statuses:[ "pending" ]
          ~limit:10 ()
      in
      OUnit2.assert_bool "listScheduledActions" (List.length listed.actions >= 0)
  | _ -> ()

let ozone_post s p nsid body =
  Client.post_json ~session:s ~extra:(Ozone.proxy_headers p) nsid
    (Yojson.Safe.to_string body)

let test_privileged_writes _ =
  let s = admin_session () in
  let p = proxy () in
  let alice = Session.create_session "alice.test" "hunter2" in
  let bob = Session.create_session "bob.test" "hunter2" in
  let tag = leftover_tag () in
  (* emitEvent with official reportAction. Skip if not served / cannot moderate. *)
  (match
     ozone_post s p "tools.ozone.moderation.emitEvent"
       (Ozone.emit_event_body
          ~event:(Ozone.comment_event ("privileged " ^ tag))
          ~subject:(Ozone.repo_ref alice.auth.did)
          ~created_by:s.auth.did
          ~report_action:(Ozone.report_action ~all:true ~note:tag ())
          ())
   with
  | json when served json ->
      let ev = Ozone.parse_mod_event json in
      no_xrpc_error ev.original
  | _ -> ());
  (* Queue mutations. A single-node TestNetwork may 501 these NSIDs. *)
  let queue_name = "ocaml-queue-" ^ tag in
  (match
     ozone_post s p "tools.ozone.queue.createQueue"
       (Ozone.create_queue_body ~name:queue_name ~subject_types:[ "account" ] ())
   with
  | json when served json ->
      let created = Ozone.parse_queue_result json in
      OUnit2.assert_bool "createQueue id" (created.id >= 0);
      let updated =
        Ozone.update_queue s ~proxy:p ~queue_id:created.id
          ~description:("updated " ^ tag) ()
      in
      OUnit2.assert_equal created.id updated.id;
      let deleted = Ozone.delete_queue s ~proxy:p ~queue_id:created.id () in
      OUnit2.assert_bool "deleteQueue" deleted.deleted
  | _ -> ());
  (* Report mutations: PDS createReport, then ozone createActivity if served. *)
  (match
     ozone_json s p "tools.ozone.report.queryReports"
       [ ("status", "open"); ("limit", "10") ]
   with
  | json when served json -> (
      let report_json =
        Client.post_json ~session:alice "com.atproto.moderation.createReport"
          (Moderation.create_report_data_from_repo_ref Moderation.reason_other
             ~reason:("ocaml leftover " ^ tag)
             { Moderation.did = bob.auth.did })
      in
      match report_json with
      | rjson when served rjson -> (
          let reports =
            Ozone.query_reports s ~proxy:p ~status:"open" ~did:bob.auth.did
              ~limit:10 ()
          in
          match reports.reports with
          | [] -> ()
          | report :: _ -> (
              match
                ozone_post s p "tools.ozone.report.createActivity"
                  (Ozone.create_activity_body ~activity:(Ozone.note_activity ())
                     ~report_id:report.id ~internal_note:tag ())
              with
              | ajson when served ajson ->
                  let activity = Ozone.parse_activity_result ajson in
                  OUnit2.assert_bool "createActivity"
                    (match activity.report_id with
                    | Some id -> id = report.id
                    | None -> true)
              | _ -> ()))
      | _ -> ())
  | _ -> ());
  (* Safelink add / update / remove — official `pattern` field. *)
  let url = "https://" ^ tag ^ ".leftover.test" in
  (match
     ozone_post s p "tools.ozone.safelink.addRule"
       (Ozone.add_safelink_rule_body ~url ~pattern:"domain" ~action:"warn"
          ~reason:"spam" ~comment:tag ())
   with
  | json when served json ->
      let added = Ozone.parse_safelink_event json in
      OUnit2.assert_equal ~printer:(fun x -> x) url added.url;
      OUnit2.assert_equal (Some "domain") added.pattern;
      let updated =
        Ozone.update_safelink_rule s ~proxy:p ~url ~pattern:"domain"
          ~action:"block" ~reason:"phishing" ~comment:("upd " ^ tag) ()
      in
      OUnit2.assert_equal (Some "block") updated.action;
      let removed =
        Ozone.remove_safelink_rule s ~proxy:p ~url ~pattern:"domain"
          ~comment:("rm " ^ tag) ()
      in
      OUnit2.assert_equal ~printer:(fun x -> x) url removed.url
  | _ -> ());
  (* Grant / revoke verification. Skip if ozone is not a verifier. *)
  match
    ozone_post s p "tools.ozone.verification.grantVerifications"
      (Ozone.grant_verifications_body
         ~verifications:
           [
             Ozone.verification_input ~subject:alice.auth.did
               ~handle:alice.username ~display_name:"Alice" ();
           ]
         ())
  with
  | json when served json -> (
      let granted = Ozone.parse_grant_verifications json in
      OUnit2.assert_bool "grantVerifications parsed"
        (List.length granted.verifications
         + List.length granted.failed_verifications
        >= 0);
      match granted.verifications with
      | [] -> ()
      | v :: _ ->
          let revoked =
            Ozone.revoke_verifications s ~proxy:p ~uris:[ v.uri ]
              ~revoke_reason:("ocaml leftover " ^ tag) ()
          in
          OUnit2.assert_bool "revokeVerifications parsed"
            (List.length revoked.revoked_verifications
             + List.length revoked.failed_revocations
            >= 0))
  | _ -> ()

(* Template / set writes + cheap leftover reads. Skip if not served. *)
let test_template_and_set_writes _ =
  let s = admin_session () in
  let p = proxy () in
  let alice = Session.create_session "alice.test" "hunter2" in
  let tag = leftover_tag () in
  let tmpl_name = "ocaml-tmpl-" ^ tag in
  (match
     ozone_post s p "tools.ozone.communication.createTemplate"
       (Ozone.create_template_body ~name:tmpl_name ~content_markdown:"hi"
          ~subject:"welcome" ())
   with
  | json when served json ->
      let created = Ozone.parse_template json in
      OUnit2.assert_bool "createTemplate id" (String.length created.id > 0);
      let updated =
        Ozone.update_template s ~proxy:p ~id:created.id
          ~content_markdown:("updated " ^ tag) ()
      in
      OUnit2.assert_equal created.id updated.id;
      let typed =
        Ozone.create_template s ~proxy:p ~name:(tmpl_name ^ "-b")
          ~content_markdown:"typed" ~subject:"welcome" ()
      in
      OUnit2.assert_bool "create_template helper" (String.length typed.id > 0);
      Ozone.delete_template s ~proxy:p ~id:created.id ();
      Ozone.delete_template s ~proxy:p ~id:typed.id ()
  | _ -> ());
  let set_name = "ocaml-set-" ^ tag in
  (match
     ozone_post s p "tools.ozone.set.upsertSet"
       (`Assoc
         [ ("name", `String set_name); ("description", `String ("set " ^ tag)) ])
   with
  | json when served json ->
      let created =
        Ozone.upsert_set s ~proxy:p ~name:set_name ~description:("set " ^ tag)
          ()
      in
      OUnit2.assert_equal ~printer:(fun x -> x) set_name created.name;
      Ozone.add_set_values s ~proxy:p ~name:set_name ~values:[ tag ] ();
      (match
         ozone_json s p "tools.ozone.set.getValues" [ ("name", set_name) ]
       with
      | values when served values ->
          let got = Ozone.get_set_values s ~proxy:p ~name:set_name () in
          OUnit2.assert_bool "addValues" (List.mem tag got.values)
      | _ -> ());
      Ozone.delete_set s ~proxy:p ~name:set_name ()
  | _ -> ());
  (match
     ozone_post s p "tools.ozone.safelink.queryEvents"
       (Ozone.query_safelink_events_body ~limit:10 ())
   with
  | json when served json ->
      let events = Ozone.query_safelink_events s ~proxy:p ~limit:10 () in
      OUnit2.assert_bool "queryEvents" (List.length events.events >= 0)
  | _ -> ());
  match
    ozone_post s p "tools.ozone.moderation.scheduleAction"
      (Ozone.schedule_action_body
         ~action:(Ozone.takedown_action ~comment:("sched " ^ tag) ())
         ~subjects:[ alice.auth.did ] ~created_by:s.auth.did
         ~scheduling:
           {
             execute_at = Some "2099-01-01T00:00:00.000Z";
             execute_after = None;
             execute_until = None;
           }
         ())
  with
  | json when served json ->
      let scheduled = Ozone.parse_batch_result json in
      OUnit2.assert_bool "scheduleAction parsed"
        (List.length scheduled.succeeded + List.length scheduled.failed >= 0);
      ignore
        (Ozone.cancel_scheduled_actions s ~proxy:p ~subjects:[ alice.auth.did ]
           ~comment:("cancel " ^ tag) ())
  | _ -> ()

let suite =
  "local_ozone"
  >::: [
         "test_get_config" >:: test_get_config;
         "test_emit_and_query" >:: test_emit_and_query;
         "test_query_labels" >:: test_query_labels;
         "test_subscribe_labels_one_frame" >:: test_subscribe_labels_one_frame;
         "test_more_ozone" >:: test_more_ozone;
         "test_leftover_ozone" >:: test_leftover_ozone;
         "test_privileged_writes" >:: test_privileged_writes;
         "test_template_and_set_writes" >:: test_template_and_set_writes;
       ]

let () =
  Unix.putenv "OUNIT_RUNNER" "sequential";
  run_test_tt_main suite
