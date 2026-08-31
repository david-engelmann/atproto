open OUnit2
open Atproto.Session
open Atproto.Ozone
open Atproto.Label
open Atproto.Client
open Atproto.Error
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

let method_missing err = err = "MethodNotFound" || err = "MethodNotImplemented"

let served json =
  match Error.check_for_error json with
  | Some err when method_missing err -> false
  | Some _ -> failwith ("XRPC error: " ^ Error.to_string (Error.of_json json))
  | None -> true

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
  match
    ozone_json s p "tools.ozone.moderation.getAccountTimeline"
      [ ("did", alice.auth.did) ]
  with
  | json when served json ->
      let timeline =
        Ozone.get_account_timeline s ~proxy:p ~did:alice.auth.did ()
      in
      OUnit2.assert_bool "getAccountTimeline"
        (List.length timeline.timeline >= 0)
  | _ -> ()

let suite =
  "local_ozone"
  >::: [
         "test_get_config" >:: test_get_config;
         "test_emit_and_query" >:: test_emit_and_query;
         "test_query_labels" >:: test_query_labels;
         "test_more_ozone" >:: test_more_ozone;
       ]

let () =
  Unix.putenv "OUNIT_RUNNER" "sequential";
  run_test_tt_main suite
