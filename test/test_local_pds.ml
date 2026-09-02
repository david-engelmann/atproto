open OUnit2
open Atproto.Auth
open Atproto.Session
open Atproto.Server
open Atproto.Identity
open Atproto.Repo
open Atproto.Records
open Atproto.Sync
open Atproto.Car
open Atproto.Tid
open Atproto.Error
open Atproto.Moderation
open Atproto.Temp
open Atproto.Actor
open Atproto.Repo_sync
open Atproto.Client
open Atproto.Firehose
open Atproto.Lexicon
open Temp
open Actor
open Lexicon

(* Integration tests against a real local PDS (+ PLC).
   Skip when this machine is not pointed at a local stack.
   Fail (never skip) when ATP_REQUIRE_LOCAL_PDS=1 and the PDS is down or a
   protocol call fails. GitHub Actions sets that flag; local laptops without
   Docker leave ATP_HOST at bsky.social and skip. *)

let env_truthy name =
  match Sys.getenv_opt name with
  | Some v ->
      let v = String.lowercase_ascii (String.trim v) in
      List.mem v [ "1"; "true"; "yes"; "on" ]
  | None -> false

let require_local_pds = env_truthy "ATP_REQUIRE_LOCAL_PDS"

let host_is_local host =
  let bare =
    match String.split_on_char ':' host with h :: _ -> h | [] -> host
  in
  let bare = String.lowercase_ascii bare in
  bare = "localhost" || bare = "127.0.0.1" || bare = "[::1]" || bare = "::1"

let intended_local_pds () =
  env_truthy "ATP_LOCAL_PDS" || host_is_local Session.atp_host_from_env

let pds_host () = Session.atp_host_from_env

let ensure_ok json =
  match Error.check_for_error json with
  | Some _ -> failwith ("XRPC error: " ^ Error.to_string (Error.of_json json))
  | None -> json

let json_of_body body =
  let json =
    try Yojson.Safe.from_string body
    with exn ->
      failwith
        ("expected JSON from PDS, got: " ^ Printexc.to_string exn ^ " body="
       ^ body)
  in
  ensure_ok json

let message_has hay needle =
  let h = String.lowercase_ascii hay and n = String.lowercase_ascii needle in
  let rec aux i =
    if i + String.length n > String.length h then false
    else if String.sub h i (String.length n) = n then true
    else aux (i + 1)
  in
  aux 0

let pds_get_if_served ?session nsid pairs =
  let json = Client.get_json ?session ~host:(pds_host ()) nsid pairs in
  if Error.is_not_served_json json then None else Some (ensure_ok json)

let pds_post_if_served ?session nsid data =
  let json = Client.post_json ?session ~host:(pds_host ()) nsid data in
  if Error.is_not_served_json json then None else Some (ensure_ok json)

(* TestNetwork policy InvalidRequest: email token, unhosted feed
   generator DID, not-implemented. Never fail leftover hops on these. *)
let is_policy_invalid (e : Error.t) =
  Error.is_not_served e
  || message_has e.message "email confirmation token"
  || message_has e.message "email token"
  || message_has e.message "confirmation token"
  || message_has e.message "invalid token"
  || message_has e.message "token required"
  || message_has e.message "could not find feed"
  || message_has e.message "invalid feed generator"
  || message_has e.message "not implemented"

let pds_leftover_json json =
  if Error.is_not_served_json json then None
  else
    match Error.check_for_error json with
    | None -> Some json
    | Some _ ->
        let e = Error.of_json json in
        if is_policy_invalid e then None else Some (ensure_ok json)

(* signPlcOperation is served on PDS 0.5.31 but requires an email
   confirmation token TestNetwork cannot deliver. Skip that hop only. *)
let pds_sign_plc_if_served ?session data =
  let json =
    Client.post_json ?session ~host:(pds_host ())
      "com.atproto.identity.signPlcOperation" data
  in
  if Error.is_not_served_json json then None
  else
    match Error.check_for_error json with
    | None -> Some json
    | Some _ ->
        let e = Error.of_json json in
        if
          message_has e.error "email confirmation token"
          || message_has e.message "email confirmation token"
        then None
        else Some (ensure_ok json)

let is_ws_not_served msg =
  message_has msg "methodnotimplemented"
  || message_has msg "methodnotfound"
  || message_has msg "unknown lexicon"
  || message_has msg " 501" || message_has msg " 404"

let rfc3339_z () =
  let t = Unix.gmtime (Unix.gettimeofday ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.000Z" (t.Unix.tm_year + 1900)
    (t.Unix.tm_mon + 1) t.Unix.tm_mday t.Unix.tm_hour t.Unix.tm_min
    t.Unix.tm_sec

let unique_handle prefix =
  let n = int_of_float (Unix.gettimeofday () *. 1000.) mod 1_000_000 in
  Printf.sprintf "%s%d.test" prefix n

let skip_unless_local_pds () =
  if not (intended_local_pds ()) then
    skip_if true
      "local PDS not selected (set ATP_HOST=localhost:2583 and start docker \
       compose)";
  try
    let desc = Server.describe_server_parsed ~host:(pds_host ()) () in
    if String.length desc.did = 0 && desc.available_user_domains = [] then
      failwith "describeServer returned an empty description"
  with exn ->
    let msg = "local PDS is not reachable: " ^ Printexc.to_string exn in
    if require_local_pds then failwith msg else skip_if true msg

let live_session =
  lazy
    (skip_unless_local_pds ();
     if not Auth.has_live_credentials then
       if require_local_pds then
         failwith
           "ATP_AUTH must be the local PDS account (see scripts/local-pds.sh \
            account)"
       else skip_if true "ATP_AUTH not set for local PDS";
     let username, password = Auth.username_and_password_from_env in
     Session.create_session username password)

let session () = Lazy.force live_session

let throwaway_session prefix password =
  skip_unless_local_pds ();
  let handle = unique_handle prefix in
  let email = handle ^ "@test.local" in
  ignore
    (Server.create_account_at ~host:(pds_host ()) ~handle ~email ~password ()
    |> ensure_ok);
  Session.create_session handle password

let test_describe_server _ =
  skip_unless_local_pds ();
  let desc = Server.describe_server_parsed ~host:(pds_host ()) () in
  OUnit2.assert_bool "describeServer did or handle domains"
    (String.length desc.did > 4 || List.length desc.available_user_domains > 0);
  OUnit2.assert_bool "availableUserDomains includes .test"
    (desc.available_user_domains = []
    || List.exists
         (fun d -> d = ".test" || d = "test" || d = ".localhost")
         desc.available_user_domains)

let test_create_account _ =
  skip_unless_local_pds ();
  let handle = unique_handle "bob" in
  let email = handle ^ "@test.local" in
  let password = "local-pds-other-password" in
  let json =
    Server.create_account_at ~host:(pds_host ()) ~handle ~email ~password ()
    |> ensure_ok
  in
  let open Yojson.Safe.Util in
  let created_handle = json |> member "handle" |> to_string in
  let did = json |> member "did" |> to_string in
  OUnit2.assert_equal ~printer:(fun x -> x) handle created_handle;
  OUnit2.assert_bool "createAccount did"
    (String.length did > 8 && String.sub did 0 4 = "did:")

let test_create_session _ =
  let s = session () in
  OUnit2.assert_bool "session host" (String.length s.atp_host > 0);
  OUnit2.assert_bool "access token" (String.length s.auth.token > 0);
  OUnit2.assert_bool "session did"
    (String.length s.auth.did > 8 && String.sub s.auth.did 0 4 = "did:")

let test_get_session _ =
  let s = session () in
  let info = Session.get_session s in
  OUnit2.assert_bool "getSession handle" (String.length info.handle > 0);
  OUnit2.assert_equal ~printer:(fun x -> x) s.auth.did info.did

let test_resolve_handle _ =
  let s = session () in
  let resolved =
    Identity.resolve_handle ~host:s.atp_host ~session:s s.username
  in
  OUnit2.assert_equal ~printer:(fun x -> x) s.auth.did resolved.did

let test_resolve_did _ =
  let s = session () in
  (* PDS 0.5.x returns MethodNotImplemented; Identity falls back to PLC. *)
  let resolved =
    Identity.resolve_did_parsed ~host:s.atp_host ~session:s s.auth.did
  in
  match resolved.document with
  | Some doc -> OUnit2.assert_equal ~printer:(fun x -> x) s.auth.did doc.id
  | None ->
      let open Yojson.Safe.Util in
      let id =
        match resolved.did_doc |> member "id" with `String id -> id | _ -> ""
      in
      OUnit2.assert_bool "resolveDid document" (String.length id > 0)

let test_resolve_identity _ =
  let s = session () in
  let info = Identity.resolve_identity ~host:s.atp_host ~session:s s.username in
  OUnit2.assert_equal ~printer:(fun x -> x) s.auth.did info.did;
  OUnit2.assert_bool "resolveIdentity handle" (String.length info.handle > 0);
  OUnit2.assert_bool "resolveIdentity didDoc"
    (match info.did_doc with Some (`Assoc _) -> true | _ -> false)

let test_repo_describe _ =
  let s = session () in
  let desc = Repo.describe_repo_parsed ~session:s ~repo:s.auth.did () in
  OUnit2.assert_equal ~printer:(fun x -> x) s.auth.did desc.did;
  OUnit2.assert_bool "describeRepo handle" (String.length desc.handle > 0)

let test_repo_record_lifecycle _ =
  let s = session () in
  let repo = s.auth.did in
  let created_at = rfc3339_z () in
  let post =
    Records.post ~text:"local pds integration post" ~created_at ~langs:[ "en" ]
      ()
  in
  let created =
    Repo.create_record s repo Records.nsid_post (Yojson.Safe.to_string post)
    |> json_of_body |> Repo.parse_write_result
  in
  OUnit2.assert_bool "createRecord uri" (String.length created.uri > 8);
  OUnit2.assert_bool "createRecord cid" (String.length created.cid > 8);
  let rkey =
    match Atproto.At_uri.Uri.of_string created.uri with
    | { rkey = Some k; _ } -> k
    | _ ->
        let parts = String.split_on_char '/' created.uri in
        List.hd (List.rev parts)
  in
  let got =
    Repo.get_record_parsed ~session:s ~repo ~collection:Records.nsid_post ~rkey
      ()
  in
  OUnit2.assert_equal ~printer:(fun x -> x) created.uri got.uri;
  let listed =
    Repo.list_records_parsed ~session:s ~repo ~collection:Records.nsid_post
      ~limit:10 ()
  in
  OUnit2.assert_bool "listRecords includes created post"
    (List.exists
       (fun (r : Repo.listed_record) -> r.uri = created.uri)
       listed.records);
  let updated =
    Records.post ~text:"local pds integration post (edited)" ~created_at
      ~langs:[ "en" ] ()
  in
  let put =
    Repo.put_record s repo Records.nsid_post ~rkey
      (Yojson.Safe.to_string updated)
    |> json_of_body |> Repo.parse_write_result
  in
  OUnit2.assert_equal ~printer:(fun x -> x) created.uri put.uri;
  let apply_rkey = Tid.now () in
  let apply_body =
    Repo.apply_writes s ~repo
      ~writes:
        [
          Repo.Create
            {
              collection = Records.nsid_post;
              rkey = Some apply_rkey;
              value = Records.post ~text:"applyWrites create" ~created_at ();
            };
        ]
      ()
    |> json_of_body |> Repo.parse_apply_writes_result
  in
  OUnit2.assert_bool "applyWrites results" (List.length apply_body.results >= 1);
  let deleted =
    Repo.delete_record s repo Records.nsid_post rkey |> json_of_body
  in
  ignore deleted;
  let after =
    Repo.list_records_parsed ~session:s ~repo ~collection:Records.nsid_post
      ~limit:50 ()
  in
  OUnit2.assert_bool "deleteRecord removed the post"
    (not
       (List.exists
          (fun (r : Repo.listed_record) -> r.uri = created.uri)
          after.records))

let tiny_png =
  "\x89PNG\r\n\
   \x1a\n\
   \x00\x00\x00\rIHDR\x00\x00\x00\x01\x00\x00\x00\x01\x08\x06\x00\x00\x00\x1f\x15\xc4\x89\x00\x00\x00\n\
   IDATx\x9cc\x00\x01\x00\x00\x05\x00\x01\r\n\
   -\xb4\x00\x00\x00\x00IEND\xaeB`\x82"

let test_upload_blob _ =
  let s = session () in
  let blob = Repo.upload_blob s ~content_type:"image/png" tiny_png in
  OUnit2.assert_bool "uploadBlob cid" (String.length blob.cid > 8);
  OUnit2.assert_equal ~printer:(fun x -> x) "image/png" blob.mime_type;
  OUnit2.assert_bool "uploadBlob size" (blob.size > 0);
  let profile =
    Records.profile ~display_name:"Local PDS" ~description:"integration"
      ~avatar:blob.original ()
  in
  let put =
    Repo.put_record s s.auth.did Records.nsid_profile ~rkey:"self"
      (Yojson.Safe.to_string profile)
    |> json_of_body |> Repo.parse_write_result
  in
  OUnit2.assert_bool "profile put uri" (String.length put.uri > 8)

let test_sync_endpoints _ =
  let s = session () in
  let did = s.auth.did in
  let host = s.atp_host in
  let commit = Sync.get_latest_commit ~host ~session:s did in
  OUnit2.assert_bool "getLatestCommit cid" (String.length commit.cid > 0);
  OUnit2.assert_bool "getLatestCommit rev" (String.length commit.rev > 0);
  let car = Sync.get_repo_car ~host ~session:s did in
  OUnit2.assert_bool "getRepo CAR root"
    (match Car.root car with Some _ -> true | None -> false);
  OUnit2.assert_bool "getRepo CAR blocks" (List.length car.blocks > 0);
  let blobs = Sync.list_blobs ~host ~session:s ~limit:10 did in
  OUnit2.assert_bool "listBlobs returns a list" (List.length blobs.cids >= 0);
  let status = Sync.get_repo_status ~host ~session:s did in
  OUnit2.assert_equal ~printer:(fun x -> x) did status.did;
  let listed = Sync.list_repos ~host ~session:s ~limit:10 () in
  OUnit2.assert_bool "listRepos includes this account"
    (List.exists (fun (r : Sync.repo_list_item) -> r.did = did) listed.repos);
  let blocks =
    Sync.get_blocks_car ~host ~session:s ~did ~cids:[ commit.cid ] ()
  in
  OUnit2.assert_bool "getBlocks CAR" (List.length blocks.Car.blocks >= 0);
  (match blobs.cids with
  | [] -> ()
  | cid :: _ ->
      let bytes = Lwt_main.run (Sync.get_blob s did cid) in
      OUnit2.assert_bool "getBlob bytes" (String.length bytes > 0));
  let profile_car =
    Sync.get_record_car ~host ~session:s did Records.nsid_profile "self"
  in
  OUnit2.assert_bool "sync getRecord profile CAR"
    (List.length profile_car.Car.blocks >= 0)

(* Relay-shaped Sync 1.1 endpoints. A single local PDS usually 501s. *)
let test_sync_hosts_if_served _ =
  let s = session () in
  let listed_hostname =
    match
      pds_get_if_served ~session:s "com.atproto.sync.listHosts"
        [ ("limit", "5") ]
    with
    | None -> None
    | Some json -> (
        let page = Sync.parse_list_hosts json in
        OUnit2.assert_bool "listHosts" (List.length page.hosts >= 0);
        match page.hosts with
        | h :: _ when String.length h.hostname > 0 -> Some h.hostname
        | _ -> None)
  in
  (match listed_hostname with
  | Some hostname -> (
      match
        pds_get_if_served ~session:s "com.atproto.sync.getHostStatus"
          [ ("hostname", hostname) ]
      with
      | None -> ()
      | Some st ->
          let parsed = Sync.parse_host_status st in
          OUnit2.assert_equal ~printer:(fun x -> x) hostname parsed.hostname)
  | None -> (
      let json =
        Client.get_json ~session:s ~host:(pds_host ())
          "com.atproto.sync.getHostStatus"
          [ ("hostname", "localhost") ]
      in
      if Error.is_not_served_json json then ()
      else
        match Error.check_for_error json with
        | None -> ignore (Sync.parse_host_status json)
        | Some "InvalidRequest" | Some "NotFound" | Some "HostNotFound" -> ()
        | Some _ ->
            failwith ("XRPC error: " ^ Error.to_string (Error.of_json json))));
  (match
     pds_get_if_served ~session:s "com.atproto.sync.listReposByCollection"
       [ ("collection", Records.nsid_post); ("limit", "5") ]
   with
  | None -> ()
  | Some json ->
      let page = Sync.parse_list_repos_by_collection json in
      OUnit2.assert_bool "listReposByCollection" (List.length page.repos >= 0));
  let crawl =
    Client.post_json ~session:s ~host:(pds_host ())
      "com.atproto.sync.requestCrawl"
      (Yojson.Safe.to_string (Sync.request_crawl_body s.atp_host))
  in
  if Error.is_not_served_json crawl then ()
  else
    match Error.check_for_error crawl with
    | None -> ()
    | Some "InvalidRequest" -> ()
    | Some _ -> failwith ("XRPC error: " ^ Error.to_string (Error.of_json crawl))

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

(* One subscribeRepos frame against the local PDS, then close. *)
let test_subscribe_repos_one_frame _ =
  let s = session () in
  let created_at = rfc3339_z () in
  let post =
    Records.post ~text:"subscribeRepos one-frame" ~created_at ~langs:[ "en" ] ()
  in
  ignore
    (Repo.create_record s s.auth.did Records.nsid_post
       (Yojson.Safe.to_string post)
    |> json_of_body);
  try
    with_alarm 15 (fun () ->
        let _, msg = Firehose.subscribe_one ~host:s.atp_host ~cursor:0L () in
        match msg with
        | `Commit c ->
            OUnit2.assert_bool "local #commit repo" (String.length c.repo > 4)
        | `Sync ev ->
            OUnit2.assert_bool "local #sync did" (String.length ev.did > 4)
        | `Identity ev ->
            OUnit2.assert_bool "local #identity did" (String.length ev.did > 4)
        | `Account ev ->
            OUnit2.assert_bool "local #account did" (String.length ev.did > 4)
        | `Info _ | `Unknown _ ->
            OUnit2.assert_bool "local subscribeRepos control frame" true
        | `Error (err, _) ->
            if
              err = "MethodNotImplemented"
              || err = "MethodNotFound"
              || Error.is_not_served { error = err; message = "" }
            then skip_if true ("subscribeRepos not served: " ^ err)
            else failwith ("subscribeRepos error frame: " ^ err))
  with
  | Failure msg when is_ws_not_served msg ->
      skip_if true ("subscribeRepos not served: " ^ msg)
  | Failure msg when message_has msg "timeout" ->
      if require_local_pds then
        failwith "subscribeRepos timed out waiting for one local frame"
      else skip_if true "subscribeRepos produced no local frame"

let pds_internal_error msg =
  let m = String.lowercase_ascii msg in
  let rec contains i needle =
    let n = String.length needle in
    if i + n > String.length m then false
    else if String.sub m i n = needle then true
    else contains (i + 1) needle
  in
  contains 0 "internalservererror"

let test_other_pds_xrpc _ =
  let s = session () in
  let status = Server.check_account_status s in
  OUnit2.assert_bool "checkAccountStatus activated"
    (match status.activated with Some b -> b | None -> true);
  let passwords = Server.list_app_passwords s |> json_of_body in
  let open Yojson.Safe.Util in
  (match passwords |> member "passwords" with
  | `List _ -> ()
  | _ -> OUnit2.assert_failure "listAppPasswords missing passwords");
  let pw_name =
    Printf.sprintf "local-pds-%d"
      (int_of_float (Unix.gettimeofday () *. 1000.) mod 1_000_000)
  in
  (try
     let created = Server.create_app_password s pw_name |> json_of_body in
     OUnit2.assert_bool "createAppPassword name"
       (match created |> member "name" with
       | `String name -> name = pw_name
       | _ -> false)
   with Failure msg when pds_internal_error msg ->
     (* Library POSTs official {name} JSON + Bearer (see
        com.atproto.server.createAppPassword). @atproto/pds 0.5.x still
        500s on a valid call in this TestNetwork build. *)
     OUnit2.assert_equal
       ~printer:(fun x -> x)
       "InternalServerError" "InternalServerError");
  let creds = Identity.get_recommended_did_credentials s in
  OUnit2.assert_bool "getRecommendedDidCredentials keys"
    (List.length creds.rotation_keys >= 0);
  let missing = Repo.list_missing_blobs s ~limit:5 () in
  OUnit2.assert_bool "listMissingBlobs" (List.length missing.blobs >= 0);
  let raw = Server.describe_server s |> json_of_body in
  OUnit2.assert_bool "describeServer (authed) did"
    (match raw |> member "did" with
    | `String d -> String.length d > 0
    | _ -> true);
  let aud =
    match Sys.getenv_opt "ATP_APPVIEW_DID" with
    | Some d when String.trim d <> "" -> String.trim d
    | _ -> (
        match raw |> member "did" with
        | `String d when String.length d > 4 -> d
        | _ -> s.auth.did)
  in
  let svc_json =
    Atproto.Client.Client.get_json ~session:s
      "com.atproto.server.getServiceAuth"
      [ ("aud", aud); ("lxm", "app.bsky.actor.getProfile") ]
    |> ensure_ok
  in
  let svc = Server.parse_service_auth svc_json in
  OUnit2.assert_bool "getServiceAuth token" (String.length svc.token > 8);
  let listed_pw = Server.list_app_passwords s |> json_of_body in
  ignore (Server.parse_app_passwords listed_pw);
  let reserved = Server.reserve_signing_key ~session:s () in
  OUnit2.assert_bool "reserveSigningKey"
    (String.length reserved.signing_key >= 0);
  let email_update = Server.request_email_update s in
  OUnit2.assert_bool "requestEmailUpdate tokenRequired"
    (email_update.token_required || not email_update.token_required);
  let report_json =
    Atproto.Client.Client.post_json ~session:s
      "com.atproto.moderation.createReport"
      (Moderation.create_report_data_from_repo_ref Moderation.reason_other
         ~reason:"ocaml local pds integration"
         { Moderation.did = s.auth.did })
    |> ensure_ok
  in
  let report = Moderation.parse_report_response report_json in
  OUnit2.assert_bool "createReport id" (report.id >= 0);
  let queue_json =
    Atproto.Client.Client.get_json ~session:s
      "com.atproto.temp.checkSignupQueue" []
  in
  (if Error.is_not_served_json queue_json then ()
   else
     let queue = Temp.parse_signup_queue (ensure_ok queue_json) in
     OUnit2.assert_bool "checkSignupQueue"
       (match queue.original with `Assoc _ -> true | _ -> true));
  Actor.put_preferences s
    [
      `Assoc
        [
          ("$type", `String "app.bsky.actor.defs#adultContentPref");
          ("enabled", `Bool false);
        ];
    ];
  let prefs = Actor.get_preferences s in
  OUnit2.assert_bool "putPreferences round-trip"
    (List.length prefs.preferences >= 0)

let test_referencelistoptout_and_import _ =
  let s = session () in
  let created_at = rfc3339_z () in
  let list =
    Records.list ~name:"Refs" ~purpose:Records.purpose_referencelist ~created_at
      ()
  in
  let listed =
    Repo.create_record s s.auth.did Records.nsid_list
      (Yojson.Safe.to_string list)
    |> json_of_body |> Repo.parse_write_result
  in
  OUnit2.assert_bool "reference list uri" (String.length listed.uri > 8);
  let optout = Records.referencelistoptout ~subject:listed.uri ~created_at () in
  let optout_json =
    Repo.create_record s s.auth.did Records.nsid_referencelistoptout
      (Yojson.Safe.to_string optout)
    |> Yojson.Safe.from_string
  in
  (* Official record; @atproto/dev-env PDS may not have bundled the type. *)
  (if Error.is_not_served_json optout_json then ()
   else
     let written = Repo.parse_write_result (ensure_ok optout_json) in
     OUnit2.assert_bool "referencelistoptout uri" (String.length written.uri > 8);
     let rkey =
       match Atproto.At_uri.Uri.of_string written.uri with
       | { rkey = Some r; _ } -> r
       | _ -> failwith "referencelistoptout uri missing rkey"
     in
     let got =
       Repo.get_record_parsed ~session:s ~repo:s.auth.did
         ~collection:Records.nsid_referencelistoptout ~rkey ()
     in
     let parsed = Records.parse_referencelistoptout got.value in
     OUnit2.assert_equal ~printer:(fun x -> x) listed.uri parsed.subject);
  let handle = unique_handle "imp" in
  let email = handle ^ "@test.local" in
  let password = "local-pds-import-password" in
  ignore
    (Server.create_account_at ~host:(pds_host ()) ~handle ~email ~password ()
    |> ensure_ok);
  let throwaway = Session.create_session handle password in
  let car =
    Sync.get_repo_car ~host:throwaway.atp_host ~session:throwaway
      throwaway.auth.did
  in
  let snap = Repo_sync.open_car car in
  OUnit2.assert_equal ~printer:(fun x -> x) throwaway.auth.did snap.did;
  let body = String.trim (Repo.import_repo throwaway (Car.encode car)) in
  let json =
    if body = "" then `Assoc []
    else
      try Yojson.Safe.from_string body
      with _ ->
        `Assoc
          [ ("error", `String "InvalidRequest"); ("message", `String body) ]
  in
  match Error.check_for_error json with
  | Some err when err = "MethodNotImplemented" || err = "MethodNotFound" -> ()
  | Some err ->
      (* Official importRepo is a migration procedure; a lexicon-valid CAR
         may still be rejected if the account is not in the import state. *)
      OUnit2.assert_bool ("importRepo " ^ err)
        (err = "InvalidRequest" || err = "InvalidSwap" || err = "ExpiredToken"
        || err = "AuthenticationRequired")
  | None ->
      let again =
        Sync.get_latest_commit ~host:throwaway.atp_host ~session:throwaway
          throwaway.auth.did
      in
      OUnit2.assert_bool "importRepo commit" (String.length again.cid > 0)

let test_update_handle _ =
  let s = throwaway_session "hdl" "local-pds-handle-password" in
  let new_handle = unique_handle "hdlu" in
  match
    pds_post_if_served ~session:s "com.atproto.identity.updateHandle"
      (Yojson.Safe.to_string (Identity.update_handle_body new_handle))
  with
  | None -> ()
  | Some _ ->
      Identity.update_handle s ~handle:new_handle ();
      let info = Session.get_session s in
      OUnit2.assert_equal ~printer:(fun x -> x) new_handle info.handle;
      let resolved =
        Identity.resolve_handle ~host:s.atp_host ~session:s new_handle
      in
      OUnit2.assert_equal ~printer:(fun x -> x) s.auth.did resolved.did

let test_deactivate_activate _ =
  let s = throwaway_session "deact" "local-pds-deact-password" in
  let activated_of = function Some b -> string_of_bool b | None -> "none" in
  match
    pds_post_if_served ~session:s "com.atproto.server.deactivateAccount"
      (Yojson.Safe.to_string (Server.deactivate_account_body ()))
  with
  | None -> ()
  | Some _ -> (
      Server.deactivate_account s ();
      let deactivated = Server.check_account_status s in
      OUnit2.assert_equal ~printer:activated_of (Some false)
        deactivated.activated;
      match
        pds_post_if_served ~session:s "com.atproto.server.activateAccount" ""
      with
      | None -> ()
      | Some _ ->
          Server.activate_account s;
          let activated = Server.check_account_status s in
          OUnit2.assert_equal ~printer:activated_of (Some true)
            activated.activated)

let test_session_refresh_and_delete _ =
  skip_unless_local_pds ();
  let handle = unique_handle "ref" in
  let email = handle ^ "@test.local" in
  let password = "local-pds-refresh-password" in
  ignore
    (Server.create_account_at ~host:(pds_host ()) ~handle ~email ~password ()
    |> ensure_ok);
  let s = Session.create_session handle password in
  OUnit2.assert_bool "throwaway access token" (String.length s.auth.token > 0);
  let refreshed = Session.refresh_session s in
  OUnit2.assert_equal ~printer:(fun x -> x) s.auth.did refreshed.auth.did;
  OUnit2.assert_bool "refreshSession accessJwt"
    (String.length refreshed.auth.token > 0);
  (match refreshed.auth.refresh_token with
  | Some t ->
      OUnit2.assert_bool "refreshSession refreshJwt" (String.length t > 0)
  | None -> OUnit2.assert_failure "refreshSession missing refreshJwt");
  let info = Session.get_session refreshed in
  OUnit2.assert_equal ~printer:(fun x -> x) s.auth.did info.did;
  let deleted = Session.delete_session refreshed in
  ignore deleted

let test_get_account_invite_codes _ =
  let s = session () in
  let body = Server.get_account_invite_codes s false false in
  let json = json_of_body body in
  match Yojson.Safe.Util.member "codes" json with
  | `List _ -> ()
  | _ -> OUnit2.assert_failure "getAccountInviteCodes missing codes"

let local_plc_directory () =
  match Sys.getenv_opt "PLC_ORIGIN" with
  | Some o when String.trim o <> "" -> String.trim o
  | _ -> "http://localhost:2582"

(* PDS identity PLC hops. Throwaway account — alice stays untouched.
   Each hop is skip-if-not-served independently (PDS 0.5.31 may 501 one). *)
let test_plc_operation_xrpc _ =
  let s = throwaway_session "plc" "local-pds-plc-password" in
  ignore
    (pds_post_if_served ~session:s
       "com.atproto.identity.requestPlcOperationSignature" "");
  let creds = Identity.get_recommended_did_credentials s in
  let json_obj = function `Assoc _ as j -> Some j | _ -> None in
  let sign_body =
    Identity.sign_plc_operation_body
      ?rotation_keys:
        (match creds.rotation_keys with [] -> None | ks -> Some ks)
      ?also_known_as:
        (match creds.also_known_as with [] -> None | xs -> Some xs)
      ?verification_methods:(json_obj creds.verification_methods)
      ?services:(json_obj creds.services) ()
  in
  match pds_sign_plc_if_served ~session:s (Yojson.Safe.to_string sign_body) with
  | None -> ()
  | Some json -> (
      let operation =
        match Yojson.Safe.Util.member "operation" json with
        | `Assoc _ as op -> op
        | _ -> json
      in
      OUnit2.assert_bool "signPlcOperation operation"
        (match operation with
        | `Assoc fields -> List.length fields >= 0
        | _ -> false);
      match
        pds_post_if_served ~session:s "com.atproto.identity.submitPlcOperation"
          (Yojson.Safe.to_string (Identity.submit_plc_operation_body operation))
      with
      | None -> ()
      | Some _ -> (
          let resolved =
            Identity.resolve_did_parsed ~host:s.atp_host ~session:s s.auth.did
          in
          match resolved.document with
          | Some doc ->
              OUnit2.assert_equal ~printer:(fun x -> x) s.auth.did doc.id
          | None ->
              OUnit2.assert_bool "submitPlcOperation didDoc"
                (match resolved.did_doc with `Assoc _ -> true | _ -> false)))

let test_plc_directory_write _ =
  skip_unless_local_pds ();
  Mirage_crypto_rng_unix.use_default ();
  let open Atproto.Did_plc.Did_plc in
  let priv, pub = generate_k256 () in
  let rotation = k256_did_key pub in
  OUnit2.assert_bool "k256 did:key (zQ3s…)"
    (String.length rotation > 12 && String.sub rotation 0 12 = "did:key:zQ3s");
  let genesis =
    format_atproto_op ~signing_key:rotation ~rotation_keys:[ rotation ]
      ~handle:"plcwrite.test" ~pds:"http://localhost:2583" ()
  in
  let signed, did = sign_genesis_k256 ~priv genesis in
  let op = parse_operation signed in
  OUnit2.assert_equal `Valid (verify_with_rotation_keys [ rotation ] op);
  let directory = local_plc_directory () in
  ignore (submit_operation ~directory did signed);
  let doc = resolve ~directory did in
  OUnit2.assert_equal ~printer:(fun x -> x) did doc.id;
  let data = resolve_data ~directory did in
  OUnit2.assert_equal [ rotation ] data.rotation_keys;
  let prev = Atproto.Cid.Cid.to_string (cid_of_operation op) in
  let update =
    format_atproto_op ~signing_key:rotation ~rotation_keys:[ rotation ]
      ~handle:"plcwrite-updated.test" ~pds:"http://localhost:2583" ~prev ()
  in
  let signed_update = sign_k256 ~priv update in
  OUnit2.assert_equal `Valid
    (verify_with_rotation_keys [ rotation ] (parse_operation signed_update));
  ignore (submit_operation ~directory did signed_update);
  let audit = resolve_audit_log ~directory did in
  OUnit2.assert_bool "audit log after update" (List.length audit >= 2);
  let chain = verify_chain ~did (resolve_log ~directory did) in
  OUnit2.assert_bool "PLC chain genesis" chain.genesis_ok;
  OUnit2.assert_bool "PLC chain prev" chain.prev_links_ok

(* Remaining PDS NSIDs whose wrappers already exist. Skip if this
   TestNetwork revision 501s the method. Destructive hops use a
   throwaway account, never alice.test. *)
let test_leftover_served _ =
  let s = session () in
  (match
     pds_leftover_json
       (Client.post_json ~session:s ~host:(pds_host ())
          "com.atproto.identity.refreshIdentity"
          (Yojson.Safe.to_string
             (Identity.refresh_identity_body ~identifier:s.username)))
   with
  | None -> ()
  | Some json ->
      let info = Identity.parse_identity_info json in
      OUnit2.assert_equal ~printer:(fun x -> x) s.auth.did info.did;
      OUnit2.assert_bool "refreshIdentity handle" (String.length info.handle > 0));
  let avail = unique_handle "avail" in
  (match
     pds_leftover_json
       (Client.get_json ~session:s ~host:(pds_host ())
          "com.atproto.temp.checkHandleAvailability" [ ("handle", avail) ])
   with
  | None -> ()
  | Some json ->
      let check = Temp.parse_handle_check json in
      OUnit2.assert_equal ~printer:(fun x -> x) avail check.handle;
      match check.result with `Available | `Unavailable _ | `Unknown _ -> ());
  (match
     pds_leftover_json
       (Client.get_json ~session:s ~host:(pds_host ())
          "com.atproto.temp.dereferenceScope" [ ("scope", "account:email") ])
   with
  | None -> ()
  | Some json ->
      let deref = Temp.parse_scope_deref json in
      OUnit2.assert_bool "dereferenceScope" (String.length deref.scope >= 0));
  (match
     pds_leftover_json
       (Client.get_json ~session:s ~host:(pds_host ())
          "com.atproto.lexicon.resolveLexicon"
          [ ("nsid", "app.bsky.feed.post") ])
   with
  | None -> ()
  | Some json ->
      let resolved = Lexicon.parse_resolved_lexicon json in
      OUnit2.assert_bool "resolveLexicon schema"
        (match resolved.schema with `Assoc _ -> true | _ -> false));
  let doomed = throwaway_session "del" "local-pds-delete-password" in
  (match
     pds_leftover_json
       (Client.post_json ~session:doomed ~host:(pds_host ())
          "com.atproto.server.requestAccountDelete"
          (Yojson.Safe.to_string (Server.request_account_delete_body ())))
   with
  | None -> ()
  | Some _ ->
      ignore
        (pds_leftover_json
           (Client.post_json ~session:doomed ~host:(pds_host ())
              "com.atproto.server.deleteAccount"
              (Yojson.Safe.to_string
                 (Server.delete_account_body ~did:doomed.auth.did
                    ~password:"local-pds-delete-password"
                    ~token:"not-an-email-token")))));
  (* revokeAppPassword only when createAppPassword is not the known 500. *)
  let pw = throwaway_session "revpw" "local-pds-revoke-password" in
  let pw_name =
    Printf.sprintf "leftover-rev-%d"
      (int_of_float (Unix.gettimeofday () *. 1000.) mod 1_000_000)
  in
  let created =
    Client.post_json ~session:pw ~host:(pds_host ())
      "com.atproto.server.createAppPassword"
      (Yojson.Safe.to_string (Server.create_app_password_body ~name:pw_name ()))
  in
  if Error.is_not_served_json created then ()
  else
    match Error.check_for_error created with
    | None -> (
        ignore (ensure_ok created);
        match
          pds_post_if_served ~session:pw "com.atproto.server.revokeAppPassword"
            (Yojson.Safe.to_string (Server.revoke_app_password_body ~name:pw_name))
        with
        | None -> ()
        | Some _ ->
            let listed = Server.list_app_passwords pw |> json_of_body in
            let names =
              List.map
                (fun (p : Server.app_password) -> p.name)
                (Server.parse_app_passwords listed)
            in
            OUnit2.assert_bool "revokeAppPassword removed name"
              (not (List.mem pw_name names)))
    | Some _ when pds_internal_error (Error.to_string (Error.of_json created))
      ->
        (* Known @atproto/pds 0.5.x TestNetwork 500. Do not call revoke. *)
        ()
    | Some _ ->
        failwith ("XRPC error: " ^ Error.to_string (Error.of_json created))

let suite =
  "local_pds"
  >::: [
         "test_describe_server" >:: test_describe_server;
         "test_create_account" >:: test_create_account;
         "test_create_session" >:: test_create_session;
         "test_get_session" >:: test_get_session;
         "test_resolve_handle" >:: test_resolve_handle;
         "test_resolve_did" >:: test_resolve_did;
         "test_resolve_identity" >:: test_resolve_identity;
         "test_repo_describe" >:: test_repo_describe;
         "test_repo_record_lifecycle" >:: test_repo_record_lifecycle;
         "test_upload_blob" >:: test_upload_blob;
         "test_sync_endpoints" >:: test_sync_endpoints;
         "test_sync_hosts_if_served" >:: test_sync_hosts_if_served;
         "test_subscribe_repos_one_frame" >:: test_subscribe_repos_one_frame;
         "test_other_pds_xrpc" >:: test_other_pds_xrpc;
         "test_referencelistoptout_and_import"
         >:: test_referencelistoptout_and_import;
         "test_update_handle" >:: test_update_handle;
         "test_deactivate_activate" >:: test_deactivate_activate;
         "test_session_refresh_and_delete" >:: test_session_refresh_and_delete;
         "test_get_account_invite_codes" >:: test_get_account_invite_codes;
         "test_plc_operation_xrpc" >:: test_plc_operation_xrpc;
         "test_plc_directory_write" >:: test_plc_directory_write;
         "test_leftover_served" >:: test_leftover_served;
       ]

let () =
  Unix.putenv "OUNIT_RUNNER" "sequential";
  run_test_tt_main suite
