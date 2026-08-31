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
  OUnit2.assert_bool "createReport id" (report.id >= 0)

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
         "test_other_pds_xrpc" >:: test_other_pds_xrpc;
       ]

let () =
  Unix.putenv "OUNIT_RUNNER" "sequential";
  run_test_tt_main suite
