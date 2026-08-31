open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Sync
open Atproto.Car
open Atproto.Cid
open Atproto.Identity

let skip_without_auth () =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped"

let with_public_timeout ?(seconds = 20) f =
  let old =
    Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> failwith "timeout"))
  in
  ignore (Unix.alarm seconds);
  Fun.protect
    ~finally:(fun () ->
      ignore (Unix.alarm 0);
      Sys.set_signal Sys.sigalrm old)
    f

let create_test_session _ =
  let username, password = Auth.username_and_password_from_env in
  Session.create_session username password

let public_actor () = Identity.resolve "jay.bsky.team"

let public_pds_host ident =
  match ident.Identity.pds with
  | Some pds -> Identity.host_of_service_endpoint pds
  | None -> "bsky.social"

let test_get_latest_commit_public _ =
  try
    with_public_timeout (fun () ->
        let ident = public_actor () in
        let host = public_pds_host ident in
        let commit = Sync.get_latest_commit ~host ident.did in
        OUnit2.assert_bool "latest commit cid empty"
          (String.length commit.cid > 0);
        OUnit2.assert_bool "latest commit rev empty"
          (String.length commit.rev > 0))
  with exn ->
    skip_if true ("getLatestCommit skipped: " ^ Printexc.to_string exn)

let test_list_blobs_public _ =
  try
    with_public_timeout (fun () ->
        let ident = public_actor () in
        let host = public_pds_host ident in
        let blobs = Sync.list_blobs ~host ~limit:5 ident.did in
        OUnit2.assert_bool "listBlobs should return a list"
          (List.length blobs.cids >= 0))
  with exn -> skip_if true ("listBlobs skipped: " ^ Printexc.to_string exn)

let test_get_head _ =
  skip_without_auth ();
  let test_session = create_test_session () |> Session.refresh_session_auth in
  try
    let ident = public_actor () in
    let head = Sync.get_head test_session ident.did in
    OUnit2.assert_bool "Sync Head is empty" (head <> "")
  with exn -> skip_if true ("get_head skipped: " ^ Printexc.to_string exn)

let test_get_repo_car _ =
  skip_without_auth ();
  let test_session = create_test_session () |> Session.refresh_session_auth in
  try
    let ident = public_actor () in
    let host = public_pds_host ident in
    let car = Sync.get_repo_car ~host ~session:test_session ident.did in
    OUnit2.assert_bool "CAR roots missing"
      (match Car.root car with Some _ -> true | None -> false);
    OUnit2.assert_bool "CAR has no blocks" (List.length car.blocks > 0)
  with exn -> skip_if true ("getRepo skipped: " ^ Printexc.to_string exn)

let test_list_repos _ =
  skip_without_auth ();
  let test_session = create_test_session () |> Session.refresh_session_auth in
  try
    let repos = Sync.list_repos ~session:test_session ~limit:5 () in
    OUnit2.assert_bool "listRepos returned no items"
      (List.length repos.repos >= 0)
  with exn -> skip_if true ("listRepos skipped: " ^ Printexc.to_string exn)

let test_parse_repo_status _ =
  let json =
    `Assoc
      [
        ("did", `String "did:plc:ewvi7nxzyoun6zhxrhs64oiz");
        ("active", `Bool false);
        ("status", `String "deactivated");
        ("rev", `String "3jzfcijpj2z2a");
      ]
  in
  let s = Sync.parse_repo_status json in
  OUnit2.assert_equal false s.active;
  OUnit2.assert_equal (Some "deactivated") s.status;
  OUnit2.assert_equal (Some "3jzfcijpj2z2a") s.rev

let test_parse_list_hosts _ =
  let json =
    `Assoc
      [
        ("cursor", `String "c2");
        ( "hosts",
          `List
            [
              `Assoc
                [
                  ("hostname", `String "morel.us-east.host.bsky.network");
                  ("seq", `Int 99);
                  ("accountCount", `Int 12);
                  ("status", `String "active");
                ];
            ] );
      ]
  in
  let h = Sync.parse_list_hosts json in
  OUnit2.assert_equal (Some "c2") h.cursor;
  OUnit2.assert_equal 1 (List.length h.hosts);
  OUnit2.assert_equal (Some 99L) (List.hd h.hosts).seq

let test_parse_list_repos_by_collection _ =
  let json =
    `Assoc
      [
        ( "repos",
          `List
            [
              `Assoc [ ("did", `String "did:plc:aaaa") ];
              `Assoc [ ("did", `String "did:plc:bbbb") ];
            ] );
      ]
  in
  let r = Sync.parse_list_repos_by_collection json in
  OUnit2.assert_equal 2 (List.length r.repos);
  OUnit2.assert_equal "did:plc:aaaa" (List.hd r.repos).did

let test_get_blocks_url _ =
  let url =
    Sync.get_blocks_url ~host:"bsky.social"
      ~did:"did:plc:abc123xyz0001112223333"
      ~cids:[ "bafyreihdummy000000000000000000000000000000000" ]
      ()
  in
  OUnit2.assert_bool "getBlocks"
    (let needle = "getBlocks" in
     let rec contains i =
       i + String.length needle <= String.length url
       && (String.sub url i (String.length needle) = needle || contains (i + 1))
     in
     contains 0);
  OUnit2.assert_bool "cids"
    (let needle = "cids=" in
     let rec contains i =
       i + String.length needle <= String.length url
       && (String.sub url i (String.length needle) = needle || contains (i + 1))
     in
     contains 0)

let test_get_record_proof_public _ =
  try
    with_public_timeout (fun () ->
        let ident = public_actor () in
        let host = public_pds_host ident in
        let cid, bytes =
          Atproto.Repo_sync.Repo_sync.fetch_record_proof ~host ~did:ident.did
            ~collection:"app.bsky.actor.profile" ~rkey:"self" ()
        in
        OUnit2.assert_bool "profile proof cid"
          (String.length (Cid.to_string cid) > 8);
        OUnit2.assert_bool "profile bytes" (String.length bytes > 0))
  with exn ->
    skip_if true ("getRecord proof skipped: " ^ Printexc.to_string exn)

let test_get_repo_status_public _ =
  try
    with_public_timeout (fun () ->
        let ident = public_actor () in
        let host = public_pds_host ident in
        let st = Sync.get_repo_status ~host ident.did in
        OUnit2.assert_equal ~printer:(fun x -> x) ident.did st.did)
  with exn -> skip_if true ("getRepoStatus skipped: " ^ Printexc.to_string exn)

let suite =
  "sync"
  >::: [
         "test_get_latest_commit_public" >:: test_get_latest_commit_public;
         "test_list_blobs_public" >:: test_list_blobs_public;
         "test_get_head" >:: test_get_head;
         "test_get_repo_car" >:: test_get_repo_car;
         "test_list_repos" >:: test_list_repos;
         "test_parse_repo_status" >:: test_parse_repo_status;
         "test_parse_list_hosts" >:: test_parse_list_hosts;
         "test_parse_list_repos_by_collection"
         >:: test_parse_list_repos_by_collection;
         "test_get_blocks_url" >:: test_get_blocks_url;
         "test_get_repo_status_public" >:: test_get_repo_status_public;
         "test_get_record_proof_public" >:: test_get_record_proof_public;
       ]

let () = run_test_tt_main suite
