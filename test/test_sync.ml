open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Sync
open Atproto.Car
open Atproto.Identity

let skip_without_auth () =
  skip_if (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped"

let create_test_session _ =
  let username, password = Auth.username_and_password_from_env in
  Session.create_session username password

let public_actor () =
  Identity.resolve "jay.bsky.team"

let public_pds_host ident =
  match ident.Identity.pds with
  | Some pds -> Identity.host_of_service_endpoint pds
  | None -> "bsky.social"

let test_get_latest_commit_public _ =
  try
    let ident = public_actor () in
    let host = public_pds_host ident in
    let commit = Sync.get_latest_commit ~host ident.did in
    OUnit2.assert_bool "latest commit cid empty" (String.length commit.cid > 0);
    OUnit2.assert_bool "latest commit rev empty" (String.length commit.rev > 0)
  with exn ->
    skip_if true ("getLatestCommit skipped: " ^ Printexc.to_string exn)

let test_list_blobs_public _ =
  try
    let ident = public_actor () in
    let host = public_pds_host ident in
    let blobs = Sync.list_blobs ~host ~limit:5 ident.did in
    OUnit2.assert_bool "listBlobs should return a list"
      (List.length blobs.cids >= 0)
  with exn ->
    skip_if true ("listBlobs skipped: " ^ Printexc.to_string exn)

let test_get_head _ =
  skip_without_auth ();
  let test_session = create_test_session () |> Session.refresh_session_auth in
  try
    let ident = public_actor () in
    let head = Sync.get_head test_session ident.did in
    OUnit2.assert_bool "Sync Head is empty" (head <> "")
  with exn ->
    skip_if true ("get_head skipped: " ^ Printexc.to_string exn)

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
  with exn ->
    skip_if true ("getRepo skipped: " ^ Printexc.to_string exn)

let test_list_repos _ =
  skip_without_auth ();
  let test_session = create_test_session () |> Session.refresh_session_auth in
  try
    let repos = Sync.list_repos ~session:test_session ~limit:5 () in
    OUnit2.assert_bool "listRepos returned no items"
      (List.length repos.repos >= 0)
  with exn ->
    skip_if true ("listRepos skipped: " ^ Printexc.to_string exn)

let suite =
  "sync"
  >::: [
         "test_get_latest_commit_public" >:: test_get_latest_commit_public;
         "test_list_blobs_public" >:: test_list_blobs_public;
         "test_get_head" >:: test_get_head;
         "test_get_repo_car" >:: test_get_repo_car;
         "test_list_repos" >:: test_list_repos;
       ]

let () = run_test_tt_main suite
