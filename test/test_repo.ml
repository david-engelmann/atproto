open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Repo
open Atproto.Tid

let create_test_session _ =
  let username, password = Auth.username_and_password_from_env in
  Session.create_session username password

let test_describe_repo _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let repo_description =
    Repo.describe_repo test_session "go-bluesky-tester.bsky.social"
  in
  Printf.printf "Repo Description: %s\n" repo_description;
  OUnit2.assert_bool "Repo Description is not empty" (repo_description <> "")

(*
let test_create_record _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let created_record = Repo.create_record test_session "david-engelmann.bsky.social"
*)

let test_apply_writes_body _ =
  let rkey = Tid.create ~clock_id:1 1_700_000_000_000_000L in
  let body =
    Repo.apply_writes_body ~repo:"did:plc:7iza6de2dwap2sbkpav7c6c6"
      ~swap_commit:"bafyreihdummy"
      ~writes:
        [
          Repo.Create
            {
              collection = "app.bsky.feed.post";
              rkey = Some rkey;
              value = `Assoc [ ("text", `String "hi"); ("$type", `String "app.bsky.feed.post") ];
            };
          Repo.Update
            {
              collection = "app.bsky.actor.profile";
              rkey = "self";
              value = `Assoc [ ("displayName", `String "Ada") ];
            };
          Repo.Delete { collection = "app.bsky.feed.like"; rkey = "3jzfcijpj2z2a" };
        ]
      ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal ~printer:(fun x -> x)
    "did:plc:7iza6de2dwap2sbkpav7c6c6"
    (body |> member "repo" |> to_string);
  OUnit2.assert_equal ~printer:(fun x -> x) "bafyreihdummy"
    (body |> member "swapCommit" |> to_string);
  let writes = body |> member "writes" |> to_list in
  OUnit2.assert_equal 3 (List.length writes);
  OUnit2.assert_equal ~printer:(fun x -> x)
    "com.atproto.repo.applyWrites#create"
    (List.nth writes 0 |> member "$type" |> to_string);
  OUnit2.assert_equal ~printer:(fun x -> x)
    "com.atproto.repo.applyWrites#update"
    (List.nth writes 1 |> member "$type" |> to_string);
  OUnit2.assert_equal ~printer:(fun x -> x)
    "com.atproto.repo.applyWrites#delete"
    (List.nth writes 2 |> member "$type" |> to_string);
  OUnit2.assert_equal ~printer:(fun x -> x) rkey
    (List.nth writes 0 |> member "rkey" |> to_string)

let test_parse_blob_ref _ =
  let json =
    `Assoc
      [
        ( "blob",
          `Assoc
            [
              ("$type", `String "blob");
              ( "ref",
                `Assoc
                  [
                    ( "$link",
                      `String
                        "bafkreihdwdcefgh4dqkjv67uzcmw7ojee6xedzdetojuzjevtenxquvyku"
                    );
                  ] );
              ("mimeType", `String "image/png");
              ("size", `Int 1234);
            ] );
      ]
  in
  let blob = Repo.parse_blob_ref json in
  OUnit2.assert_equal ~printer:(fun x -> x) "image/png" blob.mime_type;
  OUnit2.assert_equal ~printer:string_of_int 1234 blob.size;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "bafkreihdwdcefgh4dqkjv67uzcmw7ojee6xedzdetojuzjevtenxquvyku" blob.cid

let suite =
  "suite"
  >::: [
         "test_describe_repo" >:: test_describe_repo;
         "test_apply_writes_body" >:: test_apply_writes_body;
         "test_parse_blob_ref" >:: test_parse_blob_ref;
       ]
let () = run_test_tt_main suite
