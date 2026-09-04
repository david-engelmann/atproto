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

let test_create_put_record_json_body _ =
  let record =
    `Assoc
      [
        ("$type", `String "app.bsky.feed.post");
        ("text", `String "hi");
        ("createdAt", `String "2024-01-01T00:00:00.000Z");
      ]
  in
  let created =
    Repo.create_record_body ~repo:"did:plc:7iza6de2dwap2sbkpav7c6c6"
      ~collection:"app.bsky.feed.post" ~rkey:"3jzfcijpj2z2a"
      ~swap_commit:"bafyreihdummy" record
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:plc:7iza6de2dwap2sbkpav7c6c6"
    (created |> member "repo" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "app.bsky.feed.post"
    (created |> member "collection" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "3jzfcijpj2z2a"
    (created |> member "rkey" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "bafyreihdummy"
    (created |> member "swapCommit" |> to_string);
  OUnit2.assert_equal true (created |> member "validate" |> to_bool);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "hi"
    (created |> member "record" |> member "text" |> to_string);
  let from_string =
    Repo.create_record_body ~repo:"did:plc:7iza6de2dwap2sbkpav7c6c6"
      ~collection:"app.bsky.feed.post"
      (Repo.record_json_of_string (Yojson.Safe.to_string record))
  in
  OUnit2.assert_equal ~printer:Yojson.Safe.to_string
    (created |> member "record")
    (from_string |> member "record");
  let put =
    Repo.put_record_body ~repo:"did:plc:7iza6de2dwap2sbkpav7c6c6"
      ~collection:"app.bsky.actor.profile" ~rkey:"self"
      ~swap_record:"bafyreihswaprecord" ~swap_commit:"bafyreihdummy" record
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "self"
    (put |> member "rkey" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "bafyreihswaprecord"
    (put |> member "swapRecord" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "hi"
    (put |> member "record" |> member "text" |> to_string);
  ignore Repo.create_record_json;
  ignore Repo.put_record_json

let test_delete_record_body _ =
  let body =
    Repo.delete_record_body ~repo:"did:plc:7iza6de2dwap2sbkpav7c6c6"
      ~collection:"app.bsky.feed.post" ~rkey:"3jzfcijpj2z2a"
      ~swap_record:"bafyreihswaprecord" ~swap_commit:"bafyreihdummy" ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:plc:7iza6de2dwap2sbkpav7c6c6"
    (body |> member "repo" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "app.bsky.feed.post"
    (body |> member "collection" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "3jzfcijpj2z2a"
    (body |> member "rkey" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "bafyreihswaprecord"
    (body |> member "swapRecord" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "bafyreihdummy"
    (body |> member "swapCommit" |> to_string);
  let keys =
    match body with
    | `Assoc fields -> List.map fst fields
    | _ -> OUnit2.assert_failure "delete_record_body must be an object"
  in
  OUnit2.assert_equal
    [ "repo"; "collection"; "rkey"; "swapRecord"; "swapCommit" ]
    keys;
  let minimal =
    Repo.delete_record_body ~repo:"did:plc:7iza6de2dwap2sbkpav7c6c6"
      ~collection:"app.bsky.feed.like" ~rkey:"3jzfcijpj2z2a" ()
  in
  OUnit2.assert_equal `Null (minimal |> member "swapRecord");
  OUnit2.assert_equal `Null (minimal |> member "swapCommit")

let test_apply_writes_parsed _ =
  let sample =
    `Assoc
      [
        ( "commit",
          `Assoc
            [
              ("cid", `String "bafyreihdummy000000000000000000000000000000000");
              ("rev", `String "3jzfcijpj2z2a");
            ] );
        ( "results",
          `List
            [
              `Assoc
                [
                  ("$type", `String "com.atproto.repo.applyWrites#createResult");
                  ( "uri",
                    `String
                      "at://did:plc:alice/app.bsky.feed.post/3jzfcijpj2z2a" );
                  ( "cid",
                    `String "bafyreihdummy000000000000000000000000000000000" );
                ];
            ] );
      ]
  in
  let parsed =
    sample |> Yojson.Safe.to_string |> Yojson.Safe.from_string
    |> Repo.parse_apply_writes_result
  in
  (match parsed.commit with
  | Some c ->
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "bafyreihdummy000000000000000000000000000000000" c.cid;
      OUnit2.assert_equal ~printer:(fun x -> x) "3jzfcijpj2z2a" c.rev
  | None -> OUnit2.assert_failure "expected commit");
  OUnit2.assert_equal 1 (List.length parsed.results);
  ignore Repo.apply_writes_parsed;
  ignore Repo.apply_writes

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
              value =
                `Assoc
                  [
                    ("text", `String "hi");
                    ("$type", `String "app.bsky.feed.post");
                  ];
            };
          Repo.Update
            {
              collection = "app.bsky.actor.profile";
              rkey = "self";
              value = `Assoc [ ("displayName", `String "Ada") ];
            };
          Repo.Delete
            { collection = "app.bsky.feed.like"; rkey = "3jzfcijpj2z2a" };
        ]
      ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:plc:7iza6de2dwap2sbkpav7c6c6"
    (body |> member "repo" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "bafyreihdummy"
    (body |> member "swapCommit" |> to_string);
  let writes = body |> member "writes" |> to_list in
  OUnit2.assert_equal 3 (List.length writes);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "com.atproto.repo.applyWrites#create"
    (List.nth writes 0 |> member "$type" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "com.atproto.repo.applyWrites#update"
    (List.nth writes 1 |> member "$type" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "com.atproto.repo.applyWrites#delete"
    (List.nth writes 2 |> member "$type" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    rkey
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

let test_blob_ref_to_json _ =
  let cid = "bafkreihdwdcefgh4dqkjv67uzcmw7ojee6xedzdetojuzjevtenxquvyku" in
  let json =
    `Assoc
      [
        ( "blob",
          `Assoc
            [
              ("$type", `String "blob");
              ("ref", `Assoc [ ("$link", `String cid) ]);
              ("mimeType", `String "image/png");
              ("size", `Int 1234);
            ] );
      ]
  in
  let blob = Repo.parse_blob_ref json in
  let encoded = Repo.blob_ref_to_json blob in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "blob"
    (encoded |> member "$type" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    cid
    (encoded |> member "ref" |> member "$link" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "image/png"
    (encoded |> member "mimeType" |> to_string);
  OUnit2.assert_equal ~printer:string_of_int 1234
    (encoded |> member "size" |> to_int);
  let again = Repo.parse_blob_ref encoded in
  OUnit2.assert_equal ~printer:(fun x -> x) blob.cid again.cid;
  OUnit2.assert_equal ~printer:(fun x -> x) blob.mime_type again.mime_type;
  OUnit2.assert_equal ~printer:string_of_int blob.size again.size;
  let rebuilt = Repo.blob_ref_to_json { blob with original = `Null } in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "blob"
    (rebuilt |> member "$type" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    cid
    (rebuilt |> member "ref" |> member "$link" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "image/png"
    (rebuilt |> member "mimeType" |> to_string);
  OUnit2.assert_equal ~printer:string_of_int 1234
    (rebuilt |> member "size" |> to_int)

let test_parse_list_missing_blobs _ =
  let json =
    `Assoc
      [
        ("cursor", `String "next");
        ( "blobs",
          `List
            [
              `Assoc
                [
                  ( "cid",
                    `String
                      "bafkreihdwdcefgh4dqkjv67uzcmw7ojee6xedzdetojuzjevtenxquvyku"
                  );
                  ( "recordUri",
                    `String
                      "at://did:plc:alice/app.bsky.feed.post/3jzfcijpj2z2a" );
                ];
            ] );
      ]
  in
  let missing = Repo.parse_list_missing_blobs json in
  OUnit2.assert_equal (Some "next") missing.cursor;
  OUnit2.assert_equal 1 (List.length missing.blobs);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "bafkreihdwdcefgh4dqkjv67uzcmw7ojee6xedzdetojuzjevtenxquvyku"
    (List.hd missing.blobs).cid

let test_parse_record_get_and_describe _ =
  let rec_ =
    Repo.parse_record_get
      (`Assoc
        [
          ("uri", `String "at://did:plc:alice/app.bsky.feed.post/3jzfcijpj2z2a");
          ("cid", `String "bafyreihdummy000000000000000000000000000000000");
          ( "value",
            `Assoc
              [
                ("$type", `String "app.bsky.feed.post");
                ("text", `String "hello");
                ("createdAt", `String "2024-01-01T00:00:00.000Z");
                ("tags", `List [ `String "atp" ]);
                ( "labels",
                  `Assoc
                    [
                      ("$type", `String "com.atproto.label.defs#selfLabels");
                      ("values", `List [ `Assoc [ ("val", `String "nudity") ] ]);
                    ] );
              ] );
        ])
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "at://did:plc:alice/app.bsky.feed.post/3jzfcijpj2z2a" rec_.uri;
  let post = Repo.parse_post_record rec_.value in
  OUnit2.assert_equal (Some [ "atp" ]) post.tags;
  OUnit2.assert_equal (Some [ "nudity" ]) post.self_labels;
  let desc =
    Repo.parse_repo_description
      (`Assoc
        [
          ("handle", `String "alice.test");
          ("did", `String "did:plc:alice000111222333444555666");
          ( "didDoc",
            `Assoc [ ("id", `String "did:plc:alice000111222333444555666") ] );
          ("collections", `List [ `String "app.bsky.feed.post" ]);
          ("handleIsCorrect", `Bool true);
        ])
  in
  OUnit2.assert_equal ~printer:(fun x -> x) "alice.test" desc.handle;
  OUnit2.assert_equal true desc.handle_is_correct;
  let listed =
    Repo.parse_listed_records
      (`Assoc
        [
          ("cursor", `String "next");
          ( "records",
            `List
              [
                `Assoc
                  [
                    ( "uri",
                      `String
                        "at://did:plc:alice/app.bsky.feed.post/3jzfcijpj2z2a" );
                    ( "cid",
                      `String "bafyreihdummy000000000000000000000000000000000"
                    );
                    ("value", `Assoc [ ("text", `String "hello") ]);
                  ];
              ] );
        ])
  in
  OUnit2.assert_equal 1 (List.length listed.records);
  let write =
    Repo.parse_write_result
      (`Assoc
        [
          ("uri", `String "at://did:plc:alice/app.bsky.feed.post/3jzfcijpj2z2a");
          ("cid", `String "bafyreihdummy000000000000000000000000000000000");
          ( "commit",
            `Assoc
              [
                ("cid", `String "bafyreihdummy000000000000000000000000000000000");
                ("rev", `String "3jzfcijpj2z2a");
              ] );
          ("validationStatus", `String "valid");
        ])
  in
  OUnit2.assert_equal (Some "valid") write.validation_status

let test_import_repo_url _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () in
  let url = Repo.import_repo_url test_session in
  OUnit2.assert_bool "importRepo URL"
    (let n = String.length url in
     n > 10 && String.sub url (n - 10) 10 = "importRepo")

let suite =
  "suite"
  >::: [
         "test_describe_repo" >:: test_describe_repo;
         "test_create_put_record_json_body" >:: test_create_put_record_json_body;
         "test_delete_record_body" >:: test_delete_record_body;
         "test_apply_writes_parsed" >:: test_apply_writes_parsed;
         "test_apply_writes_body" >:: test_apply_writes_body;
         "test_parse_blob_ref" >:: test_parse_blob_ref;
         "test_blob_ref_to_json" >:: test_blob_ref_to_json;
         "test_parse_list_missing_blobs" >:: test_parse_list_missing_blobs;
         "test_parse_record_get_and_describe"
         >:: test_parse_record_get_and_describe;
         "test_import_repo_url" >:: test_import_repo_url;
       ]

let () = run_test_tt_main suite
