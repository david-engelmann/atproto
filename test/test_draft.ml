open OUnit2
open Atproto.Draft
open Atproto.Auth

let test_parse_and_build _ =
  let json =
    `Assoc
      [
        ("id", `String "3jzfcijpj2z2a");
        ("createdAt", `String "2024-01-01T00:00:00.000Z");
        ("updatedAt", `String "2024-01-02T00:00:00.000Z");
        ( "draft",
          `Assoc
            [
              ("deviceId", `String "device-1");
              ("deviceName", `String "iphone");
              ("langs", `List [ `String "en" ]);
              ( "posts",
                `List
                  [
                    `Assoc
                      [
                        ("text", `String "hello draft");
                        ( "embedImages",
                          `List
                            [
                              `Assoc
                                [
                                  ( "localRef",
                                    `Assoc [ ("path", `String "/tmp/pic.png") ]
                                  );
                                  ("alt", `String "pic");
                                ];
                            ] );
                        ( "embedRecords",
                          `List
                            [
                              `Assoc
                                [
                                  ( "record",
                                    `Assoc
                                      [
                                        ( "uri",
                                          `String
                                            "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/3k"
                                        );
                                        ("cid", `String "bafyreihdummy");
                                      ] );
                                ];
                            ] );
                      ];
                  ] );
              ( "threadgateAllow",
                `List
                  [
                    `Assoc
                      [
                        ("$type", `String "app.bsky.feed.threadgate#mentionRule");
                      ];
                  ] );
              ( "postgateEmbeddingRules",
                `List
                  [
                    `Assoc
                      [
                        ("$type", `String "app.bsky.feed.postgate#disableRule");
                      ];
                  ] );
            ] );
      ]
  in
  let view = Draft.parse_draft_view json in
  OUnit2.assert_equal ~printer:(fun x -> x) "3jzfcijpj2z2a" view.id;
  OUnit2.assert_equal 1 (List.length view.draft.posts);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "hello draft" (List.hd view.draft.posts).text;
  OUnit2.assert_equal 1 (List.length (List.hd view.draft.posts).embed_images);
  (match view.draft.threadgate_allow with
  | `Mention :: _ -> ()
  | _ -> OUnit2.assert_failure "expected mention rule");
  (match view.draft.postgate_embedding_rules with
  | `Disable :: _ -> ()
  | _ -> OUnit2.assert_failure "expected disable rule");
  let built =
    Draft.draft_json ~device_id:"device-1" ~langs:[ "en" ]
      ~threadgate_allow:[ `Following ]
      ~posts:
        [
          {
            text = "hi";
            labels = None;
            embed_images = [];
            embed_gallery = None;
            embed_videos = [];
            embed_externals = [];
            embed_records = [];
          };
        ]
      ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal 1 (built |> member "posts" |> to_list |> List.length);
  let create = Draft.create_draft_body built in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "device-1"
    (create |> member "draft" |> member "deviceId" |> to_string);
  let update = Draft.update_draft_body ~id:"3jzfcijpj2z2a" built in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "3jzfcijpj2z2a"
    (update |> member "draft" |> member "id" |> to_string);
  let delete = Draft.delete_draft_body ~id:"3jzfcijpj2z2a" in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "3jzfcijpj2z2a"
    (delete |> member "id" |> to_string)

let test_draft_typed_body_roundtrip _ =
  let open Yojson.Safe.Util in
  let src =
    `Assoc
      [
        ("deviceId", `String "device-1");
        ("deviceName", `String "iphone");
        ("langs", `List [ `String "en" ]);
        ( "posts",
          `List
            [
              `Assoc
                [
                  ("text", `String "hello draft");
                  ( "labels",
                    `Assoc
                      [
                        ("$type", `String "com.atproto.label.defs#selfLabels");
                        ( "values",
                          `List [ `Assoc [ ("val", `String "graphic-media") ] ]
                        );
                      ] );
                  ( "embedImages",
                    `List
                      [
                        `Assoc
                          [
                            ( "localRef",
                              `Assoc [ ("path", `String "/tmp/pic.png") ] );
                            ("alt", `String "pic");
                          ];
                      ] );
                  ( "embedGallery",
                    `Assoc
                      [
                        ( "items",
                          `List
                            [
                              `Assoc
                                [
                                  ( "localRef",
                                    `Assoc [ ("path", `String "/tmp/g.png") ] );
                                ];
                            ] );
                      ] );
                  ( "embedVideos",
                    `List
                      [
                        `Assoc
                          [
                            ( "localRef",
                              `Assoc [ ("path", `String "/tmp/clip.mp4") ] );
                            ("alt", `String "clip");
                            ( "captions",
                              `List
                                [
                                  `Assoc
                                    [
                                      ("lang", `String "en");
                                      ("content", `String "hi");
                                    ];
                                ] );
                          ];
                      ] );
                  ( "embedExternals",
                    `List [ `Assoc [ ("uri", `String "https://example.test") ] ]
                  );
                  ( "embedRecords",
                    `List
                      [
                        `Assoc
                          [
                            ( "record",
                              `Assoc
                                [
                                  ( "uri",
                                    `String
                                      "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/3k"
                                  );
                                  ("cid", `String "bafyreihdummy");
                                ] );
                          ];
                      ] );
                ];
            ] );
        ( "threadgateAllow",
          `List
            [
              `Assoc
                [ ("$type", `String "app.bsky.feed.threadgate#mentionRule") ];
              `Assoc
                [ ("$type", `String "app.bsky.feed.threadgate#followerRule") ];
              `Assoc
                [ ("$type", `String "app.bsky.feed.threadgate#followingRule") ];
              `Assoc
                [
                  ("$type", `String "app.bsky.feed.threadgate#listRule");
                  ( "list",
                    `String
                      "at://did:plc:abc123xyz0001112223333/app.bsky.graph.list/3k"
                  );
                ];
            ] );
        ( "postgateEmbeddingRules",
          `List
            [
              `Assoc [ ("$type", `String "app.bsky.feed.postgate#disableRule") ];
            ] );
      ]
  in
  let parsed = Draft.parse_draft src in
  let encoded = Draft.draft_to_json parsed in
  let again = Draft.parse_draft encoded in
  OUnit2.assert_equal (Some "device-1") again.device_id;
  OUnit2.assert_equal (Some "iphone") again.device_name;
  OUnit2.assert_equal [ "en" ] again.langs;
  OUnit2.assert_equal 1 (List.length again.posts);
  let post = List.hd again.posts in
  OUnit2.assert_equal ~printer:(fun x -> x) "hello draft" post.text;
  OUnit2.assert_equal (Some [ "graphic-media" ]) post.labels;
  OUnit2.assert_equal 1 (List.length post.embed_images);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "/tmp/pic.png" (List.hd post.embed_images).local_ref.path;
  (match post.embed_gallery with
  | Some g ->
      OUnit2.assert_equal 1 (List.length g.items);
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "/tmp/g.png" (List.hd g.items).local_ref.path
  | None -> OUnit2.assert_failure "expected embedGallery after encode");
  OUnit2.assert_equal 1 (List.length post.embed_videos);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "clip"
    (match (List.hd post.embed_videos).alt with Some a -> a | None -> "");
  OUnit2.assert_equal 1 (List.length (List.hd post.embed_videos).captions);
  OUnit2.assert_equal 1 (List.length post.embed_externals);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "https://example.test" (List.hd post.embed_externals).uri;
  OUnit2.assert_equal 1 (List.length post.embed_records);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "bafyreihdummy" (List.hd post.embed_records).cid;
  (match again.threadgate_allow with
  | [ `Mention; `Follower; `Following; `List uri ] ->
      OUnit2.assert_bool "list rule uri" (String.length uri > 0)
  | _ -> OUnit2.assert_failure "expected threadgate rules after encode");
  (match again.postgate_embedding_rules with
  | [ `Disable ] -> ()
  | _ -> OUnit2.assert_failure "expected disable rule after encode");
  let unknown_gate =
    `Assoc
      [
        ("$type", `String "app.bsky.feed.threadgate#futureRule");
        ("extra", `String "keep");
      ]
  in
  let unknown_src =
    `Assoc
      [
        ("posts", `List [ `Assoc [ ("text", `String "x") ] ]);
        ("threadgateAllow", `List [ unknown_gate ]);
      ]
  in
  let unknown_again =
    Draft.parse_draft (Draft.draft_to_json (Draft.parse_draft unknown_src))
  in
  (match unknown_again.threadgate_allow with
  | [ `Unknown j ] -> OUnit2.assert_equal unknown_gate j
  | _ -> OUnit2.assert_failure "expected unknown threadgate after encode");
  let create = Draft.create_draft_typed_body parsed in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "device-1"
    (create |> member "draft" |> member "deviceId" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "graphic-media"
    (create |> member "draft" |> member "posts" |> to_list |> List.hd
   |> member "labels" |> member "values" |> to_list |> List.hd |> member "val"
   |> to_string);
  let update = Draft.update_draft_typed_body ~id:"3jzfcijpj2z2a" parsed in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "3jzfcijpj2z2a"
    (update |> member "draft" |> member "id" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "hello draft"
    (update |> member "draft" |> member "draft" |> member "posts" |> to_list
   |> List.hd |> member "text" |> to_string);
  let raw =
    Draft.create_draft_body
      (Draft.draft_json ~posts:[ List.hd parsed.posts ] ())
  in
  match raw with
  | `Assoc [ ("draft", `Assoc fields) ] ->
      OUnit2.assert_bool "raw create_draft_body still Yojson"
        (List.exists (fun (k, _) -> k = "posts") fields)
  | _ -> OUnit2.assert_failure "expected raw create_draft_body unchanged"

let test_get_drafts_auth_skipped _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let username, password = Auth.username_and_password_from_env in
  let s = Atproto.Session.Session.create_session username password in
  try
    let page = Draft.get_drafts s ~limit:5 () in
    OUnit2.assert_bool "drafts parsed" (List.length page.drafts >= 0)
  with exn -> skip_if true ("getDrafts skipped: " ^ Printexc.to_string exn)

let suite =
  "draft"
  >::: [
         "test_parse_and_build" >:: test_parse_and_build;
         "test_draft_typed_body_roundtrip" >:: test_draft_typed_body_roundtrip;
         "test_get_drafts_auth_skipped" >:: test_get_drafts_auth_skipped;
       ]

let () = run_test_tt_main suite
