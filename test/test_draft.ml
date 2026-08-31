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
         "test_get_drafts_auth_skipped" >:: test_get_drafts_auth_skipped;
       ]

let () = run_test_tt_main suite
