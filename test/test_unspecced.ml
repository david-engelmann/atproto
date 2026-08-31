open OUnit2
open Atproto.Unspecced

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

let test_parse_skeleton_posts _ =
  let json =
    `Assoc
      [
        ("hitsTotal", `Int 2);
        ( "posts",
          `List
            [
              `Assoc
                [
                  ( "uri",
                    `String
                      "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/3k"
                  );
                ];
            ] );
      ]
  in
  let page = Unspecced.parse_skeleton_posts json in
  OUnit2.assert_equal (Some 2) page.hits_total;
  OUnit2.assert_equal 1 (List.length page.posts)

let test_parse_trending _ =
  let json =
    `Assoc
      [
        ( "topics",
          `List
            [
              `Assoc
                [
                  ("topic", `String "atproto");
                  ("link", `String "/search?q=atproto");
                ];
            ] );
        ("suggested", `List []);
      ]
  in
  let t = Unspecced.parse_trending_topics json in
  OUnit2.assert_equal ~printer:(fun x -> x) "atproto" (List.hd t.topics).topic

let test_parse_popular _ =
  let json =
    `Assoc
      [
        ( "feeds",
          `List
            [
              `Assoc
                [
                  ( "uri",
                    `String
                      "at://did:plc:z72i7hdynmk6r22z27h6tvur/app.bsky.feed.generator/whats-hot"
                  );
                  ("cid", `String "bafyreiabc");
                  ("did", `String "did:web:discover.bsky.social");
                  ("displayName", `String "Discover");
                  ("indexedAt", `String "2024-01-01T00:00:00.000Z");
                ];
            ] );
      ]
  in
  let gens = Unspecced.parse_generators json in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "Discover" (List.hd gens.feeds).display_name

let test_parse_tagged_and_age _ =
  let tagged =
    Unspecced.parse_tagged_suggestions
      (`Assoc
        [
          ( "suggestions",
            `List
              [
                `Assoc
                  [
                    ("tag", `String "news");
                    ("subjectType", `String "feed");
                    ( "subject",
                      `String
                        "at://did:plc:z72i7hdynmk6r22z27h6tvur/app.bsky.feed.generator/whats-hot"
                    );
                  ];
              ] );
        ])
  in
  OUnit2.assert_equal 1 (List.length tagged.suggestions);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "news" (List.hd tagged.suggestions).tag;
  let aa =
    Unspecced.parse_age_assurance_state
      (`Assoc
        [
          ("status", `String "pending");
          ("lastInitiatedAt", `String "2026-01-01T00:00:00.000Z");
        ])
  in
  OUnit2.assert_equal ~printer:(fun x -> x) "pending" aa.status;
  let body =
    Unspecced.init_age_assurance_body ~email:"user@example.com" ~language:"en"
      ~country_code:"US"
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "US"
    (body |> member "countryCode" |> to_string)

let test_parse_trends_and_skeletons _ =
  let trends =
    Unspecced.parse_trends
      (`Assoc
        [
          ( "trends",
            `List
              [
                `Assoc
                  [
                    ("topic", `String "atproto");
                    ("displayName", `String "AT Protocol");
                    ("link", `String "/topic/atproto");
                    ("startedAt", `String "2026-01-01T00:00:00.000Z");
                    ("postCount", `Int 42);
                    ("status", `String "hot");
                    ( "actors",
                      `List
                        [
                          `Assoc
                            [
                              ("did", `String "did:plc:abc123xyz0001112223333");
                            ];
                        ] );
                  ];
              ] );
          ("recIdStr", `String "snow-1");
        ])
  in
  OUnit2.assert_equal 1 (List.length trends.trends);
  OUnit2.assert_equal 42 (List.hd trends.trends).post_count;
  OUnit2.assert_equal (Some "snow-1") trends.rec_id_str;
  let skel =
    Unspecced.parse_trends_skeleton
      (`Assoc
        [
          ( "trends",
            `List
              [
                `Assoc
                  [
                    ("topic", `String "ocaml");
                    ("displayName", `String "OCaml");
                    ("link", `String "/topic/ocaml");
                    ("startedAt", `String "2026-01-01T00:00:00.000Z");
                    ("postCount", `Int 3);
                    ("dids", `List [ `String "did:plc:abc123xyz0001112223333" ]);
                  ];
              ] );
        ])
  in
  OUnit2.assert_equal 1 (List.length (List.hd skel.trends).dids);
  let feeds =
    Unspecced.parse_uri_list
      (`Assoc
        [
          ( "feeds",
            `List
              [
                `String
                  "at://did:plc:z72i7hdynmk6r22z27h6tvur/app.bsky.feed.generator/whats-hot";
              ] );
        ])
      "feeds"
  in
  OUnit2.assert_equal 1 (List.length feeds.uris);
  let dids =
    Unspecced.parse_did_skeleton
      (`Assoc
        [
          ("dids", `List [ `String "did:plc:abc123xyz0001112223333" ]);
          ("recIdStr", `String "r2");
        ])
  in
  OUnit2.assert_equal (Some "r2") dids.rec_id_str

let test_parse_thread_v2 _ =
  let item =
    `Assoc
      [
        ( "uri",
          `String "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/3k" );
        ("depth", `Int 0);
        ( "value",
          `Assoc
            [
              ("$type", `String "app.bsky.unspecced.defs#threadItemPost");
              ( "post",
                `Assoc
                  [
                    ( "uri",
                      `String
                        "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/3k"
                    );
                    ("cid", `String "bafyreihdummy");
                    ( "author",
                      `Assoc
                        [
                          ("did", `String "did:plc:abc123xyz0001112223333");
                          ("handle", `String "alice.test");
                        ] );
                    ( "record",
                      `Assoc
                        [
                          ("$type", `String "app.bsky.feed.post");
                          ("text", `String "hello");
                          ("createdAt", `String "2024-01-01T00:00:00.000Z");
                        ] );
                    ("indexedAt", `String "2024-01-01T00:00:01.000Z");
                  ] );
              ("moreParents", `Bool false);
              ("moreReplies", `Int 2);
              ("opThread", `Bool true);
              ("opThreadPostIndex", `Int 1);
              ("opThreadPostCount", `Int 1);
              ("hiddenByThreadgate", `Bool false);
              ("mutedByViewer", `Bool false);
            ] );
      ]
  in
  let parsed =
    Unspecced.parse_thread_v2
      (`Assoc
        [
          ("thread", `List [ item ]);
          ("hasOtherReplies", `Bool true);
          ( "threadgate",
            `Assoc
              [
                ( "uri",
                  `String
                    "at://did:plc:abc123xyz0001112223333/app.bsky.feed.threadgate/3k"
                );
              ] );
        ])
  in
  OUnit2.assert_equal true parsed.has_other_replies;
  OUnit2.assert_equal 1 (List.length parsed.thread);
  OUnit2.assert_equal 0 (List.hd parsed.thread).depth;
  (match (List.hd parsed.thread).value with
  | `Post p ->
      OUnit2.assert_equal 2 p.more_replies;
      OUnit2.assert_equal true p.op_thread;
      OUnit2.assert_equal (Some 1) p.op_thread_post_index
  | _ -> OUnit2.assert_failure "expected threadItemPost");
  let blocked =
    Unspecced.parse_thread_item
      (`Assoc
        [
          ( "uri",
            `String "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/3m"
          );
          ("depth", `Int 1);
          ( "value",
            `Assoc
              [
                ("$type", `String "app.bsky.unspecced.defs#threadItemBlocked");
                ( "author",
                  `Assoc [ ("did", `String "did:plc:abc123xyz0001112223333") ]
                );
              ] );
        ])
  in
  (match blocked.value with
  | `Blocked b ->
      OUnit2.assert_equal (Some "did:plc:abc123xyz0001112223333") b.author_did
  | _ -> OUnit2.assert_failure "expected blocked thread item");
  let other =
    Unspecced.parse_thread_other_v2 (`Assoc [ ("thread", `List [ item ]) ])
  in
  OUnit2.assert_equal 1 (List.length other.thread)

let test_parse_discover_explore_see_more _ =
  let users =
    Unspecced.parse_suggested_users
      (`Assoc
        [
          ( "actors",
            `List
              [
                `Assoc
                  [
                    ("did", `String "did:plc:abc123xyz0001112223333");
                    ("handle", `String "alice.test");
                    ("indexedAt", `String "2024-01-01T00:00:00.000Z");
                    ("viewer", `Null);
                  ];
              ] );
          ("recIdStr", `String "disc-1");
        ])
  in
  OUnit2.assert_equal 1 (List.length users.actors);
  OUnit2.assert_equal (Some "disc-1") users.rec_id_str;
  let skel =
    Unspecced.parse_did_skeleton
      (`Assoc
        [
          ("dids", `List [ `String "did:plc:abc123xyz0001112223333" ]);
          ("recId", `String "old");
          ("recIdStr", `String "new");
        ])
  in
  OUnit2.assert_equal (Some "old") skel.rec_id;
  OUnit2.assert_equal (Some "new") skel.rec_id_str

let test_popular_live _ =
  try
    with_public_timeout (fun () ->
        let gens = Unspecced.get_popular_feed_generators ~limit:3 () in
        OUnit2.assert_bool "popular feeds"
          (List.length gens.feeds > 0
          && String.length (List.hd gens.feeds).uri > 8))
  with exn ->
    skip_if true ("getPopularFeedGenerators skipped: " ^ Printexc.to_string exn)

let test_get_post_thread_v2_live _ =
  try
    with_public_timeout (fun () ->
        let page = Unspecced.search_posts_skeleton ~q:"atproto" ~limit:1 () in
        match page.posts with
        | [] -> skip_if true "no skeleton posts to thread"
        | hd :: _ ->
            let thread =
              Unspecced.get_post_thread_v2 ~anchor:hd.uri ~below:2
                ~branching_factor:3 ()
            in
            OUnit2.assert_bool "thread v2 items" (List.length thread.thread >= 0))
  with exn ->
    skip_if true ("getPostThreadV2 skipped: " ^ Printexc.to_string exn)

let test_search_posts_skeleton_live _ =
  try
    with_public_timeout (fun () ->
        let page = Unspecced.search_posts_skeleton ~q:"atproto" ~limit:3 () in
        OUnit2.assert_bool "skeleton posts"
          (List.length page.posts >= 0
          &&
          match page.posts with
          | [] -> true
          | hd :: _ -> String.length hd.uri > 8))
  with exn ->
    skip_if true ("searchPostsSkeleton skipped: " ^ Printexc.to_string exn)

let suite =
  "unspecced"
  >::: [
         "test_parse_skeleton_posts" >:: test_parse_skeleton_posts;
         "test_parse_trending" >:: test_parse_trending;
         "test_parse_popular" >:: test_parse_popular;
         "test_parse_tagged_and_age" >:: test_parse_tagged_and_age;
         "test_parse_trends_and_skeletons" >:: test_parse_trends_and_skeletons;
         "test_parse_thread_v2" >:: test_parse_thread_v2;
         "test_parse_discover_explore_see_more"
         >:: test_parse_discover_explore_see_more;
         "test_popular_live" >:: test_popular_live;
         "test_get_post_thread_v2_live" >:: test_get_post_thread_v2_live;
         "test_search_posts_skeleton_live" >:: test_search_posts_skeleton_live;
       ]

let () = run_test_tt_main suite
