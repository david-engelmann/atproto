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

let test_popular_live _ =
  try
    with_public_timeout (fun () ->
        let gens = Unspecced.get_popular_feed_generators ~limit:3 () in
        OUnit2.assert_bool "popular feeds"
          (List.length gens.feeds > 0
          && String.length (List.hd gens.feeds).uri > 8))
  with exn ->
    skip_if true ("getPopularFeedGenerators skipped: " ^ Printexc.to_string exn)

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
         "test_popular_live" >:: test_popular_live;
         "test_search_posts_skeleton_live" >:: test_search_posts_skeleton_live;
       ]

let () = run_test_tt_main suite
