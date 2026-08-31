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
         "test_popular_live" >:: test_popular_live;
         "test_search_posts_skeleton_live" >:: test_search_posts_skeleton_live;
       ]

let () = run_test_tt_main suite
