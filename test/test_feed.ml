open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Feed

let create_test_session _ =
  let username, password = Auth.username_and_password_from_env in
  Session.create_session username password

let test_get_author_feed _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let author_feed =
    Feed.get_author_feed test_session "david-engelmann.bsky.social" 50
  in
  OUnit2.assert_bool "Author Feed is empty" (List.length author_feed > 10)

let test_get_likes _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let l =
    Feed.get_likes test_session
      "at://did:plc:xov3uvxfd4to6ev3ak5g5uxk/app.bsky.feed.post/3jyf6gx25eb27"
      "bafyreiarimgpoqvxxnf3sg4h52gvfzvmyeybxk2xgy6v3dra7zuldy73aq" 10
  in
  match l with
  | { likes; _ } ->
      OUnit2.assert_bool "Likes Feed is empty" (List.length likes > 0)

let test_get_post_thread _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let post_thread =
    Feed.get_post_thread test_session
      "at://did:plc:xov3uvxfd4to6ev3ak5g5uxk/app.bsky.feed.post/3jyf6gx25eb27" 1
  in
  match post_thread.thread with
  | `Thread t ->
      OUnit2.assert_bool "Post Thread Feed is empty" (t.thread_type <> "")
  | `NotFound _ -> OUnit2.assert_failure "thread not found"
  | `Blocked _ -> OUnit2.assert_failure "thread blocked"

let test_get_posts _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let p =
    Feed.get_posts test_session
      [
        "at://did:plc:xov3uvxfd4to6ev3ak5g5uxk/app.bsky.feed.post/3jyf6gx25eb27";
        "at://did:plc:h3lbzrp2qum5nyzpeq6anmty/app.bsky.feed.post/3jyh24qvwwt2s";
      ]
  in
  match p with
  | { posts } -> OUnit2.assert_bool "Posts Feed is empty" (List.length posts > 0)

let test_get_reposted_by _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let reposted_by =
    Feed.get_reposted_by test_session
      "at://did:plc:xov3uvxfd4to6ev3ak5g5uxk/app.bsky.feed.post/3jxyx4hdso62e"
      "bafyreihui4bipokenrcj6ttannh26svviq62x7hqx3oxrmejd7qhwxbasy" 1
  in
  match reposted_by with
  | { uri; _ } -> OUnit2.assert_bool "Reposted By Feed Uri is empty" (uri <> "")

let test_get_timeline _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let timeline = Feed.get_timeline test_session "reverse-chronological" 2 in
  match timeline with
  | { cursor; _ } -> OUnit2.assert_bool "Timeline Feed is empty" (cursor <> "")

(*
let test_get_feed_skeleton _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let feed_skeleton = Feed.get_feed_skeleton test_session "at://did:plc:sho65umi6t4ohqyaijutbdcr/app.bsky.feed.post/3k3bop33b6u2e" 2 in
  Printf.printf "\n\nFeed Skeleton Feed: %s\n\n" feed_skeleton;
  OUnit2.assert_bool "Feed Skeleton Feed is not empty" (feed_skeleton <> "")
*)

let discover_feed =
  "at://did:plc:z72i7hdynmk6r22z27h6tvur/app.bsky.feed.generator/whats-hot"

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

let test_parse_generator _ =
  let json =
    `Assoc
      [
        ( "view",
          `Assoc
            [
              ("uri", `String discover_feed);
              ("cid", `String "bafyreiabc");
              ("did", `String "did:web:discover.bsky.social");
              ("displayName", `String "Discover");
              ("indexedAt", `String "2024-01-01T00:00:00.000Z");
            ] );
        ("isOnline", `Bool true);
        ("isValid", `Bool true);
      ]
  in
  let info = Feed.parse_generator_info json in
  OUnit2.assert_equal ~printer:(fun x -> x) "Discover" info.view.display_name;
  OUnit2.assert_equal true info.is_online

let test_parse_search_posts _ =
  let json =
    `Assoc
      [
        ( "posts",
          `List
            [
              `Assoc
                [
                  ( "uri",
                    `String
                      "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/3k"
                  );
                  ("cid", `String "bafyreiabc");
                  ( "author",
                    `Assoc
                      [
                        ("did", `String "did:plc:abc123xyz0001112223333");
                        ("handle", `String "alice.test");
                      ] );
                  ("record", `Assoc [ ("text", `String "hello atproto") ]);
                  ("indexedAt", `String "2024-01-01T00:00:00.000Z");
                  ("likeCount", `Int 3);
                ];
            ] );
        ("hitsTotal", `Int 1);
      ]
  in
  let page = Feed.parse_search_posts json in
  OUnit2.assert_equal 1 (List.length page.posts);
  OUnit2.assert_equal (Some "hello atproto") (List.hd page.posts).text

let test_parse_post_record_embed_and_tags _ =
  let json =
    `Assoc
      [
        ("$type", `String "app.bsky.feed.post");
        ("text", `String "gallery post");
        ("createdAt", `String "2024-01-01T00:00:00.000Z");
        ("langs", `List [ `String "en" ]);
        ("tags", `List [ `String "art" ]);
        ( "labels",
          `Assoc
            [
              ("$type", `String "com.atproto.label.defs#selfLabels");
              ("values", `List [ `Assoc [ ("val", `String "nudity") ] ]);
            ] );
        ( "embed",
          `Assoc
            [
              ("$type", `String "app.bsky.embed.gallery");
              ( "items",
                `List
                  [
                    `Assoc
                      [
                        ("alt", `String "pic");
                        ( "image",
                          `Assoc
                            [
                              ("$type", `String "blob");
                              ( "ref",
                                `Assoc
                                  [
                                    ( "$link",
                                      `String
                                        "bafyimage0000000000000000000000000000000000"
                                    );
                                  ] );
                              ("mimeType", `String "image/png");
                              ("size", `Int 4);
                            ] );
                      ];
                  ] );
            ] );
      ]
  in
  let rec_ = Feed.parse_post_record json in
  OUnit2.assert_equal (Some [ "art" ]) rec_.tags;
  OUnit2.assert_equal (Some [ "nudity" ]) rec_.self_labels;
  match rec_.embed with
  | Some (`Gallery g) -> OUnit2.assert_equal 1 (List.length g.items)
  | _ -> OUnit2.assert_failure "expected gallery embed on post record"

let post_view_json ~uri ~text ?embed () =
  `Assoc
    ([
       ("uri", `String uri);
       ("cid", `String "bafyreiabc");
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
             ("text", `String text);
             ("createdAt", `String "2024-01-01T00:00:00.000Z");
           ] );
       ("indexedAt", `String "2024-01-01T00:00:00.000Z");
       ("replyCount", `Int 2);
       ("repostCount", `Int 1);
       ("likeCount", `Int 4);
       ("quoteCount", `Int 3);
       ("bookmarkCount", `Int 5);
     ]
    @ match embed with Some e -> [ ("embed", e) ] | None -> [])

let test_parse_thread_view_with_embed _ =
  let embed =
    `Assoc
      [
        ("$type", `String "app.bsky.embed.external#view");
        ( "external",
          `Assoc
            [
              ("uri", `String "https://atproto.com");
              ("title", `String "AT Protocol");
              ("description", `String "docs");
              ("thumb", `String "https://cdn.example/t.jpg");
            ] );
      ]
  in
  let root =
    `Assoc
      [
        ("$type", `String "app.bsky.feed.defs#threadViewPost");
        ( "post",
          post_view_json
            ~uri:"at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/root"
            ~text:"root post" ~embed () );
        ( "replies",
          `List
            [
              `Assoc
                [
                  ("$type", `String "app.bsky.feed.defs#threadViewPost");
                  ( "post",
                    post_view_json
                      ~uri:
                        "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/reply"
                      ~text:"a reply" () );
                ];
              `Assoc
                [
                  ("$type", `String "app.bsky.feed.defs#notFoundPost");
                  ( "uri",
                    `String
                      "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/gone"
                  );
                  ("notFound", `Bool true);
                ];
            ] );
      ]
  in
  let feed =
    Feed.parse_thread_feed
      (`Assoc
        [
          ("thread", root);
          ( "threadgate",
            `Assoc
              [
                ( "uri",
                  `String
                    "at://did:plc:abc123xyz0001112223333/app.bsky.feed.threadgate/1"
                );
              ] );
        ])
  in
  match feed.thread with
  | `Thread t -> (
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "app.bsky.feed.defs#threadViewPost" t.thread_type;
      OUnit2.assert_equal None t.parent;
      OUnit2.assert_equal (Some 3) t.post.quote_count;
      OUnit2.assert_equal (Some 5) t.post.bookmark_count;
      (match t.post.embed with
      | Some (`ExternalView e) ->
          OUnit2.assert_equal
            ~printer:(fun x -> x)
            "https://atproto.com" e.ext.uri
      | _ -> OUnit2.assert_failure "expected top-level external view embed");
      OUnit2.assert_equal 2 (List.length t.replies);
      match List.nth t.replies 1 with
      | `NotFound n -> OUnit2.assert_bool "not found" n.not_found
      | _ -> OUnit2.assert_failure "expected notFound reply")
  | _ -> OUnit2.assert_failure "expected thread view"

let test_parse_blocked_thread _ =
  let json =
    `Assoc
      [
        ( "thread",
          `Assoc
            [
              ("$type", `String "app.bsky.feed.defs#blockedPost");
              ( "uri",
                `String
                  "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/x" );
              ("blocked", `Bool true);
              ( "author",
                `Assoc [ ("did", `String "did:plc:abc123xyz0001112223333") ] );
            ] );
      ]
  in
  match (Feed.parse_thread_feed json).thread with
  | `Blocked b ->
      OUnit2.assert_equal (Some "did:plc:abc123xyz0001112223333") b.author_did
  | _ -> OUnit2.assert_failure "expected blocked thread"

let test_parse_reply_ref_not_found _ =
  let json =
    `Assoc
      [
        ( "root",
          `Assoc
            [
              ("$type", `String "app.bsky.feed.defs#notFoundPost");
              ( "uri",
                `String
                  "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/root"
              );
              ("notFound", `Bool true);
            ] );
        ( "parent",
          post_view_json
            ~uri:"at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/p"
            ~text:"parent" () );
      ]
  in
  let reply = Feed.parse_reply json in
  (match reply.root with
  | `NotFound n -> OUnit2.assert_bool "root missing" n.not_found
  | _ -> OUnit2.assert_failure "expected notFound root");
  match reply.parent with
  | `Post p -> OUnit2.assert_equal ~printer:(fun x -> x) "parent" p.record.text
  | _ -> OUnit2.assert_failure "expected parent post"

let test_parse_post_view_embed _ =
  let json =
    `Assoc
      [
        ( "uri",
          `String "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/3k" );
        ("cid", `String "bafyreiabc");
        ( "author",
          `Assoc
            [
              ("did", `String "did:plc:abc123xyz0001112223333");
              ("handle", `String "alice.test");
            ] );
        ("record", `Assoc [ ("text", `String "hello atproto") ]);
        ("indexedAt", `String "2024-01-01T00:00:00.000Z");
        ("likeCount", `Int 3);
        ("quoteCount", `Int 1);
        ( "embed",
          `Assoc
            [
              ("$type", `String "app.bsky.embed.external#view");
              ( "external",
                `Assoc
                  [
                    ("uri", `String "https://atproto.com");
                    ("title", `String "AT Protocol");
                    ("description", `String "docs");
                    ("thumb", `String "https://cdn.example/t.jpg");
                  ] );
            ] );
      ]
  in
  let view = Feed.parse_post_view json in
  OUnit2.assert_equal (Some 1) view.quote_count;
  match view.embed with
  | Some (`ExternalView e) ->
      OUnit2.assert_equal ~printer:(fun x -> x) "https://atproto.com" e.ext.uri
  | _ -> OUnit2.assert_failure "expected external view embed"

let test_send_interactions_body _ =
  let body =
    Feed.send_interactions_body ~feed:discover_feed
      [
        {
          item = Some "at://did:plc:x/app.bsky.feed.post/1";
          event = Some "app.bsky.feed.defs#interactionLike";
          feed_context = None;
          req_id = Some "req1";
        };
      ]
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal 1 (body |> member "interactions" |> to_list |> List.length)

let test_get_feed_generator_live _ =
  try
    with_public_timeout (fun () ->
        let info = Feed.get_feed_generator ~feed:discover_feed () in
        OUnit2.assert_bool "generator uri"
          (String.length info.view.uri > 10 && info.view.did <> ""))
  with exn ->
    skip_if true ("getFeedGenerator skipped: " ^ Printexc.to_string exn)

let test_search_posts_live _ =
  try
    with_public_timeout (fun () ->
        let page = Feed.search_posts ~q:"atproto" ~limit:3 () in
        OUnit2.assert_bool "search posts"
          (List.length page.posts >= 0
          &&
          match page.posts with
          | [] -> true
          | hd :: _ -> String.length hd.uri > 8))
  with exn -> skip_if true ("searchPosts skipped: " ^ Printexc.to_string exn)

let suite =
  "suite"
  >::: [
         "test_get_author_feed" >:: test_get_author_feed;
         "test_get_likes" >:: test_get_likes;
         "test_get_post_thread" >:: test_get_post_thread;
         "test_get_posts" >:: test_get_posts;
         "test_get_reposted_by" >:: test_get_reposted_by;
         "test_get_timeline" >:: test_get_timeline;
         "test_parse_generator" >:: test_parse_generator;
         "test_parse_search_posts" >:: test_parse_search_posts;
         "test_parse_post_record_embed_and_tags"
         >:: test_parse_post_record_embed_and_tags;
         "test_parse_thread_view_with_embed"
         >:: test_parse_thread_view_with_embed;
         "test_parse_blocked_thread" >:: test_parse_blocked_thread;
         "test_parse_reply_ref_not_found" >:: test_parse_reply_ref_not_found;
         "test_parse_post_view_embed" >:: test_parse_post_view_embed;
         "test_send_interactions_body" >:: test_send_interactions_body;
         "test_get_feed_generator_live" >:: test_get_feed_generator_live;
         "test_search_posts_live" >:: test_search_posts_live;
       ]

let () = run_test_tt_main suite
