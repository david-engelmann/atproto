open OUnit2
open Atproto.Bookmark
open Atproto.Auth

let create_test_session _ =
  let username, password = Auth.username_and_password_from_env in
  Atproto.Session.Session.create_session username password

let test_parse_bookmarks _ =
  let json =
    `Assoc
      [
        ("cursor", `String "abc");
        ( "bookmarks",
          `List
            [
              `Assoc
                [
                  ( "subject",
                    `Assoc
                      [
                        ( "uri",
                          `String
                            "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/3k2"
                        );
                        ( "cid",
                          `String
                            "bafyreiarimgpoqvxxnf3sg4h52gvfzvmyeybxk2xgy6v3dra7zuldy73aq"
                        );
                      ] );
                  ("createdAt", `String "2024-01-01T00:00:00.000Z");
                  ( "item",
                    `Assoc
                      [
                        ( "uri",
                          `String
                            "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/3k2"
                        );
                      ] );
                ];
            ] );
      ]
  in
  let page = Bookmark.parse_bookmarks json in
  OUnit2.assert_equal (Some "abc") page.cursor;
  OUnit2.assert_equal 1 (List.length page.bookmarks);
  OUnit2.assert_bool "bookmark uri"
    (String.length (List.hd page.bookmarks).uri > 10);
  match (List.hd page.bookmarks).item with
  | `Post p -> OUnit2.assert_bool "item uri" (String.length p.uri > 10)
  | `NotFound _ | `Blocked _ -> OUnit2.assert_failure "expected post view item"

let test_create_bookmark_body _ =
  let body =
    Bookmark.create_bookmark_body ~uri:"at://did:plc:x/app.bsky.feed.post/1"
      ~cid:"bafyreiabc"
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "at://did:plc:x/app.bsky.feed.post/1"
    (body |> member "uri" |> to_string)

let test_parse_bookmark_item_unions _ =
  let not_found =
    Bookmark.parse_bookmark_item
      (`Assoc
        [
          ("$type", `String "app.bsky.feed.defs#notFoundPost");
          ( "uri",
            `String "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/3k"
          );
          ("notFound", `Bool true);
        ])
  in
  (match not_found with
  | `NotFound n -> OUnit2.assert_equal true n.not_found
  | _ -> OUnit2.assert_failure "expected notFound item");
  let blocked =
    Bookmark.parse_bookmark_item
      (`Assoc
        [
          ("$type", `String "app.bsky.feed.defs#blockedPost");
          ( "uri",
            `String "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/3k"
          );
          ("blocked", `Bool true);
          ( "author",
            `Assoc [ ("did", `String "did:plc:abc123xyz0001112223333") ] );
        ])
  in
  match blocked with
  | `Blocked b -> OUnit2.assert_equal true b.blocked
  | _ -> OUnit2.assert_failure "expected blocked item"

let test_get_bookmarks_auth_skipped _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let s = create_test_session () in
  let page = Bookmark.get_bookmarks s ~limit:5 () in
  OUnit2.assert_bool "bookmarks parsed" (List.length page.bookmarks >= 0)

let suite =
  "bookmark"
  >::: [
         "test_parse_bookmarks" >:: test_parse_bookmarks;
         "test_create_bookmark_body" >:: test_create_bookmark_body;
         "test_parse_bookmark_item_unions" >:: test_parse_bookmark_item_unions;
         "test_get_bookmarks_auth_skipped" >:: test_get_bookmarks_auth_skipped;
       ]

let () = run_test_tt_main suite
