open OUnit2
open Atproto.Embed

let test_parse_images _ =
  let json =
    `Assoc
      [
        ("$type", `String "app.bsky.embed.images");
        ( "images",
          `List
            [
              `Assoc
                [
                  ("alt", `String "cat");
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
                        ("mimeType", `String "image/jpeg");
                        ("size", `Int 12);
                      ] );
                ];
            ] );
      ]
  in
  match Embed.parse_embed json with
  | `Image e ->
      OUnit2.assert_equal 1 (List.length e.images);
      OUnit2.assert_equal ~printer:(fun x -> x) "cat" (List.hd e.images).alt
  | _ -> OUnit2.assert_failure "expected image embed"

let test_parse_video _ =
  let json =
    `Assoc
      [
        ("$type", `String "app.bsky.embed.video");
        ( "video",
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
              ("mimeType", `String "video/mp4");
              ("size", `Int 99);
            ] );
        ("alt", `String "clip");
        ("aspectRatio", `Assoc [ ("width", `Int 16); ("height", `Int 9) ]);
      ]
  in
  match Embed.parse_embed json with
  | `Video e ->
      OUnit2.assert_equal ~printer:(fun x -> x) "video/mp4" e.video.mime_type;
      OUnit2.assert_equal (Some "clip") e.alt;
      OUnit2.assert_equal (Some { Embed.width = 16; height = 9 }) e.aspect_ratio
  | _ -> OUnit2.assert_failure "expected video embed"

let test_parse_video_view _ =
  let json =
    `Assoc
      [
        ("$type", `String "app.bsky.embed.video#view");
        ("cid", `String "bafyreihdummy000000000000000000000000000000000");
        ("playlist", `String "https://video.bsky.app/watch/playlist.m3u8");
        ("thumbnail", `String "https://video.bsky.app/watch/thumb.jpg");
      ]
  in
  match Embed.parse_embed json with
  | `VideoView e -> OUnit2.assert_bool "playlist" (String.length e.playlist > 0)
  | _ -> OUnit2.assert_failure "expected video view"

let test_parse_record _ =
  let json =
    `Assoc
      [
        ("$type", `String "app.bsky.embed.record");
        ( "record",
          `Assoc
            [
              ( "uri",
                `String "at://did:plc:alice/app.bsky.feed.post/3jzfcijpj2z2a" );
              ("cid", `String "bafyreihdummy000000000000000000000000000000000");
            ] );
      ]
  in
  match Embed.parse_embed json with
  | `Record e ->
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "at://did:plc:alice/app.bsky.feed.post/3jzfcijpj2z2a" e.record.uri
  | _ -> OUnit2.assert_failure "expected record embed"

let test_parse_record_with_media _ =
  let json =
    `Assoc
      [
        ("$type", `String "app.bsky.embed.recordWithMedia");
        ( "record",
          `Assoc
            [
              ("$type", `String "app.bsky.embed.record");
              ( "record",
                `Assoc
                  [
                    ( "uri",
                      `String
                        "at://did:plc:alice/app.bsky.feed.post/3jzfcijpj2z2a" );
                    ( "cid",
                      `String "bafyreihdummy000000000000000000000000000000000"
                    );
                  ] );
            ] );
        ( "media",
          `Assoc
            [
              ("$type", `String "app.bsky.embed.images");
              ( "images",
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
  match Embed.parse_embed json with
  | `RecordWithMedia e -> (
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "at://did:plc:alice/app.bsky.feed.post/3jzfcijpj2z2a"
        e.record.record.uri;
      match e.media with
      | `Image img -> OUnit2.assert_equal 1 (List.length img.images)
      | _ -> OUnit2.assert_failure "expected image media")
  | other ->
      OUnit2.assert_failure
        (match other with `Unknown _ -> "unknown" | _ -> "wrong variant")

let test_unknown_is_not_fail _ =
  match
    Embed.parse_embed (`Assoc [ ("$type", `String "app.bsky.embed.unknown") ])
  with
  | `Unknown _ -> ()
  | _ -> OUnit2.assert_failure "expected unknown embed"

let suite =
  "embed"
  >::: [
         "test_parse_images" >:: test_parse_images;
         "test_parse_video" >:: test_parse_video;
         "test_parse_video_view" >:: test_parse_video_view;
         "test_parse_record" >:: test_parse_record;
         "test_parse_record_with_media" >:: test_parse_record_with_media;
         "test_unknown_is_not_fail" >:: test_unknown_is_not_fail;
       ]

let () = run_test_tt_main suite
