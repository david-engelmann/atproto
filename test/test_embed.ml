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

let test_parse_gallery _ =
  let json =
    `Assoc
      [
        ("$type", `String "app.bsky.embed.gallery");
        ( "items",
          `List
            [
              `Assoc
                [
                  ("alt", `String "one");
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
                  ("aspectRatio", `Assoc [ ("width", `Int 4); ("height", `Int 3) ]);
                ];
            ] );
      ]
  in
  match Embed.parse_embed json with
  | `Gallery e ->
      OUnit2.assert_equal 1 (List.length e.items);
      OUnit2.assert_equal ~printer:(fun x -> x) "one" (List.hd e.items).alt;
      OUnit2.assert_equal
        (Some { Embed.width = 4; height = 3 })
        (List.hd e.items).aspect_ratio
  | _ -> OUnit2.assert_failure "expected gallery embed"

let test_parse_gallery_view _ =
  let json =
    `Assoc
      [
        ("$type", `String "app.bsky.embed.gallery#view");
        ( "items",
          `List
            [
              `Assoc
                [
                  ("thumbnail", `String "https://cdn.example/thumb.jpg");
                  ("fullsize", `String "https://cdn.example/full.jpg");
                  ("alt", `String "wide");
                ];
            ] );
      ]
  in
  match Embed.parse_embed json with
  | `GalleryView e ->
      OUnit2.assert_equal ~printer:(fun x -> x) "wide" (List.hd e.items).alt
  | _ -> OUnit2.assert_failure "expected gallery view"

let test_parse_record_view _ =
  let json =
    `Assoc
      [
        ("$type", `String "app.bsky.embed.record#view");
        ( "record",
          `Assoc
            [
              ("$type", `String "app.bsky.embed.record#viewRecord");
              ( "uri",
                `String "at://did:plc:alice/app.bsky.feed.post/3jzfcijpj2z2a" );
              ("cid", `String "bafyreihdummy000000000000000000000000000000000");
              ( "author",
                `Assoc
                  [
                    ("did", `String "did:plc:alice000111222333444555666");
                    ("handle", `String "alice.test");
                  ] );
              ("value", `Assoc [ ("text", `String "quoted") ]);
              ("indexedAt", `String "2024-01-01T00:00:00.000Z");
              ("likeCount", `Int 4);
            ] );
      ]
  in
  match Embed.parse_embed json with
  | `RecordView e -> (
      match e.record with
      | `ViewRecord v ->
          OUnit2.assert_equal (Some 4) v.like_count;
          OUnit2.assert_equal (Some "alice.test") v.author_handle
      | _ -> OUnit2.assert_failure "expected viewRecord")
  | _ -> OUnit2.assert_failure "expected record view"

let test_parse_record_view_not_found _ =
  let json =
    `Assoc
      [
        ("$type", `String "app.bsky.embed.record#view");
        ( "record",
          `Assoc
            [
              ("$type", `String "app.bsky.embed.record#viewNotFound");
              ( "uri",
                `String "at://did:plc:alice/app.bsky.feed.post/missing" );
              ("notFound", `Bool true);
            ] );
      ]
  in
  match Embed.parse_embed json with
  | `RecordView e -> (
      match e.record with
      | `ViewNotFound { uri; not_found } ->
          OUnit2.assert_bool "notFound" not_found;
          OUnit2.assert_bool "uri" (String.length uri > 8)
      | _ -> OUnit2.assert_failure "expected viewNotFound")
  | _ -> OUnit2.assert_failure "expected record view"

let test_embed_to_json_roundtrip _ =
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
  | `Record e as parsed -> (
      match Embed.embed_to_json parsed with
      | `Assoc fields ->
          OUnit2.assert_equal
            ~printer:(fun x -> x)
            "app.bsky.embed.record"
            (match List.assoc "$type" fields with
            | `String s -> s
            | _ -> "");
          OUnit2.assert_equal
            ~printer:(fun x -> x)
            e.record.uri
            (match
               Yojson.Safe.Util.member "record" (Embed.embed_to_json parsed)
             with
            | `Assoc _ as r -> (
                match Yojson.Safe.Util.member "uri" r with
                | `String s -> s
                | _ -> "")
            | _ -> "")
      | _ -> OUnit2.assert_failure "expected assoc")
  | _ -> OUnit2.assert_failure "expected record embed"

let test_parse_embed_external_view _ =
  let json =
    `Assoc
      [
        ( "view",
          `Assoc
            [
              ("$type", `String "app.bsky.embed.external#view");
              ( "external",
                `Assoc
                  [
                    ("uri", `String "https://atproto.com");
                    ("title", `String "AT Protocol");
                    ("description", `String "specs");
                    ("thumb", `String "https://cdn.example/t.jpg");
                  ] );
            ] );
        ( "associatedRefs",
          `List
            [
              `Assoc
                [
                  ("uri", `String "at://did:plc:alice/site.standard.document/1");
                  ("cid", `String "bafyreihdummy000000000000000000000000000000000");
                ];
            ] );
      ]
  in
  let v = Embed.parse_embed_external_view json in
  OUnit2.assert_equal 1 (List.length v.associated_refs);
  match v.view with
  | Some e ->
      OUnit2.assert_equal ~printer:(fun x -> x) "https://atproto.com" e.ext.uri
  | None -> OUnit2.assert_failure "expected view"

let suite =
  "embed"
  >::: [
         "test_parse_images" >:: test_parse_images;
         "test_parse_video" >:: test_parse_video;
         "test_parse_video_view" >:: test_parse_video_view;
         "test_parse_record" >:: test_parse_record;
         "test_parse_record_with_media" >:: test_parse_record_with_media;
         "test_unknown_is_not_fail" >:: test_unknown_is_not_fail;
         "test_parse_gallery" >:: test_parse_gallery;
         "test_parse_gallery_view" >:: test_parse_gallery_view;
         "test_parse_record_view" >:: test_parse_record_view;
         "test_parse_record_view_not_found" >:: test_parse_record_view_not_found;
         "test_embed_to_json_roundtrip" >:: test_embed_to_json_roundtrip;
         "test_parse_embed_external_view" >:: test_parse_embed_external_view;
       ]

let () = run_test_tt_main suite
