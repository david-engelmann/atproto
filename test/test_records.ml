open OUnit2
open Atproto.Records
open Atproto.Facet
open Atproto.Embed

let test_post_builder _ =
  let facets = [ Facet.tag ~byte_start:0 ~byte_end:8 "atproto" ] in
  let embed =
    Embed.parse_embed
      (`Assoc
        [
          ("$type", `String "app.bsky.embed.record");
          ( "record",
            `Assoc
              [
                ( "uri",
                  `String "at://did:plc:alice/app.bsky.feed.post/3jzfcijpj2z2a"
                );
                ("cid", `String "bafyreihdummy000000000000000000000000000000000");
              ] );
        ])
  in
  let json =
    Records.post ~text:"#atproto quote" ~created_at:"2024-01-01T00:00:00.000Z"
      ~langs:[ "en" ] ~facets ~embed ~tags:[ "dev" ]
      ~self_labels:[ "graphic-media" ] ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "app.bsky.feed.post"
    (json |> member "$type" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "app.bsky.embed.record"
    (json |> member "embed" |> member "$type" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "graphic-media"
    (json |> member "labels" |> member "values" |> to_list |> List.hd
   |> member "val" |> to_string)

let test_graph_and_like_builders _ =
  let like =
    Records.like ~uri:"at://did:plc:alice/app.bsky.feed.post/3jzfcijpj2z2a"
      ~cid:"bafyreihdummy000000000000000000000000000000000"
      ~created_at:"2024-01-01T00:00:00.000Z" ()
  in
  let follow =
    Records.follow ~subject:"did:plc:alice000111222333444555666"
      ~created_at:"2024-01-01T00:00:00.000Z" ()
  in
  let block =
    Records.block ~subject:"did:plc:bob000111222333444555666777"
      ~created_at:"2024-01-01T00:00:00.000Z" ()
  in
  let profile =
    Records.profile ~display_name:"Ada" ~pronouns:"she/her"
      ~website:"https://atproto.com" ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "app.bsky.feed.like"
    (like |> member "$type" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:plc:alice000111222333444555666"
    (follow |> member "subject" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "app.bsky.graph.block"
    (block |> member "$type" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "Ada"
    (profile |> member "displayName" |> to_string)

let test_parse_like_and_follow _ =
  let like =
    Records.parse_like
      (`Assoc
        [
          ("$type", `String "app.bsky.feed.like");
          ( "subject",
            `Assoc
              [
                ( "uri",
                  `String "at://did:plc:alice/app.bsky.feed.post/3jzfcijpj2z2a"
                );
                ("cid", `String "bafyreihdummy000000000000000000000000000000000");
              ] );
          ("createdAt", `String "2024-01-01T00:00:00.000Z");
        ])
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "at://did:plc:alice/app.bsky.feed.post/3jzfcijpj2z2a" like.subject.uri;
  let follow =
    Records.parse_follow
      (`Assoc
        [
          ("subject", `String "did:plc:alice000111222333444555666");
          ("createdAt", `String "2024-01-01T00:00:00.000Z");
        ])
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:plc:alice000111222333444555666" follow.subject

let suite =
  "records"
  >::: [
         "test_post_builder" >:: test_post_builder;
         "test_graph_and_like_builders" >:: test_graph_and_like_builders;
         "test_parse_like_and_follow" >:: test_parse_like_and_follow;
       ]

let () = run_test_tt_main suite
