open OUnit2
open Atproto.Facet

let test_parse_mention _ =
  let json =
    `Assoc
      [
        ("index", `Assoc [ ("byteStart", `Int 0); ("byteEnd", `Int 5) ]);
        ( "features",
          `List
            [
              `Assoc
                [
                  ("$type", `String "app.bsky.richtext.facet#mention");
                  ("did", `String "did:plc:abc123xyz0001112223333");
                ];
            ] );
      ]
  in
  match Facet.parse_facet json with
  | `Mention m ->
      OUnit2.assert_equal 0 m.facet_index.byte_start;
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "did:plc:abc123xyz0001112223333" (List.hd m.features).did
  | _ -> OUnit2.assert_failure "expected mention"

let test_parse_link _ =
  let json =
    `Assoc
      [
        ("index", `Assoc [ ("byteStart", `Int 6); ("byteEnd", `Int 20) ]);
        ( "features",
          `List
            [
              `Assoc
                [
                  ("$type", `String "app.bsky.richtext.facet#link");
                  ("uri", `String "https://atproto.com");
                ];
            ] );
      ]
  in
  match Facet.parse_facet json with
  | `Link l ->
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "https://atproto.com" (List.hd l.features).uri
  | _ -> OUnit2.assert_failure "expected link"

let test_parse_tag _ =
  let json =
    `Assoc
      [
        ("index", `Assoc [ ("byteStart", `Int 21); ("byteEnd", `Int 29) ]);
        ( "features",
          `List
            [
              `Assoc
                [
                  ("$type", `String "app.bsky.richtext.facet#tag");
                  ("tag", `String "atproto");
                ];
            ] );
      ]
  in
  match Facet.parse_facet json with
  | `Tag t ->
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "atproto" (List.hd t.features).tag
  | _ -> OUnit2.assert_failure "expected tag"

let test_builders_and_serialize _ =
  let mention =
    Facet.mention ~byte_start:0 ~byte_end:5 "did:plc:abc123xyz0001112223333"
  in
  let link = Facet.link ~byte_start:6 ~byte_end:20 "https://atproto.com" in
  let tag = Facet.tag ~byte_start:21 ~byte_end:29 "atproto" in
  (match mention with
  | `Mention m ->
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "did:plc:abc123xyz0001112223333" (List.hd m.features).did
  | _ -> OUnit2.assert_failure "expected mention builder");
  let json = Facet.facets_to_json [ mention; link; tag ] in
  match json with
  | `List xs -> (
      OUnit2.assert_equal 3 (List.length xs);
      match Facet.parse_facet (List.hd xs) with
      | `Mention _ -> ()
      | _ -> OUnit2.assert_failure "roundtrip mention")
  | _ -> OUnit2.assert_failure "expected list"

let suite =
  "facet"
  >::: [
         "test_parse_mention" >:: test_parse_mention;
         "test_parse_link" >:: test_parse_link;
         "test_parse_tag" >:: test_parse_tag;
         "test_builders_and_serialize" >:: test_builders_and_serialize;
       ]

let () = run_test_tt_main suite
