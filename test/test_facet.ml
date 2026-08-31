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

let suite =
  "facet"
  >::: [
         "test_parse_mention" >:: test_parse_mention;
         "test_parse_link" >:: test_parse_link;
         "test_parse_tag" >:: test_parse_tag;
       ]

let () = run_test_tt_main suite
