open OUnit2
open Atproto.Lexicon

let sample =
  {|
{
  "lexicon": 1,
  "id": "com.atproto.identity.resolveHandle",
  "defs": {
    "main": {
      "type": "query",
      "description": "Resolves a handle to a DID.",
      "parameters": {
        "type": "params",
        "required": ["handle"],
        "properties": {
          "handle": { "type": "string", "format": "handle" }
        }
      }
    }
  }
}
|}

let test_parse_document _ =
  let doc = Lexicon.of_string sample in
  OUnit2.assert_equal 1 doc.lexicon;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "com.atproto.identity.resolveHandle" doc.id;
  match Lexicon.main doc with
  | None -> OUnit2.assert_failure "missing main def"
  | Some main ->
      OUnit2.assert_equal Lexicon.Query main.kind;
      OUnit2.assert_bool "missing description"
        (match main.description with Some _ -> true | None -> false)

let test_lookup_helpers _ =
  OUnit2.assert_equal Lexicon.Cid_link (Lexicon.lookup_primitive "cid-link");
  OUnit2.assert_equal Lexicon.Subscription
    (Lexicon.lookup_definition "subscription")

let suite =
  "lexicon"
  >::: [
         "test_parse_document" >:: test_parse_document;
         "test_lookup_helpers" >:: test_lookup_helpers;
       ]

let () = run_test_tt_main suite
