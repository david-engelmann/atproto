open OUnit2
open Atproto.Did_web

let test_document_url_well_known _ =
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "https://example.com/.well-known/did.json"
    (Did_web.document_url "did:web:example.com")

let test_document_url_path _ =
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "https://example.com/user/alice/did.json"
    (Did_web.document_url "did:web:example.com:user:alice")

let test_document_url_port _ =
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "https://localhost:8080/.well-known/did.json"
    (Did_web.document_url "did:web:localhost%3A8080")

let test_rejects_plc _ =
  OUnit2.assert_bool "accepted did:plc as did:web"
    (try
       ignore (Did_web.document_url "did:plc:7iza6de2dwap2sbkpav7c6c6");
       false
     with Failure _ -> true)

let sample_doc =
  {|
{
  "id": "did:web:example.com",
  "alsoKnownAs": ["at://alice.example"],
  "verificationMethod": [],
  "service": [
    {
      "id": "#atproto_pds",
      "type": "AtprotoPersonalDataServer",
      "serviceEndpoint": "https://pds.example.com"
    }
  ]
}
|}

let test_parse_document _ =
  let doc = Did_web.parse_document (Yojson.Safe.from_string sample_doc) in
  OUnit2.assert_equal ~printer:(fun x -> x) "did:web:example.com" doc.id;
  OUnit2.assert_equal (Some "https://pds.example.com")
    (Atproto.Did_plc.Did_plc.pds_endpoint doc)

let test_resolve_live _ =
  try
    let doc = Did_web.resolve "did:web:w3c-ccg.github.io" in
    OUnit2.assert_bool "did:web document id"
      (String.length doc.id >= 8 && String.sub doc.id 0 8 = "did:web:")
  with exn ->
    skip_if true ("did:web live resolve skipped: " ^ Printexc.to_string exn)

let suite =
  "did_web"
  >::: [
         "test_document_url_well_known" >:: test_document_url_well_known;
         "test_document_url_path" >:: test_document_url_path;
         "test_document_url_port" >:: test_document_url_port;
         "test_rejects_plc" >:: test_rejects_plc;
         "test_parse_document" >:: test_parse_document;
         "test_resolve_live" >:: test_resolve_live;
       ]

let () = run_test_tt_main suite
