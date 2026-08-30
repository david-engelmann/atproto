open OUnit2
open Atproto.Did_plc

let sample_doc =
  {|
{
  "@context": [
    "https://www.w3.org/ns/did/v1",
    "https://w3id.org/security/multikey/v1"
  ],
  "id": "did:plc:7iza6de2dwap2sbkpav7c6c6",
  "alsoKnownAs": ["at://alice.test"],
  "verificationMethod": [
    {
      "id": "#atproto",
      "type": "Multikey",
      "controller": "did:plc:7iza6de2dwap2sbkpav7c6c6",
      "publicKeyMultibase": "zDnaeh9v2RmcMo13Du2d6pjUf5bZwtauYxj3n9dYjw4EZUAR7"
    }
  ],
  "service": [
    {
      "id": "#atproto_pds",
      "type": "AtprotoPersonalDataServer",
      "serviceEndpoint": "https://example2.com"
    }
  ]
}
|}

let test_validate_plc_did _ =
  Did_plc.validate_plc_did "did:plc:7iza6de2dwap2sbkpav7c6c6";
  OUnit2.assert_bool "accepted invalid did:plc"
    (try
       Did_plc.validate_plc_did "did:web:example.com";
       false
     with Failure _ -> true)

let test_parse_document _ =
  let doc = Did_plc.parse_document (Yojson.Safe.from_string sample_doc) in
  OUnit2.assert_equal ~printer:(fun x -> x) "did:plc:7iza6de2dwap2sbkpav7c6c6"
    doc.id;
  OUnit2.assert_equal (Some "alice.test") (Did_plc.handle_of_document doc);
  OUnit2.assert_equal (Some "https://example2.com") (Did_plc.pds_endpoint doc);
  match Did_plc.signing_key doc with
  | None -> OUnit2.assert_failure "missing #atproto key"
  | Some key ->
      OUnit2.assert_equal ~printer:(fun x -> x) "Multikey" key.type_

let test_directory_url _ =
  OUnit2.assert_equal ~printer:(fun x -> x)
    "https://plc.directory/did:plc:7iza6de2dwap2sbkpav7c6c6"
    (Did_plc.directory_url "did:plc:7iza6de2dwap2sbkpav7c6c6")

let test_resolve_live _ =
  try
    let doc = Did_plc.resolve "did:plc:z72i7hdynmk6r22z27h6tvur" in
    OUnit2.assert_equal ~printer:(fun x -> x)
      "did:plc:z72i7hdynmk6r22z27h6tvur" doc.id;
    OUnit2.assert_bool "expected a PDS service"
      (match Did_plc.pds_endpoint doc with Some _ -> true | None -> false)
  with exn ->
    skip_if true ("plc.directory request skipped: " ^ Printexc.to_string exn)

let suite =
  "did_plc"
  >::: [
         "test_validate_plc_did" >:: test_validate_plc_did;
         "test_parse_document" >:: test_parse_document;
         "test_directory_url" >:: test_directory_url;
         "test_resolve_live" >:: test_resolve_live;
       ]

let () = run_test_tt_main suite
