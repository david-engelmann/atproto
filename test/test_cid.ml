open OUnit2
open Atproto.Cid
open Atproto.Base32

let sample_from_repo_tests =
  "bafkreieva64qpnxs7zmwc6ezo7hatq4d22ot7wqlj4hi24zimjqzoye4wq"

let test_roundtrip_constructed _ =
  let cid = Cid.of_digest ~codec:Cid.Dag_cbor (String.make 32 '\xab') in
  let encoded = Cid.to_string cid in
  OUnit2.assert_bool "CID must be multibase base32" (encoded.[0] = 'b');
  let again = Cid.of_string encoded in
  OUnit2.assert_bool "constructed CID roundtrip failed" (Cid.equal cid again);
  OUnit2.assert_equal Cid.Dag_cbor again.codec

let test_roundtrip_raw_constructed _ =
  let cid = Cid.of_digest ~codec:Cid.Raw (String.make 32 '\xcd') in
  OUnit2.assert_equal Cid.Raw (Cid.of_string (Cid.to_string cid)).codec

let test_parse_known_raw _ =
  let cid = Cid.of_string sample_from_repo_tests in
  OUnit2.assert_equal Cid.Raw cid.codec;
  OUnit2.assert_equal ~printer:string_of_int 1 cid.version;
  OUnit2.assert_equal ~printer:(fun x -> x) sample_from_repo_tests
    (Cid.to_string cid)

let test_is_cid _ =
  let encoded = Cid.to_string (Cid.of_digest (String.make 32 '\x00')) in
  OUnit2.assert_bool "valid CID rejected" (Cid.is_cid encoded);
  OUnit2.assert_bool "invalid CID accepted" (not (Cid.is_cid "not-a-cid"))

let test_bytes_roundtrip _ =
  let cid = Cid.of_digest (String.make 32 '\x11') in
  let again = Cid.of_bytes (Cid.to_bytes cid) in
  OUnit2.assert_bool "binary CID roundtrip failed" (Cid.equal cid again)

let test_base32_roundtrip _ =
  let raw = "hello atproto" in
  let encoded = Base32.encode raw in
  OUnit2.assert_equal ~printer:(fun x -> x) raw (Base32.decode encoded)

let suite =
  "cid"
  >::: [
         "test_roundtrip_constructed" >:: test_roundtrip_constructed;
         "test_roundtrip_raw_constructed" >:: test_roundtrip_raw_constructed;
         "test_parse_known_raw" >:: test_parse_known_raw;
         "test_is_cid" >:: test_is_cid;
         "test_bytes_roundtrip" >:: test_bytes_roundtrip;
         "test_base32_roundtrip" >:: test_base32_roundtrip;
       ]

let () = run_test_tt_main suite
