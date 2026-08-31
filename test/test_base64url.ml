open OUnit2
open Atproto.Base64url
open Atproto.Base58

(* RFC 4648 §10 vectors *)
let test_std_vectors _ =
  OUnit2.assert_equal ~printer:(fun x -> x) "Zg==" (Base64url.encode_std "f");
  OUnit2.assert_equal ~printer:(fun x -> x) "Zm8=" (Base64url.encode_std "fo");
  OUnit2.assert_equal ~printer:(fun x -> x) "Zm9v" (Base64url.encode_std "foo");
  OUnit2.assert_equal ~printer:(fun x -> x) "f" (Base64url.decode_std "Zg==");
  OUnit2.assert_equal ~printer:(fun x -> x) "foo" (Base64url.decode_std "Zm9v")

let test_url_unpadded _ =
  let raw = "hello?>" in
  let encoded = Base64url.encode raw in
  OUnit2.assert_bool "no padding" (not (String.contains encoded '='));
  OUnit2.assert_equal ~printer:(fun x -> x) raw (Base64url.decode encoded)

let test_base58_hello _ =
  (* Bitcoin-style base58 of "Hello World" *)
  let encoded = Base58.encode "Hello World" in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "Hello World" (Base58.decode encoded)

let test_base58_leading_zeros _ =
  let raw = "\x00\x00hi" in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    raw
    (Base58.decode (Base58.encode raw))

let suite =
  "base64url"
  >::: [
         "test_std_vectors" >:: test_std_vectors;
         "test_url_unpadded" >:: test_url_unpadded;
         "test_base58_hello" >:: test_base58_hello;
         "test_base58_leading_zeros" >:: test_base58_leading_zeros;
       ]

let () = run_test_tt_main suite
