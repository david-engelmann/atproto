open OUnit2
open Atproto.Hash

(* SHA-256("abc") from FIPS 180-4 *)
let sha256_abc =
  "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"

let test_sha256_abc _ =
  OUnit2.assert_equal ~printer:(fun x -> x) sha256_abc (Hash.sha256_hex "abc")

let test_hex_roundtrip _ =
  let raw = Hash.sha256 "hello atproto" in
  let hex = Hash.hex_encode raw in
  OUnit2.assert_equal ~printer:string_of_int 64 (String.length hex);
  OUnit2.assert_equal ~printer:(fun x -> x) raw (Hash.hex_decode hex)

let test_hex_decode_odd _ =
  OUnit2.assert_raises (Failure "Hash.hex_decode: odd length") (fun () ->
      ignore (Hash.hex_decode "abc"))

let test_sha1_known _ =
  (* SHA-1("") = da39a3ee5e6b4b0d3255bfef95601890afd80709 *)
  let digest = Hash.sha1 "" in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "da39a3ee5e6b4b0d3255bfef95601890afd80709" (Hash.hex_encode digest)

let suite =
  "hash"
  >::: [
         "test_sha256_abc" >:: test_sha256_abc;
         "test_hex_roundtrip" >:: test_hex_roundtrip;
         "test_hex_decode_odd" >:: test_hex_decode_odd;
         "test_sha1_known" >:: test_sha1_known;
       ]

let () = run_test_tt_main suite
