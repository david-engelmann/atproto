open OUnit2
open Atproto.Did_key
open Atproto.Hash
open Atproto.Base58

let rfc6979_p256_priv =
  Hash.hex_decode
    "c9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721"

let p256_pair () =
  match Mirage_crypto_ec.P256.Dsa.priv_of_octets rfc6979_p256_priv with
  | Error _ -> failwith "could not load RFC 6979 P-256 private key"
  | Ok priv -> (priv, Mirage_crypto_ec.P256.Dsa.pub_of_priv priv)

let test_base58_roundtrip _ =
  let raw = "hello did:key" in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    raw
    (Base58.decode (Base58.encode raw))

let test_p256_roundtrip _ =
  let _priv, pub = p256_pair () in
  let octets = Mirage_crypto_ec.P256.Dsa.pub_to_octets ~compress:true pub in
  let key = Did_key.of_p256_octets octets in
  let encoded = Did_key.to_string key in
  OUnit2.assert_bool "did:key prefix" (Did_key.is_did_key encoded);
  let again = Did_key.of_string encoded in
  OUnit2.assert_equal Did_key.P256 again.curve;
  OUnit2.assert_equal ~printer:(fun x -> x) octets again.public_key;
  match Did_key.p256_pub again with
  | None -> OUnit2.assert_failure "compressed P-256 public key rejected"
  | Some pub2 ->
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        (Mirage_crypto_ec.P256.Dsa.pub_to_octets ~compress:true pub)
        (Mirage_crypto_ec.P256.Dsa.pub_to_octets ~compress:true pub2)

let test_rejects_not_did_key _ =
  OUnit2.assert_bool "accepted did:plc"
    (try
       ignore (Did_key.of_string "did:plc:7iza6de2dwap2sbkpav7c6c6");
       false
     with Failure _ -> true)

let suite =
  "did_key"
  >::: [
         "test_base58_roundtrip" >:: test_base58_roundtrip;
         "test_p256_roundtrip" >:: test_p256_roundtrip;
         "test_rejects_not_did_key" >:: test_rejects_not_did_key;
       ]

let () = run_test_tt_main suite
