open OUnit2
open Atproto.Base32

let test_empty _ =
  OUnit2.assert_equal ~printer:(fun x -> x) "" (Base32.encode "");
  OUnit2.assert_equal ~printer:(fun x -> x) "" (Base32.decode "")

let test_roundtrip _ =
  List.iter
    (fun s ->
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        s
        (Base32.decode (Base32.encode s)))
    [ "f"; "fo"; "foo"; "foob"; "fooba"; "foobar"; "hello world" ]

let test_rfc4648_vectors _ =
  (* RFC 4648 lowercase, unpadded — same alphabet CIDv1 multibase uses. *)
  OUnit2.assert_equal ~printer:(fun x -> x) "my" (Base32.encode "f");
  OUnit2.assert_equal ~printer:(fun x -> x) "mzxw6" (Base32.encode "foo");
  OUnit2.assert_equal ~printer:(fun x -> x) "mzxw6ytb" (Base32.encode "fooba");
  OUnit2.assert_equal ~printer:(fun x -> x) "fooba" (Base32.decode "mzxw6ytb")

let test_cid_multibase_prefix _ =
  let digest = String.make 32 '\xab' in
  let encoded = Base32.encode digest in
  OUnit2.assert_equal ~printer:(fun x -> x) digest (Base32.decode encoded);
  OUnit2.assert_bool "lowercase alphabet"
    (String.for_all
       (function 'a' .. 'z' | '2' .. '7' -> true | _ -> false)
       encoded)

let suite =
  "base32"
  >::: [
         "test_empty" >:: test_empty;
         "test_roundtrip" >:: test_roundtrip;
         "test_rfc4648_vectors" >:: test_rfc4648_vectors;
         "test_cid_multibase_prefix" >:: test_cid_multibase_prefix;
       ]

let () = run_test_tt_main suite
