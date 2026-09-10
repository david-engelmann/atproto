open OUnit2
open Atproto.Lt_hash
open Atproto.Hash

(* Official empty-state sha256 from bluesky-social/atproto#5187
   packages/space/tests/lthash.test.ts *)
let empty_hash_hex =
  "e5a00aa9991ac8a5ee3109844d84a55583bd20572ad3ffcd42792f3c36b183ad"

(* Snapshot vector from the same file: add "one" then "two". *)
let one_two_hash_hex =
  "ae05cb6d224379d9710c290c8529945c5b0e0fde9ead30b9699057ce701c63e7"

(* Official BLAKE3("abc") 32-byte digest (XOF prefix). *)
let blake3_abc_hex =
  "6437b3ac38465133ffb63b75273a8db548c558465d79db03fd359c6cd5bd9d85"

(* Multi-chunk (> 1024-byte input) lock for the parent-tree path. *)
let blake3_1025_zeros_hex =
  "d2beb49d87e59db174cb3ff1440f1899422968df670d060fd7ce759e8cc160e7"

let hex digest = Hash.hex_encode digest

let test_empty _ =
  let h = Lt_hash.empty () in
  OUnit2.assert_equal ~printer:string_of_int Lt_hash.state_bytes
    (String.length (Lt_hash.state h));
  OUnit2.assert_equal ~printer:string_of_int Lt_hash.lanes 1024;
  OUnit2.assert_bool "empty state" (Lt_hash.is_empty h);
  OUnit2.assert_equal ~printer:(fun x -> x) empty_hash_hex (hex (Lt_hash.hash h));
  OUnit2.assert_equal ~printer:string_of_int 32 (String.length (Lt_hash.hash h))

let test_add_then_remove _ =
  let h = Lt_hash.add (Lt_hash.empty ()) "a" in
  OUnit2.assert_bool "non-empty after add" (not (Lt_hash.is_empty h));
  let h = Lt_hash.remove h "a" in
  OUnit2.assert_bool "empty after remove" (Lt_hash.is_empty h);
  OUnit2.assert_equal ~printer:(fun x -> x) empty_hash_hex (hex (Lt_hash.hash h))

let test_order_independent _ =
  let a = Lt_hash.add (Lt_hash.add (Lt_hash.empty ()) "a") "b" in
  let b = Lt_hash.add (Lt_hash.add (Lt_hash.empty ()) "b") "a" in
  OUnit2.assert_bool "equal states" (Lt_hash.equal a b);
  OUnit2.assert_equal ~printer:(fun x -> x) (hex (Lt_hash.hash a))
    (hex (Lt_hash.hash b))

let test_distinguishes _ =
  OUnit2.assert_bool "different elements"
    (not
       (Lt_hash.equal
          (Lt_hash.add (Lt_hash.empty ()) "a")
          (Lt_hash.add (Lt_hash.empty ()) "b")))

let test_multiset _ =
  let h = Lt_hash.add (Lt_hash.add (Lt_hash.empty ()) "a") "a" in
  OUnit2.assert_bool "double-add is not empty" (not (Lt_hash.is_empty h));
  let h = Lt_hash.remove h "a" in
  OUnit2.assert_bool "one remain"
    (Lt_hash.equal h (Lt_hash.add (Lt_hash.empty ()) "a"))

let test_add_does_not_mutate _ =
  let original = Lt_hash.add (Lt_hash.empty ()) "a" in
  let staged = Lt_hash.add original "b" in
  OUnit2.assert_bool "original unchanged"
    (Lt_hash.equal original (Lt_hash.add (Lt_hash.empty ()) "a"));
  OUnit2.assert_bool "staged differs" (not (Lt_hash.equal original staged))

let test_of_state_roundtrip _ =
  let a = Lt_hash.add (Lt_hash.add (Lt_hash.empty ()) "a") "b" in
  OUnit2.assert_bool "resume" (Lt_hash.equal (Lt_hash.of_state (Lt_hash.state a)) a)

let test_of_state_wrong_length _ =
  OUnit2.assert_raises
    (Lt_hash.Invalid "Lt_hash state must be 2048 bytes, got 32") (fun () ->
      ignore (Lt_hash.of_state (String.make 32 '\000')))

let test_of_state_no_alias _ =
  let raw = Bytes.make Lt_hash.state_bytes '\000' in
  Bytes.set raw 0 (Char.chr 0xff);
  let h = Lt_hash.of_state (Bytes.to_string raw) in
  Bytes.set raw 0 (Char.chr 0x00);
  OUnit2.assert_equal ~printer:string_of_int 0xff
    (Char.code (Lt_hash.state h).[0])

let test_state_no_alias _ =
  let h = Lt_hash.add (Lt_hash.empty ()) "a" in
  let snap = Bytes.of_string (Lt_hash.state h) in
  Bytes.set snap 0 (Char.chr (Char.code (Bytes.get snap 0) lxor 0xff));
  OUnit2.assert_bool "handed-out state is a copy"
    (Lt_hash.state h <> Bytes.to_string snap)

let test_copy _ =
  let a = Lt_hash.add (Lt_hash.empty ()) "a" in
  let b = Lt_hash.copy a in
  OUnit2.assert_bool "copy equal" (Lt_hash.equal a b);
  let b = Lt_hash.add b "b" in
  OUnit2.assert_bool "copy independent" (not (Lt_hash.equal a b))

let test_snapshot_vector _ =
  let h = Lt_hash.add (Lt_hash.add (Lt_hash.empty ()) "one") "two" in
  OUnit2.assert_equal ~printer:(fun x -> x) one_two_hash_hex
    (hex (Lt_hash.hash h))

let test_blake3_abc _ =
  let d = Lt_hash.blake3_xof "abc" 32 in
  OUnit2.assert_equal ~printer:(fun x -> x) blake3_abc_hex (hex d)

let test_blake3_xof_prefix _ =
  let full = Lt_hash.blake3_xof "abc" 64 in
  let short = Lt_hash.blake3_xof "abc" 32 in
  OUnit2.assert_equal ~printer:(fun x -> x) short (String.sub full 0 32)

let test_blake3_tree _ =
  let d = Lt_hash.blake3_xof (String.make 1025 '\000') 32 in
  OUnit2.assert_equal ~printer:(fun x -> x) blake3_1025_zeros_hex (hex d)

let test_element _ =
  OUnit2.assert_equal ~printer:(fun x -> x)
    "app.bsky.feed.post/3jzfcijpj2z2a/bafyreia"
    (Lt_hash.element ~collection:"app.bsky.feed.post" ~rkey:"3jzfcijpj2z2a"
       ~record_cid:"bafyreia");
  let id =
    Lt_hash.element ~collection:"app.bsky.feed.post" ~rkey:"3jzfcijpj2z2a"
      ~record_cid:"bafyreia"
  in
  let h = Lt_hash.add (Lt_hash.empty ()) id in
  let h = Lt_hash.remove h id in
  OUnit2.assert_bool "record element add/remove" (Lt_hash.is_empty h)

let test_blake3_negative _ =
  OUnit2.assert_raises (Invalid_argument "Lt_hash.blake3_xof: negative length")
    (fun () -> ignore (Lt_hash.blake3_xof "abc" (-1)))

let suite =
  "lt_hash"
  >::: [
         "test_empty" >:: test_empty;
         "test_add_then_remove" >:: test_add_then_remove;
         "test_order_independent" >:: test_order_independent;
         "test_distinguishes" >:: test_distinguishes;
         "test_multiset" >:: test_multiset;
         "test_add_does_not_mutate" >:: test_add_does_not_mutate;
         "test_of_state_roundtrip" >:: test_of_state_roundtrip;
         "test_of_state_wrong_length" >:: test_of_state_wrong_length;
         "test_of_state_no_alias" >:: test_of_state_no_alias;
         "test_state_no_alias" >:: test_state_no_alias;
         "test_copy" >:: test_copy;
         "test_snapshot_vector" >:: test_snapshot_vector;
         "test_blake3_abc" >:: test_blake3_abc;
         "test_blake3_xof_prefix" >:: test_blake3_xof_prefix;
         "test_blake3_tree" >:: test_blake3_tree;
         "test_element" >:: test_element;
         "test_blake3_negative" >:: test_blake3_negative;
       ]

let () = run_test_tt_main suite
