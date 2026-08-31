open OUnit2
open Atproto.Varint

let roundtrip n =
  let encoded = Varint.encode n in
  let decoded, consumed = Varint.decode encoded in
  OUnit2.assert_equal ~printer:string_of_int n decoded;
  OUnit2.assert_equal ~printer:string_of_int (String.length encoded) consumed

let test_small _ = List.iter roundtrip [ 0; 1; 127; 128; 255; 300; 16384 ]

let test_truncated _ =
  OUnit2.assert_raises (Failure "Varint.decode: truncated") (fun () ->
      ignore (Varint.decode "\x80"))

let test_negative _ =
  OUnit2.assert_raises (Invalid_argument "Varint.encode: negative") (fun () ->
      ignore (Varint.encode (-1)))

let test_decode_from _ =
  let prefix = "xx" in
  let body = Varint.encode 42 in
  let n, off = Varint.decode_from (prefix ^ body) 2 in
  OUnit2.assert_equal ~printer:string_of_int 42 n;
  OUnit2.assert_equal ~printer:string_of_int (2 + String.length body) off

let suite =
  "varint"
  >::: [
         "test_small" >:: test_small;
         "test_truncated" >:: test_truncated;
         "test_negative" >:: test_negative;
         "test_decode_from" >:: test_decode_from;
       ]

let () = run_test_tt_main suite
