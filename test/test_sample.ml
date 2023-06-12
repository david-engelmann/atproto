open OUnit2
open Bluesky.Sample

let test_addition _ =
  OUnit2.assert_equal 6 (Sample.add1 Sample.sample_x Sample.sample_y)

let test_mysampletest _ =
  match Sample.mysampletest with
  | MySample { value } -> OUnit2.assert_equal "test" value

let test_mysample4 _ =
  match Sample.mysample4 with
  | MySample { value } -> OUnit2.assert_equal 4 value

let test_mysample4float _ =
  match Sample.mysample4float with
  | MySample { value } -> OUnit2.assert_equal 4.0 value

let suite =
  "suite"
  >::: [
         "test_addition" >:: test_addition;
         "test_mysampletest" >:: test_mysampletest;
         "test_mysample4" >:: test_mysample4;
         "test_mysample4float" >:: test_mysample4float;
       ]

let () = run_test_tt_main suite
