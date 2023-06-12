open OUnit2
open Bluesky.Method

let sample_get_method : Method.method = Get
let sample_post_method : Method.method = Post

let test_lookup_method_with_get _ =
  OUnit2.assert_equal (Method.lookup_method "get") sample_get_method

let test_lookup_method_with_post _ =
  OUnit2.assert_equal (Method.lookup_method "post") sample_post_method

let suite =
  "suite"
  >::: [
         "test_lookup_method_with_get" >:: test_lookup_method_with_get;
         "test_lookup_method_with_post" >:: test_lookup_method_with_post;
       ]

let () = run_test_tt_main suite
