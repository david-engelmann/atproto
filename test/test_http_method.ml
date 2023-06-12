open OUnit2
open Bluesky.Http_method

let sample_get_http_method : Http_method.http_method = Get
let sample_post_http_method : Http_method.http_method = Post

let test_lookup_http_method_with_get _ =
  OUnit2.assert_equal (Http_method.lookup_http_method "get") sample_get_http_method

let test_lookup_http_method_with_post _ =
  OUnit2.assert_equal (Http_method.lookup_http_method "post") sample_post_http_method

let suite =
  "suite"
  >::: [
         "test_lookup_http_method_with_get" >:: test_lookup_http_method_with_get;
         "test_lookup_http_method_with_post" >:: test_lookup_http_method_with_post;
       ]

let () = run_test_tt_main suite
