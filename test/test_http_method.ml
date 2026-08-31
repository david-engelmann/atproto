open OUnit2
open Atproto.Http_method

let sample_get_http_method : Http_method.http_method = Get
let sample_post_http_method : Http_method.http_method = Post

let test_lookup_http_method_with_get _ =
  OUnit2.assert_equal
    (Http_method.lookup_http_method "get")
    sample_get_http_method

let test_lookup_http_method_with_post _ =
  OUnit2.assert_equal
    (Http_method.lookup_http_method "post")
    sample_post_http_method

let test_lookup_remaining_verbs _ =
  OUnit2.assert_equal Http_method.Put (Http_method.lookup_http_method "PUT");
  OUnit2.assert_equal Http_method.Delete
    (Http_method.lookup_http_method "Delete");
  OUnit2.assert_equal Http_method.Patch (Http_method.lookup_http_method "patch");
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "PATCH"
    (Http_method.to_string Patch)

let test_unknown_method _ =
  OUnit2.assert_raises (Failure "Not Recognized Method") (fun () ->
      ignore (Http_method.lookup_http_method "trace"))

let suite =
  "suite"
  >::: [
         "test_lookup_http_method_with_get" >:: test_lookup_http_method_with_get;
         "test_lookup_http_method_with_post"
         >:: test_lookup_http_method_with_post;
         "test_lookup_remaining_verbs" >:: test_lookup_remaining_verbs;
         "test_unknown_method" >:: test_unknown_method;
       ]

let () = run_test_tt_main suite
