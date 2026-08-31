open OUnit2
open Atproto.Request
open Atproto.Http_method

let test_sample_request_with_body_method_ _ =
  match Request.sample_request_with_body with
  | { method_; _ } -> OUnit2.assert_equal Request.test_get method_

let test_sample_request_with_body_url _ =
  match Request.sample_request_with_body with
  | { url; _ } -> OUnit2.assert_equal "https://github.com/david-engelmann" url

let test_sample_request_with_body_headers _ =
  match Request.sample_request_with_body with
  | { headers; _ } -> (
      match headers with
      | (param_name, _) :: _ -> OUnit2.assert_equal "User-Agent" param_name
      | _ -> OUnit2.assert_equal 0 1)

let test_sample_request_with_body_body _ =
  match Request.sample_request_with_body with
  | { body; _ } -> (
      match body with
      | Some b -> OUnit2.assert_equal "{\"July\": \"Jackson\"}" b
      | None -> OUnit2.assert_equal 0 1)

let test_sample_request_without_body_method_ _ =
  match Request.sample_request_without_body with
  | { method_; _ } -> OUnit2.assert_equal Request.test_get method_

let test_sample_request_without_body_url _ =
  match Request.sample_request_without_body with
  | { url; _ } -> OUnit2.assert_equal "https://github.com/david-engelmann" url

let test_sample_request_without_body_headers _ =
  match Request.sample_request_without_body with
  | { headers; _ } -> (
      match headers with
      | (param_name, _) :: _ -> OUnit2.assert_equal "User-Agent" param_name
      | _ -> OUnit2.assert_equal 0 1)

let test_sample_request_without_body_body _ =
  match Request.sample_request_without_body with
  | { body; _ } -> (
      match body with
      | None -> OUnit2.assert_equal 1 1
      | Some _ -> OUnit2.assert_equal 0 1)

let test_create_helpers _ =
  let get = Request.get "https://public.api.bsky.app/xrpc/ping" () in
  OUnit2.assert_equal Request.test_get get.method_;
  OUnit2.assert_equal None get.body;
  let post =
    Request.post "https://public.api.bsky.app/xrpc/ping" ~body:"{}" ()
  in
  OUnit2.assert_equal Request.test_post post.method_;
  OUnit2.assert_equal (Some "{}") post.body;
  let put = Request.put "https://example.com/x" ~body:"x" () in
  OUnit2.assert_equal Http_method.Put put.method_

let suite =
  "suite"
  >::: [
         "test_sample_request_with_body_method_"
         >:: test_sample_request_with_body_method_;
         "test_sample_request_with_body_url"
         >:: test_sample_request_with_body_url;
         "test_sample_request_with_body_headers"
         >:: test_sample_request_with_body_headers;
         "test_sample_request_with_body_body"
         >:: test_sample_request_with_body_body;
         "test_sample_request_without_body_method_"
         >:: test_sample_request_without_body_method_;
         "test_sample_request_without_body_url"
         >:: test_sample_request_without_body_url;
         "test_sample_request_without_body_headers"
         >:: test_sample_request_without_body_headers;
         "test_sample_request_without_body_body"
         >:: test_sample_request_without_body_body;
         "test_create_helpers" >:: test_create_helpers;
       ]

let () = run_test_tt_main suite
