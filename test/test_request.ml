open OUnit2
open Bluesky.Request
open Bluesky.Http_method

let sample_request_with_body : Request.request = {
    method_ : Http_method.Get;
    url : "https://github.com/david-engelmann";
    headers : [("User-Agent", "david-engelmann/bluesky (OCaml SDK)")];
    body : Some "{\"July\": \"Jackson\"}";
}

let sample_request_without_body : Request.request = {
    method_ = Http_method.Get;
    url = "https://github.com/david-engelmann";
    headers = [("User-Agent", "david-engelmann/bluesky (OCaml SDK)")];
    body = None
}

let test_sample_request_with_body_method_ _ =
  match sample_request_with_body with
   | { method_; _ } ->
      OUnit2.assert_equal Http_method.Get method_

let test_sample_request_with_body_url _ =
  match sample_request_with_body with
   | { url; _ } ->
      OUnit2.assert_equal "https://github.com/david-engelmann" url

let test_sample_request_with_body_headers _ =
  match sample_request_with_body with
   | { headers; _ } ->
    match headers with
     | (param_name, _) :: _ ->
       OUnit2.assert_equal "User-Agent" param_name
     | _ -> OUnit2.assert_equal 0 1

let test_sample_request_with_body_body _ =
  match sample_request_with_body with
   | { body; _ } ->
     match body with
      | Some b ->
        OUnit2.assert_equal "{\"July\": \"Jackson\"}" b
      | None ->
        OUnit2.assert_equal 0 1

