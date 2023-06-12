open OUnit2
open Bluesky.Response

let sample_content : bytes = Bytes.create 12;
Bytes.set sample_content 0 'J';
Bytes.set sample_content 1 'u';
Bytes.set sample_content 2 'l';
Bytes.set sample_content 3 'y';
Bytes.set sample_content 4 0x20;
Bytes.set sample_content 5 'J';
Bytes.set sample_content 6 'a';
Bytes.set sample_content 7 'c';
Bytes.set sample_content 8 'k';
Bytes.set sample_content 9 's';
Bytes.set sample_content 10 'o';
Bytes.set sample_content 11 'n';

let sample_response : Response.response = {
    success = true;
    status_code = 200;
    content = sample_content;
    headers = [("User-Agent", "david-engelmann/bluesky (OCaml SDK)")];
  }

let test_sample_response_success _ =
  match sample_response with
   | { success; _ } ->
      OUnit2.assert_equal true success

let test_sample_response_status_code _ =
  match sample_response with
   | { status_code; _ } ->
      OUnit2.assert_equal 200 status_code

let test_sample_response_content _ =
  match sample_response with
   | { content; _ } ->
      OUnit2.assert_equal "July Jackson" Bytes.to_string content

let test_sample_response_headers _ =
  match sample_response with
   | { headers; _ } ->
     match headers with
      | (param_name, _) :: _ ->
        OUnit2.assert_equal "User-Agent" param_name

