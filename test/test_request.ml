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
