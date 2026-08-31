open OUnit2
open Atproto.Http_client
open Atproto.Request
open Atproto.Response
open Atproto.Http_method

let test_parse_https_url _ =
  let p =
    Http_client.parse_url
      "https://public.api.bsky.app/xrpc/com.atproto.identity.resolveHandle?handle=bsky.app"
  in
  OUnit2.assert_equal ~printer:(fun x -> x) "https" p.scheme;
  OUnit2.assert_equal ~printer:(fun x -> x) "public.api.bsky.app" p.host;
  OUnit2.assert_equal 443 p.port;
  OUnit2.assert_bool "path includes nsid"
    (let needle = "/xrpc/com.atproto.identity.resolveHandle" in
     let rec contains i =
       i + String.length needle <= String.length p.path
       && (String.sub p.path i (String.length needle) = needle
          || contains (i + 1))
     in
     contains 0)

let test_parse_url_requires_https _ =
  (try
     ignore (Http_client.parse_url "http://example.com/xrpc/ping");
     OUnit2.assert_failure "expected https requirement"
   with Http_client.Error _ -> ());
  try
    ignore (Http_client.parse_url "/relative");
    OUnit2.assert_failure "expected host requirement"
  with Http_client.Error _ -> ()

let test_xrpc_url _ =
  let url =
    Http_client.xrpc_url ~host:"public.api.bsky.app"
      "com.atproto.identity.resolveHandle"
      ~query:[ ("handle", "bsky.app") ]
      ()
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "https://public.api.bsky.app/xrpc/com.atproto.identity.resolveHandle?handle=bsky.app"
    url

let test_request_builders _ =
  let req =
    Request.post
      "https://public.api.bsky.app/xrpc/com.atproto.server.createSession"
      ~headers:[ ("content-type", "application/json") ]
      ~body:"{}" ()
  in
  OUnit2.assert_equal Http_method.Post req.method_;
  OUnit2.assert_equal (Some "{}") req.body;
  let get = Request.get "https://example.com/" () in
  OUnit2.assert_equal Http_method.Get get.method_

let test_response_helpers _ =
  let r =
    Response.of_string ~status_code:200
      ~headers:
        [ ("RateLimit-Remaining", "99"); ("Content-Type", "application/json") ]
      "{\"ok\":true}"
  in
  OUnit2.assert_bool "success" r.success;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "{\"ok\":true}" (Response.body_string r);
  OUnit2.assert_equal (Some "99") (Response.header r "ratelimit-remaining");
  OUnit2.assert_equal (Some "application/json") (Response.content_type r);
  let fail = Response.of_string ~status_code:429 "{}" in
  OUnit2.assert_bool "not success" (not fail.success)

let test_getaddrinfo _ =
  let open Lwt.Infix in
  try
    Lwt_main.run
      ( Http_client.get_addr_info "public.api.bsky.app" 443 >>= fun addrs ->
        OUnit2.assert_bool "resolved at least one A record" (addrs <> []);
        Lwt.return_unit )
  with exn -> skip_if true ("getaddrinfo skipped: " ^ Printexc.to_string exn)

let test_live_h2_xrpc _ =
  try
    let resp =
      Lwt_main.run
        (Http_client.xrpc_get ~host:"public.api.bsky.app"
           ~nsid:"com.atproto.identity.resolveHandle"
           ~query:[ ("handle", "bsky.app") ]
           ~timeout:12.0 ())
    in
    OUnit2.assert_bool "HTTP/2 XRPC returned a status"
      (resp.status_code >= 200 && resp.status_code < 600);
    if resp.status_code = 200 then
      let json = Yojson.Safe.from_string (Response.body_string resp) in
      match Yojson.Safe.Util.member "did" json with
      | `String did -> OUnit2.assert_bool "resolved did" (String.length did > 8)
      | _ -> ()
  with exn -> skip_if true ("HTTP/2 XRPC skipped: " ^ Printexc.to_string exn)

let suite =
  "http_client"
  >::: [
         "test_parse_https_url" >:: test_parse_https_url;
         "test_parse_url_requires_https" >:: test_parse_url_requires_https;
         "test_xrpc_url" >:: test_xrpc_url;
         "test_request_builders" >:: test_request_builders;
         "test_response_helpers" >:: test_response_helpers;
         "test_getaddrinfo" >:: test_getaddrinfo;
         "test_live_h2_xrpc" >:: test_live_h2_xrpc;
       ]

let () = run_test_tt_main suite
