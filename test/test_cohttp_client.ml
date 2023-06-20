open OUnit2
open Atproto.Cohttp_client

let test_cohttp_client_get_with_quotes_to_scrape _ =
  let body = Lwt_main.run (Cohttp_client.get_host "quotes.toscrape.com" 80) in
  OUnit2.assert_bool "Body is not empty" (body <> "")

let test_cohttp_client_get_with_httpbin _ =
  let body = Lwt_main.run (Cohttp_client.get_host "httpbin.org" 80) in
  OUnit2.assert_bool "Body is not empty" (body <> "")

let suite =
  "suite"
  >::: [
        "test_cohttp_client_get_with_quotes_to_scrape" >::
            test_cohttp_client_get_with_quotes_to_scrape; "test_cohttp_client_get_with_httpbin" >:: test_cohttp_client_get_with_httpbin;
       ]

let () = run_test_tt_main suite
