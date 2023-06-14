open OUnit2
open Bluesky.Cohttp_client
open Lwt.Infix


let test_http_client_with_quotes_to_scrape _ =
  let body = Lwt_main.run (Cohttp_client.get_host "quotes.toscrape.com" 443) in
  assert_bool "Body is not empty" (body <> "")

let suite =
  "suite"
  >::: [
        "test_http_client_with_quotes_to_scrape" >:: test_http_client_with_quotes_to_scrape;
       ]

let () = run_test_tt_main suite
