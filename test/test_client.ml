open OUnit2
open Bluesky.Http_client

let test_http_client_with_quotes_to_scrape _ =
    Http_client.start_client "https://quotes.toscrape.com"
    OUnit2.assert_equal 1 1

let suite =
  "suite"
  >::: [
         "test_http_client_with_quotes_to_scrape" >:: test_http_client_with_quotes_to_scrape;
       ]

let () = run_test_tt_main suite
