open OUnit2
open Bluesky.Http_client

let test_http_client_with_quotes_to_scrape _ =
    Http_client.start_client "https://quotes.toscrape.com"
