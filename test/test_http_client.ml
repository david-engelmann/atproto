open OUnit2
open Bluesky.Http_client

let test_http_client_with_quotes_to_scrape _ =
    Http_client.start_client "https://quotes.toscrape.com" 443;
    OUnit2.assert_equal 1 1
let result_func = (fun x -> x)
let unpack_addr_info addr =
    match addr.Unix.ai_addr with
     | Unix.ADDR_UNIX _ -> None
     | ADDR_INET (addr, port) -> Some (addr, port)

let test_http_client_with_getaddrinfo _ =
    (Lwt_main.run
    (List.filter_map (fun (addrs : Unix.addr_info Lwt.t list) ->
        match addrs with
         (*| _ -> (List.map unpack_addr_info) ((List.map Lwt_main.run) addrs))*)
         (*| _ -> (List.map unpack_addr_info) (Lwt_main.run addrs))*)
         | _ -> (List.map unpack_addr_info) (Http_client.convert_lwt_list addrs))
    ( Lwt_unix.getaddrinfo "https://quotes.toscrape.com" "443" [
        Unix.(AI_FAMILY PF_INET) ])));
    OUnit2.assert_equal 1 1

let suite =
  "suite"
  >::: [
         "test_http_client_with_quotes_to_scrape" >:: test_http_client_with_quotes_to_scrape;
         "test_http_client_with_getaddrinfo" >:: test_http_client_with_getaddrinfo;
       ]

let () = run_test_tt_main suite
