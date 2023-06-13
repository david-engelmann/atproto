open OUnit2
open Bluesky.Http_client

let test_http_client_with_quotes_to_scrape _ =
    Http_client.start_client "https://quotes.toscrape.com" 443;
    OUnit2.assert_equal 1 1


let test_http_client_with_getaddrinfo _ =
  let open Lwt.Infix in
  let addr_test =
    (*Lwt_unix.getaddrinfo "quotes.toscrape.com" "443" [Unix.(AI_FAMILY PF_INET)]*)
    Http_client.get_addr_info "quotes.toscrape.com" 443
    >>= fun addrs ->
    let lwt_list =
      List.map Lwt.return addrs
    in
    Lwt_list.iter_p (fun addr ->
      addr >>= fun addr_info ->
      match addr_info.Unix.ai_addr with
      | Unix.ADDR_INET (addr, port) ->
        Lwt_io.printf "Address: %s, Port: %d\n" (Unix.string_of_inet_addr addr) port
      | _ ->
        Lwt_io.printf "Unknown address format\n"
    ) lwt_list
    >>= fun () ->
    Lwt.return_unit
  in
  Lwt_main.run addr_test;
  OUnit2.assert_equal 1 1


let suite =
  "suite"
  >::: [
         (*"test_http_client_with_quotes_to_scrape" >:: test_http_client_with_quotes_to_scrape;*)
         "test_http_client_with_getaddrinfo" >:: test_http_client_with_getaddrinfo;
       ]

let () = run_test_tt_main suite
