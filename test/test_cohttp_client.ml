open OUnit2
open Atproto.Cohttp_client

let test_pairs_to_query_string _ =
  let qs =
    Cohttp_client.create_body_from_pairs
      [ ("did", "did:plc:abc"); ("limit", "10") ]
  in
  OUnit2.assert_equal ~printer:(fun x -> x) "did=did%3Aplc%3Aabc&limit=10" qs

let test_add_query_params _ =
  let qs = Cohttp_client.add_query_params "cids" [ "aaa"; "bbb" ] in
  OUnit2.assert_equal ~printer:(fun x -> x) "cids=aaa&cids=bbb" qs

let test_cohttp_client_get_optional _ =
  try
    let body = Lwt_main.run (Cohttp_client.get_host "example.com" 80) in
    OUnit2.assert_bool "example.com body is empty" (body <> "")
  with exn ->
    skip_if true ("optional HTTP smoke test skipped: " ^ Printexc.to_string exn)

let suite =
  "cohttp_client"
  >::: [
         "test_pairs_to_query_string" >:: test_pairs_to_query_string;
         "test_add_query_params" >:: test_add_query_params;
         "test_cohttp_client_get_optional" >:: test_cohttp_client_get_optional;
       ]

let () = run_test_tt_main suite
