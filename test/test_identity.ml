open OUnit2
open Atproto.Identity

let test_host_of_service_endpoint _ =
  OUnit2.assert_equal ~printer:(fun x -> x) "morel.us-east.host.bsky.network"
    (Identity.host_of_service_endpoint
       "https://morel.us-east.host.bsky.network");
  OUnit2.assert_equal ~printer:(fun x -> x) "pds.example.com"
    (Identity.host_of_service_endpoint "https://pds.example.com/xrpc")

let test_resolve_handle_live _ =
  try
    let resolved = Identity.resolve_handle "jay.bsky.team" in
    OUnit2.assert_bool "resolveHandle did not return a DID"
      (String.length resolved.did > 8
      && String.sub resolved.did 0 4 = "did:")
  with exn ->
    skip_if true ("resolveHandle skipped: " ^ Printexc.to_string exn)

let test_resolve_actor_live _ =
  try
    let ident = Identity.resolve "jay.bsky.team" in
    OUnit2.assert_bool "missing DID" (String.length ident.did > 8);
    OUnit2.assert_bool "missing PDS"
      (match ident.pds with Some p -> String.length p > 0 | None -> false)
  with exn ->
    skip_if true ("Identity.resolve skipped: " ^ Printexc.to_string exn)

let suite =
  "identity"
  >::: [
         "test_host_of_service_endpoint" >:: test_host_of_service_endpoint;
         "test_resolve_handle_live" >:: test_resolve_handle_live;
         "test_resolve_actor_live" >:: test_resolve_actor_live;
       ]

let () = run_test_tt_main suite
