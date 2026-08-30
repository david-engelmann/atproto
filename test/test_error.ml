open OUnit2
open Atproto.Error

let test_parse_rate_limit _ =
  let json =
    Yojson.Safe.from_string
      {|{"error":"RateLimitExceeded","message":"Rate Limit Exceeded"}|}
  in
  match Error.parse_error json with
  | `RateLimitExceeded e ->
      OUnit2.assert_equal ~printer:(fun x -> x) "Rate Limit Exceeded" e.message
  | `Xrpc _ -> OUnit2.assert_failure "expected RateLimitExceeded"

let test_of_body_ok _ =
  match Error.of_body {|{"did":"did:plc:abc"}|} with
  | None -> ()
  | Some _ -> OUnit2.assert_failure "did not expect an error object"

let test_of_body_error _ =
  match
    Error.of_body
      {|{"error":"HandleNotFound","message":"Unable to resolve handle"}|}
  with
  | None -> OUnit2.assert_failure "expected error object"
  | Some e -> OUnit2.assert_equal ~printer:(fun x -> x) "HandleNotFound" e.error

let test_to_string _ =
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "Nope: bad"
    (Error.to_string { error = "Nope"; message = "bad" })

let suite =
  "error"
  >::: [
         "test_parse_rate_limit" >:: test_parse_rate_limit;
         "test_of_body_ok" >:: test_of_body_ok;
         "test_of_body_error" >:: test_of_body_error;
         "test_to_string" >:: test_to_string;
       ]

let () = run_test_tt_main suite
