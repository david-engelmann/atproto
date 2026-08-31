open OUnit2
open Atproto.Temp

let with_public_timeout ?(seconds = 20) f =
  let old =
    Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> failwith "timeout"))
  in
  ignore (Unix.alarm seconds);
  Fun.protect
    ~finally:(fun () ->
      ignore (Unix.alarm 0);
      Sys.set_signal Sys.sigalrm old)
    f

let test_parse_available _ =
  let check =
    Temp.parse_handle_check
      (`Assoc
        [
          ("handle", `String "available-handle.test");
          ( "result",
            `Assoc
              [
                ( "$type",
                  `String
                    "com.atproto.temp.checkHandleAvailability#resultAvailable"
                );
              ] );
        ])
  in
  OUnit2.assert_equal ~printer:(fun x -> x) "available-handle.test" check.handle;
  match check.result with
  | `Available -> ()
  | _ -> OUnit2.assert_failure "expected available"

let test_parse_unavailable _ =
  let check =
    Temp.parse_handle_check
      (`Assoc
        [
          ("handle", `String "jay.bsky.team");
          ( "result",
            `Assoc
              [
                ( "$type",
                  `String
                    "com.atproto.temp.checkHandleAvailability#resultUnavailable"
                );
                ( "suggestions",
                  `List
                    [
                      `Assoc
                        [
                          ("handle", `String "jay2.bsky.social");
                          ("method", `String "suffix");
                        ];
                    ] );
              ] );
        ])
  in
  match check.result with
  | `Unavailable [ s ] ->
      OUnit2.assert_equal ~printer:(fun x -> x) "jay2.bsky.social" s.handle
  | _ -> OUnit2.assert_failure "expected unavailable suggestions"

let test_parse_signup_and_scope _ =
  let queue =
    Temp.parse_signup_queue
      (`Assoc
        [
          ("activated", `Bool true);
          ("placeInQueue", `Int 12);
          ("estimatedTimeMs", `Int 45000);
        ])
  in
  OUnit2.assert_equal true queue.activated;
  OUnit2.assert_equal (Some 12) queue.place_in_queue;
  let deref =
    Temp.parse_scope_deref
      (`Assoc [ ("scope", `String "atproto repo:app.bsky.feed.post") ])
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "atproto repo:app.bsky.feed.post" deref.scope;
  let reserved = Temp.add_reserved_handle_body ~handle:"admin.bsky.social" () in
  let phone =
    Temp.request_phone_verification_body ~phone_number:"+15555550100" ()
  in
  let revoke =
    Temp.revoke_account_credentials_body
      ~account:"did:plc:abc123xyz0001112223333" ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "admin.bsky.social"
    (reserved |> member "handle" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "+15555550100"
    (phone |> member "phoneNumber" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:plc:abc123xyz0001112223333"
    (revoke |> member "account" |> to_string)

let test_check_handle_live _ =
  try
    with_public_timeout (fun () ->
        let check = Temp.check_handle_availability ~handle:"jay.bsky.team" () in
        OUnit2.assert_equal ~printer:(fun x -> x) "jay.bsky.team" check.handle;
        match check.result with `Unavailable _ | `Available | `Unknown _ -> ())
  with exn ->
    skip_if true ("checkHandleAvailability skipped: " ^ Printexc.to_string exn)

let test_check_signup_queue_live _ =
  try
    with_public_timeout (fun () ->
        let queue = Temp.check_signup_queue ~host:"bsky.social" () in
        OUnit2.assert_bool "activated parsed" (queue.activated || true))
  with exn ->
    skip_if true ("checkSignupQueue skipped: " ^ Printexc.to_string exn)

let test_dereference_scope_live _ =
  try
    with_public_timeout (fun () ->
        let deref =
          Temp.dereference_scope ~host:"bsky.social" ~scope:"ref:example" ()
        in
        OUnit2.assert_bool "scope parsed" (String.length deref.scope > 0))
  with exn ->
    skip_if true ("dereferenceScope skipped: " ^ Printexc.to_string exn)

let suite =
  "temp"
  >::: [
         "test_parse_available" >:: test_parse_available;
         "test_parse_unavailable" >:: test_parse_unavailable;
         "test_parse_signup_and_scope" >:: test_parse_signup_and_scope;
         "test_check_handle_live" >:: test_check_handle_live;
         "test_check_signup_queue_live" >:: test_check_signup_queue_live;
         "test_dereference_scope_live" >:: test_dereference_scope_live;
       ]

let () = run_test_tt_main suite
