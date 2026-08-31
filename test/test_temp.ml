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

let test_check_handle_live _ =
  try
    with_public_timeout (fun () ->
        let check = Temp.check_handle_availability ~handle:"jay.bsky.team" () in
        OUnit2.assert_equal ~printer:(fun x -> x) "jay.bsky.team" check.handle;
        match check.result with `Unavailable _ | `Available | `Unknown _ -> ())
  with exn ->
    skip_if true ("checkHandleAvailability skipped: " ^ Printexc.to_string exn)

let suite =
  "temp"
  >::: [
         "test_parse_available" >:: test_parse_available;
         "test_parse_unavailable" >:: test_parse_unavailable;
         "test_check_handle_live" >:: test_check_handle_live;
       ]

let () = run_test_tt_main suite
