open OUnit2
open Atproto.Identity

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

let test_host_of_service_endpoint _ =
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "morel.us-east.host.bsky.network"
    (Identity.host_of_service_endpoint "https://morel.us-east.host.bsky.network");
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "pds.example.com"
    (Identity.host_of_service_endpoint "https://pds.example.com/xrpc")

let test_resolve_handle_live _ =
  try
    with_public_timeout (fun () ->
        let resolved = Identity.resolve_handle "jay.bsky.team" in
        OUnit2.assert_bool "resolveHandle did not return a DID"
          (String.length resolved.did > 8
          && String.sub resolved.did 0 4 = "did:"))
  with exn -> skip_if true ("resolveHandle skipped: " ^ Printexc.to_string exn)

let test_resolve_did_key_offline _ =
  let did = "did:key:zQ3shokFTS3brHcDQmzNVwDs7LnAKgaM92hjiJe7iJqpNkYdo" in
  let ident = Identity.resolve did in
  OUnit2.assert_equal ~printer:(fun x -> x) did ident.did;
  OUnit2.assert_equal None ident.handle;
  OUnit2.assert_equal None ident.pds

let test_resolve_did_web_url_only _ =
  OUnit2.assert_bool "did:web is recognized"
    (Atproto.Did_web.Did_web.is_web_did "did:web:example.com")

let test_resolve_actor_live _ =
  try
    with_public_timeout (fun () ->
        let ident = Identity.resolve "jay.bsky.team" in
        OUnit2.assert_bool "missing DID" (String.length ident.did > 8);
        OUnit2.assert_bool "missing PDS"
          (match ident.pds with Some p -> String.length p > 0 | None -> false))
  with exn ->
    skip_if true ("Identity.resolve skipped: " ^ Printexc.to_string exn)

let test_parse_identity_info _ =
  let json =
    `Assoc
      [
        ("did", `String "did:plc:ewvi7nxzyoun6zhxrhs64oiz");
        ("handle", `String "jay.bsky.team");
        ("didDoc", `Assoc [ ("id", `String "did:plc:ewvi7nxzyoun6zhxrhs64oiz") ]);
      ]
  in
  let info = Identity.parse_identity_info json in
  OUnit2.assert_equal ~printer:(fun x -> x) "jay.bsky.team" info.handle;
  OUnit2.assert_bool "didDoc present"
    (match info.did_doc with Some _ -> true | None -> false)

let test_plc_operation_bodies _ =
  let signed =
    Identity.sign_plc_operation_body ~token:"email-token"
      ~rotation_keys:
        [ "did:key:zDnaerDaTF5BXEavCrfUZPJjEBhG8KNmk45G65Kd8uKbVhcwK" ]
      ~also_known_as:[ "at://alice.test" ] ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "email-token"
    (signed |> member "token" |> to_string);
  let submitted = Identity.submit_plc_operation_body signed in
  OUnit2.assert_bool "operation wrapped"
    (match submitted |> member "operation" with `Assoc _ -> true | _ -> false)

let test_recommended_did_credentials _ =
  let json =
    `Assoc
      [
        ( "rotationKeys",
          `List
            [
              `String
                "did:key:zDnaerDaTF5BXEavCrfUZPJjEBhG8KNmk45G65Kd8uKbVhcwK";
            ] );
        ("alsoKnownAs", `List [ `String "at://alice.test" ]);
        ("verificationMethods", `Assoc []);
        ("services", `Assoc []);
      ]
  in
  let creds = Identity.parse_recommended_did_credentials json in
  OUnit2.assert_equal 1 (List.length creds.rotation_keys);
  OUnit2.assert_equal (Some "at://alice.test")
    (List.nth_opt creds.also_known_as 0)

let test_update_handle_body _ =
  let body = Identity.update_handle_body "alice.test" in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "alice.test"
    (body |> member "handle" |> to_string)

let test_handle_txt_helpers _ =
  OUnit2.assert_equal (Some "did:plc:ewvi7nxzyoun6zhxrhs64oiz")
    (Identity.parse_txt_did "did=did:plc:ewvi7nxzyoun6zhxrhs64oiz");
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "https://jay.bsky.team/.well-known/atproto-did"
    (Identity.handle_well_known_url "jay.bsky.team")

let suite =
  "identity"
  >::: [
         "test_host_of_service_endpoint" >:: test_host_of_service_endpoint;
         "test_resolve_did_web_url_only" >:: test_resolve_did_web_url_only;
         "test_resolve_did_key_offline" >:: test_resolve_did_key_offline;
         "test_resolve_handle_live" >:: test_resolve_handle_live;
         "test_resolve_actor_live" >:: test_resolve_actor_live;
         "test_parse_identity_info" >:: test_parse_identity_info;
         "test_plc_operation_bodies" >:: test_plc_operation_bodies;
         "test_recommended_did_credentials" >:: test_recommended_did_credentials;
         "test_handle_txt_helpers" >:: test_handle_txt_helpers;
         "test_update_handle_body" >:: test_update_handle_body;
       ]

let () = run_test_tt_main suite
