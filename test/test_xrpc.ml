open OUnit2
open Atproto.Xrpc
open Atproto.Base64url

let test_proxy _ =
  let p = Xrpc.parse_proxy "did:plc:abc123xyz0001112223333#atproto_labeler" in
  OUnit2.assert_equal ~printer:(fun x -> x) "atproto_labeler" p.service;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:plc:abc123xyz0001112223333#atproto_labeler" (Xrpc.proxy_to_string p);
  let h, v = Xrpc.proxy_header (Xrpc.labeler_proxy "did:web:mod.example.com") in
  OUnit2.assert_equal "atproto-proxy" h;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:web:mod.example.com#atproto_labeler" v;
  OUnit2.assert_bool "missing fragment rejected"
    (try
       ignore (Xrpc.parse_proxy "did:web:mod.example.com");
       false
     with Xrpc.Invalid _ -> true)

let test_labelers _ =
  let parsed =
    Xrpc.parse_labelers
      "did:web:mod.example.com;redact, did:plc:abc123xyz0001112223333, \
       did:plc:xyz789aaa0001112223333"
  in
  OUnit2.assert_equal 3 (List.length parsed);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:web:mod.example.com" (List.nth parsed 0).did;
  OUnit2.assert_bool "redact" (List.nth parsed 0).redact;
  OUnit2.assert_bool "no redact" (not (List.nth parsed 1).redact);
  let merged =
    Xrpc.parse_labelers
      "did:web:mod.example.com, did:web:mod.example.com;redact"
  in
  OUnit2.assert_equal 1 (List.length merged);
  OUnit2.assert_bool "redact unioned" (List.hd merged).redact;
  let h, v = Xrpc.accept_labelers_header merged in
  OUnit2.assert_equal "atproto-accept-labelers" h;
  OUnit2.assert_equal ~printer:(fun x -> x) "did:web:mod.example.com;redact" v

let test_rate_limit _ =
  let rl =
    Xrpc.parse_rate_limit
      [
        ("RateLimit-Limit", "100");
        ("RateLimit-Remaining", "42");
        ("RateLimit-Reset", "1700000000");
        ("RateLimit-Policy", "100;w=300");
      ]
  in
  OUnit2.assert_equal (Some 100) rl.limit;
  OUnit2.assert_equal (Some 42) rl.remaining;
  OUnit2.assert_equal (Some 1_700_000_000L) rl.reset;
  OUnit2.assert_equal (Some "100;w=300") rl.policy

let test_repo_rev _ =
  let h, v = Xrpc.repo_rev_header "3jzfcijpj2z2a" in
  OUnit2.assert_equal "atproto-repo-rev" h;
  OUnit2.assert_equal "3jzfcijpj2z2a" v

let b64 json = Base64url.encode (Yojson.Safe.to_string json)

let test_service_auth_jwt _ =
  let header = `Assoc [ ("alg", `String "none"); ("typ", `String "JWT") ] in
  let payload =
    `Assoc
      [
        ("iss", `String "did:plc:ewvi7nxzyoun6zhxrhs64oiz");
        ("aud", `String "did:web:mod.example.com");
        ("exp", `Int 1_700_000_000);
        ("lxm", `String "com.atproto.moderation.createReport");
      ]
  in
  let jwt = b64 header ^ "." ^ b64 payload ^ ".sig" in
  let claims = Xrpc.parse_service_auth jwt in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:plc:ewvi7nxzyoun6zhxrhs64oiz" claims.iss;
  OUnit2.assert_equal ~printer:(fun x -> x) "did:web:mod.example.com" claims.aud;
  OUnit2.assert_equal (Some "com.atproto.moderation.createReport") claims.lxm;
  let body =
    Xrpc.service_auth_body ~aud:"did:web:mod.example.com"
      ~lxm:"com.atproto.moderation.createReport" ~exp:1_700_000_000L ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:web:mod.example.com"
    (body |> member "aud" |> to_string)

let suite =
  "xrpc"
  >::: [
         "test_proxy" >:: test_proxy;
         "test_labelers" >:: test_labelers;
         "test_rate_limit" >:: test_rate_limit;
         "test_repo_rev" >:: test_repo_rev;
         "test_service_auth_jwt" >:: test_service_auth_jwt;
       ]

let () = run_test_tt_main suite
