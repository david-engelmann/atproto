open OUnit2
open Atproto.Xrpc
open Atproto.Client
open Atproto.Base64url
open Atproto.Hash
open Atproto.Did_key
open Atproto.K256

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
    "did:web:api.bsky.chat#bsky_chat"
    (Xrpc.proxy_to_string Xrpc.chat_proxy);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:web:api.bsky.app#bsky_appview"
    (Xrpc.proxy_to_string Xrpc.appview_proxy);
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

let test_topics _ =
  let h, v = Xrpc.topics_header [ "news"; "sports" ] in
  OUnit2.assert_equal "x-atproto-bsky-topics" h;
  OUnit2.assert_equal ~printer:(fun x -> x) Xrpc.topics_header_name h;
  OUnit2.assert_equal ~printer:(fun x -> x) "news,sports" v;
  OUnit2.assert_equal ~printer:(fun x -> x) "news,sports"
    (Xrpc.topics_to_string [ "news"; "sports" ]);
  let lh, lv = Xrpc.legacy_topics_header [ "news"; "sports" ] in
  OUnit2.assert_equal "x-bsky-topics" lh;
  OUnit2.assert_equal ~printer:(fun x -> x) Xrpc.legacy_topics_header_name lh;
  OUnit2.assert_equal ~printer:(fun x -> x) "news,sports" lv;
  OUnit2.assert_equal
    [ ("x-atproto-bsky-topics", "news,sports") ]
    (Xrpc.topics_headers [ "news"; "sports" ]);
  OUnit2.assert_equal
    [
      ("x-atproto-bsky-topics", "news,sports");
      ("x-bsky-topics", "news,sports");
    ]
    (Xrpc.topics_headers ~legacy:true [ "news"; "sports" ]);
  OUnit2.assert_equal [ "news"; "sports"; "tech" ]
    (Xrpc.parse_topics "news, sports, ,tech");
  let _, trimmed = Xrpc.topics_header [ " news "; ""; "sports" ] in
  OUnit2.assert_equal ~printer:(fun x -> x) "news,sports" trimmed;
  OUnit2.assert_equal [ "news"; "sports" ]
    (Xrpc.topics_from_headers
       [
         ("X-Bsky-Topics", "legacy-only");
         ("x-atproto-bsky-topics", "news,sports");
       ]);
  OUnit2.assert_equal [ "legacy" ]
    (Xrpc.topics_from_headers [ ("x-bsky-topics", "legacy") ]);
  OUnit2.assert_equal [] (Xrpc.topics_from_headers [ ("accept", "*/*") ])

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
    (body |> member "aud" |> to_string);
  let frag =
    Xrpc.service_auth_body ~aud:"did:web:video.bsky.app#bsky_transcode"
      ~lxm:"com.atproto.repo.uploadBlob" ()
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:web:video.bsky.app#bsky_transcode"
    (frag |> member "aud" |> to_string)

let rfc6979_p256_priv =
  Hash.hex_decode
    "c9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721"

let p256_pair () =
  match Mirage_crypto_ec.P256.Dsa.priv_of_octets rfc6979_p256_priv with
  | Error _ -> failwith "could not load RFC 6979 P-256 private key"
  | Ok priv -> (priv, Mirage_crypto_ec.P256.Dsa.pub_of_priv priv)

let p256_did_key pub =
  Did_key.to_string
    (Did_key.of_p256_octets
       (Mirage_crypto_ec.P256.Dsa.pub_to_octets ~compress:true pub))

let test_service_jwt_p256_roundtrip _ =
  let priv, pub = p256_pair () in
  let jwt =
    Xrpc.sign_service_jwt_p256 ~priv ~iss:"did:plc:ewvi7nxzyoun6zhxrhs64oiz"
      ~aud:"did:web:video.bsky.app#bsky_transcode"
      ~lxm:"com.atproto.repo.uploadBlob" ~exp:2_000_000_000L ~iat:1_700_000_000L
      ~jti:"aabbccddeeff00112233445566778899" ~now:1_700_000_000.0 ()
  in
  let claims =
    Xrpc.verify_service_jwt
      ~keys:[ p256_did_key pub ]
      ~aud:"did:web:video.bsky.app#bsky_transcode"
      ~lxm:"com.atproto.repo.uploadBlob" ~now:1_700_000_010.0 jwt
  in
  OUnit2.assert_equal ~printer:(fun x -> x) "ES256" claims.alg;
  OUnit2.assert_equal ~printer:(fun x -> x) "#atproto" claims.kid;
  OUnit2.assert_equal (Some "aabbccddeeff00112233445566778899") claims.jti;
  OUnit2.assert_equal (Some "bsky_transcode") claims.aud_service;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:web:video.bsky.app" claims.aud_did;
  let h, v = Xrpc.service_auth_header jwt in
  OUnit2.assert_equal "Authorization" h;
  OUnit2.assert_bool "bearer"
    (String.length v > 7 && String.sub v 0 7 = "Bearer ")

let test_service_jwt_k256_roundtrip _ =
  match K256.priv_of_octets (Hash.hex_decode (String.make 63 '0' ^ "3")) with
  | Error _ -> OUnit2.assert_failure "k256 priv rejected"
  | Ok priv ->
      let pub = K256.pub_of_priv priv in
      let key =
        Did_key.to_string
          (Did_key.of_k256_octets (K256.pub_to_octets ~compress:true pub))
      in
      let jwt =
        Xrpc.sign_service_jwt_k256 ~priv ~iss:"did:plc:ewvi7nxzyoun6zhxrhs64oiz"
          ~aud:"did:web:api.bsky.app#bsky_appview"
          ~lxm:"app.bsky.feed.getFeedSkeleton" ~exp:2_000_000_000L
          ~iat:1_700_000_000L ~jti:"k256nonce" ()
      in
      let claims =
        Xrpc.verify_service_jwt ~keys:[ key ]
          ~aud:"did:web:api.bsky.app#bsky_appview" ~now:1_700_000_000.0 jwt
      in
      OUnit2.assert_equal ~printer:(fun x -> x) "ES256K" claims.alg;
      OUnit2.assert_equal (Some "k256nonce") claims.jti

let test_service_jwt_rejects _ =
  let priv, pub = p256_pair () in
  let key = p256_did_key pub in
  let jwt =
    Xrpc.sign_service_jwt_p256 ~priv ~iss:"did:plc:ewvi7nxzyoun6zhxrhs64oiz"
      ~aud:"did:web:mod.example.com#atproto_labeler"
      ~lxm:"com.atproto.moderation.createReport" ~exp:1_700_000_060L
      ~iat:1_700_000_000L ~jti:"once" ()
  in
  OUnit2.assert_bool "wrong aud accepted"
    (try
       ignore
         (Xrpc.verify_service_jwt ~keys:[ key ] ~aud:"did:web:other.example"
            ~now:1_700_000_010.0 jwt);
       false
     with Xrpc.Invalid _ -> true);
  OUnit2.assert_bool "expired accepted"
    (try
       ignore (Xrpc.verify_service_jwt ~keys:[ key ] ~now:1_700_000_200.0 jwt);
       false
     with Xrpc.Invalid _ -> true);
  OUnit2.assert_bool "wrong lxm accepted"
    (try
       ignore
         (Xrpc.verify_service_jwt ~keys:[ key ]
            ~lxm:"com.atproto.server.createSession" ~now:1_700_000_010.0 jwt);
       false
     with Xrpc.Invalid _ -> true);
  OUnit2.assert_bool "bare aud is not a wildcard"
    (try
       ignore
         (Xrpc.verify_service_jwt ~keys:[ key ] ~aud:"did:web:mod.example.com"
            ~now:1_700_000_010.0 jwt);
       false
     with Xrpc.Invalid _ -> true);
  let cache = Xrpc.create_jti_cache () in
  let claims = Xrpc.parse_service_auth jwt in
  OUnit2.assert_bool "first jti is new"
    (not (Xrpc.remember_jti ~now:1_700_000_010.0 cache claims));
  OUnit2.assert_bool "replayed jti"
    (Xrpc.remember_jti ~now:1_700_000_010.0 cache claims)

let test_jti_cache_eviction _ =
  let cache = Xrpc.create_jti_cache ~cap:2 () in
  let claim jti : Xrpc.service_auth =
    {
      alg = "ES256";
      typ = "JWT";
      kid = "#atproto";
      iss = "did:plc:ewvi7nxzyoun6zhxrhs64oiz";
      aud = "did:web:api.bsky.app#bsky_appview";
      aud_did = "did:web:api.bsky.app";
      aud_service = Some "bsky_appview";
      exp = Some 2_000_000_000L;
      iat = Some 1_700_000_000L;
      lxm = Some "app.bsky.feed.getFeedSkeleton";
      jti = Some jti;
      raw = "";
    }
  in
  OUnit2.assert_bool "a new"
    (not (Xrpc.remember_jti ~now:1_700_000_010.0 cache (claim "a")));
  OUnit2.assert_bool "b new"
    (not (Xrpc.remember_jti ~now:1_700_000_010.0 cache (claim "b")));
  OUnit2.assert_bool "c evicts a"
    (not (Xrpc.remember_jti ~now:1_700_000_010.0 cache (claim "c")));
  OUnit2.assert_bool "a evicted" (not (Xrpc.jti_seen cache "a"));
  OUnit2.assert_bool "b kept" (Xrpc.jti_seen cache "b");
  OUnit2.assert_bool "c kept" (Xrpc.jti_seen cache "c")

let test_json_of_empty_xrpc_body _ =
  OUnit2.assert_equal (`Assoc []) (Client.json_of_body "");
  OUnit2.assert_equal (`Assoc []) (Client.json_of_body "  \n");
  OUnit2.assert_equal
    (`Assoc [ ("ok", `Bool true) ])
    (Client.json_of_body {|{"ok":true}|})

let suite =
  "xrpc"
  >::: [
         "test_proxy" >:: test_proxy;
         "test_labelers" >:: test_labelers;
         "test_topics" >:: test_topics;
         "test_rate_limit" >:: test_rate_limit;
         "test_repo_rev" >:: test_repo_rev;
         "test_service_auth_jwt" >:: test_service_auth_jwt;
         "test_service_jwt_p256_roundtrip" >:: test_service_jwt_p256_roundtrip;
         "test_service_jwt_k256_roundtrip" >:: test_service_jwt_k256_roundtrip;
         "test_service_jwt_rejects" >:: test_service_jwt_rejects;
         "test_jti_cache_eviction" >:: test_jti_cache_eviction;
         "test_json_of_empty_xrpc_body" >:: test_json_of_empty_xrpc_body;
       ]

let () = run_test_tt_main suite
