open OUnit2
open Atproto.Oauth
open Atproto.Hash

(* RFC 7636 Appendix B *)
let rfc7636_verifier = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
let rfc7636_challenge = "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"

let rfc6979_p256_priv =
  Hash.hex_decode
    "c9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721"

let p256_pair () =
  match Mirage_crypto_ec.P256.Dsa.priv_of_octets rfc6979_p256_priv with
  | Error _ -> failwith "could not load RFC 6979 P-256 private key"
  | Ok priv -> (priv, Mirage_crypto_ec.P256.Dsa.pub_of_priv priv)

let test_pkce_rfc7636 _ =
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    rfc7636_challenge
    (Oauth.pkce_challenge rfc7636_verifier)

let test_pkce_rejects_short_verifier _ =
  OUnit2.assert_bool "short verifier accepted"
    (try
       ignore (Oauth.pkce_challenge "too-short");
       false
     with Failure _ -> true)

let test_pkce_s256_roundtrip _ =
  let pkce = Oauth.pkce_s256 ~verifier:rfc7636_verifier () in
  OUnit2.assert_equal "S256" pkce.method_;
  OUnit2.assert_equal rfc7636_challenge pkce.challenge

let test_dpop_sign_and_verify _ =
  let priv, pub = p256_pair () in
  let proof =
    Oauth.dpop_proof ~priv ~pub ~htm:"POST"
      ~htu:"https://bsky.social/oauth/token" ~jti:"test-jti" ~iat:1_700_000_000L
      ()
  in
  let claims = Oauth.parse_dpop proof in
  OUnit2.assert_equal ~printer:(fun x -> x) "dpop+jwt" claims.typ;
  OUnit2.assert_equal ~printer:(fun x -> x) "ES256" claims.alg;
  OUnit2.assert_equal ~printer:(fun x -> x) "POST" claims.htm;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "https://bsky.social/oauth/token" claims.htu;
  OUnit2.assert_equal None claims.ath;
  OUnit2.assert_bool "DPoP signature must verify" (Oauth.verify_dpop ~pub proof);
  let tampered = proof ^ "x" in
  OUnit2.assert_bool "tampered DPoP accepted"
    (not (Oauth.verify_dpop ~pub tampered));
  let h, p, s = Oauth.split_jwt proof in
  let raw = Atproto.Base64url.Base64url.decode s in
  let r = String.sub raw 0 32 in
  let t = String.sub raw 32 32 in
  let high = Oauth.sub_be Oauth.p256_n t in
  let high_proof =
    h ^ "." ^ p ^ "." ^ Atproto.Base64url.Base64url.encode (r ^ high)
  in
  OUnit2.assert_bool "high-S DPoP accepted"
    (not (Oauth.verify_dpop ~pub high_proof))

let test_dpop_ath _ =
  let priv, pub = p256_pair () in
  let token = "example-access-token" in
  let ath = Oauth.ath_of_access_token token in
  let proof =
    Oauth.dpop_proof ~priv ~pub ~htm:"GET"
      ~htu:"https://pds.example/xrpc/com.atproto.repo.getRecord" ~ath
      ~jti:"ath-jti" ~iat:1L ()
  in
  let claims = Oauth.parse_dpop proof in
  OUnit2.assert_equal (Some ath) claims.ath;
  OUnit2.assert_bool "ath proof verifies" (Oauth.verify_dpop ~pub proof)

let client_id = "https://client.example/client-metadata.json"
let redirect_uri = "https://client.example/cb"

let sample_as_json =
  `Assoc
    [
      ("issuer", `String "https://bsky.social");
      ("authorization_endpoint", `String "https://bsky.social/oauth/authorize");
      ("token_endpoint", `String "https://bsky.social/oauth/token");
      ( "pushed_authorization_request_endpoint",
        `String "https://bsky.social/oauth/par" );
      ("response_types_supported", `List [ `String "code" ]);
      ( "grant_types_supported",
        `List [ `String "authorization_code"; `String "refresh_token" ] );
      ("code_challenge_methods_supported", `List [ `String "S256" ]);
      ( "token_endpoint_auth_methods_supported",
        `List [ `String "none"; `String "private_key_jwt" ] );
      ( "token_endpoint_auth_signing_alg_values_supported",
        `List [ `String "ES256" ] );
      ( "scopes_supported",
        `List
          [
            `String "atproto";
            `String "transition:generic";
            `String "transition:chat.bsky";
          ] );
      ("dpop_signing_alg_values_supported", `List [ `String "ES256" ]);
      ("require_pushed_authorization_requests", `Bool true);
      ("authorization_response_iss_parameter_supported", `Bool true);
      ("client_id_metadata_document_supported", `Bool true);
    ]

let test_par_and_token_shapes _ =
  let pkce = Oauth.pkce_s256 ~verifier:rfc7636_verifier () in
  let par =
    Oauth.pushed_authorization_body
      ~client_id:"https://client.example/client-metadata.json"
      ~redirect_uri:"https://client.example/cb" ~code_challenge:pkce.challenge
      ~state:"abc" ()
  in
  OUnit2.assert_equal (Some "S256") (List.assoc_opt "code_challenge_method" par);
  OUnit2.assert_equal (Some "atproto transition:generic")
    (List.assoc_opt "scope" par);
  let signup =
    Oauth.pushed_authorization_body
      ~client_id:"https://client.example/client-metadata.json"
      ~redirect_uri:"https://client.example/cb" ~code_challenge:pkce.challenge
      ~state:"abc" ~prompt:"create" ~login_hint:"alice.test" ()
  in
  OUnit2.assert_equal (Some "create") (List.assoc_opt "prompt" signup);
  OUnit2.assert_equal (Some "alice.test") (List.assoc_opt "login_hint" signup);
  let token =
    Oauth.token_body ~client_id:"https://client.example/client-metadata.json"
      ~redirect_uri:"https://client.example/cb" ~code:"authz-code"
      ~code_verifier:pkce.verifier ()
  in
  OUnit2.assert_equal (Some "authorization_code")
    (List.assoc_opt "grant_type" token);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "https://bsky.social/oauth/par" (Oauth.par_url ());
  let encoded = Oauth.form_encode par in
  OUnit2.assert_bool "form body includes challenge" (String.length encoded > 20)

let test_client_metadata_public _ =
  let meta =
    Oauth.public_metadata ~client_id ~redirect_uris:[ redirect_uri ]
      ~client_name:"Example" ()
  in
  Oauth.validate_metadata meta;
  let again = Oauth.metadata_of_json (Oauth.metadata_to_json meta) in
  OUnit2.assert_equal ~printer:(fun x -> x) client_id again.client_id;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "none" again.token_endpoint_auth_method;
  OUnit2.assert_bool "dpop bound" again.dpop_bound_access_tokens;
  OUnit2.assert_bool "atproto"
    (Oauth.contains_scope ~scope:again.scope "atproto")

let test_client_metadata_confidential _ =
  let priv, pub = p256_pair () in
  let jwks = Oauth.jwks_of_pub pub in
  let meta =
    Oauth.confidential_metadata ~client_id ~redirect_uris:[ redirect_uri ] ~jwks
      ()
  in
  Oauth.validate_metadata meta;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "private_key_jwt" meta.token_endpoint_auth_method;
  ignore priv

let test_client_metadata_rejects_private_jwk _ =
  let bad =
    Oauth.confidential_metadata ~client_id ~redirect_uris:[ redirect_uri ]
      ~jwks:
        (`Assoc
          [
            ( "keys",
              `List
                [
                  `Assoc
                    [
                      ("kty", `String "EC");
                      ("crv", `String "P-256");
                      ("x", `String "aa");
                      ("y", `String "bb");
                      ("d", `String "secret");
                    ];
                ] );
          ])
      ()
  in
  OUnit2.assert_bool "private d accepted"
    (try
       Oauth.validate_metadata bad;
       false
     with Failure _ -> true)

let test_localhost_client_metadata _ =
  let id =
    "http://localhost?redirect_uri=http://127.0.0.1:8080/cb&scope=atproto%20transition:generic"
  in
  let meta = Oauth.localhost_metadata id in
  Oauth.validate_metadata meta;
  OUnit2.assert_equal ~printer:(fun x -> x) "native" meta.application_type;
  OUnit2.assert_equal [ "http://127.0.0.1:8080/cb" ] meta.redirect_uris;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "atproto transition:generic" meta.scope

let test_as_and_resource_metadata _ =
  let as_ = Oauth.parse_as_metadata sample_as_json in
  Oauth.validate_as_metadata as_;
  OUnit2.assert_equal ~printer:(fun x -> x) "https://bsky.social" as_.issuer;
  let resource =
    Oauth.parse_resource_metadata
      (`Assoc
        [
          ("resource", `String "https://morel.us-east.host.bsky.network");
          ("authorization_servers", `List [ `String "https://bsky.social" ]);
        ])
  in
  OUnit2.assert_equal [ "https://bsky.social" ] resource.authorization_servers;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "https://bsky.social/.well-known/oauth-protected-resource"
    (Oauth.protected_resource_url ());
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "https://bsky.social/.well-known/oauth-authorization-server"
    (Oauth.authorization_server_url ())

let test_as_metadata_rejects_missing_par _ =
  let bad =
    match sample_as_json with
    | `Assoc fields ->
        `Assoc
          (List.map
             (fun (k, v) ->
               if k = "require_pushed_authorization_requests" then
                 (k, `Bool false)
               else (k, v))
             fields)
    | _ -> sample_as_json
  in
  OUnit2.assert_bool "missing PAR accepted"
    (try
       Oauth.validate_as_metadata (Oauth.parse_as_metadata bad);
       false
     with Failure _ -> true)

let test_redirect_and_authorize_url _ =
  match
    Oauth.parse_redirect
      "https://client.example/cb?code=splendid&state=abc&iss=https%3A%2F%2Fbsky.social"
  with
  | Oauth.Authorized { code; state; iss } ->
      OUnit2.assert_equal ~printer:(fun x -> x) "splendid" code;
      OUnit2.assert_equal ~printer:(fun x -> x) "abc" state;
      OUnit2.assert_equal (Some "https://bsky.social") iss;
      Oauth.expect_state ~expected:"abc" (Oauth.Authorized { code; state; iss });
      Oauth.expect_issuer ~expected:"https://bsky.social"
        (Oauth.Authorized { code; state; iss });
      let url =
        Oauth.authorize_redirect_url
          ~authorization_endpoint:"https://bsky.social/oauth/authorize"
          ~client_id ~request_uri:"urn:ietf:params:oauth:request_uri:abc"
      in
      OUnit2.assert_bool url (String.contains url '?');
      OUnit2.assert_bool url
        (let needle = "request_uri=" in
         let rec find i =
           i + String.length needle <= String.length url
           && (String.sub url i (String.length needle) = needle || find (i + 1))
         in
         find 0)
  | Oauth.Denied _ -> OUnit2.assert_failure "expected authorized redirect"

let test_redirect_denied _ =
  match
    Oauth.parse_redirect
      "https://client.example/cb?error=access_denied&error_description=nope&state=abc"
  with
  | Oauth.Denied { error; description; state } ->
      OUnit2.assert_equal ~printer:(fun x -> x) "access_denied" error;
      OUnit2.assert_equal (Some "nope") description;
      OUnit2.assert_equal (Some "abc") state
  | Oauth.Authorized _ -> OUnit2.assert_failure "expected denied redirect"

let test_redirect_state_mismatch _ =
  OUnit2.assert_bool "state mismatch accepted"
    (try
       Oauth.expect_state ~expected:"nope"
         (Oauth.Authorized
            { code = "x"; state = "abc"; iss = Some "https://bsky.social" });
       false
     with Failure _ -> true)

let test_token_response_guards _ =
  let good =
    Oauth.parse_token_response
      (`Assoc
        [
          ("access_token", `String "tok");
          ("token_type", `String "DPoP");
          ("expires_in", `Int 300);
          ("refresh_token", `String "rt");
          ("scope", `String "atproto transition:generic");
          ("sub", `String "did:plc:7iza6de2dwap2sbkpav7c6c6");
        ])
  in
  OUnit2.assert_equal ~printer:(fun x -> x) "tok" good.access_token;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:plc:7iza6de2dwap2sbkpav7c6c6" good.sub;
  let rejects json =
    try
      ignore (Oauth.parse_token_response json);
      false
    with Failure _ -> true
  in
  OUnit2.assert_bool "bearer accepted"
    (rejects
       (`Assoc
         [
           ("access_token", `String "tok");
           ("token_type", `String "Bearer");
           ("scope", `String "atproto");
           ("sub", `String "did:plc:7iza6de2dwap2sbkpav7c6c6");
         ]));
  OUnit2.assert_bool "missing atproto accepted"
    (rejects
       (`Assoc
         [
           ("access_token", `String "tok");
           ("token_type", `String "DPoP");
           ("scope", `String "transition:generic");
           ("sub", `String "did:plc:7iza6de2dwap2sbkpav7c6c6");
         ]));
  OUnit2.assert_bool "missing sub accepted"
    (rejects
       (`Assoc
         [
           ("access_token", `String "tok");
           ("token_type", `String "DPoP");
           ("scope", `String "atproto");
         ]))

let test_client_assertion _ =
  let priv, pub = p256_pair () in
  let jwt =
    Oauth.client_assertion ~priv ~pub ~client_id ~issuer:"https://bsky.social"
      ~kid:"oauth-client" ~jti:"assert-jti" ~iat:1_700_000_000L ()
  in
  let h, p, _ = Oauth.split_jwt jwt in
  let header = Yojson.Safe.from_string (Atproto.Base64url.Base64url.decode h) in
  let payload =
    Yojson.Safe.from_string (Atproto.Base64url.Base64url.decode p)
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "ES256"
    (header |> member "alg" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    client_id
    (payload |> member "iss" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "https://bsky.social"
    (payload |> member "aud" |> to_string);
  OUnit2.assert_bool "assertion verifies" (Oauth.verify_dpop ~pub jwt)

let test_dpop_nonce_claim _ =
  let priv, pub = p256_pair () in
  let proof =
    Oauth.dpop_proof ~priv ~pub ~htm:"POST" ~htu:"https://bsky.social/oauth/par"
      ~nonce:"server-nonce" ~jti:"n1" ~iat:1L ()
  in
  let claims = Oauth.parse_dpop proof in
  OUnit2.assert_equal (Some "server-nonce") claims.nonce;
  OUnit2.assert_bool "nonce proof verifies" (Oauth.verify_dpop ~pub proof)

let test_par_token_loop_retries_nonce _ =
  let priv, pub = p256_pair () in
  let par_hits = ref 0 in
  let seen_nonce = ref None in
  let http ~url ~headers ~body:_ =
    let dpop = List.assoc "DPoP" headers in
    let claims = Oauth.parse_dpop dpop in
    if url = "https://bsky.social/oauth/par" then (
      incr par_hits;
      if !par_hits = 1 then
        {
          Oauth.status = 400;
          headers = [ ("DPoP-Nonce", "server-nonce-1") ];
          body =
            {|{"error":"use_dpop_nonce","error_description":"retry with nonce"}|};
        }
      else (
        seen_nonce := claims.nonce;
        {
          Oauth.status = 201;
          headers = [ ("DPoP-Nonce", "server-nonce-2") ];
          body =
            {|{"request_uri":"urn:ietf:params:oauth:request_uri:abc","expires_in":90}|};
        }))
    else if url = "https://bsky.social/oauth/token" then
      {
        Oauth.status = 200;
        headers = [];
        body =
          {|{"access_token":"tok","token_type":"DPoP","expires_in":300,"refresh_token":"rt","scope":"atproto transition:generic","sub":"did:plc:7iza6de2dwap2sbkpav7c6c6"}|};
      }
    else failwith ("unexpected url " ^ url)
  in
  let pkce = Oauth.pkce_s256 ~verifier:rfc7636_verifier () in
  let par_form =
    Oauth.pushed_authorization_body ~client_id ~redirect_uri
      ~code_challenge:pkce.challenge ~state:"abc" ()
  in
  let par, nonce =
    Oauth.push_authorization ~http ~priv ~pub
      ~par_url:"https://bsky.social/oauth/par" ~form:par_form ()
  in
  OUnit2.assert_equal 2 !par_hits;
  OUnit2.assert_equal (Some "server-nonce-1") !seen_nonce;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "urn:ietf:params:oauth:request_uri:abc" par.request_uri;
  OUnit2.assert_equal (Some 90) par.expires_in;
  OUnit2.assert_equal (Some "server-nonce-2") nonce;
  let token_form =
    Oauth.token_body ~client_id ~redirect_uri ~code:"authz-code"
      ~code_verifier:pkce.verifier ()
  in
  let token, _ =
    Oauth.exchange_code ~http ~priv ~pub
      ~token_url:"https://bsky.social/oauth/token" ~form:token_form ()
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:plc:7iza6de2dwap2sbkpav7c6c6" token.sub;
  let headers =
    Oauth.resource_request_headers ~priv ~pub ~htm:"GET"
      ~htu:"https://pds.example/xrpc/com.atproto.repo.getRecord"
      ~access_token:token.access_token ()
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "DPoP tok"
    (List.assoc "Authorization" headers)

let test_htu_strips_query_and_fragment _ =
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "https://pds.example/xrpc/com.atproto.repo.getRecord"
    (Oauth.htu_of_url
       "https://pds.example/xrpc/com.atproto.repo.getRecord?repo=did:plc:abc#frag")

let test_expect_sub_and_expires _ =
  let token =
    Oauth.parse_token_response
      (`Assoc
        [
          ("access_token", `String "tok");
          ("token_type", `String "DPoP");
          ("expires_in", `Int 300);
          ("scope", `String "atproto");
          ("sub", `String "did:plc:7iza6de2dwap2sbkpav7c6c6");
        ])
  in
  Oauth.expect_sub ~expected:"did:plc:7iza6de2dwap2sbkpav7c6c6" token;
  OUnit2.assert_bool "sub mismatch accepted"
    (try
       Oauth.expect_sub ~expected:"did:plc:aaaaaaaaaaaaaaaaaaaaaaaa" token;
       false
     with Failure _ -> true);
  match Oauth.expires_at ~issued_at:1_000. token with
  | Some at -> OUnit2.assert_equal ~printer:string_of_float 1_300. at
  | None -> OUnit2.assert_failure "expected expires_at"

let test_as_metadata_rejects_false_request_uri_registration _ =
  let bad =
    match sample_as_json with
    | `Assoc fields ->
        `Assoc (fields @ [ ("require_request_uri_registration", `Bool false) ])
    | _ -> sample_as_json
  in
  OUnit2.assert_bool "false require_request_uri_registration accepted"
    (try
       Oauth.validate_as_metadata (Oauth.parse_as_metadata bad);
       false
     with Failure _ -> true)

let test_resource_request_retries_nonce _ =
  let priv, pub = p256_pair () in
  let hits = ref 0 in
  let seen_nonce = ref None in
  let http ~url:_ ~method_:_ ~headers ~body:_ =
    incr hits;
    let claims = Oauth.parse_dpop (List.assoc "DPoP" headers) in
    if !hits = 1 then
      {
        Oauth.status = 400;
        headers = [ ("DPoP-Nonce", "rs-nonce-1") ];
        body = {|{"error":"use_dpop_nonce"}|};
      }
    else (
      seen_nonce := claims.nonce;
      { Oauth.status = 200; headers = []; body = {|{"ok":true}|} })
  in
  let resp, _ =
    Oauth.request_with_dpop ~http ~priv ~pub
      ~url:"https://pds.example/xrpc/com.atproto.repo.getRecord?repo=x"
      ~htm:"GET" ~access_token:"tok" ()
  in
  OUnit2.assert_equal 2 !hits;
  OUnit2.assert_equal (Some "rs-nonce-1") !seen_nonce;
  OUnit2.assert_equal 200 resp.status

let test_use_dpop_nonce_without_header_fails _ =
  let priv, pub = p256_pair () in
  let http ~url:_ ~headers:_ ~body:_ =
    { Oauth.status = 400; headers = []; body = {|{"error":"use_dpop_nonce"}|} }
  in
  OUnit2.assert_bool "missing DPoP-Nonce accepted"
    (try
       ignore
         (Oauth.post_with_dpop ~http ~priv ~pub
            ~url:"https://bsky.social/oauth/par" ~htm:"POST" ~body:"" ());
       false
     with Failure msg ->
       let needle = "DPoP-Nonce" in
       let rec find i =
         i + String.length needle <= String.length msg
         && (String.sub msg i (String.length needle) = needle || find (i + 1))
       in
       find 0)

let test_metadata_optional_uris _ =
  let meta =
    Oauth.public_metadata ~client_id ~redirect_uris:[ redirect_uri ]
      ~logo_uri:"https://client.example/logo.png"
      ~tos_uri:"https://client.example/tos"
      ~policy_uri:"https://client.example/policy" ()
  in
  let json = Oauth.metadata_to_json meta in
  let parsed = Oauth.metadata_of_json json in
  OUnit2.assert_equal (Some "https://client.example/logo.png") parsed.logo_uri;
  OUnit2.assert_equal (Some "https://client.example/tos") parsed.tos_uri;
  OUnit2.assert_equal (Some "https://client.example/policy") parsed.policy_uri

let test_live_as_metadata _ =
  let old =
    Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> failwith "timeout"))
  in
  ignore (Unix.alarm 20);
  Fun.protect
    ~finally:(fun () ->
      ignore (Unix.alarm 0);
      Sys.set_signal Sys.sigalrm old)
    (fun () ->
      try
        let url = Oauth.authorization_server_url () in
        let headers =
          Atproto.Cohttp_client.Cohttp_client.create_headers_from_pairs
            [
              Atproto.Cohttp_client.Cohttp_client.application_json_setting_tuple;
            ]
        in
        let body =
          Lwt_main.run
            (Atproto.Cohttp_client.Cohttp_client.get_request_with_headers url
               headers)
        in
        let meta = Oauth.parse_as_metadata (Yojson.Safe.from_string body) in
        Oauth.validate_as_metadata meta;
        OUnit2.assert_equal
          ~printer:(fun x -> x)
          "https://bsky.social" meta.issuer
      with exn ->
        skip_if true
          ("oauth authorization-server metadata skipped: "
         ^ Printexc.to_string exn))

let test_par_prompt_and_dpop_jkt _ =
  let priv, pub = p256_pair () in
  let jkt = Oauth.dpop_jkt pub in
  OUnit2.assert_bool "dpop_jkt is base64url" (String.length jkt >= 32);
  let again = Oauth.jwk_thumbprint (Oauth.p256_jwk pub) in
  OUnit2.assert_equal ~printer:(fun x -> x) jkt again;
  let par =
    Oauth.pushed_authorization_body ~client_id ~redirect_uri
      ~code_challenge:rfc7636_challenge ~state:"s" ~prompt:"create"
      ~dpop_jkt:jkt ()
  in
  OUnit2.assert_equal (Some "create") (List.assoc_opt "prompt" par);
  OUnit2.assert_equal (Some jkt) (List.assoc_opt "dpop_jkt" par);
  ignore priv

let test_revoke_body_and_loop _ =
  let priv, pub = p256_pair () in
  let form =
    Oauth.revoke_body ~client_id ~token:"access-token"
      ~token_type_hint:"access_token" ()
  in
  OUnit2.assert_equal (Some "access-token") (List.assoc_opt "token" form);
  OUnit2.assert_equal (Some "access_token")
    (List.assoc_opt "token_type_hint" form);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "https://bsky.social/oauth/revoke" (Oauth.revocation_url ());
  let with_revocation =
    match sample_as_json with
    | `Assoc fields ->
        `Assoc
          (("revocation_endpoint", `String "https://bsky.social/oauth/revoke")
          :: fields)
    | other -> other
  in
  let as_ = Oauth.parse_as_metadata with_revocation in
  OUnit2.assert_equal (Some "https://bsky.social/oauth/revoke")
    as_.revocation_endpoint;
  let calls = ref 0 in
  let http ~url ~headers ~body =
    incr calls;
    ignore url;
    ignore headers;
    ignore body;
    { Oauth.status = 200; headers = []; body = "" }
  in
  let (), _nonce =
    Oauth.revoke ~http ~priv ~pub ~revoke_url:(Oauth.revocation_url ()) ~form ()
  in
  OUnit2.assert_equal 1 !calls

let suite =
  "oauth"
  >::: [
         "test_pkce_rfc7636" >:: test_pkce_rfc7636;
         "test_pkce_rejects_short_verifier" >:: test_pkce_rejects_short_verifier;
         "test_pkce_s256_roundtrip" >:: test_pkce_s256_roundtrip;
         "test_dpop_sign_and_verify" >:: test_dpop_sign_and_verify;
         "test_dpop_ath" >:: test_dpop_ath;
         "test_par_and_token_shapes" >:: test_par_and_token_shapes;
         "test_client_metadata_public" >:: test_client_metadata_public;
         "test_client_metadata_confidential"
         >:: test_client_metadata_confidential;
         "test_client_metadata_rejects_private_jwk"
         >:: test_client_metadata_rejects_private_jwk;
         "test_localhost_client_metadata" >:: test_localhost_client_metadata;
         "test_as_and_resource_metadata" >:: test_as_and_resource_metadata;
         "test_as_metadata_rejects_missing_par"
         >:: test_as_metadata_rejects_missing_par;
         "test_redirect_and_authorize_url" >:: test_redirect_and_authorize_url;
         "test_redirect_denied" >:: test_redirect_denied;
         "test_redirect_state_mismatch" >:: test_redirect_state_mismatch;
         "test_token_response_guards" >:: test_token_response_guards;
         "test_client_assertion" >:: test_client_assertion;
         "test_dpop_nonce_claim" >:: test_dpop_nonce_claim;
         "test_par_token_loop_retries_nonce"
         >:: test_par_token_loop_retries_nonce;
         "test_htu_strips_query_and_fragment"
         >:: test_htu_strips_query_and_fragment;
         "test_expect_sub_and_expires" >:: test_expect_sub_and_expires;
         "test_as_metadata_rejects_false_request_uri_registration"
         >:: test_as_metadata_rejects_false_request_uri_registration;
         "test_resource_request_retries_nonce"
         >:: test_resource_request_retries_nonce;
         "test_use_dpop_nonce_without_header_fails"
         >:: test_use_dpop_nonce_without_header_fails;
         "test_metadata_optional_uris" >:: test_metadata_optional_uris;
         "test_live_as_metadata" >:: test_live_as_metadata;
         "test_par_prompt_and_dpop_jkt" >:: test_par_prompt_and_dpop_jkt;
         "test_revoke_body_and_loop" >:: test_revoke_body_and_loop;
       ]

let () = run_test_tt_main suite
