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
  let token =
    Oauth.token_body ~client_id:"https://client.example/client-metadata.json"
      ~redirect_uri:"https://client.example/cb" ~code:"authz-code"
      ~code_verifier:pkce.verifier
  in
  OUnit2.assert_equal (Some "authorization_code")
    (List.assoc_opt "grant_type" token);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "https://bsky.social/oauth/par" (Oauth.par_url ());
  let encoded = Oauth.form_encode par in
  OUnit2.assert_bool "form body includes challenge" (String.length encoded > 20)

let suite =
  "oauth"
  >::: [
         "test_pkce_rfc7636" >:: test_pkce_rfc7636;
         "test_pkce_rejects_short_verifier" >:: test_pkce_rejects_short_verifier;
         "test_pkce_s256_roundtrip" >:: test_pkce_s256_roundtrip;
         "test_dpop_sign_and_verify" >:: test_dpop_sign_and_verify;
         "test_dpop_ath" >:: test_dpop_ath;
         "test_par_and_token_shapes" >:: test_par_and_token_shapes;
       ]

let () = run_test_tt_main suite
