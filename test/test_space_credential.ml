open OUnit2
open Atproto.Space_credential
open Atproto.Hash
open Atproto.Did_key
open Atproto.K256
open Atproto.Base64url
open Atproto.Oauth

(* Official fixtures from bluesky-social/atproto#5187
   packages/space/tests/credential.test.ts / dpop.test.ts. *)
let space_uri = "at://did:example:space/space/app.bsky.group/test"
let user_did = "did:example:alice"
let authority_did = "did:example:space"
let space_host = authority_did ^ "#atproto_space_host"
let client_id = "https://app.example.com/client-metadata.json"
let official_dpop_jkt = "0ZcOCORZNYy-DWpqq30jZyJGHTN0d2HglBV3uiguA4I"

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

let k256_pair () =
  match K256.priv_of_octets (Hash.hex_decode (String.make 63 '0' ^ "3")) with
  | Error _ -> failwith "k256 priv rejected"
  | Ok priv -> (priv, K256.pub_of_priv priv)

let k256_did_key pub =
  Did_key.to_string
    (Did_key.of_k256_octets (K256.pub_to_octets ~compress:true pub))

let now = 1_738_368_000.0
let iat = 1_738_368_000L

let retype_token jwt typ =
  let h, p, s = Oauth.split_jwt jwt in
  let header = Yojson.Safe.from_string (Base64url.decode h) in
  let fields =
    List.map
      (fun (k, v) -> if k = "typ" then (k, `String typ) else (k, v))
      (match header with `Assoc xs -> xs | _ -> [])
  in
  let retyped = Base64url.encode (Yojson.Safe.to_string (`Assoc fields)) in
  retyped ^ "." ^ p ^ "." ^ s

let test_constants _ =
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "atproto-space-delegation+jwt" Space_credential.delegation_typ;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "atproto-space-credential+jwt" Space_credential.credential_typ;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "atproto-client-attestation+jwt" Space_credential.attestation_typ;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "#atproto" Space_credential.delegation_kid;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "#atproto_space" Space_credential.space_key_kid;
  OUnit2.assert_equal ~printer:Int64.to_string 60L
    Space_credential.delegation_lifetime;
  OUnit2.assert_equal ~printer:Int64.to_string 7200L
    Space_credential.credential_lifetime;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    space_host
    (Space_credential.space_host_aud authority_did)

let test_delegation_roundtrip_p256 _ =
  let priv, pub = p256_pair () in
  let jwt =
    Space_credential.sign_delegation ~sign:(`P256 priv) ~iss:user_did
      ~sub:space_uri ~aud:space_host ~iat
      ~jti:"f47ac10b58cc4372a5670e02b2c3d479" ~now ()
  in
  let token =
    Space_credential.verify_delegation
      ~keys:[ p256_did_key pub ]
      ~aud:space_host ~sub:space_uri ~now jwt
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    Space_credential.delegation_typ token.header.typ;
  OUnit2.assert_equal (Some "#atproto") token.header.kid;
  OUnit2.assert_equal ~printer:(fun x -> x) "ES256" token.header.alg;
  OUnit2.assert_equal ~printer:(fun x -> x) user_did token.payload.iss;
  OUnit2.assert_equal ~printer:(fun x -> x) space_uri token.payload.sub;
  OUnit2.assert_equal (Some space_host) token.payload.aud;
  OUnit2.assert_equal ~printer:Int64.to_string 60L
    (Int64.sub token.payload.exp token.payload.iat);
  OUnit2.assert_equal (Some "f47ac10b58cc4372a5670e02b2c3d479")
    token.payload.jti;
  OUnit2.assert_equal None token.payload.cnf_jkt;
  let h, v = Space_credential.bearer_header jwt in
  OUnit2.assert_equal "Authorization" h;
  OUnit2.assert_bool "bearer"
    (String.length v > 7 && String.sub v 0 7 = "Bearer ")

let test_delegation_k256 _ =
  let priv, pub = k256_pair () in
  let jwt =
    Space_credential.sign_delegation ~sign:(`K256 priv) ~iss:user_did
      ~sub:space_uri ~aud:space_host ~iat
      ~jti:"aabbccddeeff00112233445566778899" ~now ()
  in
  let token =
    Space_credential.verify_delegation
      ~keys:[ k256_did_key pub ]
      ~aud:space_host ~now jwt
  in
  OUnit2.assert_equal ~printer:(fun x -> x) "ES256K" token.header.alg

let test_delegation_requires_aud _ =
  let priv, _ = p256_pair () in
  OUnit2.assert_raises
    (Space_credential.Invalid
       "token aud must be a DID (optional #service fragment)") (fun () ->
      ignore
        (Space_credential.sign_delegation ~sign:(`P256 priv) ~iss:user_did
           ~sub:space_uri ~aud:"" ~now ()))

let test_delegation_wrong_aud_and_sub _ =
  let priv, pub = p256_pair () in
  let jwt =
    Space_credential.sign_delegation ~sign:(`P256 priv) ~iss:user_did
      ~sub:space_uri ~aud:space_host ~iat ~jti:"jti-1" ~now ()
  in
  OUnit2.assert_raises
    (Space_credential.Invalid "token audience does not match this service")
    (fun () ->
      ignore
        (Space_credential.verify_delegation
           ~keys:[ p256_did_key pub ]
           ~aud:"did:example:other#atproto_space_host" ~now jwt));
  OUnit2.assert_raises
    (Space_credential.Invalid "token subject does not match the requested space")
    (fun () ->
      ignore
        (Space_credential.verify_delegation
           ~keys:[ p256_did_key pub ]
           ~aud:space_host
           ~sub:"at://did:example:space/space/app.bsky.group/other" ~now jwt))

let test_delegation_wrong_key _ =
  let priv, _ = p256_pair () in
  let _, other_pub = k256_pair () in
  let jwt =
    Space_credential.sign_delegation ~sign:(`P256 priv) ~iss:user_did
      ~sub:space_uri ~aud:space_host ~iat ~jti:"jti-2" ~now ()
  in
  OUnit2.assert_raises (Space_credential.Invalid "invalid token signature")
    (fun () ->
      ignore
        (Space_credential.verify_delegation
           ~keys:[ k256_did_key other_pub ]
           ~aud:space_host ~now jwt))

let test_wrong_typ_rejected _ =
  let priv, pub = p256_pair () in
  let jwt =
    Space_credential.sign_delegation ~sign:(`P256 priv) ~iss:user_did
      ~sub:space_uri ~aud:space_host ~iat ~jti:"jti-3" ~now ()
  in
  OUnit2.assert_raises
    (Space_credential.Invalid
       "wrong token type: expected \"atproto-space-credential+jwt\", got \
        \"atproto-space-delegation+jwt\"") (fun () ->
      ignore
        (Space_credential.verify_credential ~keys:[ p256_did_key pub ] ~now jwt))

let test_credential_roundtrip _ =
  let priv, pub = p256_pair () in
  let jwt =
    Space_credential.sign_credential ~sign:(`P256 priv) ~iss:authority_did
      ~sub:space_uri ~dpop_jkt:official_dpop_jkt ~iat ~jti:"cred-jti" ~now ()
  in
  let token =
    Space_credential.verify_credential
      ~keys:[ p256_did_key pub ]
      ~sub:space_uri ~now jwt
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    Space_credential.credential_typ token.header.typ;
  OUnit2.assert_equal (Some "#atproto") token.header.kid;
  OUnit2.assert_equal ~printer:(fun x -> x) authority_did token.payload.iss;
  OUnit2.assert_equal None token.payload.aud;
  OUnit2.assert_equal ~printer:Int64.to_string 7200L
    (Int64.sub token.payload.exp token.payload.iat);
  OUnit2.assert_equal (Some official_dpop_jkt) token.payload.cnf_jkt

let test_credential_space_key_kid _ =
  let priv, pub = p256_pair () in
  let jwt =
    Space_credential.sign_credential ~sign:(`P256 priv) ~iss:authority_did
      ~sub:space_uri ~dpop_jkt:official_dpop_jkt
      ~kid:Space_credential.space_key_kid ~iat ~jti:"kid-jti" ~now ()
  in
  let token =
    Space_credential.verify_credential ~keys:[ p256_did_key pub ] ~now jwt
  in
  OUnit2.assert_equal (Some "#atproto_space") token.header.kid

let test_credential_requires_jkt _ =
  let priv, _ = p256_pair () in
  OUnit2.assert_raises
    (Space_credential.Invalid "a credential token requires a \"dpopJkt\"")
    (fun () ->
      ignore
        (Space_credential.sign_credential ~sign:(`P256 priv) ~iss:authority_did
           ~sub:space_uri ~dpop_jkt:"" ~now ()))

let test_credential_missing_cnf _ =
  let priv, _ = p256_pair () in
  let unbound =
    Space_credential.sign_delegation ~sign:(`P256 priv) ~iss:authority_did
      ~sub:space_uri ~aud:space_host ~iat ~jti:"unbound" ~now ()
  in
  let forged = retype_token unbound Space_credential.credential_typ in
  OUnit2.assert_raises (Space_credential.Invalid "missing token \"cnf.jkt\"")
    (fun () -> ignore (Space_credential.parse_credential forged))

let test_credential_expiry_and_skew _ =
  let priv, pub = p256_pair () in
  let jwt =
    Space_credential.sign_credential ~sign:(`P256 priv) ~iss:authority_did
      ~sub:space_uri ~dpop_jkt:official_dpop_jkt ~iat ~exp:(Int64.add iat 1L)
      ~jti:"exp-jti" ~now ()
  in
  let keys = [ p256_did_key pub ] in
  ignore (Space_credential.verify_credential ~keys ~now:(now +. 3.0) jwt);
  OUnit2.assert_raises (Space_credential.Invalid "token expired") (fun () ->
      ignore (Space_credential.verify_credential ~keys ~now:(now +. 60.0) jwt))

let test_attestation_shape _ =
  let priv, pub = p256_pair () in
  let jwt =
    Space_credential.sign_attestation ~sign:(`P256 priv) ~client_id
      ~aud:space_host ~kid:"key-1" ~iat ~jti:"att-jti" ~now ()
  in
  let parsed = Space_credential.parse_attestation jwt in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    Space_credential.attestation_typ parsed.header.typ;
  OUnit2.assert_equal (Some "key-1") parsed.header.kid;
  OUnit2.assert_equal ~printer:(fun x -> x) client_id parsed.payload.iss;
  OUnit2.assert_equal ~printer:(fun x -> x) client_id parsed.payload.sub;
  OUnit2.assert_equal (Some space_host) parsed.payload.aud;
  let verified =
    Space_credential.verify_attestation
      ~keys:[ p256_did_key pub ]
      ~aud:space_host ~now jwt
  in
  OUnit2.assert_equal ~printer:(fun x -> x) client_id verified.payload.iss

let test_attestation_iss_sub_must_match _ =
  let priv, _ = p256_pair () in
  (* Mint via the shared path by signing a well-formed token then rewriting
     sub, so parse (not mint) is what rejects the mismatch. *)
  let jwt =
    Space_credential.sign_attestation ~sign:(`P256 priv) ~client_id
      ~aud:space_host ~iat ~jti:"att-bad" ~now ()
  in
  let h, p, s = Oauth.split_jwt jwt in
  let payload = Yojson.Safe.from_string (Base64url.decode p) in
  let fields =
    List.map
      (fun (k, v) ->
        if k = "sub" then (k, `String "https://other.example/x") else (k, v))
      (match payload with `Assoc xs -> xs | _ -> [])
  in
  let rewritten =
    h ^ "." ^ Base64url.encode (Yojson.Safe.to_string (`Assoc fields)) ^ "." ^ s
  in
  OUnit2.assert_raises
    (Space_credential.Invalid
       "client attestation \"iss\" and \"sub\" must both be the client_id")
    (fun () -> ignore (Space_credential.parse_attestation rewritten))

let test_rejects_record_uri _ =
  let priv, _ = p256_pair () in
  let record =
    space_uri ^ "/did:example:alice/app.bsky.feed.post/3jzfcijpj2z2a"
  in
  OUnit2.assert_raises
    (Space_credential.Invalid
       "delegation sub must be a space URI (through skey), not a record URI")
    (fun () ->
      ignore
        (Space_credential.sign_delegation ~sign:(`P256 priv) ~iss:user_did
           ~sub:record ~aud:space_host ~now ()))

let test_malformed _ =
  OUnit2.assert_raises
    (Space_credential.Invalid
       "Oauth: invalid JWT (expected three base64url parts)") (fun () ->
      ignore (Space_credential.parse_credential "nope"));
  OUnit2.assert_raises
    (Space_credential.Invalid
       "Oauth: invalid JWT (expected three base64url parts)") (fun () ->
      ignore (Space_credential.parse_credential "aaa.bbb"));
  let header =
    Base64url.encode
      (Yojson.Safe.to_string
         (`Assoc
           [
             ("alg", `String "ES256K");
             ("typ", `String Space_credential.credential_typ);
           ]))
  in
  let payload =
    Base64url.encode
      (Yojson.Safe.to_string (`Assoc [ ("iss", `String authority_did) ]))
  in
  OUnit2.assert_raises (Space_credential.Invalid "missing token \"sub\"")
    (fun () ->
      ignore
        (Space_credential.parse_credential (header ^ "." ^ payload ^ ".c2ln")))

let test_offline_bodies _ =
  OUnit2.assert_equal
    [ ("space", space_uri) ]
    (Space_credential.get_delegation_token_body ~space:space_uri);
  let body =
    Space_credential.get_space_credential_body ~space:space_uri
      ~client_attestation:"att.jwt" ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    space_uri
    (body |> member "space" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "att.jwt"
    (body |> member "clientAttestation" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "https://space.example.com/xrpc/com.atproto.space.getSpaceCredential"
    (Space_credential.get_space_credential_htu
       ~origin:"https://space.example.com")

let exchange_htu =
  "https://space.example.com/xrpc/com.atproto.space.getSpaceCredential"

let resource_htu = "https://pds.example.com/xrpc/com.atproto.space.getRepo"

let test_dpop_exchange_and_resource _ =
  let priv, pub = p256_pair () in
  let jkt = Space_credential.dpop_jkt pub in
  let cred =
    Space_credential.sign_credential ~sign:(`P256 priv) ~iss:authority_did
      ~sub:space_uri ~dpop_jkt:jkt ~iat ~jti:"bound" ~now ()
  in
  let exchange =
    Space_credential.exchange_dpop_proof ~priv ~pub ~htu:exchange_htu
      ~jti:"ex-jti" ~iat ()
  in
  let checked =
    Space_credential.verify_exchange_dpop ~htm:"POST" ~htu:exchange_htu ~now
      exchange
  in
  OUnit2.assert_equal ~printer:(fun x -> x) jkt checked.jkt;
  OUnit2.assert_equal None checked.ath;
  OUnit2.assert_equal ~printer:(fun x -> x) "POST" checked.htm;
  let headers =
    Space_credential.exchange_headers ~delegation:"del.jwt" ~dpop:exchange
  in
  OUnit2.assert_equal "Authorization" (fst (List.hd headers));
  OUnit2.assert_equal "DPoP" (fst (List.nth headers 1));
  let resource =
    Space_credential.resource_dpop_proof ~priv ~pub ~htm:"GET" ~htu:resource_htu
      ~credential:cred ~jti:"res-jti" ~iat ()
  in
  let res =
    Space_credential.verify_resource_dpop ~htm:"GET" ~htu:resource_htu
      ~credential:cred ~jkt ~now resource
  in
  OUnit2.assert_equal (Some (Space_credential.ath_of_credential cred)) res.ath;
  OUnit2.assert_equal ~printer:(fun x -> x) jkt res.jkt;
  let rh = Space_credential.resource_headers ~credential:cred ~dpop:resource in
  OUnit2.assert_bool "dpop scheme"
    (let v = snd (List.hd rh) in
     String.length v > 5 && String.sub v 0 5 = "DPoP ")

let test_dpop_strips_query _ =
  let priv, pub = p256_pair () in
  let jkt = Space_credential.dpop_jkt pub in
  let cred = "eyJ0eXAiOiJhdHByb3RvLXNwYWNlLWNyZWRlbnRpYWwrand0" in
  let proof =
    Space_credential.resource_dpop_proof ~priv ~pub ~htm:"GET"
      ~htu:(resource_htu ^ "?space=at%3A%2F%2Fdid%3Aexample%3Aspace")
      ~credential:cred ~jti:"q-jti" ~iat ()
  in
  let checked =
    Space_credential.verify_resource_dpop ~htm:"GET"
      ~htu:(resource_htu ^ "?space=something&repo=else")
      ~credential:cred ~jkt ~now proof
  in
  OUnit2.assert_equal ~printer:(fun x -> x) resource_htu checked.htu

let test_dpop_exchange_refuses_ath _ =
  let priv, pub = p256_pair () in
  let proof =
    Space_credential.resource_dpop_proof ~priv ~pub ~htm:"POST"
      ~htu:exchange_htu ~credential:"not-a-grant" ~jti:"ath-jti" ~iat ()
  in
  OUnit2.assert_raises
    (Space_credential.Invalid
       "DPoP proof \"ath\" must be omitted when obtaining a credential")
    (fun () ->
      ignore
        (Space_credential.verify_exchange_dpop ~htm:"POST" ~htu:exchange_htu
           ~now proof))

let test_dpop_mismatch_and_expiry _ =
  let priv, pub = p256_pair () in
  let other_priv, other_pub = Space_credential.generate_dpop_pair () in
  let jkt = Space_credential.dpop_jkt pub in
  let cred = "space-credential-fixture" in
  let wrong_key =
    Space_credential.resource_dpop_proof ~priv:other_priv ~pub:other_pub
      ~htm:"GET" ~htu:resource_htu ~credential:cred ~jti:"wk" ~iat ()
  in
  OUnit2.assert_raises
    (Space_credential.Invalid
       "DPoP proof is not signed by the key the credential is bound to")
    (fun () ->
      ignore
        (Space_credential.verify_resource_dpop ~htm:"GET" ~htu:resource_htu
           ~credential:cred ~jkt ~now wrong_key));
  let other_host =
    Space_credential.resource_dpop_proof ~priv ~pub ~htm:"GET"
      ~htu:"https://other-pds.example.com/xrpc/com.atproto.space.getRepo"
      ~credential:cred ~jti:"oh" ~iat ()
  in
  OUnit2.assert_raises
    (Space_credential.Invalid "DPoP proof \"htu\" does not match the request")
    (fun () ->
      ignore
        (Space_credential.verify_resource_dpop ~htm:"GET" ~htu:resource_htu
           ~credential:cred ~jkt ~now other_host));
  let other_method =
    Space_credential.resource_dpop_proof ~priv ~pub ~htm:"POST"
      ~htu:resource_htu ~credential:cred ~jti:"om" ~iat ()
  in
  OUnit2.assert_raises
    (Space_credential.Invalid "DPoP proof \"htm\" does not match the request")
    (fun () ->
      ignore
        (Space_credential.verify_resource_dpop ~htm:"GET" ~htu:resource_htu
           ~credential:cred ~jkt ~now other_method));
  let other_cred =
    Space_credential.resource_dpop_proof ~priv ~pub ~htm:"GET" ~htu:resource_htu
      ~credential:"a-different-credential" ~jti:"oc" ~iat ()
  in
  OUnit2.assert_raises
    (Space_credential.Invalid "DPoP proof \"ath\" does not match the credential")
    (fun () ->
      ignore
        (Space_credential.verify_resource_dpop ~htm:"GET" ~htu:resource_htu
           ~credential:cred ~jkt ~now other_cred));
  let stale =
    Space_credential.resource_dpop_proof ~priv ~pub ~htm:"GET" ~htu:resource_htu
      ~credential:cred ~jti:"stale" ~iat ()
  in
  OUnit2.assert_raises (Space_credential.Invalid "DPoP proof is expired")
    (fun () ->
      ignore
        (Space_credential.verify_resource_dpop ~htm:"GET" ~htu:resource_htu
           ~credential:cred ~jkt ~now:(now +. 120.0) stale));
  let fresh =
    Space_credential.resource_dpop_proof ~priv ~pub ~htm:"GET" ~htu:resource_htu
      ~credential:cred ~jti:"skew" ~iat ()
  in
  ignore
    (Space_credential.verify_resource_dpop ~htm:"GET" ~htu:resource_htu
       ~credential:cred ~jkt ~now:(now +. 2.0) fresh);
  ignore
    (Space_credential.verify_resource_dpop ~htm:"GET" ~htu:resource_htu
       ~credential:cred ~jkt ~now:(now -. 2.0) fresh)

let test_dpop_jti_distinct _ =
  let priv, pub = p256_pair () in
  let a =
    Space_credential.exchange_dpop_proof ~priv ~pub ~htu:exchange_htu ()
  in
  let b =
    Space_credential.exchange_dpop_proof ~priv ~pub ~htu:exchange_htu ()
  in
  let ca =
    Space_credential.verify_exchange_dpop ~htm:"POST" ~htu:exchange_htu a
  in
  let cb =
    Space_credential.verify_exchange_dpop ~htm:"POST" ~htu:exchange_htu b
  in
  OUnit2.assert_bool "distinct jti" (ca.jti <> cb.jti)

let suite =
  "space_credential"
  >::: [
         "test_constants" >:: test_constants;
         "test_delegation_roundtrip_p256" >:: test_delegation_roundtrip_p256;
         "test_delegation_k256" >:: test_delegation_k256;
         "test_delegation_requires_aud" >:: test_delegation_requires_aud;
         "test_delegation_wrong_aud_and_sub"
         >:: test_delegation_wrong_aud_and_sub;
         "test_delegation_wrong_key" >:: test_delegation_wrong_key;
         "test_wrong_typ_rejected" >:: test_wrong_typ_rejected;
         "test_credential_roundtrip" >:: test_credential_roundtrip;
         "test_credential_space_key_kid" >:: test_credential_space_key_kid;
         "test_credential_requires_jkt" >:: test_credential_requires_jkt;
         "test_credential_missing_cnf" >:: test_credential_missing_cnf;
         "test_credential_expiry_and_skew" >:: test_credential_expiry_and_skew;
         "test_attestation_shape" >:: test_attestation_shape;
         "test_attestation_iss_sub_must_match"
         >:: test_attestation_iss_sub_must_match;
         "test_rejects_record_uri" >:: test_rejects_record_uri;
         "test_malformed" >:: test_malformed;
         "test_offline_bodies" >:: test_offline_bodies;
         "test_dpop_exchange_and_resource" >:: test_dpop_exchange_and_resource;
         "test_dpop_strips_query" >:: test_dpop_strips_query;
         "test_dpop_exchange_refuses_ath" >:: test_dpop_exchange_refuses_ath;
         "test_dpop_mismatch_and_expiry" >:: test_dpop_mismatch_and_expiry;
         "test_dpop_jti_distinct" >:: test_dpop_jti_distinct;
       ]

let () = run_test_tt_main suite
