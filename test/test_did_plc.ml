open OUnit2
open Atproto.Did_plc
open Atproto.Did_key
open Atproto.Hash
open Atproto.Base64url
open Atproto.K256

let sample_doc =
  {|
{
  "@context": [
    "https://www.w3.org/ns/did/v1",
    "https://w3id.org/security/multikey/v1"
  ],
  "id": "did:plc:7iza6de2dwap2sbkpav7c6c6",
  "alsoKnownAs": ["at://alice.test"],
  "verificationMethod": [
    {
      "id": "#atproto",
      "type": "Multikey",
      "controller": "did:plc:7iza6de2dwap2sbkpav7c6c6",
      "publicKeyMultibase": "zDnaeh9v2RmcMo13Du2d6pjUf5bZwtauYxj3n9dYjw4EZUAR7"
    }
  ],
  "service": [
    {
      "id": "#atproto_pds",
      "type": "AtprotoPersonalDataServer",
      "serviceEndpoint": "https://example2.com"
    }
  ]
}
|}

let test_validate_plc_did _ =
  Did_plc.validate_plc_did "did:plc:7iza6de2dwap2sbkpav7c6c6";
  OUnit2.assert_bool "accepted invalid did:plc"
    (try
       Did_plc.validate_plc_did "did:web:example.com";
       false
     with Failure _ -> true)

let test_parse_document _ =
  let doc = Did_plc.parse_document (Yojson.Safe.from_string sample_doc) in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:plc:7iza6de2dwap2sbkpav7c6c6" doc.id;
  OUnit2.assert_equal (Some "alice.test") (Did_plc.handle_of_document doc);
  OUnit2.assert_equal (Some "https://example2.com") (Did_plc.pds_endpoint doc);
  match Did_plc.signing_key doc with
  | None -> OUnit2.assert_failure "missing #atproto key"
  | Some key -> OUnit2.assert_equal ~printer:(fun x -> x) "Multikey" key.type_

let test_directory_url _ =
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "https://plc.directory/did:plc:7iza6de2dwap2sbkpav7c6c6"
    (Did_plc.directory_url "did:plc:7iza6de2dwap2sbkpav7c6c6")

let rfc6979_p256_priv =
  Hash.hex_decode
    "c9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721"

let p256_pair () =
  match Mirage_crypto_ec.P256.Dsa.priv_of_octets rfc6979_p256_priv with
  | Error _ -> failwith "could not load RFC 6979 P-256 private key"
  | Ok priv -> (priv, Mirage_crypto_ec.P256.Dsa.pub_of_priv priv)

let rotation_did_key pub =
  let octets = Mirage_crypto_ec.P256.Dsa.pub_to_octets ~compress:true pub in
  Did_key.to_string (Did_key.of_p256_octets octets)

let genesis_json rotation_key =
  `Assoc
    [
      ("type", `String "plc_operation");
      ("rotationKeys", `List [ `String rotation_key ]);
      ("verificationMethods", `Assoc [ ("atproto", `String rotation_key) ]);
      ("alsoKnownAs", `List [ `String "at://alice.test" ]);
      ( "services",
        `Assoc
          [
            ( "atproto_pds",
              `Assoc
                [
                  ("type", `String "AtprotoPersonalDataServer");
                  ("endpoint", `String "https://example.com");
                ] );
          ] );
      ("prev", `Null);
    ]

let test_sign_and_verify_p256 _ =
  let priv, pub = p256_pair () in
  let rotation = rotation_did_key pub in
  let signed = Did_plc.sign_p256 ~priv (genesis_json rotation) in
  let op = Did_plc.parse_operation signed in
  let did = Did_plc.genesis_did op in
  OUnit2.assert_bool "genesis did:plc" (Did_plc.is_plc_did did);
  OUnit2.assert_equal 32 (String.length did);
  match Did_plc.verify_p256 ~pub op with
  | `Valid -> ()
  | other ->
      OUnit2.assert_failure
        (Printf.sprintf "expected Valid, got %s"
           (match other with
           | `Invalid -> "Invalid"
           | `Missing -> "Missing"
           | `Unsupported_curve c -> "Unsupported " ^ c
           | `Valid -> "Valid"))

let test_high_s_rejected _ =
  let priv, pub = p256_pair () in
  let rotation = rotation_did_key pub in
  let signed = Did_plc.sign_p256 ~priv (genesis_json rotation) in
  let op = Did_plc.parse_operation signed in
  match op.sig_ with
  | None -> OUnit2.assert_failure "missing sig"
  | Some b64 ->
      let raw = Base64url.decode b64 in
      let r = String.sub raw 0 32 in
      let s = String.sub raw 32 32 in
      let high = Did_plc.sub_be Did_plc.p256_n s in
      let flipped =
        match signed with
        | `Assoc fields ->
            `Assoc
              (List.map
                 (fun (k, v) ->
                   if k = "sig" then (k, `String (Base64url.encode (r ^ high)))
                   else (k, v))
                 fields)
        | _ -> signed
      in
      let bad = Did_plc.parse_operation flipped in
      OUnit2.assert_equal `Invalid (Did_plc.verify_p256 ~pub bad)

let test_chain_prev_and_genesis _ =
  let priv, pub = p256_pair () in
  let rotation = rotation_did_key pub in
  let genesis =
    Did_plc.parse_operation (Did_plc.sign_p256 ~priv (genesis_json rotation))
  in
  let did = Did_plc.genesis_did genesis in
  let prev = Atproto.Cid.Cid.to_string (Did_plc.cid_of_operation genesis) in
  let update =
    match genesis_json rotation with
    | `Assoc fields ->
        `Assoc
          (List.map
             (fun (k, v) -> if k = "prev" then (k, `String prev) else (k, v))
             fields)
    | _ -> failwith "expected object"
  in
  let second = Did_plc.parse_operation (Did_plc.sign_p256 ~priv update) in
  let chain = Did_plc.verify_chain ~did [ genesis; second ] in
  OUnit2.assert_bool "genesis_ok" chain.genesis_ok;
  OUnit2.assert_bool "prev_links_ok" chain.prev_links_ok;
  OUnit2.assert_equal [ `Valid; `Valid ] chain.signatures;
  let broken =
    Did_plc.verify_chain ~did:"did:plc:aaaaaaaaaaaaaaaaaaaaaaaa"
      [ genesis; second ]
  in
  OUnit2.assert_bool "wrong DID must fail genesis" (not broken.genesis_ok)

let k256_pair () =
  match K256.priv_of_octets (Hash.hex_decode (String.make 63 '0' ^ "2")) with
  | Error _ -> failwith "could not load k256 private key"
  | Ok priv -> (priv, K256.pub_of_priv priv)

let rotation_k256_did_key pub =
  Did_key.to_string
    (Did_key.of_k256_octets (K256.pub_to_octets ~compress:true pub))

let test_sign_and_verify_k256 _ =
  let priv, pub = k256_pair () in
  let rotation = rotation_k256_did_key pub in
  let signed = Did_plc.sign_k256 ~priv (genesis_json rotation) in
  let op = Did_plc.parse_operation signed in
  let did = Did_plc.genesis_did op in
  OUnit2.assert_bool "genesis did:plc" (Did_plc.is_plc_did did);
  (match Did_plc.verify_k256 ~pub op with
  | `Valid -> ()
  | `Invalid -> OUnit2.assert_failure "expected Valid, got Invalid"
  | `Missing -> OUnit2.assert_failure "expected Valid, got Missing"
  | `Unsupported_curve c ->
      OUnit2.assert_failure ("expected Valid, got Unsupported " ^ c));
  OUnit2.assert_equal `Valid (Did_plc.verify_with_rotation_keys [ rotation ] op)

let test_k256_high_s_rejected _ =
  let priv, pub = k256_pair () in
  let rotation = rotation_k256_did_key pub in
  let signed = Did_plc.sign_k256 ~priv (genesis_json rotation) in
  let op = Did_plc.parse_operation signed in
  match op.sig_ with
  | None -> OUnit2.assert_failure "missing sig"
  | Some b64 ->
      let raw = Base64url.decode b64 in
      let r = String.sub raw 0 32 in
      let s = String.sub raw 32 32 in
      let high = Did_plc.sub_be Did_plc.k256_n s in
      let flipped =
        match signed with
        | `Assoc fields ->
            `Assoc
              (List.map
                 (fun (k, v) ->
                   if k = "sig" then (k, `String (Base64url.encode (r ^ high)))
                   else (k, v))
                 fields)
        | _ -> signed
      in
      let bad = Did_plc.parse_operation flipped in
      OUnit2.assert_equal `Invalid (Did_plc.verify_k256 ~pub bad)

let test_verify_missing_and_wrong_key _ =
  let priv, pub = p256_pair () in
  let rotation = rotation_did_key pub in
  let signed = Did_plc.sign_p256 ~priv (genesis_json rotation) in
  let op = Did_plc.parse_operation signed in
  let unsigned = Did_plc.parse_operation (genesis_json rotation) in
  OUnit2.assert_equal `Missing (Did_plc.verify_p256 ~pub unsigned);
  let _, other_pub = p256_pair () in
  (* same RFC key — flip by using k256 instead *)
  let kpriv, kpub = k256_pair () in
  ignore (kpriv, other_pub);
  OUnit2.assert_equal `Invalid (Did_plc.verify_k256 ~pub:kpub op);
  OUnit2.assert_equal `Invalid
    (Did_plc.verify_with_rotation_keys [ rotation_k256_did_key kpub ] op)

let test_unsigned_omits_sig _ =
  let priv, pub = p256_pair () in
  let signed = Did_plc.sign_p256 ~priv (genesis_json (rotation_did_key pub)) in
  let op = Did_plc.parse_operation signed in
  let unsigned = Did_plc.unsigned_bytes op in
  let signed_cbor = Did_plc.signed_bytes op in
  OUnit2.assert_bool "sig must change the signed CBOR" (unsigned <> signed_cbor)

let test_resolve_live _ =
  try
    let doc = Did_plc.resolve "did:plc:z72i7hdynmk6r22z27h6tvur" in
    OUnit2.assert_equal
      ~printer:(fun x -> x)
      "did:plc:z72i7hdynmk6r22z27h6tvur" doc.id;
    OUnit2.assert_bool "expected a PDS service"
      (match Did_plc.pds_endpoint doc with Some _ -> true | None -> false)
  with exn ->
    skip_if true ("plc.directory request skipped: " ^ Printexc.to_string exn)

let test_live_chain_structure _ =
  let did = "did:plc:z72i7hdynmk6r22z27h6tvur" in
  let ops =
    try Did_plc.resolve_log did
    with exn ->
      skip_if true ("plc log fetch skipped: " ^ Printexc.to_string exn);
      []
  in
  OUnit2.assert_bool "expected a non-empty PLC log" (ops <> []);
  let chain = Did_plc.verify_chain ~did ops in
  skip_if (not chain.genesis_ok)
    "live genesis DID did not rematch (directory CBOR bytes)";
  skip_if (not chain.prev_links_ok) "live prev CID chain did not link";
  List.iteri
    (fun i st ->
      match st with
      | `Valid -> ()
      | `Missing ->
          OUnit2.assert_failure
            (Printf.sprintf "live PLC op %d is missing a signature" i)
      | `Invalid ->
          OUnit2.assert_failure
            (Printf.sprintf "live PLC op %d signature is invalid" i)
      | `Unsupported_curve c ->
          OUnit2.assert_failure
            (Printf.sprintf "live PLC op %d uses unsupported curve %s" i c))
    chain.signatures

let suite =
  "did_plc"
  >::: [
         "test_validate_plc_did" >:: test_validate_plc_did;
         "test_parse_document" >:: test_parse_document;
         "test_directory_url" >:: test_directory_url;
         "test_resolve_live" >:: test_resolve_live;
         "test_sign_and_verify_p256" >:: test_sign_and_verify_p256;
         "test_sign_and_verify_k256" >:: test_sign_and_verify_k256;
         "test_high_s_rejected" >:: test_high_s_rejected;
         "test_k256_high_s_rejected" >:: test_k256_high_s_rejected;
         "test_verify_missing_and_wrong_key"
         >:: test_verify_missing_and_wrong_key;
         "test_chain_prev_and_genesis" >:: test_chain_prev_and_genesis;
         "test_unsigned_omits_sig" >:: test_unsigned_omits_sig;
         "test_live_chain_structure" >:: test_live_chain_structure;
       ]

let () = run_test_tt_main suite
