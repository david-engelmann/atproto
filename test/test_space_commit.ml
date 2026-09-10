open OUnit2
open Atproto.Space_commit
open Atproto.Lt_hash
open Atproto.Hash
open Atproto.Did_key
open Atproto.K256

(* Official empty-state sha256 from bluesky-social/atproto#5187. *)
let empty_hash_hex =
  "e5a00aa9991ac8a5ee3109844d84a55583bd20572ad3ffcd42792f3c36b183ad"

(* Official CommitCtx from packages/space/tests/repo-commit.test.ts. *)
let official_ctx : Space_commit.ctx =
  {
    space = "at://did:example:space/space/app.bsky.group/test";
    author = "did:example:alice";
    rev = "3kbcq3p7ad400";
  }

(* ikm = 32 bytes of 0x07, same as the official encodeCommitCtx suite. *)
let official_ikm = String.make 32 (Char.chr 7)

(* Locked encodeCommitCtx snapshot for official_ctx + official_ikm. *)
let official_ctx_hex =
  "617470726f746f2d73706163652d7631003061743a2f2f6469643a6578616d706c653a73706163652f73706163652f6170702e62736b792e67726f75702f7465737400116469643a6578616d706c653a616c696365000d336b626371337037616434303000200707070707070707070707070707070707070707070707070707070707070707"

let official_hkdf_hex =
  "f4844f93409b0a68f0601ebc1dd3c3694662bf05f9a2465679c65b1aa8fc9c65"

let official_mac_empty_hex =
  "9d2eade2d98656ad2592b104638e2fe6e5f6628ed4db61bdef837d4f64845222"

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

let hex = Hash.hex_encode

let test_context_official_vector _ =
  let ctx_bytes = Space_commit.context ~ctx:official_ctx ~ikm:official_ikm in
  OUnit2.assert_equal ~printer:(fun x -> x) official_ctx_hex (hex ctx_bytes);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    Space_commit.domain_tag
    (String.sub ctx_bytes 0 16);
  let space_len =
    (Char.code ctx_bytes.[16] lsl 8) lor Char.code ctx_bytes.[17]
  in
  OUnit2.assert_equal ~printer:string_of_int
    (String.length official_ctx.space)
    space_len

let test_context_length_prefix_unambiguous _ =
  let ikm = official_ikm in
  let a =
    Space_commit.context ~ctx:{ space = "ab"; author = "c"; rev = "d" } ~ikm
  in
  let b =
    Space_commit.context ~ctx:{ space = "a"; author = "bc"; rev = "d" } ~ikm
  in
  OUnit2.assert_bool "field boundaries" (a <> b)

let test_hkdf_and_mac_vectors _ =
  let ctx_bytes = Space_commit.context ~ctx:official_ctx ~ikm:official_ikm in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    official_hkdf_hex
    (hex (Space_commit.hkdf_expand ~ikm:official_ikm ~info:ctx_bytes));
  let empty_hash = Hash.hex_decode empty_hash_hex in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    official_mac_empty_hex
    (hex (Space_commit.mac ~ikm:official_ikm ~ctx_bytes ~hash:empty_hash))

let test_of_lt_hash_wires_digest _ =
  let priv, pub = p256_pair () in
  let h = Lt_hash.empty () in
  let commit =
    Space_commit.of_lt_hash ~ikm:official_ikm ~ctx:official_ctx
      ~sign:(`P256 priv) h ()
  in
  OUnit2.assert_equal ~printer:string_of_int Space_commit.version commit.ver;
  OUnit2.assert_equal ~printer:(fun x -> x) official_ctx.rev commit.rev;
  OUnit2.assert_equal ~printer:(fun x -> x) empty_hash_hex (hex commit.hash);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    official_mac_empty_hex (hex commit.mac);
  OUnit2.assert_equal ~printer:string_of_int 32 (String.length commit.ikm);
  OUnit2.assert_equal ~printer:string_of_int 64 (String.length commit.sig_);
  OUnit2.assert_bool "matches empty" (Space_commit.matches h commit);
  OUnit2.assert_bool "verify p256"
    (Space_commit.verify ~keys:[ p256_did_key pub ] ~ctx:official_ctx commit)

let test_sign_verify_k256 _ =
  let priv, pub = k256_pair () in
  let h = Lt_hash.add (Lt_hash.empty ()) "one" in
  let commit =
    Space_commit.of_lt_hash ~ikm:official_ikm ~ctx:official_ctx
      ~sign:(`K256 priv) h ()
  in
  OUnit2.assert_bool "matches" (Space_commit.matches h commit);
  OUnit2.assert_bool "verify k256"
    (Space_commit.verify ~keys:[ k256_did_key pub ] ~ctx:official_ctx commit)

let test_fresh_ikm_per_commit _ =
  let priv, _ = p256_pair () in
  let h = Lt_hash.empty () in
  let a = Space_commit.of_lt_hash ~ctx:official_ctx ~sign:(`P256 priv) h () in
  let b = Space_commit.of_lt_hash ~ctx:official_ctx ~sign:(`P256 priv) h () in
  OUnit2.assert_bool "distinct ikm" (a.ikm <> b.ikm);
  OUnit2.assert_bool "distinct mac" (a.mac <> b.mac);
  OUnit2.assert_bool "same hash" (a.hash = b.hash)

let test_wrong_key _ =
  let priv, _ = p256_pair () in
  let _, kpub = k256_pair () in
  let commit =
    Space_commit.of_lt_hash ~ikm:official_ikm ~ctx:official_ctx
      ~sign:(`P256 priv) (Lt_hash.empty ()) ()
  in
  OUnit2.assert_bool "wrong key"
    (not
       (Space_commit.verify
          ~keys:[ k256_did_key kpub ]
          ~ctx:official_ctx commit))

let test_ctx_mismatch _ =
  let priv, pub = p256_pair () in
  let key = p256_did_key pub in
  let commit =
    Space_commit.of_lt_hash ~ikm:official_ikm ~ctx:official_ctx
      ~sign:(`P256 priv) (Lt_hash.empty ()) ()
  in
  let other_space =
    {
      official_ctx with
      space = "at://did:example:space/space/app.bsky.group/other";
    }
  in
  let other_author = { official_ctx with author = "did:example:bob" } in
  let other_rev = { official_ctx with rev = "3kbcq3p7ad999" } in
  OUnit2.assert_bool "space"
    (not (Space_commit.verify ~keys:[ key ] ~ctx:other_space commit));
  OUnit2.assert_bool "author"
    (not (Space_commit.verify ~keys:[ key ] ~ctx:other_author commit));
  OUnit2.assert_bool "rev"
    (not (Space_commit.verify ~keys:[ key ] ~ctx:other_rev commit))

let test_tampered_hash _ =
  let priv, pub = p256_pair () in
  let commit =
    Space_commit.of_lt_hash ~ikm:official_ikm ~ctx:official_ctx
      ~sign:(`P256 priv) (Lt_hash.empty ()) ()
  in
  let tampered =
    { commit with hash = Lt_hash.hash (Lt_hash.add (Lt_hash.empty ()) "x") }
  in
  (* Signature still covers only ctx; MAC is what fails. *)
  (match
     Space_commit.verify_sig
       ~keys:[ p256_did_key pub ]
       ~ctx:official_ctx tampered
   with
  | `Valid -> ()
  | _ -> OUnit2.assert_failure "sig should still verify");
  OUnit2.assert_bool "mac fails"
    (not (Space_commit.verify_mac ~ctx:official_ctx tampered));
  OUnit2.assert_bool "verify fails"
    (not
       (Space_commit.verify
          ~keys:[ p256_did_key pub ]
          ~ctx:official_ctx tampered))

let test_rev_and_version _ =
  let priv, pub = p256_pair () in
  let key = p256_did_key pub in
  let commit =
    Space_commit.of_lt_hash ~ikm:official_ikm ~ctx:official_ctx
      ~sign:(`P256 priv) (Lt_hash.empty ()) ()
  in
  OUnit2.assert_bool "rev disagree"
    (not
       (Space_commit.verify ~keys:[ key ] ~ctx:official_ctx
          { commit with rev = "3kbcq3p7ad999" }));
  OUnit2.assert_bool "unknown ver"
    (not
       (Space_commit.verify ~keys:[ key ] ~ctx:official_ctx
          { commit with ver = 2 }))

let test_matches_after_add _ =
  let priv, _ = p256_pair () in
  let h = Lt_hash.empty () in
  let commit =
    Space_commit.of_lt_hash ~ikm:official_ikm ~ctx:official_ctx
      ~sign:(`P256 priv) h ()
  in
  let advanced = Lt_hash.add h "app.bsky.feed.post/1/bafy" in
  OUnit2.assert_bool "advanced differs"
    (not (Space_commit.matches advanced commit))

let test_encode_decode _ =
  let priv, pub = p256_pair () in
  let commit =
    Space_commit.of_lt_hash ~ikm:official_ikm ~ctx:official_ctx
      ~sign:(`P256 priv) (Lt_hash.empty ()) ()
  in
  let again = Space_commit.decode (Space_commit.encode commit) in
  OUnit2.assert_equal ~printer:string_of_int commit.ver again.ver;
  OUnit2.assert_equal ~printer:(fun x -> x) commit.rev again.rev;
  OUnit2.assert_equal ~printer:(fun x -> x) (hex commit.hash) (hex again.hash);
  OUnit2.assert_equal ~printer:(fun x -> x) (hex commit.ikm) (hex again.ikm);
  OUnit2.assert_equal ~printer:(fun x -> x) (hex commit.mac) (hex again.mac);
  OUnit2.assert_equal ~printer:(fun x -> x) (hex commit.sig_) (hex again.sig_);
  OUnit2.assert_bool "verify after decode"
    (Space_commit.verify ~keys:[ p256_did_key pub ] ~ctx:official_ctx again)

let test_create_rejects_record_uri _ =
  let priv, _ = p256_pair () in
  let ctx =
    {
      official_ctx with
      space =
        official_ctx.space
        ^ "/did:example:alice/app.bsky.feed.post/3jzfcijpj2z2a";
    }
  in
  OUnit2.assert_raises
    (Space_commit.Invalid
       "commit space must be a space URI (through skey), not a record URI")
    (fun () ->
      let _ =
        Space_commit.of_lt_hash ~ikm:official_ikm ~ctx ~sign:(`P256 priv)
          (Lt_hash.empty ()) ()
      in
      ())

let test_random_ikm_length _ =
  OUnit2.assert_equal ~printer:string_of_int 32
    (String.length (Space_commit.random_ikm ()))

let suite =
  "space_commit"
  >::: [
         "test_context_official_vector" >:: test_context_official_vector;
         "test_context_length_prefix_unambiguous"
         >:: test_context_length_prefix_unambiguous;
         "test_hkdf_and_mac_vectors" >:: test_hkdf_and_mac_vectors;
         "test_of_lt_hash_wires_digest" >:: test_of_lt_hash_wires_digest;
         "test_sign_verify_k256" >:: test_sign_verify_k256;
         "test_fresh_ikm_per_commit" >:: test_fresh_ikm_per_commit;
         "test_wrong_key" >:: test_wrong_key;
         "test_ctx_mismatch" >:: test_ctx_mismatch;
         "test_tampered_hash" >:: test_tampered_hash;
         "test_rev_and_version" >:: test_rev_and_version;
         "test_matches_after_add" >:: test_matches_after_add;
         "test_encode_decode" >:: test_encode_decode;
         "test_create_rejects_record_uri" >:: test_create_rejects_record_uri;
         "test_random_ikm_length" >:: test_random_ikm_length;
       ]

let () = run_test_tt_main suite
