open OUnit2
open Atproto.Cid
open Atproto.Mst
open Atproto.Did_key
open Atproto.Hash
open Atproto.K256

let official_heights =
  [
    ("", 0);
    ("asdf", 0);
    ("blue", 1);
    ("2653ae71", 0);
    ("88bfafc7", 2);
    ("2a92d355", 4);
    ("884976f5", 6);
    ("app.bsky.feed.post/454397e440ec", 4);
    ("app.bsky.feed.post/9adeb165882c", 8);
  ]

let official_prefixes =
  [
    ("", "", 0);
    ("abc", "abc", 3);
    ("", "abc", 0);
    ("abc", "", 0);
    ("ab", "abc", 2);
    ("abc", "ab", 2);
    ("abcde", "abc", 3);
    ("abc", "abcde", 3);
    ("abcde", "abc1", 3);
    ("abcde", "abb", 2);
    ("abcde", "qbb", 0);
    ("abc", "abc\x00", 3);
    ("abc\x00", "abc", 3);
  ]

let test_official_heights _ =
  List.iter
    (fun (key, height) ->
      OUnit2.assert_equal ~printer:string_of_int height (Mst.layer_for_key key))
    official_heights

let test_official_prefixes _ =
  List.iter
    (fun (left, right, len) ->
      OUnit2.assert_equal ~printer:string_of_int len
        (Mst.common_prefix_len left right))
    official_prefixes

let entry ~prefix_len ~key_suffix ~value ?right () : Mst.entry =
  { Mst.prefix_len; key_suffix; value; right }

let test_single_node_roundtrip_and_lookup _ =
  let k1 = "2653ae71" in
  let k2 = "asdf" in
  let v1 = Cid.create ~codec:Cid.Raw "v1" in
  let v2 = Cid.create ~codec:Cid.Raw "v2" in
  let prefix = Mst.common_prefix_len k1 k2 in
  let node =
    {
      Mst.left = None;
      entries =
        [
          entry ~prefix_len:0 ~key_suffix:k1 ~value:v1 ();
          entry ~prefix_len:prefix
            ~key_suffix:(String.sub k2 prefix (String.length k2 - prefix))
            ~value:v2 ();
        ];
    }
  in
  let items = Mst.verify_node ~expected_layer:0 node in
  OUnit2.assert_equal [ k1; k2 ] (List.map (fun r -> r.Mst.key) items);
  let root = Mst.cid_of_node node in
  let get_block cid =
    if Cid.equal cid root then Some (Mst.to_bytes node) else None
  in
  Mst.verify_tree ~get_block root;
  (match Mst.lookup ~get_block root k1 with
  | Some cid -> OUnit2.assert_bool "v1" (Cid.equal cid v1)
  | None -> OUnit2.assert_failure "missing k1");
  (match Mst.lookup ~get_block root k2 with
  | Some cid -> OUnit2.assert_bool "v2" (Cid.equal cid v2)
  | None -> OUnit2.assert_failure "missing k2");
  OUnit2.assert_equal None (Mst.lookup ~get_block root "missing")

let test_two_level_tree _ =
  let leaf_key = "2653ae71" in
  let root_key = "blue" in
  let v_leaf = Cid.create ~codec:Cid.Raw "leaf-val" in
  let v_root = Cid.create ~codec:Cid.Raw "root-val" in
  let leaf =
    {
      Mst.left = None;
      entries = [ entry ~prefix_len:0 ~key_suffix:leaf_key ~value:v_leaf () ];
    }
  in
  let leaf_cid = Mst.cid_of_node leaf in
  let root =
    {
      Mst.left = Some leaf_cid;
      entries = [ entry ~prefix_len:0 ~key_suffix:root_key ~value:v_root () ];
    }
  in
  let root_cid = Mst.cid_of_node root in
  let get_block cid =
    if Cid.equal cid leaf_cid then Some (Mst.to_bytes leaf)
    else if Cid.equal cid root_cid then Some (Mst.to_bytes root)
    else None
  in
  Mst.verify_tree ~get_block root_cid;
  (match Mst.lookup ~get_block root_cid leaf_key with
  | Some cid -> OUnit2.assert_bool "leaf" (Cid.equal cid v_leaf)
  | None -> OUnit2.assert_failure "left-child lookup failed");
  match Mst.lookup ~get_block root_cid root_key with
  | Some cid -> OUnit2.assert_bool "root" (Cid.equal cid v_root)
  | None -> OUnit2.assert_failure "root lookup failed"

let test_rejects_unsorted_keys _ =
  let v = Cid.create ~codec:Cid.Raw "x" in
  let node =
    {
      Mst.left = None;
      entries =
        [
          entry ~prefix_len:0 ~key_suffix:"zeta" ~value:v ();
          entry ~prefix_len:0 ~key_suffix:"alpha" ~value:v ();
        ];
    }
  in
  OUnit2.assert_bool "unsorted accepted"
    (try
       ignore (Mst.verify_node node);
       false
     with Mst.Verify_error _ -> true)

let test_rejects_bad_prefix _ =
  let v = Cid.create ~codec:Cid.Raw "x" in
  let node =
    {
      Mst.left = None;
      entries =
        [
          entry ~prefix_len:0 ~key_suffix:"abc" ~value:v ();
          entry ~prefix_len:2 ~key_suffix:"zzz" ~value:v ();
        ];
    }
  in
  OUnit2.assert_bool "bad prefix accepted"
    (try
       ignore (Mst.verify_node node);
       false
     with Mst.Verify_error _ -> true)

let value_cid label = Cid.create ~codec:Cid.Raw label

let test_insert_lookup_remove _ =
  let store = Mst.store_of_get (fun _ -> None) in
  let t = Mst.empty_tree store in
  let v1 = value_cid "one"
  and v2 = value_cid "two"
  and v3 = value_cid "three" in
  let t, prev = Mst.insert t "2653ae71" v1 in
  OUnit2.assert_equal None prev;
  let t, prev = Mst.insert t "asdf" v2 in
  OUnit2.assert_equal None prev;
  let t, prev = Mst.insert t "blue" v3 in
  OUnit2.assert_equal None prev;
  (match Mst.get t "2653ae71" with
  | Some c -> OUnit2.assert_bool "v1" (Cid.equal c v1)
  | None -> OUnit2.assert_failure "missing 2653ae71");
  (match Mst.get t "blue" with
  | Some c -> OUnit2.assert_bool "v3" (Cid.equal c v3)
  | None -> OUnit2.assert_failure "missing blue");
  let t, prev = Mst.remove t "asdf" in
  (match prev with
  | Some c -> OUnit2.assert_bool "removed v2" (Cid.equal c v2)
  | None -> OUnit2.assert_failure "remove missed asdf");
  OUnit2.assert_equal None (Mst.get t "asdf");
  Mst.verify_tree ~get_block:(Mst.store_get t.store) (Mst.root_cid t)

let test_insert_replace_returns_prev _ =
  let store = Mst.store_of_get (fun _ -> None) in
  let t = Mst.empty_tree store in
  let a = value_cid "a" and b = value_cid "b" in
  let t, _ = Mst.insert t "asdf" a in
  let t, prev = Mst.insert t "asdf" b in
  (match prev with
  | Some c -> OUnit2.assert_bool "prev a" (Cid.equal c a)
  | None -> OUnit2.assert_failure "replace should return previous CID");
  match Mst.get t "asdf" with
  | Some c -> OUnit2.assert_bool "now b" (Cid.equal c b)
  | None -> OUnit2.assert_failure "key missing after replace"

let test_invert_create_update_delete _ =
  let store = Mst.store_of_get (fun _ -> None) in
  let t0 = Mst.empty_tree store in
  let va = value_cid "rec-a"
  and vb = value_cid "rec-b"
  and vc = value_cid "rec-c" in
  let t, _ = Mst.insert t0 "app.bsky.feed.post/aaa" va in
  let before_create = Mst.root_cid t in
  let t, _ = Mst.insert t "app.bsky.feed.post/bbb" vb in
  let after_create = Mst.root_cid t in
  let inverted =
    Mst.invert_ops t
      [
        {
          Mst.action = "create";
          path = "app.bsky.feed.post/bbb";
          cid = Some vb;
          prev = None;
        };
      ]
  in
  OUnit2.assert_bool "invert create"
    (Cid.equal (Mst.root_cid inverted) before_create);
  let t, _ = Mst.insert inverted "app.bsky.feed.post/bbb" vb in
  OUnit2.assert_bool "re-apply create" (Cid.equal (Mst.root_cid t) after_create);
  let t, _ = Mst.insert t "app.bsky.feed.post/bbb" vc in
  let inverted =
    Mst.invert_ops t
      [
        {
          Mst.action = "update";
          path = "app.bsky.feed.post/bbb";
          cid = Some vc;
          prev = Some vb;
        };
      ]
  in
  (match Mst.get inverted "app.bsky.feed.post/bbb" with
  | Some c -> OUnit2.assert_bool "update inverted to vb" (Cid.equal c vb)
  | None -> OUnit2.assert_failure "update invert dropped key");
  let t, _ = Mst.remove inverted "app.bsky.feed.post/aaa" in
  let inverted =
    Mst.invert_ops t
      [
        {
          Mst.action = "delete";
          path = "app.bsky.feed.post/aaa";
          cid = None;
          prev = Some va;
        };
      ]
  in
  match Mst.get inverted "app.bsky.feed.post/aaa" with
  | Some c -> OUnit2.assert_bool "delete inverted to va" (Cid.equal c va)
  | None -> OUnit2.assert_failure "delete invert did not restore key"

let test_invert_create_mismatch _ =
  let store = Mst.store_of_get (fun _ -> None) in
  let t = Mst.empty_tree store in
  let va = value_cid "rec-a" and wrong = value_cid "nope" in
  let t, _ = Mst.insert t "app.bsky.feed.post/aaa" va in
  OUnit2.assert_bool "bad invert accepted"
    (try
       ignore
         (Mst.invert_op t
            {
              Mst.action = "create";
              path = "app.bsky.feed.post/aaa";
              cid = Some wrong;
              prev = None;
            });
       false
     with Mst.Verify_error _ -> true)

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

let test_sign_verify_commit_p256 _ =
  let priv, pub = p256_pair () in
  let data = Cid.create ~codec:Cid.Raw "mst-root" in
  let signed =
    Mst.sign_p256 ~priv ~did:"did:plc:7iza6de2dwap2sbkpav7c6c6" ~data
      ~rev:"3jzfcijpj2z2a" ()
  in
  let commit =
    Mst.parse_repo_commit (Atproto.Dag_cbor.Dag_cbor.decode signed)
  in
  OUnit2.assert_equal ~printer:(fun x -> x) "3jzfcijpj2z2a" commit.rev;
  match Mst.verify_commit_sig ~keys:[ p256_did_key pub ] commit with
  | `Valid -> ()
  | other ->
      OUnit2.assert_failure
        (match other with
        | `Invalid -> "p256 commit sig invalid"
        | `Missing -> "p256 commit sig missing"
        | `Unsupported_curve c -> "unsupported " ^ c
        | `Valid -> "valid")

let test_sign_verify_commit_k256 _ =
  match K256.priv_of_octets (Hash.hex_decode (String.make 63 '0' ^ "3")) with
  | Error _ -> OUnit2.assert_failure "k256 priv rejected"
  | Ok priv -> (
      let pub = K256.pub_of_priv priv in
      let data = Cid.create ~codec:Cid.Raw "mst-root-k" in
      let signed =
        Mst.sign_k256 ~priv ~did:"did:plc:7iza6de2dwap2sbkpav7c6c6" ~data
          ~rev:"3jzfcijpj2z2a" ()
      in
      let commit =
        Mst.parse_repo_commit (Atproto.Dag_cbor.Dag_cbor.decode signed)
      in
      let key =
        Did_key.to_string
          (Did_key.of_k256_octets (K256.pub_to_octets ~compress:true pub))
      in
      match Mst.verify_commit_sig ~keys:[ key ] commit with
      | `Valid -> ()
      | _ -> OUnit2.assert_failure "k256 commit sig invalid")

let test_commit_sig_wrong_key_and_missing _ =
  let priv, pub = p256_pair () in
  let data = Cid.create ~codec:Cid.Raw "mst-root" in
  let unsigned =
    Mst.unsigned_repo_commit ~did:"did:plc:7iza6de2dwap2sbkpav7c6c6" ~data
      ~rev:"3jzfcijpj2z2a" ()
  in
  let unsigned_c =
    Mst.parse_repo_commit (Atproto.Dag_cbor.Dag_cbor.decode unsigned)
  in
  (match Mst.verify_commit_sig ~keys:[ p256_did_key pub ] unsigned_c with
  | `Missing -> ()
  | _ -> OUnit2.assert_failure "unsigned commit should be Missing");
  let signed =
    Mst.sign_p256 ~priv ~did:"did:plc:7iza6de2dwap2sbkpav7c6c6" ~data
      ~rev:"3jzfcijpj2z2a" ()
  in
  let commit =
    Mst.parse_repo_commit (Atproto.Dag_cbor.Dag_cbor.decode signed)
  in
  match K256.priv_of_octets (String.make 31 '\x00' ^ "\x01") with
  | Error _ -> OUnit2.assert_failure "k256 priv=1 rejected"
  | Ok kpriv -> (
      let kpub = K256.pub_of_priv kpriv in
      let wrong =
        Did_key.to_string
          (Did_key.of_k256_octets (K256.pub_to_octets ~compress:true kpub))
      in
      match Mst.verify_commit_sig ~keys:[ wrong ] commit with
      | `Invalid -> ()
      | _ -> OUnit2.assert_failure "wrong-key commit should be Invalid")

let test_cid_mismatch _ =
  let v = Cid.create ~codec:Cid.Raw "x" in
  let node =
    {
      Mst.left = None;
      entries = [ entry ~prefix_len:0 ~key_suffix:"2653ae71" ~value:v () ];
    }
  in
  let root = Mst.cid_of_node node in
  let get_block _ = Some (Mst.to_bytes node ^ "tamper") in
  OUnit2.assert_bool "tampered block accepted"
    (try
       Mst.verify_tree ~get_block root;
       false
     with Mst.Verify_error _ -> true)

let suite =
  "mst"
  >::: [
         "test_official_heights" >:: test_official_heights;
         "test_official_prefixes" >:: test_official_prefixes;
         "test_single_node_roundtrip_and_lookup"
         >:: test_single_node_roundtrip_and_lookup;
         "test_two_level_tree" >:: test_two_level_tree;
         "test_rejects_unsorted_keys" >:: test_rejects_unsorted_keys;
         "test_rejects_bad_prefix" >:: test_rejects_bad_prefix;
         "test_insert_lookup_remove" >:: test_insert_lookup_remove;
         "test_insert_replace_returns_prev" >:: test_insert_replace_returns_prev;
         "test_invert_create_update_delete" >:: test_invert_create_update_delete;
         "test_invert_create_mismatch" >:: test_invert_create_mismatch;
         "test_sign_verify_commit_p256" >:: test_sign_verify_commit_p256;
         "test_sign_verify_commit_k256" >:: test_sign_verify_commit_k256;
         "test_commit_sig_wrong_key_and_missing"
         >:: test_commit_sig_wrong_key_and_missing;
         "test_cid_mismatch" >:: test_cid_mismatch;
       ]

let () = run_test_tt_main suite
