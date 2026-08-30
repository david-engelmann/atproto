open OUnit2
open Atproto.Cid
open Atproto.Mst

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
         "test_cid_mismatch" >:: test_cid_mismatch;
       ]

let () = run_test_tt_main suite
