open OUnit2
open Atproto.Space_sync
open Atproto.Space_commit
open Atproto.Space_xrpc
open Atproto.Lt_hash
open Atproto.Cid
open Atproto.Car
open Atproto.Dag_cbor
open Atproto.Hash
open Atproto.Did_key

(* Official fixtures from bluesky-social/atproto#5187
   packages/space/tests/sync.test.ts *)
let official_space = "at://did:example:space/space/app.bsky.group/test"
let official_author = "did:example:alice"
let official_rev = "3kbcq3p7ad400"

let official_ctx : Space_commit.ctx =
  { space = official_space; author = official_author; rev = official_rev }

let empty_hash_hex =
  "e5a00aa9991ac8a5ee3109844d84a55583bd20572ad3ffcd42792f3c36b183ad"

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

let hex = Hash.hex_encode

let records () =
  [
    Space_sync.record_of_json ~collection:"app.bsky.feed.post"
      ~rkey:"3kbcq3p7ad401"
      ~value:(`Assoc [ ("text", `String "hello") ]);
    Space_sync.record_of_json ~collection:"app.bsky.feed.post"
      ~rkey:"3kbcq3p7ad402"
      ~value:(`Assoc [ ("text", `String "world") ]);
    Space_sync.record_of_json ~collection:"app.bsky.feed.like"
      ~rkey:"3kbcq3p7ad403"
      ~value:(`Assoc [ ("subject", `String "at://x") ]);
  ]

let entry_of_record (r : Space_sync.record_block) : Space_sync.index_entry =
  {
    Space_sync.collection = r.Space_sync.collection;
    rkey = r.Space_sync.rkey;
    cid = r.Space_sync.cid;
  }

let sign_records recs =
  let priv, pub = p256_pair () in
  let state =
    Space_sync.fold_index (Lt_hash.empty ()) (List.map entry_of_record recs)
  in
  let commit =
    Space_commit.of_lt_hash ~ctx:official_ctx ~sign:(`P256 priv) state ()
  in
  (commit, p256_did_key pub)

let car_for recs =
  let commit, key = sign_records recs in
  (Space_sync.encode ~commit ~records:recs (), commit, key)

let apply_ok ?expect_values car key =
  Space_sync.apply ?expect_values ~keys:[ key ] ~space:official_space
    ~author:official_author car

let contains hay needle =
  let n = String.length needle in
  let rec find i =
    if i + n > String.length hay then false
    else if String.sub hay i n = needle then true
    else find (i + 1)
  in
  find 0

let raises_invalid needle f =
  match f () with
  | exception Space_sync.Invalid msg ->
      OUnit2.assert_bool
        (Printf.sprintf "expected %S in %S" needle msg)
        (contains msg needle)
  | _ -> OUnit2.assert_failure ("expected Invalid containing " ^ needle)

let test_path_and_index_order _ =
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "app.bsky.feed.post/3kbcq3p7ad401"
    (Space_sync.path ~collection:"app.bsky.feed.post" ~rkey:"3kbcq3p7ad401");
  OUnit2.assert_equal
    ("app.bsky.feed.post", "3kbcq3p7ad401")
    (Space_sync.parse_path "app.bsky.feed.post/3kbcq3p7ad401");
  let like = "app.bsky.feed.like/3kbcq3p7ad403" in
  let post1 = "app.bsky.feed.post/3kbcq3p7ad401" in
  let post2 = "app.bsky.feed.post/3kbcq3p7ad402" in
  OUnit2.assert_bool "like before post"
    (Space_sync.compare_index_key like post1 < 0);
  OUnit2.assert_bool "401 before 402"
    (Space_sync.compare_index_key post1 post2 < 0);
  (* shorter key first (canonical DAG-CBOR) *)
  OUnit2.assert_bool "short first"
    (Space_sync.compare_index_key "a/b" "aa/bb" < 0)

let test_apply_ops_create_update_delete _ =
  let recs = records () in
  let a = List.nth recs 0 in
  let b = List.nth recs 1 in
  let creates =
    List.map
      (fun (r : Space_sync.record_block) ->
        Space_sync.op ~collection:r.Space_sync.collection
          ~rkey:r.Space_sync.rkey
          ~cid:(Cid.to_string r.Space_sync.cid)
          ())
      recs
  in
  let from_ops = Space_sync.apply_ops (Lt_hash.empty ()) creates in
  let from_index =
    Space_sync.fold_index (Lt_hash.empty ()) (List.map entry_of_record recs)
  in
  OUnit2.assert_bool "ops match index fold" (Lt_hash.equal from_ops from_index);
  let local =
    Space_sync.apply_ops
      (Space_sync.fold_index (Lt_hash.empty ())
         [
           {
             Space_sync.collection = a.Space_sync.collection;
             rkey = a.Space_sync.rkey;
             cid = a.Space_sync.cid;
           };
         ])
      [
        Space_sync.op ~collection:a.Space_sync.collection
          ~rkey:a.Space_sync.rkey
          ~cid:(Cid.to_string b.Space_sync.cid)
          ~prev:(Cid.to_string a.Space_sync.cid)
          ();
        Space_sync.op ~collection:a.Space_sync.collection
          ~rkey:a.Space_sync.rkey
          ~prev:(Cid.to_string b.Space_sync.cid)
          ();
      ]
  in
  OUnit2.assert_bool "delete empties" (Lt_hash.is_empty local);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    empty_hash_hex
    (hex (Lt_hash.hash local))

let test_catch_up_and_diverge _ =
  let recs = records () in
  let commit, _ = sign_records recs in
  let ops =
    List.map
      (fun (r : Space_sync.record_block) ->
        Space_sync.op ~collection:r.Space_sync.collection
          ~rkey:r.Space_sync.rkey
          ~cid:(Cid.to_string r.Space_sync.cid)
          ())
      recs
  in
  let listed_full : Space_xrpc.listed_ops =
    {
      ops =
        List.map
          (fun (o : Space_sync.op) ->
            {
              Space_xrpc.rev = official_rev;
              collection = o.Space_sync.collection;
              rkey = o.Space_sync.rkey;
              cid = o.Space_sync.cid;
              prev = o.Space_sync.prev;
              value = None;
            })
          ops;
      commit = Some commit;
      cursor = None;
    }
  in
  (match Space_sync.apply_listed (Lt_hash.empty ()) listed_full with
  | Space_sync.Caught_up (h, c) ->
      OUnit2.assert_bool "matches" (Space_commit.matches h c)
  | _ -> OUnit2.assert_failure "expected Caught_up");
  let listed_partial =
    {
      listed_full with
      Space_xrpc.ops = List.filteri (fun i _ -> i < 2) listed_full.ops;
    }
  in
  (match Space_sync.apply_listed (Lt_hash.empty ()) listed_partial with
  | Space_sync.Diverged (h, c) ->
      OUnit2.assert_bool "diverged" (not (Space_commit.matches h c))
  | _ -> OUnit2.assert_failure "expected Diverged");
  let listed_no_commit = { listed_full with Space_xrpc.commit = None } in
  match Space_sync.apply_listed (Lt_hash.empty ()) listed_no_commit with
  | Space_sync.Partial _ -> ()
  | _ -> OUnit2.assert_failure "expected Partial"

let test_two_root_roundtrip _ =
  let recs = records () in
  let car, commit, key = car_for recs in
  let parsed = Car.parse car in
  OUnit2.assert_equal ~printer:string_of_int 2 (List.length parsed.Car.roots);
  OUnit2.assert_equal ~printer:string_of_int
    (2 + List.length recs)
    (List.length parsed.Car.blocks);
  OUnit2.assert_bool "commit block leads"
    (Cid.equal (List.nth parsed.Car.blocks 0).Car.cid
       (List.nth parsed.Car.roots 0));
  OUnit2.assert_bool "index follows"
    (Cid.equal (List.nth parsed.Car.blocks 1).Car.cid
       (List.nth parsed.Car.roots 1));
  let recovered = apply_ok car key in
  OUnit2.assert_equal official_rev recovered.Space_sync.commit.Space_commit.rev;
  OUnit2.assert_equal ~printer:string_of_int (List.length recs)
    (List.length recovered.Space_sync.index);
  OUnit2.assert_equal ~printer:string_of_int (List.length recs)
    (List.length recovered.Space_sync.records);
  OUnit2.assert_bool "set hash matches"
    (Space_commit.matches recovered.Space_sync.state recovered.Space_sync.commit);
  OUnit2.assert_equal commit.Space_commit.hash
    recovered.Space_sync.commit.Space_commit.hash;
  let like =
    List.find
      (fun (r : Space_sync.record_block) -> r.Space_sync.rkey = "3kbcq3p7ad403")
      recovered.Space_sync.records
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "at://x"
    (Yojson.Safe.Util.to_string
       (Yojson.Safe.Util.member "subject" like.Space_sync.value));
  let hello =
    List.find
      (fun (r : Space_sync.record_block) -> r.Space_sync.rkey = "3kbcq3p7ad401")
      recovered.Space_sync.records
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "hello"
    (Yojson.Safe.Util.to_string
       (Yojson.Safe.Util.member "text" hello.Space_sync.value));
  (* record blocks follow index order *)
  let index_cids =
    List.map
      (fun (e : Space_sync.index_entry) -> Cid.to_string e.Space_sync.cid)
      recovered.Space_sync.index
  in
  let record_cids =
    List.map
      (fun (r : Space_sync.record_block) -> Cid.to_string r.Space_sync.cid)
      recovered.Space_sync.records
  in
  OUnit2.assert_equal index_cids record_cids

let test_empty_repo _ =
  let car, _, key = car_for [] in
  let recovered = apply_ok car key in
  OUnit2.assert_equal 0 (List.length recovered.Space_sync.index);
  OUnit2.assert_equal 0 (List.length recovered.Space_sync.records);
  OUnit2.assert_bool "empty" (Lt_hash.is_empty recovered.Space_sync.state);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    empty_hash_hex
    (hex (Lt_hash.hash recovered.Space_sync.state))

let test_index_only_car _ =
  let recs = records () in
  let commit, key = sign_records recs in
  let car = Space_sync.encode ~commit ~records:recs ~exclude_values:true () in
  let recovered = apply_ok ~expect_values:false car key in
  OUnit2.assert_equal (List.length recs)
    (List.length recovered.Space_sync.index);
  OUnit2.assert_equal 0 (List.length recovered.Space_sync.records);
  OUnit2.assert_bool "index authenticates"
    (Space_commit.matches recovered.Space_sync.state commit)

let test_index_only_requires_flag _ =
  let recs = records () in
  let commit, key = sign_records recs in
  let car = Space_sync.encode ~commit ~records:recs ~exclude_values:true () in
  raises_invalid "missing" (fun () ->
      Space_sync.apply ~keys:[ key ] ~space:official_space
        ~author:official_author car)

let test_rejects_wrong_key _ =
  let car, _, _ = car_for (records ()) in
  let other =
    match
      Atproto.K256.K256.priv_of_octets
        (Hash.hex_decode (String.make 63 '0' ^ "3"))
    with
    | Error _ -> failwith "k256 priv rejected"
    | Ok priv ->
        Did_key.to_string
          (Did_key.of_k256_octets
             (Atproto.K256.K256.pub_to_octets ~compress:true
                (Atproto.K256.K256.pub_of_priv priv)))
  in
  raises_invalid "commit failed verification" (fun () ->
      Space_sync.apply ~keys:[ other ] ~space:official_space
        ~author:official_author car)

let test_rejects_wrong_space _ =
  let car, _, key = car_for (records ()) in
  raises_invalid "commit failed verification" (fun () ->
      Space_sync.apply ~keys:[ key ]
        ~space:"at://did:example:space/space/app.bsky.group/other"
        ~author:official_author car)

let test_rejects_wrong_author _ =
  let car, _, key = car_for (records ()) in
  raises_invalid "commit failed verification" (fun () ->
      Space_sync.apply ~keys:[ key ] ~space:official_space
        ~author:"did:example:bob" car)

let test_rejects_index_hash_mismatch _ =
  let recs = records () in
  let commit, key = sign_records (List.filteri (fun i _ -> i < 2) recs) in
  let car = Space_sync.encode ~commit ~records:recs () in
  raises_invalid "index does not match the commit hash" (fun () ->
      Space_sync.apply ~keys:[ key ] ~space:official_space
        ~author:official_author car)

let test_rejects_tampered_record_bytes _ =
  let recs = records () in
  let car, _, key = car_for recs in
  let parsed = Car.parse car in
  let blocks =
    match parsed.Car.blocks with
    | commit :: index :: first :: rest ->
        let bogus =
          Dag_cbor.encode (Dag_cbor.Map [ ("text", Dag_cbor.Text "tampered") ])
        in
        commit :: index :: { first with Car.data = bogus } :: rest
    | _ -> failwith "expected commit, index, records"
  in
  let tampered = Car.encode { parsed with Car.blocks } in
  raises_invalid "not a valid cid for bytes" (fun () ->
      Space_sync.apply ~keys:[ key ] ~space:official_space
        ~author:official_author tampered)

let test_rejects_missing_record _ =
  let recs = records () in
  let car, _, key = car_for recs in
  let parsed = Car.parse car in
  let kept =
    match List.rev parsed.Car.blocks with
    | _ :: rest -> List.rev rest
    | _ -> failwith "empty"
  in
  let truncated = Car.encode { parsed with Car.blocks = kept } in
  raises_invalid "missing 1 record" (fun () ->
      Space_sync.apply ~keys:[ key ] ~space:official_space
        ~author:official_author truncated)

let test_rejects_wrong_root_count _ =
  let recs = records () in
  let commit, key = sign_records recs in
  let data = Space_commit.encode commit in
  let cid = Cid.create ~codec:Cid.Dag_cbor data in
  let car =
    Car.encode { Car.roots = [ cid ]; blocks = [ { Car.cid; data } ] }
  in
  raises_invalid "expected 2 car roots" (fun () ->
      Space_sync.apply ~keys:[ key ] ~space:official_space
        ~author:official_author car)

let test_rejects_truncated_car _ =
  let car, _, _ = car_for (records ()) in
  match
    Space_sync.apply ~keys:[] ~space:official_space ~author:official_author
      (String.sub car 0 3)
  with
  | exception Space_sync.Invalid _ -> ()
  | _ -> OUnit2.assert_failure "expected truncated CAR to fail"

let test_rejects_invalid_index_cid _ =
  let recs = records () in
  let commit, key = sign_records recs in
  let commit_data = Space_commit.encode commit in
  let commit_cid = Cid.create ~codec:Cid.Dag_cbor commit_data in
  let index_data =
    Dag_cbor.encode
      (Dag_cbor.Map [ ("app.bsky.feed.post/1", Dag_cbor.Text "not-a-cid") ])
  in
  let index_cid = Cid.create ~codec:Cid.Dag_cbor index_data in
  let car =
    Car.encode
      {
        Car.roots = [ commit_cid; index_cid ];
        blocks =
          [
            { Car.cid = commit_cid; data = commit_data };
            { Car.cid = index_cid; data = index_data };
          ];
      }
  in
  raises_invalid "invalid repo index" (fun () ->
      Space_sync.apply ~keys:[ key ] ~space:official_space
        ~author:official_author car)

let test_index_roundtrip _ =
  let recs = records () in
  let entries =
    List.map entry_of_record recs
    |> List.sort
         (fun (a : Space_sync.index_entry) (b : Space_sync.index_entry) ->
           Space_sync.compare_index_key
             (Space_sync.path ~collection:a.Space_sync.collection
                ~rkey:a.Space_sync.rkey)
             (Space_sync.path ~collection:b.Space_sync.collection
                ~rkey:b.Space_sync.rkey))
  in
  let again = Space_sync.decode_index (Space_sync.encode_index entries) in
  OUnit2.assert_equal (List.length entries) (List.length again);
  List.iter2
    (fun (a : Space_sync.index_entry) (b : Space_sync.index_entry) ->
      OUnit2.assert_equal a.Space_sync.collection b.Space_sync.collection;
      OUnit2.assert_equal a.Space_sync.rkey b.Space_sync.rkey;
      OUnit2.assert_bool "cid" (Cid.equal a.Space_sync.cid b.Space_sync.cid))
    entries again

let suite =
  "space_sync"
  >::: [
         "test_path_and_index_order" >:: test_path_and_index_order;
         "test_apply_ops_create_update_delete"
         >:: test_apply_ops_create_update_delete;
         "test_catch_up_and_diverge" >:: test_catch_up_and_diverge;
         "test_two_root_roundtrip" >:: test_two_root_roundtrip;
         "test_empty_repo" >:: test_empty_repo;
         "test_index_only_car" >:: test_index_only_car;
         "test_index_only_requires_flag" >:: test_index_only_requires_flag;
         "test_rejects_wrong_key" >:: test_rejects_wrong_key;
         "test_rejects_wrong_space" >:: test_rejects_wrong_space;
         "test_rejects_wrong_author" >:: test_rejects_wrong_author;
         "test_rejects_index_hash_mismatch" >:: test_rejects_index_hash_mismatch;
         "test_rejects_tampered_record_bytes"
         >:: test_rejects_tampered_record_bytes;
         "test_rejects_missing_record" >:: test_rejects_missing_record;
         "test_rejects_wrong_root_count" >:: test_rejects_wrong_root_count;
         "test_rejects_truncated_car" >:: test_rejects_truncated_car;
         "test_rejects_invalid_index_cid" >:: test_rejects_invalid_index_cid;
         "test_index_roundtrip" >:: test_index_roundtrip;
       ]

let () = run_test_tt_main suite
