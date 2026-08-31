open OUnit2
open Atproto.Cid
open Atproto.Car
open Atproto.Mst
open Atproto.Firehose
open Atproto.Repo_sync
open Atproto.Did_key
open Atproto.Hash

let did = "did:plc:7iza6de2dwap2sbkpav7c6c6"

let car_of_tree ~did ~rev ?(records = []) tree =
  let mst_root = Mst.root_cid tree in
  let commit_bytes = Mst.encode_repo_commit ~did ~data:mst_root ~rev () in
  let commit_cid = Cid.create commit_bytes in
  let mst_blocks =
    Hashtbl.fold
      (fun k data acc -> { Car.cid = Cid.of_string k; data } :: acc)
      tree.store.created []
  in
  let record_blocks = List.map (fun (cid, data) -> { Car.cid; data }) records in
  {
    Car.roots = [ commit_cid ];
    blocks =
      ({ Car.cid = commit_cid; data = commit_bytes } :: mst_blocks)
      @ record_blocks;
  }

let firehose_commit ~prev_data ~car ~ops ~rev =
  let snap = Repo_sync.open_car car in
  {
    Firehose.seq = 1L;
    rebase = false;
    too_big = false;
    repo = did;
    commit = snap.commit_cid;
    rev;
    since = None;
    prev_data;
    blocks = car;
    raw_blocks = Car.encode car;
    ops;
    blobs = [];
    time = "2024-01-01T00:00:00.000Z";
  }

let test_open_car_and_walk _ =
  let store = Mst.store_of_get (fun _ -> None) in
  let t = Mst.empty_tree store in
  let va = Cid.create ~codec:Cid.Raw "rec-a" in
  let vb = Cid.create ~codec:Cid.Raw "rec-b" in
  let t, _ = Mst.insert t "app.bsky.feed.post/aaa" va in
  let t, _ = Mst.insert t "app.bsky.feed.like/bbb" vb in
  let car =
    car_of_tree ~did ~rev:"3jzfcijpj2z2a"
      ~records:[ (va, "rec-a"); (vb, "rec-b") ]
      t
  in
  let snap = Repo_sync.open_car car in
  OUnit2.assert_equal ~printer:(fun x -> x) did snap.did;
  OUnit2.assert_equal ~printer:(fun x -> x) "3jzfcijpj2z2a" snap.rev;
  Repo_sync.verify_snapshot snap;
  let walked = Repo_sync.walk snap in
  OUnit2.assert_equal 2 (List.length walked);
  OUnit2.assert_equal
    [ "app.bsky.feed.like/bbb"; "app.bsky.feed.post/aaa" ]
    (List.sort String.compare (List.map fst walked));
  let cid, bytes =
    Repo_sync.verify_record_proof ~car ~path:"app.bsky.feed.post/aaa"
  in
  OUnit2.assert_bool "proof cid" (Cid.equal cid va);
  OUnit2.assert_equal ~printer:(fun x -> x) "rec-a" bytes

let test_record_table_backfill_and_commit _ =
  let store = Mst.store_of_get (fun _ -> None) in
  let t = Mst.empty_tree store in
  let va = Cid.create ~codec:Cid.Raw "rec-a" in
  let vb = Cid.create ~codec:Cid.Raw "rec-b" in
  let t, _ = Mst.insert t "app.bsky.feed.post/aaa" va in
  let prev_data = Mst.root_cid t in
  let car1 = car_of_tree ~did ~rev:"3jzfcijpj2z2a" t in
  let acct = Repo_sync.create_account ~did () in
  OUnit2.assert_equal Repo_sync.Desynchronized acct.status;
  let created = Repo_sync.resync_from_car ~live:false acct car1 in
  OUnit2.assert_equal Repo_sync.Synchronized acct.status;
  OUnit2.assert_equal 1 (List.length created);
  (match List.hd created with
  | Repo_sync.Created { path; live; _ } ->
      OUnit2.assert_equal ~printer:(fun x -> x) "app.bsky.feed.post/aaa" path;
      OUnit2.assert_bool "historical" (not live)
  | _ -> OUnit2.assert_failure "expected create from backfill");
  let t, _ = Mst.insert t "app.bsky.feed.post/bbb" vb in
  let car2 = car_of_tree ~did ~rev:"3jzfcijpj2z2b" t in
  let commit =
    firehose_commit ~prev_data:(Some prev_data) ~car:car2 ~rev:"3jzfcijpj2z2b"
      ~ops:
        [
          {
            Firehose.action = "create";
            path = "app.bsky.feed.post/bbb";
            cid = Some vb;
            prev = None;
          };
        ]
  in
  let events = Repo_sync.process_commit ~live:true acct commit in
  OUnit2.assert_equal 1 (List.length events);
  (match List.hd events with
  | Repo_sync.Created { path; live; _ } ->
      OUnit2.assert_equal ~printer:(fun x -> x) "app.bsky.feed.post/bbb" path;
      OUnit2.assert_bool "live" live
  | _ -> OUnit2.assert_failure "expected live create");
  OUnit2.assert_equal ~printer:(fun x -> x) "3jzfcijpj2z2b" acct.rev;
  OUnit2.assert_equal 2 (Hashtbl.length acct.records)

let test_broken_chain_and_sync _ =
  let store = Mst.store_of_get (fun _ -> None) in
  let t = Mst.empty_tree store in
  let va = Cid.create ~codec:Cid.Raw "rec-a" in
  let t, _ = Mst.insert t "app.bsky.feed.post/aaa" va in
  let car = car_of_tree ~did ~rev:"3jzfcijpj2z2a" t in
  let acct = Repo_sync.create_account ~did () in
  ignore (Repo_sync.resync_from_car acct car);
  let other = Cid.create ~codec:Cid.Raw "other-root" in
  let commit =
    firehose_commit ~prev_data:(Some other) ~car ~rev:"3jzfcijpj2z2c"
      ~ops:
        [
          {
            Firehose.action = "create";
            path = "app.bsky.feed.post/zzz";
            cid = Some va;
            prev = None;
          };
        ]
  in
  (* inversion will fail (wrong op / prevData); treat as broken *)
  OUnit2.assert_bool "broken invert accepted"
    (try
       ignore (Repo_sync.process_commit acct commit);
       acct.status = Repo_sync.Desynchronized
     with Repo_sync.Error _ | Failure _ | Mst.Verify_error _ -> true);
  let sync =
    {
      Firehose.seq = 2L;
      did;
      blocks = car;
      raw_blocks = Car.encode car;
      rev = "3jzfcijpj2z2z";
      time = "2024-01-01T00:00:00.000Z";
    }
  in
  ignore (Repo_sync.process_sync acct sync);
  OUnit2.assert_equal Repo_sync.Desynchronized acct.status;
  let ignored =
    Repo_sync.process_commit acct
      (firehose_commit ~prev_data:None ~car ~rev:"3jzfcijpj2z2d" ~ops:[])
  in
  OUnit2.assert_equal [] ignored

let test_collection_filter_and_delete _ =
  let store = Mst.store_of_get (fun _ -> None) in
  let t = Mst.empty_tree store in
  let va = Cid.create ~codec:Cid.Raw "rec-a" in
  let vb = Cid.create ~codec:Cid.Raw "rec-b" in
  let t, _ = Mst.insert t "app.bsky.feed.post/aaa" va in
  let t, _ = Mst.insert t "app.bsky.feed.like/bbb" vb in
  let car = car_of_tree ~did ~rev:"3jzfcijpj2z2a" t in
  let acct =
    Repo_sync.create_account ~did ~collections:[ "app.bsky.feed.post" ] ()
  in
  let events = Repo_sync.resync_from_car acct car in
  OUnit2.assert_equal 1 (List.length events);
  OUnit2.assert_equal 1 (Hashtbl.length acct.records);
  let t, _ = Mst.remove t "app.bsky.feed.post/aaa" in
  let car2 = car_of_tree ~did ~rev:"3jzfcijpj2z2b" t in
  let replay = Repo_sync.resync_from_car ~live:false acct car2 in
  match replay with
  | [ Repo_sync.Deleted { path; _ } ] ->
      OUnit2.assert_equal ~printer:(fun x -> x) "app.bsky.feed.post/aaa" path
  | _ -> OUnit2.assert_failure "expected delete of filtered record"

let test_apply_commit_tree _ =
  let store = Mst.store_of_get (fun _ -> None) in
  let t = Mst.empty_tree store in
  let va = Cid.create ~codec:Cid.Raw "rec-a" in
  let vb = Cid.create ~codec:Cid.Raw "rec-b" in
  let t, _ = Mst.insert t "app.bsky.feed.post/aaa" va in
  let prev = Repo_sync.open_car (car_of_tree ~did ~rev:"3jzfcijpj2z2a" t) in
  let t, _ = Mst.insert t "app.bsky.feed.post/bbb" vb in
  let car2 = car_of_tree ~did ~rev:"3jzfcijpj2z2b" t in
  let commit =
    firehose_commit ~prev_data:(Some prev.data) ~car:car2 ~rev:"3jzfcijpj2z2b"
      ~ops:
        [
          {
            Firehose.action = "create";
            path = "app.bsky.feed.post/bbb";
            cid = Some vb;
            prev = None;
          };
        ]
  in
  let next = Repo_sync.apply_commit_tree prev commit in
  OUnit2.assert_equal ~printer:(fun x -> x) "3jzfcijpj2z2b" next.rev;
  (match Mst.get next.tree "app.bsky.feed.post/bbb" with
  | Some c -> OUnit2.assert_bool "forward apply" (Cid.equal c vb)
  | None -> OUnit2.assert_failure "forward apply missing bbb");
  let tree = Firehose.apply_commit ~prev_tree:prev.tree commit in
  OUnit2.assert_bool "firehose apply_commit"
    (Cid.equal (Mst.root_cid tree) next.data)

let rfc6979_p256_priv =
  Hash.hex_decode
    "c9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721"

let test_signed_snapshot _ =
  match Mirage_crypto_ec.P256.Dsa.priv_of_octets rfc6979_p256_priv with
  | Error _ -> OUnit2.assert_failure "p256 priv"
  | Ok priv ->
      let pub = Mirage_crypto_ec.P256.Dsa.pub_of_priv priv in
      let key =
        Did_key.to_string
          (Did_key.of_p256_octets
             (Mirage_crypto_ec.P256.Dsa.pub_to_octets ~compress:true pub))
      in
      let store = Mst.store_of_get (fun _ -> None) in
      let t = Mst.empty_tree store in
      let va = Cid.create ~codec:Cid.Raw "rec-a" in
      let t, _ = Mst.insert t "app.bsky.feed.post/aaa" va in
      let data = Mst.root_cid t in
      let commit_bytes =
        Mst.sign_p256 ~priv ~did ~data ~rev:"3jzfcijpj2z2a" ()
      in
      let commit_cid = Cid.create commit_bytes in
      let mst_blocks =
        Hashtbl.fold
          (fun k d acc -> { Car.cid = Cid.of_string k; data = d } :: acc)
          t.store.created []
      in
      let car =
        {
          Car.roots = [ commit_cid ];
          blocks = { Car.cid = commit_cid; data = commit_bytes } :: mst_blocks;
        }
      in
      let snap = Repo_sync.open_car car in
      Repo_sync.verify_snapshot ~keys:[ key ] snap;
      OUnit2.assert_equal `Valid
        (Mst.verify_commit_sig ~keys:[ key ] snap.commit)

let test_split_path _ =
  OUnit2.assert_equal
    ("app.bsky.feed.post", "3jzfcijpj2z2a")
    (Repo_sync.split_path "app.bsky.feed.post/3jzfcijpj2z2a");
  OUnit2.assert_equal ("nocollection", "") (Repo_sync.split_path "nocollection")

let suite =
  "repo_sync"
  >::: [
         "test_open_car_and_walk" >:: test_open_car_and_walk;
         "test_record_table_backfill_and_commit"
         >:: test_record_table_backfill_and_commit;
         "test_broken_chain_and_sync" >:: test_broken_chain_and_sync;
         "test_collection_filter_and_delete"
         >:: test_collection_filter_and_delete;
         "test_apply_commit_tree" >:: test_apply_commit_tree;
         "test_signed_snapshot" >:: test_signed_snapshot;
         "test_split_path" >:: test_split_path;
       ]

let () = run_test_tt_main suite
