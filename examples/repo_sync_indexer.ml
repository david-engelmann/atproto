(* Offline sketch: wire a TAP-like indexer / backfill against Repo_sync.
   This is not a hosted Tap service. Official TestNetwork is a local
   PDS + AppView + Ozone stack, not a Tap host. No network is used here.

   Indexer path (library helpers only):

     1. Open / verify a repo CAR ([Repo_sync.open_car] /
        [verify_snapshot]). Production backfill uses
        [Repo_sync.fetch_repo] / [backfill] (com.atproto.sync.getRepo).
     2. Walk records ([walk] / [walk_json] / [record_json]).
     3. Check a getRecord inclusion proof ([export_record_proof] /
        [verify_record_proof]; live: [fetch_record_proof]).
     4. Apply firehose [#commit] ops ([process_commit] /
        [apply_commit_tree]) while [Synchronized].
     5. On [#sync] desync ([process_sync]), re-backfill
        ([resync_from_car] / [backfill]).
     6. Sync 1.1 export ([export_car] pre-order / [export_subset]).
     7. Offline fixture: [write_signed_repo]
        (JSON → DAG-CBOR → MST → commit → CAR).
*)

open Atproto.Repo_sync
open Atproto.Mst
open Atproto.Cid
open Atproto.Car
open Atproto.Firehose
open Atproto.Sync

let did = "did:plc:abc123xyz0001112223333"
let post_path = "app.bsky.feed.post/3jzfcijpj2z2a"
let like_path = "app.bsky.feed.like/3jzfcijpj2z2b"

let post_json =
  `Assoc
    [
      ("$type", `String "app.bsky.feed.post");
      ("text", `String "offline indexer fixture");
      ("createdAt", `String "2024-01-01T00:00:00.000Z");
    ]

let like_json =
  `Assoc
    [
      ("$type", `String "app.bsky.feed.like");
      ( "subject",
        `Assoc
          [
            ("uri", `String ("at://" ^ did ^ "/" ^ post_path));
            ("cid", `String "bafyreiexamplecid000000000000000000000000000");
          ] );
      ("createdAt", `String "2024-01-01T00:00:01.000Z");
    ]

let unsigned ~did ~data ~rev ?prev () =
  Mst.encode_repo_commit ~did ~data ~rev ?prev ()

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

let () =
  assert (
    Sync.record_path ~collection:"app.bsky.feed.post" ~rkey:"3jzfcijpj2z2a"
    = post_path);
  let snap =
    Repo_sync.write_signed_repo ~did ~rev:"3jzfcijpj2z2a" ~sign:unsigned
      ~records:[ (post_path, post_json); (like_path, like_json) ]
      ()
  in
  Repo_sync.verify_snapshot snap;
  assert (snap.did = did);
  assert (snap.rev = "3jzfcijpj2z2a");
  let walked = Repo_sync.walk snap in
  assert (List.length walked = 2);
  let json_walk = Repo_sync.walk_json snap in
  assert (List.length json_walk = 2);
  let rec_json =
    match List.find_opt (fun (path, _, _) -> path = post_path) json_walk with
    | Some (_, _, json) -> json
    | None -> failwith "walk_json missing post"
  in
  (match Yojson.Safe.Util.member "text" rec_json with
  | `String "offline indexer fixture" -> ()
  | _ -> assert false);
  (match Yojson.Safe.Util.member "$type" rec_json with
  | `String "app.bsky.feed.post" -> ()
  | _ -> assert false);
  let proof = Repo_sync.export_record_proof snap ~path:post_path in
  let cid, bytes = Repo_sync.verify_record_proof ~car:proof ~path:post_path in
  (match Yojson.Safe.Util.member "text" (Repo_sync.record_json bytes) with
  | `String "offline indexer fixture" -> ()
  | _ -> assert false);
  assert (
    match List.assoc_opt post_path walked with
    | Some expected -> Cid.equal cid expected
    | None -> false);
  ignore (Repo_sync.export_record_proof_bytes snap ~path:post_path);
  let exported = Repo_sync.export_car snap in
  assert (Repo_sync.is_preorder (Repo_sync.open_car exported));
  let subset =
    Repo_sync.export_subset snap ~collections:[ "app.bsky.feed.post" ]
  in
  let proven =
    Repo_sync.verify_subset ~collections:[ "app.bsky.feed.post" ] subset
  in
  assert (proven.did = did);
  let acct =
    Repo_sync.create_account ~did ~collections:[ "app.bsky.feed.post" ] ()
  in
  assert (acct.status = Repo_sync.Desynchronized);
  assert (Repo_sync.status_to_string acct.status = "desynchronized");
  let created = Repo_sync.resync_from_car ~live:false acct snap.car in
  assert (acct.status = Repo_sync.Synchronized);
  assert (Repo_sync.status_to_string acct.status = "synchronized");
  assert (List.length created = 1);
  (match List.hd created with
  | Repo_sync.Created { path; live; _ } ->
      assert (path = post_path);
      assert (not live)
  | _ -> assert false);
  let one =
    Repo_sync.write_signed_repo ~did ~rev:"3jzfcijpj2z2a" ~sign:unsigned
      ~records:[ (post_path, post_json) ]
      ()
  in
  let two =
    Repo_sync.write_signed_repo ~did ~rev:"3jzfcijpj2z2b" ~prev:one.commit_cid
      ~sign:unsigned
      ~records:[ (post_path, post_json); (like_path, like_json) ]
      ()
  in
  let like_cid =
    match List.assoc_opt like_path (Repo_sync.walk two) with
    | Some c -> c
    | None -> failwith "like cid"
  in
  let commit =
    firehose_commit ~prev_data:(Some one.data) ~car:two.car ~rev:"3jzfcijpj2z2b"
      ~ops:
        [
          {
            Firehose.action = "create";
            path = like_path;
            cid = Some like_cid;
            prev = None;
          };
        ]
  in
  let next = Repo_sync.apply_commit_tree one commit in
  assert (next.rev = "3jzfcijpj2z2b");
  assert (Cid.equal next.data two.data);
  let live_acct = Repo_sync.create_account ~did () in
  ignore (Repo_sync.resync_from_car live_acct one.car);
  let live_events = Repo_sync.process_commit ~live:true live_acct commit in
  (match live_events with
  | [ Repo_sync.Created { path; live; _ } ] ->
      assert (path = like_path);
      assert live
  | _ -> assert false);
  let sync =
    {
      Firehose.seq = 2L;
      did;
      blocks = two.car;
      raw_blocks = Car.encode two.car;
      rev = "3jzfcijpj2z2z";
      time = "2024-01-01T00:00:00.000Z";
    }
  in
  ignore (Repo_sync.process_sync live_acct sync);
  assert (live_acct.status = Repo_sync.Desynchronized);
  ignore
    (Repo_sync.process_message live_acct
       (`Sync { sync with rev = "3jzfcijpj2z2z" }));
  print_endline "repo_sync_indexer: TAP-like indexer / backfill library path ok"
