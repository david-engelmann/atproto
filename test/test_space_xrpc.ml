open OUnit2
open Atproto.Space_xrpc
open Atproto.Space_commit
open Atproto.Space_credential
open Atproto.Base64url

(* Draft lexicon fixtures from proposal 0016 / bluesky-social/atproto#5187. *)
let space_uri = "at://did:example:space/space/app.bsky.group/test"
let repo_did = "did:example:alice"
let collection = "app.bsky.feed.post"
let rkey = "3jzfcijpj2z2a"
let cid = "bafyreia3zx2c7x6s7q5x5q5x5q5x5q5x5q5x5q5x5q5x5q"

let record_json =
  `Assoc
    [
      ("$type", `String collection);
      ("text", `String "hi");
      ("createdAt", `String "2024-01-01T00:00:00.000Z");
    ]

let digest32 = String.make 32 (Char.chr 7)

let json_string json field =
  match Yojson.Safe.Util.member field json with
  | `String s -> s
  | _ -> failwith ("missing string " ^ field)

let json_bool_opt json field =
  match Yojson.Safe.Util.member field json with `Bool b -> Some b | _ -> None

let test_nsids _ =
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "com.atproto.space." Space_xrpc.nsid_prefix;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "com.atproto.space.getDelegationToken" Space_xrpc.get_delegation_token_nsid;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    Space_credential.get_space_credential_nsid
    Space_xrpc.get_space_credential_nsid;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "com.atproto.space.getRecord" Space_xrpc.get_record_nsid;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "com.atproto.space.listRepoOps" Space_xrpc.list_repo_ops_nsid;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "com.atproto.space.applyWrites" Space_xrpc.apply_writes_nsid;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "com.atproto.space.notifyWrite" Space_xrpc.notify_write_nsid;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "com.atproto.space.getRecord"
    (Space_xrpc.create_space_endpoint "getRecord")

let test_no_default_host _ =
  OUnit2.assert_bool "no invented space host"
    (Space_xrpc.space_host_from_env = None);
  OUnit2.assert_bool "live hops off by default" (not Space_xrpc.live_enabled)

let test_query_bodies _ =
  OUnit2.assert_equal
    [ ("space", space_uri) ]
    (Space_xrpc.get_delegation_token_body ~space:space_uri);
  OUnit2.assert_equal
    [
      ("space", space_uri);
      ("repo", repo_did);
      ("collection", collection);
      ("rkey", rkey);
    ]
    (Space_xrpc.get_record_body ~space:space_uri ~repo:repo_did ~collection
       ~rkey);
  OUnit2.assert_equal
    [ ("space", space_uri); ("repo", repo_did) ]
    (Space_xrpc.get_latest_commit_body ~space:space_uri ~repo:repo_did);
  let listed =
    Space_xrpc.list_records_body ~space:space_uri ~repo:repo_did ~collection
      ~limit:10 ~cursor:"c1" ~reverse:true ~exclude_values:true ()
  in
  OUnit2.assert_equal (Some "true") (List.assoc_opt "excludeValues" listed);
  OUnit2.assert_equal (Some "10") (List.assoc_opt "limit" listed);
  OUnit2.assert_equal (Some collection) (List.assoc_opt "collection" listed);
  let ops =
    Space_xrpc.list_repo_ops_body ~space:space_uri ~repo:repo_did
      ~since:"3jzfcijpj2z2a" ~exclude_values:true ()
  in
  OUnit2.assert_equal (Some "3jzfcijpj2z2a") (List.assoc_opt "since" ops);
  let blobs =
    Space_xrpc.list_blobs_body ~space:space_uri ~repo:repo_did
      ~since:"3jzfcijpj2z2a" ~limit:20 ()
  in
  OUnit2.assert_equal (Some "20") (List.assoc_opt "limit" blobs);
  OUnit2.assert_equal
    [ ("space", space_uri); ("repo", repo_did); ("cid", cid) ]
    (Space_xrpc.get_blob_body ~space:space_uri ~repo:repo_did ~cid);
  let repo_pairs =
    Space_xrpc.get_repo_body ~space:space_uri ~repo:repo_did
      ~exclude_values:true ()
  in
  OUnit2.assert_equal (Some "true") (List.assoc_opt "excludeValues" repo_pairs);
  let repos =
    Space_xrpc.list_repos_body ~space:space_uri ~limit:5 ~cursor:"n" ()
  in
  OUnit2.assert_equal (Some "5") (List.assoc_opt "limit" repos);
  let spaces =
    Space_xrpc.list_spaces_body ~space_type:"app.bsky.group"
      ~did:"did:example:space" ~limit:2 ()
  in
  OUnit2.assert_equal (Some "app.bsky.group") (List.assoc_opt "type" spaces);
  OUnit2.assert_equal (Some "did:example:space") (List.assoc_opt "did" spaces)

let test_write_bodies _ =
  let created =
    Space_xrpc.create_record_body ~space:space_uri ~repo:repo_did ~collection
      ~rkey ~validate:true record_json
  in
  OUnit2.assert_equal space_uri (json_string created "space");
  OUnit2.assert_equal repo_did (json_string created "repo");
  OUnit2.assert_equal collection (json_string created "collection");
  OUnit2.assert_equal rkey (json_string created "rkey");
  OUnit2.assert_equal (Some true) (json_bool_opt created "validate");
  OUnit2.assert_equal "hi"
    (json_string (Yojson.Safe.Util.member "record" created) "text");
  let put =
    Space_xrpc.put_record_body ~space:space_uri ~repo:repo_did ~collection ~rkey
      record_json
  in
  OUnit2.assert_equal rkey (json_string put "rkey");
  OUnit2.assert_equal None (json_bool_opt put "validate");
  let deleted =
    Space_xrpc.delete_record_body ~space:space_uri ~repo:repo_did ~collection
      ~rkey
  in
  OUnit2.assert_equal rkey (json_string deleted "rkey");
  let writes =
    [
      Space_xrpc.Create { collection; rkey = Some rkey; value = record_json };
      Space_xrpc.Update { collection; rkey; value = record_json };
      Space_xrpc.Delete { collection; rkey };
    ]
  in
  let batch =
    Space_xrpc.apply_writes_body ~space:space_uri ~repo:repo_did ~writes
      ~validate:false ()
  in
  OUnit2.assert_equal (Some false) (json_bool_opt batch "validate");
  match Yojson.Safe.Util.member "writes" batch with
  | `List [ c; u; d ] ->
      OUnit2.assert_equal "com.atproto.space.applyWrites#create"
        (json_string c "$type");
      OUnit2.assert_equal "com.atproto.space.applyWrites#update"
        (json_string u "$type");
      OUnit2.assert_equal "com.atproto.space.applyWrites#delete"
        (json_string d "$type")
  | _ -> OUnit2.assert_failure "applyWrites writes must be a 3-op list"

let test_notify_bodies _ =
  let reg =
    Space_xrpc.register_notify_body ~space:space_uri
      ~service:"did:web:syncer.example.com#atproto_space_syncer"
  in
  OUnit2.assert_equal space_uri (json_string reg "space");
  OUnit2.assert_equal "did:web:syncer.example.com#atproto_space_syncer"
    (json_string reg "service");
  let unreg =
    Space_xrpc.unregister_notify_body ~space:space_uri
      ~service:"did:web:syncer.example.com#atproto_space_syncer"
  in
  OUnit2.assert_equal space_uri (json_string unreg "space");
  let nw =
    Space_xrpc.notify_write_body ~space:space_uri ~repo:repo_did ~rev:rkey
      ~hash:digest32
  in
  OUnit2.assert_equal rkey (json_string nw "rev");
  (match Yojson.Safe.Util.member "hash" nw with
  | `Assoc [ ("$bytes", `String b64) ] ->
      OUnit2.assert_equal digest32 (Base64url.decode b64)
  | _ -> OUnit2.assert_failure "notifyWrite hash must be $bytes");
  let deleted = Space_xrpc.notify_space_deleted_body ~space:space_uri in
  OUnit2.assert_equal space_uri (json_string deleted "space")

let test_rejects_record_uri _ =
  let record =
    space_uri ^ "/did:example:alice/app.bsky.feed.post/3jzfcijpj2z2a"
  in
  OUnit2.assert_raises
    (Space_xrpc.Invalid
       "getRecord space must be a space URI (through skey), not a record URI")
    (fun () ->
      ignore
        (Space_xrpc.get_record_body ~space:record ~repo:repo_did ~collection
           ~rkey))

let test_htu _ =
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "https://pds.example.com/xrpc/com.atproto.space.getRepo"
    (Space_xrpc.xrpc_htu ~host:"pds.example.com" Space_xrpc.get_repo_nsid);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    (Space_credential.get_space_credential_htu
       ~origin:"https://space.example.com")
    (Space_xrpc.xrpc_htu ~host:"space.example.com"
       Space_xrpc.get_space_credential_nsid)

let test_parse_record_and_list _ =
  let rec_json =
    `Assoc
      [
        ( "uri",
          `String (space_uri ^ "/" ^ repo_did ^ "/" ^ collection ^ "/" ^ rkey)
        );
        ("cid", `String cid);
        ("value", record_json);
      ]
  in
  let got = Space_xrpc.parse_record rec_json in
  OUnit2.assert_equal cid got.cid;
  OUnit2.assert_equal "hi" (json_string got.value "text");
  let listed =
    Space_xrpc.parse_listed_records
      (`Assoc
        [
          ("cursor", `String "next");
          ( "records",
            `List
              [
                `Assoc
                  [
                    ("collection", `String collection);
                    ("rkey", `String rkey);
                    ("cid", `String cid);
                  ];
              ] );
        ])
  in
  OUnit2.assert_equal (Some "next") listed.cursor;
  OUnit2.assert_equal 1 (List.length listed.records);
  OUnit2.assert_equal None (List.hd listed.records).value

let test_parse_ops_and_commit _ =
  let hash_json =
    `Assoc [ ("$bytes", `String (Base64url.encode_std digest32)) ]
  in
  let commit_json =
    `Assoc
      [
        ("ver", `Int 1);
        ("hash", hash_json);
        ("ikm", hash_json);
        ("sig", hash_json);
        ("mac", hash_json);
        ("rev", `String rkey);
      ]
  in
  let commit = Space_xrpc.parse_signed_commit commit_json in
  OUnit2.assert_equal 1 commit.Space_commit.ver;
  OUnit2.assert_equal digest32 commit.Space_commit.hash;
  OUnit2.assert_equal rkey commit.Space_commit.rev;
  let round = Space_xrpc.signed_commit_to_json commit in
  let again = Space_xrpc.parse_signed_commit round in
  OUnit2.assert_equal commit.Space_commit.mac again.Space_commit.mac;
  let ops =
    Space_xrpc.parse_listed_ops
      (`Assoc
        [
          ( "ops",
            `List
              [
                `Assoc
                  [
                    ("rev", `String rkey);
                    ("collection", `String collection);
                    ("rkey", `String rkey);
                    ("cid", `String cid);
                    ("prev", `Null);
                    ("value", record_json);
                  ];
                `Assoc
                  [
                    ("rev", `String rkey);
                    ("collection", `String collection);
                    ("rkey", `String "gone");
                    ("cid", `Null);
                    ("prev", `String cid);
                  ];
              ] );
          ("commit", commit_json);
        ])
  in
  OUnit2.assert_equal 2 (List.length ops.ops);
  OUnit2.assert_equal (Some cid) (List.hd ops.ops).cid;
  OUnit2.assert_equal None (List.nth ops.ops 1).cid;
  OUnit2.assert_bool "commit present" (Option.is_some ops.commit);
  let repos =
    Space_xrpc.parse_listed_repos
      (`Assoc
        [
          ( "repos",
            `List
              [
                `Assoc
                  [
                    ("did", `String repo_did);
                    ("rev", `String rkey);
                    ("hash", hash_json);
                  ];
              ] );
        ])
  in
  OUnit2.assert_equal digest32 (List.hd repos.repos).hash;
  let spaces =
    Space_xrpc.parse_listed_spaces
      (`Assoc [ ("spaces", `List [ `Assoc [ ("uri", `String space_uri) ] ]) ])
  in
  OUnit2.assert_equal space_uri (List.hd spaces.spaces).uri;
  let write =
    Space_xrpc.parse_write_result
      (`Assoc
        [
          ("uri", `String "at://x");
          ("cid", `String cid);
          ("validationStatus", `String "valid");
        ])
  in
  OUnit2.assert_equal (Some "valid") write.validation_status

let test_credential_bodies_shared _ =
  let body = Space_xrpc.get_space_credential_body ~space:space_uri () in
  OUnit2.assert_equal space_uri (json_string body "space");
  OUnit2.assert_equal
    (Space_credential.get_space_credential_body ~space:space_uri ())
    body

let test_live_skips_without_host _ =
  skip_if
    (not Space_xrpc.live_enabled)
    "ATP_SPACE / ATP_SPACE_HOST not set; no space host (not faked)";
  (* Reaching here means the operator pointed at a real host. A missing
     draft NSID still fails rather than inventing a product. *)
  match Space_xrpc.space_host_from_env with
  | None -> OUnit2.assert_failure "live_enabled requires ATP_SPACE_HOST"
  | Some host ->
      OUnit2.assert_bool "space host is non-empty" (String.trim host <> "")

let suite =
  "space"
  >::: [
         "test_nsids" >:: test_nsids;
         "test_no_default_host" >:: test_no_default_host;
         "test_query_bodies" >:: test_query_bodies;
         "test_write_bodies" >:: test_write_bodies;
         "test_notify_bodies" >:: test_notify_bodies;
         "test_rejects_record_uri" >:: test_rejects_record_uri;
         "test_htu" >:: test_htu;
         "test_parse_record_and_list" >:: test_parse_record_and_list;
         "test_parse_ops_and_commit" >:: test_parse_ops_and_commit;
         "test_credential_bodies_shared" >:: test_credential_bodies_shared;
         "test_live_skips_without_host" >:: test_live_skips_without_host;
       ]

let () = run_test_tt_main suite
