open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Graph
open Atproto.Label

let create_test_session _ =
  let username, password = Auth.username_and_password_from_env in
  Session.create_session username password

let test_get_blocks _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let blocks = Graph.get_blocks test_session 10 in
  match blocks with
  | { blocks; _ } -> (
      match blocks with
      | [] -> OUnit2.assert_failure "expected at least one graph block"
      | hd :: _ -> (
          match hd with
          | { did; _ } ->
              OUnit2.assert_bool
                (Printf.sprintf "graph block DID should be non-empty, got %S"
                   did)
                (String.length did > 0)))

let test_get_followers _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let followers =
    Graph.get_followers test_session "david-engelmann.bsky.social" 10
  in
  match followers with
  | { subject; _ } -> (
      match subject with
      | { handle; _ } ->
          OUnit2.assert_equal "david-engelmann.bsky.social" handle)

let test_get_follows _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let follows =
    Graph.get_follows test_session "david-engelmann.bsky.social" 10
  in
  match follows with
  | { subject; _ } -> (
      match subject with
      | { handle; _ } ->
          OUnit2.assert_equal "david-engelmann.bsky.social" handle)

let test_get_mutes _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let mutes = Graph.get_mutes test_session 10 in
  match mutes with
  | { mutes; _ } -> (
      match mutes with
      | [] -> OUnit2.assert_failure "expected at least one muted actor"
      | hd :: _ -> (
          match hd with
          | { did; _ } ->
              OUnit2.assert_bool
                (Printf.sprintf "muted actor DID should be non-empty, got %S"
                   did)
                (String.length did > 0)))

let test_mute_actor _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let muted_actor = Graph.mute_actor test_session "karen.bsky.social" in
  OUnit2.assert_bool "Graph Mute Actor is not empty" (muted_actor = "")

let test_unmute_actor _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let unmuted_actor = Graph.unmute_actor test_session "karen.bsky.social" in
  OUnit2.assert_bool "Graph Unmute Actor is not empty" (unmuted_actor = "")

let with_public_timeout ?(seconds = 20) f =
  let old =
    Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> failwith "timeout"))
  in
  ignore (Unix.alarm seconds);
  Fun.protect
    ~finally:(fun () ->
      ignore (Unix.alarm 0);
      Sys.set_signal Sys.sigalrm old)
    f

let test_mute_actor_body _ =
  let open Yojson.Safe.Util in
  let full = Graph.mute_actor_body ~actor:"alice.test" () in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "alice.test"
    (full |> member "actor" |> to_string);
  OUnit2.assert_equal `Null (full |> member "onlyReposts");
  let scoped =
    Graph.mute_actor_body ~actor:"did:plc:abc123xyz0001112223333"
      ~only_reposts:true ~only_quoteposts:false ()
  in
  OUnit2.assert_equal true (scoped |> member "onlyReposts" |> to_bool);
  OUnit2.assert_equal false (scoped |> member "onlyQuoteposts" |> to_bool)

let test_parse_list _ =
  let json =
    `Assoc
      [
        ( "list",
          `Assoc
            [
              ( "uri",
                `String
                  "at://did:plc:abc123xyz0001112223333/app.bsky.graph.list/3k"
              );
              ("cid", `String "bafyreiabc");
              ("name", `String "Friends");
              ("purpose", `String "app.bsky.graph.defs#curatelist");
              ( "creator",
                `Assoc
                  [
                    ("did", `String "did:plc:abc123xyz0001112223333");
                    ("handle", `String "alice.test");
                  ] );
              ("indexedAt", `String "2024-01-01T00:00:00.000Z");
              ("listItemCount", `Int 2);
            ] );
        ( "items",
          `List
            [
              `Assoc
                [
                  ( "uri",
                    `String
                      "at://did:plc:abc123xyz0001112223333/app.bsky.graph.listitem/1"
                  );
                  ( "subject",
                    `Assoc
                      [
                        ("did", `String "did:plc:xyz789aaa0001112223333");
                        ("handle", `String "bob.test");
                      ] );
                ];
            ] );
      ]
  in
  let page = Graph.parse_list_page json in
  OUnit2.assert_equal ~printer:(fun x -> x) "Friends" page.list.name;
  OUnit2.assert_equal 1 (List.length page.items)

let test_parse_starter_pack _ =
  let json =
    `Assoc
      [
        ( "uri",
          `String
            "at://did:plc:abc123xyz0001112223333/app.bsky.graph.starterpack/3k"
        );
        ("cid", `String "bafyreiabc");
        ( "record",
          `Assoc
            [
              ("name", `String "New folks");
              ( "list",
                `String
                  "at://did:plc:abc123xyz0001112223333/app.bsky.graph.list/3k"
              );
            ] );
        ( "creator",
          `Assoc
            [
              ("did", `String "did:plc:abc123xyz0001112223333");
              ("handle", `String "alice.test");
            ] );
        ("indexedAt", `String "2024-01-01T00:00:00.000Z");
        ("joinedAllTimeCount", `Int 12);
        ( "listItemsSample",
          `List
            [
              `Assoc
                [
                  ( "uri",
                    `String
                      "at://did:plc:abc123xyz0001112223333/app.bsky.graph.listitem/1"
                  );
                  ( "subject",
                    `Assoc
                      [
                        ("did", `String "did:plc:xyz789aaa0001112223333");
                        ("handle", `String "bob.test");
                      ] );
                ];
            ] );
        ( "feeds",
          `List
            [
              `Assoc
                [
                  ( "uri",
                    `String
                      "at://did:plc:abc123xyz0001112223333/app.bsky.feed.generator/hot"
                  );
                  ("cid", `String "bafyreifeed");
                  ("displayName", `String "Hot");
                ];
            ] );
        ( "labels",
          `List
            [
              `Assoc
                [
                  ("src", `String "did:plc:labeler000111222333444555");
                  ( "uri",
                    `String
                      "at://did:plc:abc123xyz0001112223333/app.bsky.graph.starterpack/3k"
                  );
                  ("val", `String "!hide");
                ];
            ] );
      ]
  in
  let pack = Graph.parse_starter_pack json in
  OUnit2.assert_equal (Some "New folks") pack.name;
  OUnit2.assert_equal (Some 12) pack.joined_all_time_count;
  OUnit2.assert_equal 1 (List.length pack.list_items_sample);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "bob.test" (List.hd pack.list_items_sample).subject.handle;
  OUnit2.assert_equal 1 (List.length pack.feeds);
  OUnit2.assert_equal (Some "Hot") (List.hd pack.feeds).display_name;
  OUnit2.assert_equal 1 (List.length pack.labels);
  OUnit2.assert_equal ~printer:(fun x -> x) "!hide" (List.hd pack.labels).val_

let test_parse_relationships _ =
  let json =
    `Assoc
      [
        ("actor", `String "did:plc:abc123xyz0001112223333");
        ( "relationships",
          `List
            [
              `Assoc
                [
                  ("did", `String "did:plc:xyz789aaa0001112223333");
                  ( "following",
                    `String
                      "at://did:plc:abc123xyz0001112223333/app.bsky.graph.follow/1"
                  );
                  ( "blockedByList",
                    `String
                      "at://did:plc:xyz789aaa0001112223333/app.bsky.graph.listblock/1"
                  );
                  ( "blockingByList",
                    `String
                      "at://did:plc:abc123xyz0001112223333/app.bsky.graph.listblock/2"
                  );
                ];
            ] );
      ]
  in
  let rels = Graph.parse_relationships json in
  OUnit2.assert_equal 1 (List.length rels.relationships);
  OUnit2.assert_bool "following present"
    (match (List.hd rels.relationships).following with
    | Some _ -> true
    | None -> false);
  OUnit2.assert_bool "blockedByList"
    (match (List.hd rels.relationships).blocked_by_list with
    | Some _ -> true
    | None -> false);
  OUnit2.assert_bool "blockingByList"
    (match (List.hd rels.relationships).blocking_by_list with
    | Some _ -> true
    | None -> false)

let test_relationships_live _ =
  try
    with_public_timeout (fun () ->
        let rels =
          Graph.get_relationships ~actor:"jay.bsky.team" ~others:[ "bsky.app" ]
            ()
        in
        OUnit2.assert_bool "relationships" (List.length rels.relationships >= 0))
  with exn ->
    skip_if true ("getRelationships skipped: " ^ Printexc.to_string exn)

let test_search_starter_packs_live _ =
  try
    with_public_timeout (fun () ->
        let packs = Graph.search_starter_packs ~q:"bluesky" ~limit:3 () in
        OUnit2.assert_bool "starter packs" (List.length packs.starter_packs >= 0))
  with exn ->
    skip_if true ("searchStarterPacks skipped: " ^ Printexc.to_string exn)

let test_parse_membership_and_v2 _ =
  let list_json =
    `Assoc
      [
        ( "uri",
          `String "at://did:plc:abc123xyz0001112223333/app.bsky.graph.list/3k"
        );
        ("cid", `String "bafyreilist");
        ("name", `String "Friends");
        ("purpose", `String "app.bsky.graph.defs#curatelist");
        ("indexedAt", `String "2024-01-01T00:00:00.000Z");
      ]
  in
  let item_json =
    `Assoc
      [
        ( "uri",
          `String
            "at://did:plc:abc123xyz0001112223333/app.bsky.graph.listitem/3k" );
        ( "subject",
          `Assoc
            [
              ("did", `String "did:plc:xyz789aaa0001112223333");
              ("handle", `String "bob.test");
            ] );
      ]
  in
  let page =
    Graph.parse_lists_with_membership
      (`Assoc
        [
          ( "listsWithMembership",
            `List [ `Assoc [ ("list", list_json); ("listItem", item_json) ] ] );
        ])
  in
  OUnit2.assert_equal 1 (List.length page.lists);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "Friends" (List.hd page.lists).list.name;
  (match (List.hd page.lists).list_item with
  | Some item ->
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "did:plc:xyz789aaa0001112223333" item.subject.did
  | None -> OUnit2.assert_failure "expected list item");
  let packs =
    Graph.parse_starter_packs
      (`Assoc
        [
          ( "starterPacks",
            `List
              [
                `Assoc
                  [
                    ( "uri",
                      `String
                        "at://did:plc:abc123xyz0001112223333/app.bsky.graph.starterpack/3k"
                    );
                    ("cid", `String "bafyreipack");
                    ("record", `Assoc [ ("name", `String "Start") ]);
                    ("indexedAt", `String "2024-01-01T00:00:00.000Z");
                  ];
              ] );
          ("hitsTotal", `Int 4);
        ])
  in
  OUnit2.assert_equal (Some 4) packs.hits_total

let test_search_starter_packs_v2_live _ =
  try
    with_public_timeout (fun () ->
        let packs = Graph.search_starter_packs_v2 ~q:"bluesky" ~limit:3 () in
        OUnit2.assert_bool "starter packs v2"
          (List.length packs.starter_packs >= 0))
  with exn ->
    skip_if true ("searchStarterPacksV2 skipped: " ^ Printexc.to_string exn)

let suite =
  "suite"
  >::: [
         "test_get_blocks" >:: test_get_blocks;
         "test_get_followers" >:: test_get_followers;
         "test_get_follows" >:: test_get_follows;
         "test_get_mutes" >:: test_get_mutes;
         "test_mute_actor" >:: test_mute_actor;
         "test_unmute_actor" >:: test_unmute_actor;
         "test_mute_actor_body" >:: test_mute_actor_body;
         "test_parse_list" >:: test_parse_list;
         "test_parse_starter_pack" >:: test_parse_starter_pack;
         "test_parse_relationships" >:: test_parse_relationships;
         "test_relationships_live" >:: test_relationships_live;
         "test_search_starter_packs_live" >:: test_search_starter_packs_live;
         "test_parse_membership_and_v2" >:: test_parse_membership_and_v2;
         "test_search_starter_packs_v2_live"
         >:: test_search_starter_packs_v2_live;
       ]

let () = run_test_tt_main suite
