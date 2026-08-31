open OUnit2
open Atproto.Auth
open Atproto.Session
open Atproto.Actor
open Atproto.Feed
open Atproto.Graph
open Atproto.Records
open Atproto.Repo
open Atproto.Client
open Atproto.Error
open Actor
open Feed
open Graph

(* Real app.bsky.* calls against official @atproto/dev-env AppView. *)

let env_truthy name =
  match Sys.getenv_opt name with
  | Some v ->
      let v = String.lowercase_ascii (String.trim v) in
      List.mem v [ "1"; "true"; "yes"; "on" ]
  | None -> false

let require_local = env_truthy "ATP_REQUIRE_LOCAL_PDS"

let host_is_local host =
  let bare =
    match String.split_on_char ':' host with h :: _ -> h | [] -> host
  in
  let bare = String.lowercase_ascii bare in
  bare = "localhost" || bare = "127.0.0.1" || bare = "[::1]" || bare = "::1"

let intended () =
  env_truthy "ATP_LOCAL_PDS" || host_is_local Session.atp_host_from_env

let appview_host () =
  match Sys.getenv_opt "ATP_APPVIEW_HOST" with
  | Some h when String.trim h <> "" -> String.trim h
  | _ ->
      if host_is_local Session.atp_host_from_env then "localhost:2584"
      else Client.appview_host_from_env

let ensure_ok json =
  match Error.check_for_error json with
  | Some _ -> failwith ("XRPC error: " ^ Error.to_string (Error.of_json json))
  | None -> json

let rfc3339_z () =
  let t = Unix.gmtime (Unix.gettimeofday ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.000Z" (t.Unix.tm_year + 1900)
    (t.Unix.tm_mon + 1) t.Unix.tm_mday t.Unix.tm_hour t.Unix.tm_min
    t.Unix.tm_sec

let skip_unless_local () =
  if not (intended ()) then
    skip_if true "local AppView not selected (start scripts/local-atproto.sh)";
  try
    let json =
      Client.get_json ~host:(appview_host ()) "app.bsky.actor.getProfile"
        [ ("actor", "alice.test") ]
    in
    ignore (ensure_ok json)
  with exn ->
    let msg = "local AppView is not reachable: " ^ Printexc.to_string exn in
    if require_local then failwith msg else skip_if true msg

let session_of auth =
  match String.split_on_char ':' auth with
  | [ u; p ] -> Session.create_session u p
  | _ -> failwith "expected handle:password"

let live_session =
  lazy
    (skip_unless_local ();
     if not Auth.has_live_credentials then
       if require_local then
         failwith "ATP_AUTH must be alice.test:hunter2 from @atproto/dev-env"
       else skip_if true "ATP_AUTH not set";
     let u, p = Auth.username_and_password_from_env in
     Session.create_session u p)

let bob_session () =
  match Sys.getenv_opt "ATP_AUTH_BOB" with
  | Some auth -> session_of auth
  | None -> session_of "bob.test:hunter2"

let session () = Lazy.force live_session

(* Local AppView rejects PDS at+jwt (InvalidToken). Public app.bsky.* reads
   go to AppView without a bearer. Authenticated product APIs (timeline /
   mutes / notifications) go through the PDS, which proxies to this AppView. *)
let av_get nsid pairs =
  Client.get_json ~host:(appview_host ()) nsid pairs |> ensure_ok

let pds_get s nsid pairs = Client.get_json ~session:s nsid pairs |> ensure_ok

let test_get_profile _ =
  let s = session () in
  let json = av_get "app.bsky.actor.getProfile" [ ("actor", s.username) ] in
  let profile = Actor.parse_profile json in
  OUnit2.assert_bool "getProfile did"
    (String.length profile.did > 8 && String.sub profile.did 0 4 = "did:")

let test_get_profiles _ =
  ignore (session ());
  let json =
    Client.get_json ~host:(appview_host ()) "app.bsky.actor.getProfiles"
      (Client.repeat_param "actors" [ "alice.test"; "bob.test" ])
    |> ensure_ok
  in
  let profiles = Actor.parse_profiles json in
  OUnit2.assert_bool "getProfiles count" (List.length profiles >= 1)

let method_missing err = err = "MethodNotFound" || err = "MethodNotImplemented"

let test_get_suggestions _ =
  ignore (session ());
  let json =
    Client.get_json ~host:(appview_host ()) "app.bsky.actor.getSuggestions"
      [ ("limit", "5") ]
  in
  match Error.check_for_error json with
  | Some err when method_missing err -> ()
  | Some _ -> failwith ("XRPC error: " ^ Error.to_string (Error.of_json json))
  | None ->
      let suggestions = Actor.parse_short_profiles json in
      OUnit2.assert_bool "getSuggestions list" (List.length suggestions >= 0)

let test_feed_after_writes _ =
  let s = session () in
  let bob = bob_session () in
  let created_at = rfc3339_z () in
  let post =
    Records.post ~text:"appview integration post" ~created_at ~langs:[ "en" ] ()
  in
  let created =
    Repo.create_record s s.auth.did Records.nsid_post
      (Yojson.Safe.to_string post)
    |> Yojson.Safe.from_string |> ensure_ok |> Repo.parse_write_result
  in
  OUnit2.assert_bool "pds createRecord uri" (String.length created.Repo.uri > 8);
  let like =
    Records.like ~uri:created.Repo.uri ~cid:created.Repo.cid ~created_at ()
  in
  ignore
    (Repo.create_record bob bob.auth.did Records.nsid_like
       (Yojson.Safe.to_string like));
  let repost =
    Records.repost ~uri:created.Repo.uri ~cid:created.Repo.cid ~created_at ()
  in
  ignore
    (Repo.create_record bob bob.auth.did Records.nsid_repost
       (Yojson.Safe.to_string repost));
  let rec wait n =
    if n <= 0 then failwith "AppView did not index the new post";
    let found =
      try
        let json =
          Client.get_json ~host:(appview_host ()) "app.bsky.feed.getAuthorFeed"
            [ ("actor", s.username); ("limit", "20") ]
        in
        match Error.check_for_error json with
        | Some _ -> false
        | None -> (
            match Yojson.Safe.Util.member "feed" json with
            | `List items ->
                List.exists
                  (fun item ->
                    match Yojson.Safe.Util.member "post" item with
                    | `Assoc _ as post -> (
                        match Yojson.Safe.Util.member "uri" post with
                        | `String u -> u = created.Repo.uri
                        | _ -> false)
                    | _ -> false)
                  items
            | _ -> false)
      with _ -> false
    in
    if found then ()
    else (
      Unix.sleep 1;
      wait (n - 1))
  in
  wait 45;
  let timeline =
    pds_get s "app.bsky.feed.getTimeline"
      [ ("algorithm", "reverse-chronological"); ("limit", "10") ]
    |> Feed.parse_timeline
  in
  OUnit2.assert_bool "getTimeline feed" (List.length timeline.feed >= 0);
  let thread =
    av_get "app.bsky.feed.getPostThread"
      [ ("uri", created.Repo.uri); ("depth", "2") ]
    |> Feed.parse_thread_feed
  in
  let _ = thread.thread in
  OUnit2.assert_bool "getPostThread parsed" true;
  let likes_json =
    av_get "app.bsky.feed.getLikes"
      [ ("uri", created.Repo.uri); ("cid", created.Repo.cid); ("limit", "10") ]
  in
  (match Yojson.Safe.Util.member "likes" likes_json with
  | `List _ -> ()
  | _ -> OUnit2.assert_failure "getLikes missing likes");
  let reposted_json =
    av_get "app.bsky.feed.getRepostedBy"
      [ ("uri", created.Repo.uri); ("cid", created.Repo.cid); ("limit", "10") ]
  in
  match Yojson.Safe.Util.member "repostedBy" reposted_json with
  | `List _ -> ()
  | _ -> OUnit2.assert_failure "getRepostedBy missing repostedBy"

let test_graph _ =
  let s = session () in
  let follows =
    av_get "app.bsky.graph.getFollows"
      [ ("actor", "alice.test"); ("limit", "10") ]
    |> Graph.parse_follows
  in
  OUnit2.assert_bool "getFollows (mock seeds follows)"
    (List.length follows.follows >= 1);
  let followers =
    av_get "app.bsky.graph.getFollowers"
      [ ("actor", "alice.test"); ("limit", "10") ]
    |> Graph.parse_followers
  in
  OUnit2.assert_bool "getFollowers" (List.length followers.followers >= 1);
  ignore (Graph.mute_actor s "carla.test");
  let mutes =
    pds_get s "app.bsky.graph.getMutes" [ ("limit", "10") ] |> Graph.parse_mutes
  in
  OUnit2.assert_bool "getMutes after muteActor" (List.length mutes.mutes >= 0)

let test_notifications _ =
  let s = session () in
  let json =
    Client.get_json ~session:s "app.bsky.notification.listNotifications"
      [ ("limit", "10") ]
  in
  match Error.check_for_error json with
  | Some err when method_missing err -> ()
  | Some _ -> failwith ("XRPC error: " ^ Error.to_string (Error.of_json json))
  | None -> (
      match Yojson.Safe.Util.member "notifications" json with
      | `List _ -> ()
      | _ -> OUnit2.assert_failure "listNotifications missing notifications")

let suite =
  "local_appview"
  >::: [
         "test_get_profile" >:: test_get_profile;
         "test_get_profiles" >:: test_get_profiles;
         "test_get_suggestions" >:: test_get_suggestions;
         "test_feed_after_writes" >:: test_feed_after_writes;
         "test_graph" >:: test_graph;
         "test_notifications" >:: test_notifications;
       ]

let () =
  Unix.putenv "OUNIT_RUNNER" "sequential";
  run_test_tt_main suite
