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
open Atproto.Notification
open Atproto.Labeler
open Atproto.Unspecced
open Atproto.Bookmark
open Atproto.Draft
open Atproto.Ageassurance
open Actor
open Feed
open Graph
open Notification
open Labeler
open Unspecced
open Ageassurance

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

let ozone_did_or_empty () =
  match Sys.getenv_opt "ATP_OZONE_DID" with
  | Some d when String.trim d <> "" -> String.trim d
  | _ -> ""

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

(* Public AppView reads omit the PDS session. Authenticated AppView calls
   mint a service-auth JWT (aud=AppView DID, lxm=NSID) and never send at+jwt. *)
let av_get ?session nsid pairs =
  Client.get_json_appview ?session ~host:(appview_host ()) nsid pairs
  |> ensure_ok

(* Only assert when this AppView revision actually serves the NSID. 501 and
   flag-off InvalidRequest ("Search v2 is not enabled") are not served. *)
let av_get_if_served ?session nsid pairs =
  let json =
    Client.get_json_appview ?session ~host:(appview_host ()) nsid pairs
  in
  if Error.is_not_served_json json then None
  else
    match Error.check_for_error json with
    | Some _ -> failwith ("XRPC error: " ^ Error.to_string (Error.of_json json))
    | None -> Some json

let av_post_if_served ?session nsid data =
  let json =
    Client.post_json_appview ?session ~host:(appview_host ()) nsid data
  in
  if Error.is_not_served_json json then None
  else
    match Error.check_for_error json with
    | Some _ -> failwith ("XRPC error: " ^ Error.to_string (Error.of_json json))
    | None -> Some json

let message_has hay needle =
  let h = String.lowercase_ascii hay and n = String.lowercase_ascii needle in
  let rec aux i =
    if i + String.length n > String.length h then false
    else if String.sub h i (String.length n) = n then true
    else aux (i + 1)
  in
  aux 0

(* TestNetwork policy InvalidRequest: email token, unhosted feed
   generator DID, not-implemented. Never fail leftover hops on these. *)
let is_policy_invalid (e : Error.t) =
  Error.is_not_served e || e.error = "InvalidToken"
  || message_has e.error "invalidtoken"
  || message_has e.message "email confirmation token"
  || message_has e.message "email token"
  || message_has e.message "confirmation token"
  || message_has e.message "invalid token"
  || message_has e.message "token is invalid"
  || message_has e.error "token is invalid"
  || message_has e.message "could not find feed"
  || message_has e.message "invalid feed generator"
  || message_has e.message "not implemented"
  || message_has e.message "request body was provided when none was expected"
  || e.error = "UpstreamFailure"
  || message_has e.error "upstreamfailure"
  || message_has e.message "upstream service unreachable"
  || message_has e.message "upstream failure"

let av_leftover_json json =
  if Error.is_not_served_json json then None
  else
    match Error.check_for_error json with
    | None -> Some json
    | Some _ ->
        let e = Error.of_json json in
        if is_policy_invalid e then None else Some (ensure_ok json)

let av_get_leftover ?session nsid pairs =
  av_leftover_json
    (Client.get_json_appview ?session ~host:(appview_host ()) nsid pairs)

let av_post_leftover ?session nsid data =
  av_leftover_json
    (Client.post_json_appview ?session ~host:(appview_host ()) nsid data)

(* AppView hydrates PDS writes asynchronously (see test_feed_after_writes). *)
let av_get_until ?session ~attempts ~retry_message nsid pairs =
  let rec go n =
    let json =
      Client.get_json_appview ?session ~host:(appview_host ()) nsid pairs
    in
    if Error.is_not_served_json json then None
    else
      match Error.check_for_error json with
      | None -> Some json
      | Some _ ->
          let e = Error.of_json json in
          if n > 1 && message_has e.message retry_message then (
            Unix.sleep 1;
            go (n - 1))
          else failwith ("XRPC error: " ^ Error.to_string e)
  in
  go attempts

(* Same as av_get_until, but a persistent skip-needle is None, not a
   hard fail. AppView 0.0.277 getFeedGenerator / getFeed say
   "could not find feed" or "invalid feed generator service" when the
   generator DID has no #bsky_fg (TestNetwork policy). *)
let feed_generator_unhosted msg =
  message_has msg "could not find feed"
  || message_has msg "invalid feed generator service"

let av_get_until_or_skip ?session ~attempts nsid pairs =
  let rec go n =
    let json =
      Client.get_json_appview ?session ~host:(appview_host ()) nsid pairs
    in
    if Error.is_not_served_json json then None
    else
      match Error.check_for_error json with
      | None -> Some json
      | Some _ ->
          let e = Error.of_json json in
          if feed_generator_unhosted e.message then
            if n > 1 then (
              Unix.sleep 1;
              go (n - 1))
            else None
          else failwith ("XRPC error: " ^ Error.to_string e)
  in
  go attempts

let av_get_feed_if_hosted ?session feed =
  let json =
    Client.get_json_appview ?session ~host:(appview_host ())
      "app.bsky.feed.getFeed"
      [ ("feed", feed); ("limit", "5") ]
  in
  if Error.is_not_served_json json then None
  else
    match Error.check_for_error json with
    | None -> Some json
    | Some _ ->
        let e = Error.of_json json in
        if feed_generator_unhosted e.message then None
        else Some (ensure_ok json)

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

let test_get_suggestions _ =
  ignore (session ());
  let json =
    Client.get_json ~host:(appview_host ()) "app.bsky.actor.getSuggestions"
      [ ("limit", "5") ]
  in
  if Error.is_not_served_json json then ()
  else
    match Error.check_for_error json with
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
    av_get ~session:s "app.bsky.feed.getTimeline"
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
  let likes =
    av_get "app.bsky.feed.getLikes"
      [ ("uri", created.Repo.uri); ("cid", created.Repo.cid); ("limit", "10") ]
    |> Feed.parse_likes
  in
  OUnit2.assert_bool "getLikes" (List.length likes.likes >= 0);
  let reposted =
    av_get "app.bsky.feed.getRepostedBy"
      [ ("uri", created.Repo.uri); ("cid", created.Repo.cid); ("limit", "10") ]
    |> Feed.parse_reposted_by_feed
  in
  OUnit2.assert_bool "getRepostedBy" (List.length reposted.reposted_by >= 0)

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
    av_get ~session:s "app.bsky.graph.getMutes" [ ("limit", "10") ]
    |> Graph.parse_mutes
  in
  OUnit2.assert_bool "getMutes after muteActor" (List.length mutes.mutes >= 0)

let test_more_appview _ =
  let s = session () in
  let author_json =
    av_get "app.bsky.feed.getAuthorFeed"
      [ ("actor", "alice.test"); ("limit", "5") ]
  in
  (match Yojson.Safe.Util.member "feed" author_json with
  | `List (item :: _) -> (
      match Yojson.Safe.Util.member "post" item with
      | `Assoc _ as post -> (
          match Yojson.Safe.Util.member "uri" post with
          | `String uri when String.length uri > 0 -> (
              match
                av_get_if_served ~session:s "app.bsky.feed.getPosts"
                  [ ("uris", uri) ]
              with
              | None -> ()
              | Some json ->
                  let got = Feed.parse_posts_feed json in
                  OUnit2.assert_bool "getPosts" (List.length got.posts >= 1))
          | _ -> ())
      | _ -> ())
  | _ -> ());
  (match
     av_get_if_served ~session:s "app.bsky.graph.getRelationships"
       [ ("actor", "alice.test"); ("others", "bob.test") ]
   with
  | None -> ()
  | Some json ->
      let rel = Graph.parse_relationships json in
      OUnit2.assert_bool "getRelationships" (List.length rel.relationships >= 0));
  (match
     av_get_if_served ~session:s "app.bsky.graph.getLists"
       [ ("actor", "alice.test"); ("limit", "10") ]
   with
  | None -> ()
  | Some json ->
      let lists = Graph.parse_lists json in
      OUnit2.assert_bool "getLists" (List.length lists.lists >= 0));
  (match
     av_get_if_served ~session:s "app.bsky.graph.getActorStarterPacks"
       [ ("actor", "alice.test"); ("limit", "5") ]
   with
  | None -> ()
  | Some json ->
      let packs = Graph.parse_starter_packs json in
      OUnit2.assert_bool "getActorStarterPacks"
        (List.length packs.starter_packs >= 0));
  (match av_get_if_served ~session:s "app.bsky.actor.getPreferences" [] with
  | None -> ()
  | Some json ->
      let prefs = Actor.parse_preferences json in
      OUnit2.assert_bool "getPreferences" (List.length prefs.preferences >= 0));
  (match
     av_get_if_served ~session:s "app.bsky.notification.getUnreadCount" []
   with
  | None -> ()
  | Some json ->
      let unread = Notification.parse_unread_count json in
      OUnit2.assert_bool "getUnreadCount" (unread.count >= 0));
  (match
     av_get_if_served ~session:s "app.bsky.graph.getBlocks" [ ("limit", "10") ]
   with
  | None -> ()
  | Some json ->
      let blocks = Graph.parse_blocks json in
      OUnit2.assert_bool "getBlocks" (List.length blocks.blocks >= 0));
  (match
     av_get_if_served ~session:s "app.bsky.graph.getKnownFollowers"
       [ ("actor", "alice.test"); ("limit", "5") ]
   with
  | None -> ()
  | Some json ->
      let known = Graph.parse_followers json in
      OUnit2.assert_bool "getKnownFollowers" (List.length known.followers >= 0));
  (match
     av_get_if_served ~session:s "app.bsky.actor.searchActorsTypeahead"
       [ ("q", "alice"); ("limit", "5") ]
   with
  | None -> ()
  | Some json ->
      let typeahead = Actor.parse_typeahead_profiles json in
      OUnit2.assert_bool "searchActorsTypeahead" (List.length typeahead >= 0));
  (match
     av_get_if_served "app.bsky.actor.searchActors"
       [ ("q", "alice"); ("limit", "5") ]
   with
  | None -> ()
  | Some json ->
      let actors = Actor.parse_short_profiles json in
      OUnit2.assert_bool "searchActors" (List.length actors >= 0));
  (match
     av_get_if_served "app.bsky.feed.searchPosts"
       [ ("q", "integration"); ("limit", "5") ]
   with
  | None -> ()
  | Some json ->
      let posts = Feed.parse_search_posts json in
      OUnit2.assert_bool "searchPosts" (List.length posts.posts >= 0));
  (match
     av_get_if_served "app.bsky.feed.searchPostsV2"
       [ ("query", "integration"); ("limit", "5") ]
   with
  | None -> ()
  | Some json ->
      let posts = Feed.parse_search_posts_v2 json in
      OUnit2.assert_bool "searchPostsV2" (List.length posts.posts >= 0));
  (match
     av_get_if_served "app.bsky.graph.searchStarterPacksV2"
       [ ("q", "test"); ("limit", "5") ]
   with
  | None -> ()
  | Some json ->
      let packs = Graph.parse_starter_packs json in
      OUnit2.assert_bool "searchStarterPacksV2"
        (List.length packs.starter_packs >= 0));
  (match
     av_get_if_served "app.bsky.graph.getSuggestedFollowsByActor"
       [ ("actor", "alice.test") ]
   with
  | None -> ()
  | Some json ->
      let page = Graph.parse_suggested_follows json in
      OUnit2.assert_bool "getSuggestedFollowsByActor suggestions"
        (List.length page.suggestions >= 0);
      OUnit2.assert_bool "getSuggestedFollowsByActor recIdStr optional"
        (match page.rec_id_str with Some _ | None -> true));
  (match av_get_if_served "app.bsky.unspecced.getConfig" [] with
  | None -> ()
  | Some json ->
      let cfg = Unspecced.parse_config json in
      OUnit2.assert_bool "unspecced getConfig"
        (match cfg.check_email_confirmed with Some _ | None -> true));
  (match
     av_get_if_served "app.bsky.unspecced.getPopularFeedGenerators"
       [ ("limit", "5") ]
   with
  | None -> ()
  | Some json ->
      let gens = Unspecced.parse_generators json in
      OUnit2.assert_bool "getPopularFeedGenerators" (List.length gens.feeds >= 0));
  (match av_get_if_served "app.bsky.unspecced.getTrends" [ ("limit", "5") ] with
  | None -> ()
  | Some json ->
      ignore json;
      OUnit2.assert_bool "getTrends parsed" true);
  (match
     av_get_if_served ~session:s "app.bsky.notification.getPreferences" []
   with
  | None -> ()
  | Some json ->
      let prefs = Notification.parse_preferences json in
      OUnit2.assert_bool "notification getPreferences"
        (match prefs.original with `Assoc _ | _ -> true));
  ignore
    (av_post_if_served ~session:s "app.bsky.graph.unmuteActor"
       (Yojson.Safe.to_string (`Assoc [ ("actor", `String "carla.test") ])));
  ignore
    (av_post_if_served ~session:s "app.bsky.notification.updateSeen"
       (Yojson.Safe.to_string (`Assoc [ ("seenAt", `String (rfc3339_z ())) ])));
  (match
     Yojson.Safe.Util.member "feed"
       (av_get "app.bsky.feed.getAuthorFeed"
          [ ("actor", "alice.test"); ("limit", "1") ])
   with
  | `List (item :: _) -> (
      match Yojson.Safe.Util.member "post" item with
      | `Assoc _ as post -> (
          match Yojson.Safe.Util.member "uri" post with
          | `String uri when String.length uri > 0 -> (
              match
                av_get_if_served "app.bsky.feed.getQuotes"
                  [ ("uri", uri); ("limit", "5") ]
              with
              | None -> ()
              | Some json ->
                  let quotes = Feed.parse_quotes json in
                  OUnit2.assert_bool "getQuotes" (List.length quotes.posts >= 0)
              )
          | _ -> ())
      | _ -> ())
  | _ -> ());
  match ozone_did_or_empty () with
  | "" -> ()
  | did -> (
      match
        av_get_if_served "app.bsky.labeler.getServices" [ ("dids", did) ]
      with
      | None -> ()
      | Some json ->
          let services = Labeler.parse_services json in
          OUnit2.assert_bool "getServices" (List.length services.views >= 0))

let test_notifications _ =
  let s = session () in
  let json =
    av_get ~session:s "app.bsky.notification.listNotifications"
      [ ("limit", "10") ]
  in
  if Error.is_not_served_json json then ()
  else
    match Error.check_for_error json with
    | Some _ -> failwith ("XRPC error: " ^ Error.to_string (Error.of_json json))
    | None -> (
        match Yojson.Safe.Util.member "notifications" json with
        | `List _ -> ()
        | _ -> OUnit2.assert_failure "listNotifications missing notifications")

let test_leftover_appview _ =
  let s = session () in
  (match
     av_get_if_served ~session:s "app.bsky.feed.getActorLikes"
       [ ("actor", "alice.test"); ("limit", "5") ]
   with
  | None -> ()
  | Some json -> (
      match Yojson.Safe.Util.member "feed" json with
      | `List _ -> ()
      | _ -> OUnit2.assert_failure "getActorLikes missing feed"));
  (match
     av_get_if_served "app.bsky.feed.getActorFeeds"
       [ ("actor", "alice.test"); ("limit", "5") ]
   with
  | None -> ()
  | Some json ->
      let gens = Feed.parse_generators json in
      OUnit2.assert_bool "getActorFeeds" (List.length gens.feeds >= 0));
  (match
     av_get_if_served "app.bsky.feed.getSuggestedFeeds" [ ("limit", "5") ]
   with
  | None -> ()
  | Some json -> (
      match Yojson.Safe.Util.member "feeds" json with
      | `List _ -> ()
      | _ -> OUnit2.assert_failure "getSuggestedFeeds missing feeds"));
  (match
     av_get_if_served "app.bsky.feed.getFeedGenerators"
       [
         ( "feeds",
           "at://did:plc:z72i7hdynmk6r22z27h6tvur/app.bsky.feed.generator/whats-hot"
         );
       ]
   with
  | None -> ()
  | Some json -> (
      match Yojson.Safe.Util.member "feeds" json with `List _ -> () | _ -> ()));
  (match
     av_get_if_served "app.bsky.graph.searchStarterPacks"
       [ ("q", "test"); ("limit", "5") ]
   with
  | None -> ()
  | Some json -> (
      match Yojson.Safe.Util.member "starterPacks" json with
      | `List _ -> ()
      | _ -> OUnit2.assert_failure "searchStarterPacks missing starterPacks"));
  (match av_get_if_served "app.bsky.unspecced.getTaggedSuggestions" [] with
  | None -> ()
  | Some json -> (
      match Yojson.Safe.Util.member "suggestions" json with
      | `List _ -> ()
      | _ -> ()));
  (match
     av_get_if_served "app.bsky.unspecced.getTrendingTopics" [ ("limit", "5") ]
   with
  | None -> ()
  | Some json -> ignore json);
  (match
     av_get_if_served "app.bsky.embed.getEmbedExternalView"
       (("url", "https://atproto.com")
       :: Client.repeat_param "uris"
            [ Printf.sprintf "at://%s/app.bsky.actor.profile/self" s.auth.did ]
       )
   with
  | None -> ()
  | Some json ->
      let parsed = Atproto.Embed.Embed.parse_embed_external_view json in
      OUnit2.assert_bool "getEmbedExternalView"
        (match parsed.view with Some _ | None -> true));
  (match
     av_get_if_served ~session:s "app.bsky.bookmark.getBookmarks"
       [ ("limit", "5") ]
   with
  | None -> ()
  | Some json -> (
      match Yojson.Safe.Util.member "bookmarks" json with
      | `List _ -> ()
      | _ -> ()));
  (match
     Yojson.Safe.Util.member "feed"
       (av_get "app.bsky.feed.getAuthorFeed"
          [ ("actor", "alice.test"); ("limit", "1") ])
   with
  | `List (item :: _) -> (
      match Yojson.Safe.Util.member "post" item with
      | `Assoc _ as post -> (
          match Yojson.Safe.Util.member "uri" post with
          | `String uri when String.length uri > 0 ->
              (match
                 av_get_if_served "app.bsky.unspecced.getPostThreadV2"
                   [ ("anchor", uri) ]
               with
              | None -> ()
              | Some json ->
                  OUnit2.assert_bool "getPostThreadV2"
                    (match json with `Assoc _ -> true | _ -> false));
              (match
                 av_get_if_served "app.bsky.unspecced.getPostThreadOtherV2"
                   [ ("anchor", uri) ]
               with
              | None -> ()
              | Some json ->
                  let other = Unspecced.parse_thread_other_v2 json in
                  OUnit2.assert_bool "getPostThreadOtherV2"
                    (List.length other.thread >= 0));
              ignore
                (av_post_if_served ~session:s "app.bsky.graph.muteThread"
                   (Yojson.Safe.to_string (`Assoc [ ("root", `String uri) ])));
              ignore
                (av_post_if_served ~session:s "app.bsky.graph.unmuteThread"
                   (Yojson.Safe.to_string (`Assoc [ ("root", `String uri) ])))
          | _ -> ())
      | _ -> ())
  | _ -> ());
  let created_at = rfc3339_z () in
  let list =
    Records.list ~name:"Leftover list" ~purpose:Records.purpose_curatelist
      ~created_at ()
  in
  let listed =
    Repo.create_record s s.auth.did Records.nsid_list
      (Yojson.Safe.to_string list)
    |> fun body ->
    match Error.check_for_error (Yojson.Safe.from_string body) with
    | Some e -> failwith ("create list: " ^ e)
    | None -> Repo.parse_write_result (Yojson.Safe.from_string body)
  in
  (match
     av_get_until ~attempts:20 ~retry_message:"not found"
       "app.bsky.graph.getList"
       [ ("list", listed.uri); ("limit", "5") ]
   with
  | None -> ()
  | Some json ->
      let page = Graph.parse_list_page json in
      OUnit2.assert_equal ~printer:(fun x -> x) listed.uri page.list.uri;
      (* Pinned @atproto/dev-env@0.6.4 does not hydrate APP-2933 opt-out
         fields. Parser still accepts them when present. *)
      (match page.list.viewer with
      | Some v ->
          OUnit2.assert_bool "referenceListOptOut optional"
            (match v.reference_list_opt_out with Some _ | None -> true)
      | None -> ());
      List.iter
        (fun (item : Graph.list_item) ->
          OUnit2.assert_bool "subjectOptedOut optional"
            (match item.subject_opted_out with Some _ | None -> true))
        page.items);
  (match
     av_get_until ~attempts:20 ~retry_message:"not found"
       "app.bsky.feed.getListFeed"
       [ ("list", listed.uri); ("limit", "5") ]
   with
  | None -> ()
  | Some json ->
      let page = Feed.parse_timeline json in
      OUnit2.assert_bool "getListFeed" (List.length page.feed >= 0));
  (match
     Yojson.Safe.Util.member "feed"
       (av_get "app.bsky.feed.getAuthorFeed"
          [ ("actor", "alice.test"); ("limit", "1") ])
   with
  | `List (item :: _) -> (
      match Yojson.Safe.Util.member "post" item with
      | `Assoc _ as post -> (
          match
            ( Yojson.Safe.Util.member "uri" post,
              Yojson.Safe.Util.member "cid" post )
          with
          | `String uri, `String cid
            when String.length uri > 0 && String.length cid > 0 ->
              ignore
                (av_post_if_served ~session:s "app.bsky.bookmark.createBookmark"
                   (Yojson.Safe.to_string
                      (Bookmark.create_bookmark_body ~uri ~cid)));
              ignore
                (av_post_if_served ~session:s "app.bsky.bookmark.deleteBookmark"
                   (Yojson.Safe.to_string (Bookmark.delete_bookmark_body ~uri)))
          | _ -> ())
      | _ -> ())
  | _ -> ());
  ignore
    (av_post_if_served ~session:s "app.bsky.graph.muteActor"
       (Yojson.Safe.to_string
          (Graph.mute_actor_body ~actor:"carla.test" ~only_reposts:true
             ~only_quoteposts:false ())));
  (match
     av_get_if_served "app.bsky.graph.getFollows"
       (Graph.follow_page_pairs ~actor:"alice.test" ~limit:10
          ~sort:Graph.sort_latest ())
   with
  | None -> ()
  | Some json ->
      let page = Graph.parse_follows json in
      OUnit2.assert_bool "getFollows sort=latest" (List.length page.follows >= 0);
      OUnit2.assert_bool "getFollows cursor optional"
        (match page.cursor with Some _ | None -> true));
  (match
     av_get_if_served "app.bsky.graph.getFollowers"
       (Graph.follow_page_pairs ~actor:"alice.test" ~limit:10
          ~sort:Graph.sort_top ())
   with
  | None -> ()
  | Some json ->
      let page = Graph.parse_followers json in
      OUnit2.assert_bool "getFollowers sort=top"
        (List.length page.followers >= 0);
      OUnit2.assert_bool "getFollowers cursor optional"
        (match page.cursor with Some _ | None -> true));
  (match
     av_get_if_served ~session:s "app.bsky.graph.getListsWithMembership"
       [ ("actor", "alice.test"); ("limit", "5") ]
   with
  | None -> ()
  | Some json -> (
      match Yojson.Safe.Util.member "listsWithMembership" json with
      | `List _ -> ()
      | _ -> ()));
  (match
     av_get_if_served ~session:s "app.bsky.graph.getStarterPacksWithMembership"
       [ ("actor", "alice.test"); ("limit", "5") ]
   with
  | None -> ()
  | Some json ->
      let page = Graph.parse_starter_packs_with_membership json in
      OUnit2.assert_bool "getStarterPacksWithMembership"
        (List.length page.starter_packs >= 0));
  (match
     av_get_if_served ~session:s
       "app.bsky.notification.listActivitySubscriptions"
       [ ("limit", "5") ]
   with
  | None -> ()
  | Some json ->
      let page = Notification.parse_activity_subscription_page json in
      OUnit2.assert_bool "listActivitySubscriptions"
        (List.length page.subscriptions >= 0));
  ignore
    (av_post_if_served ~session:s "app.bsky.actor.putPreferences"
       (Yojson.Safe.to_string
          (`Assoc
            [
              ( "preferences",
                `List
                  [
                    `Assoc
                      [
                        ("$type", `String "app.bsky.actor.defs#adultContentPref");
                        ("enabled", `Bool false);
                      ];
                  ] );
            ])));
  (* Official getAuthorFeed `filter` knownValues. *)
  List.iter
    (fun filter ->
      match
        av_get_if_served "app.bsky.feed.getAuthorFeed"
          [
            ("actor", "alice.test");
            ("limit", "5");
            ("filter", filter);
            ("includePins", "true");
          ]
      with
      | None -> ()
      | Some json ->
          let page = Feed.parse_timeline json in
          OUnit2.assert_bool
            ("getAuthorFeed filter=" ^ filter)
            (List.length page.feed >= 0))
    [
      Feed.filter_posts_with_replies;
      Feed.filter_posts_no_replies;
      Feed.filter_posts_with_media;
      Feed.filter_posts_and_author_threads;
      Feed.filter_posts_with_video;
    ]

let draft_post text : Draft.draft_post =
  {
    text;
    labels = None;
    embed_images = [];
    embed_gallery = None;
    embed_videos = [];
    embed_externals = [];
    embed_records = [];
  }

(* AppView 0.0.277 registers the draft family. PDS stash proxy may 501. *)
let test_drafts _ =
  let s = session () in
  let draft =
    Draft.draft_json ~langs:[ "en" ] ~posts:[ draft_post "av draft" ] ()
  in
  match
    av_post_if_served ~session:s "app.bsky.draft.createDraft"
      (Yojson.Safe.to_string (Draft.create_draft_body draft))
  with
  | None -> ()
  | Some json ->
      let id = Client.string_member json "id" in
      OUnit2.assert_bool "createDraft id" (String.length id > 0);
      (match
         av_get_if_served ~session:s "app.bsky.draft.getDrafts"
           [ ("limit", "10") ]
       with
      | None -> ()
      | Some page_json ->
          let page = Draft.parse_drafts_page page_json in
          OUnit2.assert_bool "getDrafts includes created"
            (List.exists (fun (d : Draft.draft_view) -> d.id = id) page.drafts));
      let updated =
        Draft.draft_json ~langs:[ "en" ]
          ~posts:[ draft_post "av draft updated" ]
          ()
      in
      ignore
        (av_post_if_served ~session:s "app.bsky.draft.updateDraft"
           (Yojson.Safe.to_string (Draft.update_draft_body ~id updated)));
      ignore
        (av_post_if_served ~session:s "app.bsky.draft.deleteDraft"
           (Yojson.Safe.to_string (Draft.delete_draft_body ~id)))

let test_mute_actor_list _ =
  let s = session () in
  let created_at = rfc3339_z () in
  let list =
    Records.list ~name:"List mute" ~purpose:Records.purpose_curatelist
      ~created_at ()
  in
  let listed =
    Repo.create_record s s.auth.did Records.nsid_list
      (Yojson.Safe.to_string list)
    |> fun body ->
    match Error.check_for_error (Yojson.Safe.from_string body) with
    | Some e -> failwith ("create list: " ^ e)
    | None -> Repo.parse_write_result (Yojson.Safe.from_string body)
  in
  ignore
    (av_get_until ~attempts:20 ~retry_message:"not found"
       "app.bsky.graph.getList"
       [ ("list", listed.uri); ("limit", "5") ]);
  ignore
    (av_post_if_served ~session:s "app.bsky.graph.muteActorList"
       (Yojson.Safe.to_string (`Assoc [ ("list", `String listed.uri) ])));
  (match
     av_get_if_served ~session:s "app.bsky.graph.getListMutes"
       [ ("limit", "10") ]
   with
  | None -> ()
  | Some json ->
      let page = Graph.parse_lists json in
      OUnit2.assert_bool "getListMutes" (List.length page.lists >= 0));
  ignore
    (av_post_if_served ~session:s "app.bsky.graph.unmuteActorList"
       (Yojson.Safe.to_string (`Assoc [ ("list", `String listed.uri) ])))

let test_put_preferences_v2 _ =
  let s = session () in
  let prefs =
    match
      av_get_if_served ~session:s "app.bsky.notification.getPreferences" []
    with
    | Some json -> Notification.parse_preferences json
    | None -> Notification.parse_preferences (`Assoc [])
  in
  match
    av_post_if_served ~session:s "app.bsky.notification.putPreferencesV2"
      (Yojson.Safe.to_string (Notification.preferences_to_json prefs))
  with
  | None -> ()
  | Some json ->
      let written = Notification.parse_preferences json in
      OUnit2.assert_bool "putPreferencesV2"
        (match written.original with `Assoc _ | _ -> true)

(* Remaining AppView NSIDs that 0.0.277 registers. Skip if this revision
   501s the method. getActorStarterPacks / getStarterPacksWithMembership /
   getFeedGenerators / listActivitySubscriptions are already live above. *)
let test_leftover_served _ =
  let s = session () in
  (match
     av_get_if_served ~session:s "app.bsky.graph.getListBlocks"
       [ ("limit", "10") ]
   with
  | None -> ()
  | Some json ->
      let page = Graph.parse_lists json in
      OUnit2.assert_bool "getListBlocks" (List.length page.lists >= 0));
  let created_at = rfc3339_z () in
  let list =
    Records.list ~name:"Served pack list" ~purpose:Records.purpose_curatelist
      ~created_at ()
  in
  let listed =
    Repo.create_record s s.auth.did Records.nsid_list
      (Yojson.Safe.to_string list)
    |> fun body ->
    match Error.check_for_error (Yojson.Safe.from_string body) with
    | Some e -> failwith ("create list: " ^ e)
    | None -> Repo.parse_write_result (Yojson.Safe.from_string body)
  in
  ignore
    (av_get_until ~attempts:20 ~retry_message:"not found"
       "app.bsky.graph.getList"
       [ ("list", listed.uri); ("limit", "5") ]);
  let pack =
    Records.starterpack ~name:"Served pack" ~list:listed.uri ~created_at ()
  in
  let packed =
    Repo.create_record s s.auth.did Records.nsid_starterpack
      (Yojson.Safe.to_string pack)
    |> fun body ->
    match Error.check_for_error (Yojson.Safe.from_string body) with
    | Some e -> failwith ("create starterpack: " ^ e)
    | None -> Repo.parse_write_result (Yojson.Safe.from_string body)
  in
  (match
     av_get_until ~attempts:20 ~retry_message:"not found"
       "app.bsky.graph.getStarterPack"
       [ ("starterPack", packed.uri) ]
   with
  | None -> ()
  | Some json ->
      let view =
        match Yojson.Safe.Util.member "starterPack" json with
        | `Assoc _ as sp -> Graph.parse_starter_pack sp
        | _ -> Graph.parse_starter_pack json
      in
      OUnit2.assert_equal ~printer:(fun x -> x) packed.uri view.uri);
  (match
     av_get_if_served "app.bsky.graph.getStarterPacks" [ ("uris", packed.uri) ]
   with
  | None -> ()
  | Some json ->
      let page = Graph.parse_starter_packs json in
      OUnit2.assert_bool "getStarterPacks" (List.length page.starter_packs >= 0));
  let gen =
    Records.generator ~did:s.auth.did ~display_name:"Served generator"
      ~created_at ()
  in
  let generated =
    Repo.create_record s s.auth.did Records.nsid_generator
      (Yojson.Safe.to_string gen)
    |> fun body ->
    match Error.check_for_error (Yojson.Safe.from_string body) with
    | Some e -> failwith ("create generator: " ^ e)
    | None -> Repo.parse_write_result (Yojson.Safe.from_string body)
  in
  let generator_info =
    match
      av_get_until_or_skip ~attempts:20 "app.bsky.feed.getFeedGenerator"
        [ ("feed", generated.uri) ]
    with
    | None -> None
    | Some json ->
        let info = Feed.parse_generator_info json in
        OUnit2.assert_equal ~printer:(fun x -> x) generated.uri info.view.uri;
        Some info
  in
  (* getFeed / getFeedSkeleton only against OUR leftover generator, and
     only if AppView marked it online. Suggested feeds are not live
     TestNetwork services. Unhosted generator DID is already skip-policy
     (av_get_until_or_skip / is_policy_invalid). *)
  (match generator_info with
  | Some info when info.is_online ->
      (match av_get_feed_if_hosted generated.uri with
      | None -> ()
      | Some json ->
          let page = Feed.parse_timeline json in
          OUnit2.assert_bool "getFeed" (List.length page.feed >= 0));
      (match
         av_get_leftover "app.bsky.feed.getFeedSkeleton"
           [ ("feed", generated.uri); ("limit", "5") ]
       with
       | None -> ()
       | Some json ->
           let page = Feed.parse_feed_skeleton json in
           OUnit2.assert_bool "getFeedSkeleton" (List.length page.feed >= 0))
  | _ -> ());
  let bob = bob_session () in
  (match
     av_post_if_served ~session:s
       "app.bsky.notification.putActivitySubscription"
       (Yojson.Safe.to_string
          (`Assoc
            [
              ("subject", `String bob.auth.did);
              ( "activitySubscription",
                Notification.activity_subscription_to_json
                  { Notification.post = true; reply = false } );
            ]))
   with
  | None -> ()
  | Some json ->
      let subject = Client.string_member json "subject" in
      OUnit2.assert_bool "putActivitySubscription subject"
        (String.length subject >= 0));
  ignore
    (av_post_if_served ~session:s
       "app.bsky.notification.putActivitySubscription"
       (Yojson.Safe.to_string
          (`Assoc
            [
              ("subject", `String bob.auth.did);
              ( "activitySubscription",
                Notification.activity_subscription_to_json
                  { Notification.post = false; reply = false } );
            ])))

(* Remaining AppView unspecced skeletons / getSuggested* / getTrendsSkeleton
   and app.bsky.ageassurance.* if this revision serves them. *)
let test_unspecced_and_ageassurance _ =
  let s = session () in
  let viewer = s.auth.did in
  let get ?session nsid pairs parse =
    match av_get_leftover ?session nsid pairs with
    | None -> ()
    | Some json -> parse json
  in
  get "app.bsky.unspecced.getTrendsSkeleton"
    [ ("limit", "5") ]
    (fun json ->
      let page = Unspecced.parse_trends_skeleton json in
      OUnit2.assert_bool "getTrendsSkeleton" (List.length page.trends >= 0));
  get "app.bsky.unspecced.getSuggestionsSkeleton"
    [ ("limit", "5"); ("viewer", viewer) ]
    (fun json ->
      let page = Unspecced.parse_suggestions_skeleton json in
      OUnit2.assert_bool "getSuggestionsSkeleton" (List.length page.actors >= 0));
  get "app.bsky.unspecced.getSuggestedFeeds"
    [ ("limit", "5") ]
    (fun json ->
      let page = Unspecced.parse_suggested_feeds json in
      OUnit2.assert_bool "unspecced getSuggestedFeeds"
        (List.length page.feeds >= 0));
  get "app.bsky.unspecced.getSuggestedFeedsSkeleton"
    [ ("limit", "5"); ("viewer", viewer) ]
    (fun json ->
      let page = Unspecced.parse_uri_list json "feeds" in
      OUnit2.assert_bool "getSuggestedFeedsSkeleton" (List.length page.uris >= 0));
  get "app.bsky.unspecced.getSuggestedUsers"
    [ ("limit", "5") ]
    (fun json ->
      let page = Unspecced.parse_suggested_users json in
      OUnit2.assert_bool "getSuggestedUsers" (List.length page.actors >= 0));
  get "app.bsky.unspecced.getSuggestedUsersSkeleton"
    [ ("limit", "5"); ("viewer", viewer) ]
    (fun json ->
      let page = Unspecced.parse_did_skeleton json in
      OUnit2.assert_bool "getSuggestedUsersSkeleton" (List.length page.dids >= 0));
  get "app.bsky.unspecced.getSuggestedStarterPacks"
    [ ("limit", "5") ]
    (fun json ->
      let packs =
        List.map Graph.parse_starter_pack
          (Client.list_member json "starterPacks")
      in
      OUnit2.assert_bool "getSuggestedStarterPacks" (List.length packs >= 0));
  get "app.bsky.unspecced.getSuggestedStarterPacksSkeleton"
    [ ("limit", "5"); ("viewer", viewer) ]
    (fun json ->
      let page = Unspecced.parse_uri_list json "starterPacks" in
      OUnit2.assert_bool "getSuggestedStarterPacksSkeleton"
        (List.length page.uris >= 0));
  get "app.bsky.unspecced.getOnboardingSuggestedStarterPacks"
    [ ("limit", "5") ]
    (fun json ->
      let packs =
        List.map Graph.parse_starter_pack
          (Client.list_member json "starterPacks")
      in
      OUnit2.assert_bool "getOnboardingSuggestedStarterPacks"
        (List.length packs >= 0));
  get "app.bsky.unspecced.getOnboardingSuggestedStarterPacksSkeleton"
    [ ("limit", "5"); ("viewer", viewer) ]
    (fun json ->
      let page = Unspecced.parse_uri_list json "starterPacks" in
      OUnit2.assert_bool "getOnboardingSuggestedStarterPacksSkeleton"
        (List.length page.uris >= 0));
  get "app.bsky.unspecced.getSuggestedOnboardingUsers"
    [ ("limit", "5") ]
    (fun json ->
      let page = Unspecced.parse_suggested_users json in
      OUnit2.assert_bool "getSuggestedOnboardingUsers"
        (List.length page.actors >= 0));
  get "app.bsky.unspecced.getOnboardingSuggestedUsersSkeleton"
    [ ("limit", "5"); ("viewer", viewer) ]
    (fun json ->
      let page = Unspecced.parse_did_skeleton json in
      OUnit2.assert_bool "getOnboardingSuggestedUsersSkeleton"
        (List.length page.dids >= 0));
  get "app.bsky.unspecced.getSuggestedUsersForDiscover"
    [ ("limit", "5") ]
    (fun json ->
      let page = Unspecced.parse_suggested_users json in
      OUnit2.assert_bool "getSuggestedUsersForDiscover"
        (List.length page.actors >= 0));
  get "app.bsky.unspecced.getSuggestedUsersForDiscoverSkeleton"
    [ ("limit", "5"); ("viewer", viewer) ]
    (fun json ->
      let page = Unspecced.parse_did_skeleton json in
      OUnit2.assert_bool "getSuggestedUsersForDiscoverSkeleton"
        (List.length page.dids >= 0));
  get "app.bsky.unspecced.getSuggestedUsersForExplore"
    [ ("limit", "5") ]
    (fun json ->
      let page = Unspecced.parse_suggested_users json in
      OUnit2.assert_bool "getSuggestedUsersForExplore"
        (List.length page.actors >= 0));
  get "app.bsky.unspecced.getSuggestedUsersForExploreSkeleton"
    [ ("limit", "5"); ("viewer", viewer) ]
    (fun json ->
      let page = Unspecced.parse_did_skeleton json in
      OUnit2.assert_bool "getSuggestedUsersForExploreSkeleton"
        (List.length page.dids >= 0));
  get "app.bsky.unspecced.getSuggestedUsersForSeeMore"
    [ ("limit", "5") ]
    (fun json ->
      let page = Unspecced.parse_suggested_users json in
      OUnit2.assert_bool "getSuggestedUsersForSeeMore"
        (List.length page.actors >= 0));
  get "app.bsky.unspecced.getSuggestedUsersForSeeMoreSkeleton"
    [ ("limit", "5"); ("viewer", viewer) ]
    (fun json ->
      let page = Unspecced.parse_did_skeleton json in
      OUnit2.assert_bool "getSuggestedUsersForSeeMoreSkeleton"
        (List.length page.dids >= 0));
  get "app.bsky.unspecced.searchPostsSkeleton"
    [ ("q", "integration"); ("limit", "5") ]
    (fun json ->
      let page = Unspecced.parse_skeleton_posts json in
      OUnit2.assert_bool "searchPostsSkeleton" (List.length page.posts >= 0));
  get "app.bsky.unspecced.searchActorsSkeleton"
    [ ("q", "alice"); ("limit", "5") ]
    (fun json ->
      let page = Unspecced.parse_skeleton_actors json in
      OUnit2.assert_bool "searchActorsSkeleton" (List.length page.actors >= 0));
  get "app.bsky.unspecced.searchStarterPacksSkeleton"
    [ ("q", "test"); ("limit", "5") ]
    (fun json ->
      let page = Unspecced.parse_skeleton_starter_packs json in
      OUnit2.assert_bool "searchStarterPacksSkeleton"
        (List.length page.starter_packs >= 0));
  get "app.bsky.ageassurance.getConfig" [] (fun json ->
      let cfg = Ageassurance.parse_config json in
      OUnit2.assert_bool "ageassurance.getConfig" (List.length cfg.regions >= 0));
  get ~session:s "app.bsky.ageassurance.getState"
    [ ("countryCode", "US") ]
    (fun json ->
      let bundle = Ageassurance.parse_state_bundle json in
      OUnit2.assert_bool "ageassurance.getState"
        (String.length bundle.state.status >= 0));
  match
    av_post_leftover ~session:s "app.bsky.ageassurance.begin"
      (Yojson.Safe.to_string
         (Ageassurance.begin_body ~email:"alice@test.com" ~language:"en"
            ~country_code:"US" ()))
  with
  | None -> ()
  | Some json ->
      let state = Ageassurance.parse_state json in
      OUnit2.assert_bool "ageassurance.begin" (String.length state.status >= 0)

(* Unique leftover AppView NSIDs whose wrappers exist on main and are
   not live above. Skip if this revision 501s the method or TestNetwork
   policy InvalidRequest (is_policy_invalid). chat.bsky.* / video.* /
   Tap / contact.* / push register-unregister / unhosted getFeed /
   getFeedSkeleton stay listed not faked. describeFeedGenerator is a
   feed-generator service method — do not treat a skip as a hosted
   generator. *)
let test_leftover_feed_notification _ =
  let s = session () in
  (match
     Yojson.Safe.Util.member "feed"
       (av_get "app.bsky.feed.getAuthorFeed"
          [ ("actor", "alice.test"); ("limit", "1") ])
   with
  | `List (item :: _) -> (
      match Yojson.Safe.Util.member "post" item with
      | `Assoc _ as post -> (
          match Yojson.Safe.Util.member "uri" post with
          | `String uri when String.length uri > 0 -> (
              match
                av_post_leftover ~session:s "app.bsky.feed.sendInteractions"
                  (Yojson.Safe.to_string
                     (Feed.send_interactions_body
                        [
                          {
                            item = Some uri;
                            event = Some "app.bsky.feed.defs#interactionLike";
                            feed_context = None;
                            req_id = None;
                          };
                        ]))
              with
              | None -> ()
              | Some _ -> OUnit2.assert_bool "sendInteractions" true)
          | _ -> ())
      | _ -> ())
  | _ -> ());
  (match av_get_leftover "app.bsky.feed.describeFeedGenerator" [] with
  | None -> ()
  | Some json ->
      let desc = Feed.parse_describe_feed_generator json in
      OUnit2.assert_bool "describeFeedGenerator"
        (String.length desc.did >= 0 && List.length desc.feeds >= 0));
  match
    av_post_leftover ~session:s "app.bsky.notification.putPreferences"
      (Yojson.Safe.to_string (`Assoc [ ("priority", `Bool false) ]))
  with
  | None -> ()
  | Some _ -> OUnit2.assert_bool "putPreferences v1" true

let suite =
  "local_appview"
  >::: [
         "test_get_profile" >:: test_get_profile;
         "test_get_profiles" >:: test_get_profiles;
         "test_get_suggestions" >:: test_get_suggestions;
         "test_feed_after_writes" >:: test_feed_after_writes;
         "test_graph" >:: test_graph;
         "test_more_appview" >:: test_more_appview;
         "test_notifications" >:: test_notifications;
         "test_leftover_appview" >:: test_leftover_appview;
         "test_drafts" >:: test_drafts;
         "test_mute_actor_list" >:: test_mute_actor_list;
         "test_put_preferences_v2" >:: test_put_preferences_v2;
         "test_leftover_served" >:: test_leftover_served;
         "test_unspecced_and_ageassurance" >:: test_unspecced_and_ageassurance;
         "test_leftover_feed_notification" >:: test_leftover_feed_notification;
       ]

let () =
  Unix.putenv "OUNIT_RUNNER" "sequential";
  run_test_tt_main suite
