open Session
open Cohttp_client
open App
open Actor
open Notification
open Facet
open Embed

module Feed = struct
  (* Official app.bsky.feed.getAuthorFeed `filter` knownValues. *)
  let filter_posts_with_replies = "posts_with_replies"
  let filter_posts_no_replies = "posts_no_replies"
  let filter_posts_with_media = "posts_with_media"
  let filter_posts_and_author_threads = "posts_and_author_threads"
  let filter_posts_with_video = "posts_with_video"

  (* Authors feed comes with either "post" "post"+"reply" or "post"+"reason"
   * Depending on get_post_thread results, might want a type for each
   * combination ie. type feed_post, type feed_reply, feed_repost, feed_like?, feed_follow?
   * *)

  type post_record = {
    text : string;
    record_type : string;
    langs : string list option;
    facets : Facet.facet list option;
    embed : Embed.embed option;
    tags : string list option;
    self_labels : string list option;
    reply : Notification.reply option;
    created_at : string;
  }

  type thread_record = post_record
  type reply_record = post_record
  type repost_record = post_record
  type like_viewer = { like : string }
  type repost_viewer = { repost : string; like : string }

  type feed_viewer =
    [ `LikeViewer of like_viewer
    | `ViewerStatus of Actor.viewer_status
    | `RepostViewer of repost_viewer
    | `EmptyViewer ]

  type like = {
    created_at : string;
    indexed_at : string;
    actor : Actor.short_profile;
  }

  type likes = {
    uri : string;
    cid : string;
    cursor : string;
    likes : like list;
  }

  (* app.bsky.feed.defs#knownLikers — likers the viewer also follows. *)
  type known_liker = {
    did : string;
    handle : string;
    display_name : string option;
  }

  type known_likers = { count : int; actors : known_liker list }

  type post = {
    uri : string;
    cid : string;
    author : Actor.typeahead_profile;
    record : post_record;
    reply_count : int;
    repost_count : int;
    like_count : int;
    quote_count : int option;
    bookmark_count : int option;
    indexed_at : string;
    viewer : feed_viewer;
    known_likers : known_likers option;
    bookmarked : bool option;
    thread_muted : bool option;
    reply_disabled : bool option;
    embedding_disabled : bool option;
    pinned : bool option;
    labels : string list option;
    embed : Embed.embed option;
  }

  type reply_post = post
  type repost_post = post
  type thread_post = post
  type not_found_post = { uri : string; not_found : bool }

  type blocked_post = {
    uri : string;
    blocked : bool;
    author_did : string option;
  }

  type reply_ref_item =
    [ `Post of post | `NotFound of not_found_post | `Blocked of blocked_post ]

  type reply = {
    root : reply_ref_item;
    parent : reply_ref_item;
    grandparent_author : Actor.typeahead_profile option;
  }

  type reply_feed = { post : reply_post; reply : reply }

  type reason = {
    reason_type : string;
    by : Actor.typeahead_profile;
    indexed_at : string;
  }

  type repost_feed = { post : repost_post; reason : reason }
  type post_feed = { post : repost_post }
  type get_post_feed = { post : reply_post }

  type feed =
    [ `Post of post_feed | `Reply of reply_feed | `Repost of repost_feed ]

  type posts_feed = { posts : reply_post list }

  type reposted_by_feed = {
    uri : string;
    cid : string;
    reposted_by : Actor.short_profile_without_description list;
    cursor : string;
  }

  type timeline = { cursor : string; feed : feed list }

  let check_for_field field json =
    match json with
    | `Assoc fields -> List.exists (fun (key, _) -> key = field) fields
    | _ -> false

  let extract_langs_option json : string list option =
    let open Yojson.Safe.Util in
    try Some (json |> member "langs" |> to_list |> List.map to_string)
    with Type_error _ -> None

  let extract_facets_option json : Facet.facet list option =
    let open Yojson.Safe.Util in
    try Some (json |> member "facets" |> to_list |> List.map Facet.parse_facet)
    with Type_error _ -> None

  let extract_tags_option json : string list option =
    let open Yojson.Safe.Util in
    try Some (json |> member "tags" |> to_list |> List.map to_string)
    with Type_error _ -> None

  let extract_self_labels_option json : string list option =
    Label.Label.parse_self_labels (Yojson.Safe.Util.member "labels" json)

  let string_or_empty json field =
    match Yojson.Safe.Util.member field json with `String s -> s | _ -> ""

  let int_or_zero json field =
    match Yojson.Safe.Util.member field json with `Int n -> n | _ -> 0

  let int_opt json field =
    match Yojson.Safe.Util.member field json with `Int n -> Some n | _ -> None

  let type_name json =
    match Yojson.Safe.Util.member "$type" json with `String s -> s | _ -> ""

  let ends_with suffix s =
    let n = String.length suffix in
    let m = String.length s in
    m >= n && String.sub s (m - n) n = suffix

  let parse_reply_ref json : Notification.reply option =
    match Yojson.Safe.Util.member "reply" json with
    | `Assoc _ as r -> ( try Some (Notification.parse_reply r) with _ -> None)
    | _ -> None

  let parse_post_record json : post_record =
    let text = string_or_empty json "text" in
    let record_type = string_or_empty json "$type" in
    let langs = extract_langs_option json in
    let facets = extract_facets_option json in
    let embed = Embed.parse_embed_option json in
    let tags = extract_tags_option json in
    let self_labels = extract_self_labels_option json in
    let reply = parse_reply_ref json in
    let created_at = string_or_empty json "createdAt" in
    {
      text;
      record_type;
      langs;
      facets;
      embed;
      tags;
      self_labels;
      reply;
      created_at;
    }

  let parse_reply_record json : reply_record = parse_post_record json
  let parse_thread_record json : thread_record = parse_post_record json
  let parse_repost_record json : repost_record = parse_post_record json

  let parse_like_viewer json : like_viewer =
    let open Yojson.Safe.Util in
    let like = json |> member "like" |> to_string in
    { like }

  let parse_repost_viewer json : repost_viewer =
    let open Yojson.Safe.Util in
    let repost = json |> member "repost" |> to_string in
    let like = json |> member "like" |> to_string in
    { repost; like }

  let parse_feed_viewer json : feed_viewer =
    let repost_check = check_for_field "repost" json in
    let like_check = check_for_field "like" json in
    let muted_check = check_for_field "muted" json in
    match repost_check with
    | true -> `RepostViewer (parse_repost_viewer json)
    | false -> (
        match like_check with
        | true -> `LikeViewer (parse_like_viewer json)
        | false -> (
            match muted_check with
            | true -> `ViewerStatus (Actor.parse_viewer_status json)
            | false -> `EmptyViewer))

  let parse_known_liker json : known_liker =
    {
      did = string_or_empty json "did";
      handle = string_or_empty json "handle";
      display_name =
        (match Yojson.Safe.Util.member "displayName" json with
        | `String s -> Some s
        | _ -> None);
    }

  let parse_known_likers json : known_likers =
    {
      count = int_or_zero json "count";
      actors =
        (match Yojson.Safe.Util.member "actors" json with
        | `List xs -> List.map parse_known_liker xs
        | _ -> []);
    }

  let parse_known_likers_opt json : known_likers option =
    match json with
    | `Assoc _ -> (
        match Yojson.Safe.Util.member "knownLikers" json with
        | `Assoc _ as obj -> Some (parse_known_likers obj)
        | _ -> None)
    | _ -> None

  let viewer_bool_opt json field =
    match json with
    | `Assoc _ -> (
        match Yojson.Safe.Util.member field json with
        | `Bool b -> Some b
        | _ -> None)
    | _ -> None

  let parse_post json : post =
    let open Yojson.Safe.Util in
    let uri = string_or_empty json "uri" in
    let cid = string_or_empty json "cid" in
    let author = json |> member "author" |> Actor.parse_typeahead_profile in
    let record = json |> member "record" |> parse_post_record in
    let reply_count = int_or_zero json "replyCount" in
    let repost_count = int_or_zero json "repostCount" in
    let like_count = int_or_zero json "likeCount" in
    let quote_count = int_opt json "quoteCount" in
    let bookmark_count = int_opt json "bookmarkCount" in
    let indexed_at = string_or_empty json "indexedAt" in
    let viewer_json = json |> member "viewer" in
    let viewer = parse_feed_viewer viewer_json in
    let known_likers = parse_known_likers_opt viewer_json in
    let labels = Label.Label.parse_label_values (json |> member "labels") in
    let embed = Embed.parse_embed_option json in
    {
      uri;
      cid;
      author;
      record;
      reply_count;
      repost_count;
      like_count;
      quote_count;
      bookmark_count;
      indexed_at;
      viewer;
      known_likers;
      bookmarked = viewer_bool_opt viewer_json "bookmarked";
      thread_muted = viewer_bool_opt viewer_json "threadMuted";
      reply_disabled = viewer_bool_opt viewer_json "replyDisabled";
      embedding_disabled = viewer_bool_opt viewer_json "embeddingDisabled";
      pinned = viewer_bool_opt viewer_json "pinned";
      labels;
      embed;
    }

  let parse_reply_post json : reply_post = parse_post json
  let parse_thread_post json : thread_post = parse_post json

  let parse_like json : like =
    let open Yojson.Safe.Util in
    let created_at = json |> member "createdAt" |> to_string in
    let indexed_at = json |> member "indexedAt" |> to_string in
    let actor = json |> member "actor" |> Actor.parse_short_profile in
    { created_at; indexed_at; actor }

  let parse_likes json : likes =
    let open Yojson.Safe.Util in
    let uri = json |> member "uri" |> to_string in
    let cid = json |> member "cid" |> to_string in
    let cursor =
      match json |> member "cursor" with `String s -> s | _ -> ""
    in
    let likes = json |> member "likes" |> to_list |> List.map parse_like in
    { uri; cid; cursor; likes }

  let parse_reason json : reason =
    let open Yojson.Safe.Util in
    let reason_type = json |> member "$type" |> to_string in
    let by = json |> member "by" |> Actor.parse_typeahead_profile in
    let indexed_at = json |> member "indexedAt" |> to_string in
    { reason_type; by; indexed_at }

  let parse_repost_post json : repost_post = parse_post json

  let parse_not_found_post json : not_found_post =
    {
      uri = string_or_empty json "uri";
      not_found =
        (match Yojson.Safe.Util.member "notFound" json with
        | `Bool b -> b
        | _ -> true);
    }

  let parse_blocked_post json : blocked_post =
    let author = Yojson.Safe.Util.member "author" json in
    {
      uri = string_or_empty json "uri";
      blocked =
        (match Yojson.Safe.Util.member "blocked" json with
        | `Bool b -> b
        | _ -> true);
      author_did =
        (match author with
        | `Assoc _ -> (
            match Yojson.Safe.Util.member "did" author with
            | `String s -> Some s
            | _ -> None)
        | _ -> None);
    }

  let parse_reply_ref_item json : reply_ref_item =
    let t = type_name json in
    if
      ends_with "notFoundPost" t
      ||
      match Yojson.Safe.Util.member "notFound" json with
      | `Bool true -> true
      | _ -> false
    then `NotFound (parse_not_found_post json)
    else if
      ends_with "blockedPost" t
      ||
      match Yojson.Safe.Util.member "blocked" json with
      | `Bool true -> true
      | _ -> false
    then `Blocked (parse_blocked_post json)
    else `Post (parse_post json)

  let parse_reply json : reply =
    let open Yojson.Safe.Util in
    let root = json |> member "root" |> parse_reply_ref_item in
    let parent = json |> member "parent" |> parse_reply_ref_item in
    let grandparent_author =
      match json |> member "grandparentAuthor" with
      | `Assoc _ as a -> (
          try Some (Actor.parse_typeahead_profile a) with _ -> None)
      | _ -> None
    in
    { root; parent; grandparent_author }

  let parse_repost_feed json : repost_feed =
    let open Yojson.Safe.Util in
    let post = json |> member "post" |> parse_repost_post in
    let reason = json |> member "reason" |> parse_reason in
    { post; reason }

  let parse_post_feed json : post_feed =
    let open Yojson.Safe.Util in
    let post = json |> member "post" |> parse_repost_post in
    { post }

  let parse_get_post_feed json : get_post_feed =
    let open Yojson.Safe.Util in
    let post = json |> member "post" |> parse_reply_post in
    { post }

  let parse_reply_feed json : reply_feed =
    let open Yojson.Safe.Util in
    let post = json |> member "post" |> parse_reply_post in
    let reply = json |> member "reply" |> parse_reply in
    { post; reply }

  let parse_feed json : feed =
    let reason_field_check = check_for_field "reason" json in
    let reply_field_check = check_for_field "reply" json in
    match reason_field_check with
    | true -> `Repost (parse_repost_feed json)
    | false -> (
        match reply_field_check with
        | true -> `Reply (parse_reply_feed json)
        | false -> `Post (parse_post_feed json))

  let parse_timeline json : timeline =
    let open Yojson.Safe.Util in
    let cursor =
      match json |> member "cursor" with `String s -> s | _ -> ""
    in
    let feed =
      match json |> member "feed" with
      | `List xs -> List.map parse_feed xs
      | _ -> []
    in
    { cursor; feed }

  type thread_context = { root_author_like : string option }

  type thread = {
    thread_type : string;
    post : thread_post;
    parent : thread_item option;
    replies : thread_item list;
    thread_context : thread_context option;
  }

  and thread_item =
    [ `Thread of thread
    | `NotFound of not_found_post
    | `Blocked of blocked_post ]

  type replies = thread_item
  type thread_parent = thread
  type thread_feed = { thread : thread_item; threadgate : Yojson.Safe.t option }

  let parse_thread_context json : thread_context =
    {
      root_author_like =
        (match Yojson.Safe.Util.member "rootAuthorLike" json with
        | `String s -> Some s
        | _ -> None);
    }

  let rec parse_thread_item json : thread_item =
    let t = type_name json in
    if
      ends_with "notFoundPost" t
      ||
      match Yojson.Safe.Util.member "notFound" json with
      | `Bool true -> true
      | _ -> false
    then `NotFound (parse_not_found_post json)
    else if
      ends_with "blockedPost" t
      ||
      match Yojson.Safe.Util.member "blocked" json with
      | `Bool true -> true
      | _ -> false
    then `Blocked (parse_blocked_post json)
    else `Thread (parse_thread json)

  and parse_thread json : thread =
    let open Yojson.Safe.Util in
    let thread_type = type_name json in
    let post = json |> member "post" |> parse_thread_post in
    let parent =
      match json |> member "parent" with
      | `Assoc _ as p -> Some (parse_thread_item p)
      | _ -> None
    in
    let replies =
      match json |> member "replies" with
      | `List xs -> List.map parse_thread_item xs
      | _ -> []
    in
    let thread_context =
      match json |> member "threadContext" with
      | `Assoc _ as c -> Some (parse_thread_context c)
      | _ -> None
    in
    { thread_type; post; parent; replies; thread_context }

  let parse_replies json : replies = parse_thread_item json
  let parse_thread_parent json : thread_parent = parse_thread json

  let parse_thread_feed json : thread_feed =
    let open Yojson.Safe.Util in
    {
      thread = json |> member "thread" |> parse_thread_item;
      threadgate =
        (match json |> member "threadgate" with
        | `Assoc _ as g -> Some g
        | _ -> None);
    }

  let parse_posts_feed json : posts_feed =
    let open Yojson.Safe.Util in
    let posts =
      json |> member "posts" |> to_list |> List.map parse_reply_post
    in
    { posts }

  let parse_reposted_by_feed json : reposted_by_feed =
    let open Yojson.Safe.Util in
    let uri = json |> member "uri" |> to_string in
    let cid = json |> member "cid" |> to_string in
    let reposted_by =
      json |> member "repostedBy" |> to_list
      |> List.map Actor.parse_short_profile_without_description
    in
    let cursor =
      match json |> member "cursor" with `String s -> s | _ -> ""
    in
    { uri; cid; reposted_by; cursor }

  let convert_body_to_json (body : string) : Yojson.Safe.t =
    let json = Yojson.Safe.from_string body in
    json

  let create_feed_endpoint (query_name : string) : string =
    "app.bsky.feed" ^ "." ^ query_name

  let get_author_feed ?filter ?include_pins (s : Session.session)
      (actor : string) (limit : int) : feed list =
    let open Yojson.Safe.Util in
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let get_author_feed_url =
      App.create_endpoint_url base_url (create_feed_endpoint "getAuthorFeed")
    in
    let body =
      Cohttp_client.create_body_from_pairs
        ([ ("actor", actor); ("limit", string_of_int limit) ]
        @ (match filter with Some f -> [ ("filter", f) ] | None -> [])
        @
        match include_pins with
        | Some b -> [ ("includePins", string_of_bool b) ]
        | None -> [])
    in
    let author_feed =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers get_author_feed_url
           body headers)
    in
    let feed = author_feed |> convert_body_to_json |> member "feed" in
    feed |> to_list |> List.map parse_feed

  let get_likes (s : Session.session) (uri : string) (cid : string)
      (limit : int) : likes =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let get_likes_url =
      App.create_endpoint_url base_url (create_feed_endpoint "getLikes")
    in
    let body =
      Cohttp_client.create_body_from_pairs
        [ ("uri", uri); ("cid", cid); ("limit", string_of_int limit) ]
    in
    let likes =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers get_likes_url body
           headers)
    in
    likes |> convert_body_to_json |> parse_likes

  let get_post_thread (s : Session.session) (uri : string) (depth : int) :
      thread_feed =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let get_post_thread_url =
      App.create_endpoint_url base_url (create_feed_endpoint "getPostThread")
    in
    let body =
      Cohttp_client.create_body_from_pairs
        [ ("uri", uri); ("depth", string_of_int depth) ]
    in
    let post_thread =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers get_post_thread_url
           body headers)
    in
    post_thread |> convert_body_to_json |> parse_thread_feed

  let get_posts (s : Session.session) (uris : string list) : posts_feed =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let get_posts_url =
      App.create_endpoint_url base_url (create_feed_endpoint "getPosts")
    in
    let body = Cohttp_client.add_query_params "uris" uris in
    let posts =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers get_posts_url body
           headers)
    in
    posts |> convert_body_to_json |> parse_posts_feed (* used function name *)

  let get_reposted_by (s : Session.session) (uri : string) (cid : string)
      (limit : int) : reposted_by_feed =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let get_reposted_by_url =
      App.create_endpoint_url base_url (create_feed_endpoint "getRepostedBy")
    in
    let body =
      Cohttp_client.create_body_from_pairs
        [ ("uri", uri); ("cid", cid); ("limit", string_of_int limit) ]
    in
    let reposted_by =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers get_reposted_by_url
           body headers)
    in
    reposted_by |> convert_body_to_json |> parse_reposted_by_feed

  let get_timeline (s : Session.session) (algorithm : string) (limit : int) :
      timeline =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let get_timeline_url =
      App.create_endpoint_url base_url (create_feed_endpoint "getTimeline")
    in
    let body =
      Cohttp_client.create_body_from_pairs
        [ ("algorithm", algorithm); ("limit", string_of_int limit) ]
    in
    let timeline =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers get_timeline_url body
           headers)
    in
    timeline |> convert_body_to_json |> parse_timeline

  let get_feed_skeleton (s : Session.session) (feed : string) (limit : int) :
      string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let get_feed_skeleton_url =
      App.create_endpoint_url base_url (create_feed_endpoint "getFeedSkeleton")
    in
    let body =
      Cohttp_client.create_body_from_pairs
        [ ("feed", feed); ("limit", string_of_int limit) ]
    in
    let feed_skeleton =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers get_feed_skeleton_url
           body headers)
    in
    feed_skeleton

  (* ---- feed generators, search, quotes, interactions ------------------- *)

  type generator_view = {
    uri : string;
    cid : string;
    did : string;
    display_name : string;
    description : string option;
    creator_did : string option;
    like_count : int option;
    accepts_interactions : bool option;
    indexed_at : string;
    original : Yojson.Safe.t;
  }

  type generator_info = {
    view : generator_view;
    is_online : bool;
    is_valid : bool;
  }

  type generators = { cursor : string option; feeds : generator_view list }

  type skeleton_item = {
    post : string;
    reason : Yojson.Safe.t option;
    feed_context : string option;
  }

  type feed_skeleton = {
    cursor : string option;
    req_id : string option;
    feed : skeleton_item list;
  }

  type describe_feed_generator = {
    did : string;
    feeds : string list;
    privacy_policy : string option;
    terms_of_service : string option;
  }

  type post_view = {
    uri : string;
    cid : string;
    author_did : string option;
    author_handle : string option;
    text : string option;
    embed : Embed.embed option;
    tags : string list option;
    indexed_at : string;
    reply_count : int option;
    repost_count : int option;
    like_count : int option;
    quote_count : int option;
    bookmark_count : int option;
    known_likers : known_likers option;
    bookmarked : bool option;
    thread_muted : bool option;
    reply_disabled : bool option;
    embedding_disabled : bool option;
    pinned : bool option;
    original : Yojson.Safe.t;
  }

  type search_posts = {
    cursor : string option;
    hits_total : int option;
    posts : post_view list;
  }

  type search_posts_v2 = {
    cursor : string option;
    hits_total : int option;
    posts : post_view list;
    detected_query_languages : string list;
  }

  type quotes = {
    uri : string;
    cid : string option;
    cursor : string option;
    posts : post_view list;
  }

  type interaction = {
    item : string option;
    event : string option;
    feed_context : string option;
    req_id : string option;
  }

  let parse_generator_view json : generator_view =
    let creator_did =
      match Yojson.Safe.Util.member "creator" json with
      | `Assoc _ as c -> (
          match Yojson.Safe.Util.member "did" c with
          | `String s -> Some s
          | _ -> None)
      | _ -> None
    in
    {
      uri =
        (match Yojson.Safe.Util.member "uri" json with
        | `String s -> s
        | _ -> "");
      cid =
        (match Yojson.Safe.Util.member "cid" json with
        | `String s -> s
        | _ -> "");
      did =
        (match Yojson.Safe.Util.member "did" json with
        | `String s -> s
        | _ -> "");
      display_name =
        (match Yojson.Safe.Util.member "displayName" json with
        | `String s -> s
        | _ -> "");
      description =
        (match Yojson.Safe.Util.member "description" json with
        | `String s -> Some s
        | _ -> None);
      creator_did;
      like_count =
        (match Yojson.Safe.Util.member "likeCount" json with
        | `Int n -> Some n
        | _ -> None);
      accepts_interactions =
        (match Yojson.Safe.Util.member "acceptsInteractions" json with
        | `Bool b -> Some b
        | _ -> None);
      indexed_at =
        (match Yojson.Safe.Util.member "indexedAt" json with
        | `String s -> s
        | _ -> "");
      original = json;
    }

  let parse_generator_info json : generator_info =
    let open Yojson.Safe.Util in
    {
      view = json |> member "view" |> parse_generator_view;
      is_online =
        (match json |> member "isOnline" with `Bool b -> b | _ -> false);
      is_valid =
        (match json |> member "isValid" with `Bool b -> b | _ -> false);
    }

  let parse_generators json : generators =
    {
      cursor =
        (match Yojson.Safe.Util.member "cursor" json with
        | `String s -> Some s
        | _ -> None);
      feeds =
        List.map parse_generator_view
          (match Yojson.Safe.Util.member "feeds" json with
          | `List xs -> xs
          | _ -> []);
    }

  let parse_skeleton_item json : skeleton_item =
    {
      post =
        (match Yojson.Safe.Util.member "post" json with
        | `String s -> s
        | _ -> "");
      reason =
        (match Yojson.Safe.Util.member "reason" json with
        | `Null -> None
        | other -> Some other);
      feed_context =
        (match Yojson.Safe.Util.member "feedContext" json with
        | `String s -> Some s
        | _ -> None);
    }

  let parse_feed_skeleton json : feed_skeleton =
    {
      cursor =
        (match Yojson.Safe.Util.member "cursor" json with
        | `String s -> Some s
        | _ -> None);
      req_id =
        (match Yojson.Safe.Util.member "reqId" json with
        | `String s -> Some s
        | _ -> None);
      feed =
        List.map parse_skeleton_item
          (match Yojson.Safe.Util.member "feed" json with
          | `List xs -> xs
          | _ -> []);
    }

  let parse_describe_feed_generator json : describe_feed_generator =
    let open Yojson.Safe.Util in
    let links = json |> member "links" in
    {
      did = (match json |> member "did" with `String s -> s | _ -> "");
      feeds =
        (match json |> member "feeds" with
        | `List items ->
            List.filter_map
              (fun item ->
                match item |> member "uri" with
                | `String s -> Some s
                | _ -> None)
              items
        | _ -> []);
      privacy_policy =
        (match links |> member "privacyPolicy" with
        | `String s -> Some s
        | _ -> None);
      terms_of_service =
        (match links |> member "termsOfService" with
        | `String s -> Some s
        | _ -> None);
    }

  let parse_post_view json : post_view =
    let open Yojson.Safe.Util in
    let author = json |> member "author" in
    let record = json |> member "record" in
    {
      uri = (match json |> member "uri" with `String s -> s | _ -> "");
      cid = (match json |> member "cid" with `String s -> s | _ -> "");
      author_did =
        (match author |> member "did" with `String s -> Some s | _ -> None);
      author_handle =
        (match author |> member "handle" with `String s -> Some s | _ -> None);
      text =
        (match record |> member "text" with `String s -> Some s | _ -> None);
      embed =
        (match Embed.parse_embed_option json with
        | Some e -> Some e
        | None -> Embed.parse_embed_option record);
      tags = extract_tags_option record;
      indexed_at =
        (match json |> member "indexedAt" with `String s -> s | _ -> "");
      reply_count =
        (match json |> member "replyCount" with `Int n -> Some n | _ -> None);
      repost_count =
        (match json |> member "repostCount" with `Int n -> Some n | _ -> None);
      like_count =
        (match json |> member "likeCount" with `Int n -> Some n | _ -> None);
      quote_count =
        (match json |> member "quoteCount" with `Int n -> Some n | _ -> None);
      bookmark_count =
        (match json |> member "bookmarkCount" with
        | `Int n -> Some n
        | _ -> None);
      known_likers = parse_known_likers_opt (json |> member "viewer");
      bookmarked = viewer_bool_opt (json |> member "viewer") "bookmarked";
      thread_muted = viewer_bool_opt (json |> member "viewer") "threadMuted";
      reply_disabled = viewer_bool_opt (json |> member "viewer") "replyDisabled";
      embedding_disabled =
        viewer_bool_opt (json |> member "viewer") "embeddingDisabled";
      pinned = viewer_bool_opt (json |> member "viewer") "pinned";
      original = json;
    }

  let parse_search_posts json : search_posts =
    {
      cursor =
        (match Yojson.Safe.Util.member "cursor" json with
        | `String s -> Some s
        | _ -> None);
      hits_total =
        (match Yojson.Safe.Util.member "hitsTotal" json with
        | `Int n -> Some n
        | _ -> None);
      posts =
        List.map parse_post_view
          (match Yojson.Safe.Util.member "posts" json with
          | `List xs -> xs
          | _ -> []);
    }

  let parse_search_posts_v2 json : search_posts_v2 =
    let base = parse_search_posts json in
    {
      cursor = base.cursor;
      hits_total = base.hits_total;
      posts = base.posts;
      detected_query_languages =
        List.filter_map
          (function `String s -> Some s | _ -> None)
          (match Yojson.Safe.Util.member "detectedQueryLanguages" json with
          | `List xs -> xs
          | _ -> []);
    }

  let parse_quotes json : quotes =
    {
      uri =
        (match Yojson.Safe.Util.member "uri" json with
        | `String s -> s
        | _ -> "");
      cid =
        (match Yojson.Safe.Util.member "cid" json with
        | `String s -> Some s
        | _ -> None);
      cursor =
        (match Yojson.Safe.Util.member "cursor" json with
        | `String s -> Some s
        | _ -> None);
      posts =
        List.map parse_post_view
          (match Yojson.Safe.Util.member "posts" json with
          | `List xs -> xs
          | _ -> []);
    }

  let interaction_to_json (i : interaction) : Yojson.Safe.t =
    let fields =
      (match i.item with Some v -> [ ("item", `String v) ] | None -> [])
      @ (match i.event with Some v -> [ ("event", `String v) ] | None -> [])
      @ (match i.feed_context with
        | Some v -> [ ("feedContext", `String v) ]
        | None -> [])
      @ match i.req_id with Some v -> [ ("reqId", `String v) ] | None -> []
    in
    `Assoc fields

  let send_interactions_body ?feed interactions : Yojson.Safe.t =
    let fields =
      [ ("interactions", `List (List.map interaction_to_json interactions)) ]
      @ match feed with Some u -> [ ("feed", `String u) ] | None -> []
    in
    `Assoc fields

  let get_feed ?session ?host ~feed ?limit ?cursor () : timeline =
    Client.Client.get_json ?session ?host "app.bsky.feed.getFeed"
      ((("feed", feed) :: Client.Client.opt_int "limit" limit)
      @ Client.Client.opt_pair "cursor" cursor)
    |> parse_timeline

  let get_feed_generator ?session ?host ~feed () : generator_info =
    Client.Client.get_json ?session ?host "app.bsky.feed.getFeedGenerator"
      [ ("feed", feed) ]
    |> parse_generator_info

  let get_feed_generators ?session ?host ~feeds () : generator_view list =
    Client.Client.get_json ?session ?host "app.bsky.feed.getFeedGenerators"
      (Client.Client.repeat_param "feeds" feeds)
    |> parse_generators
    |> fun (g : generators) -> g.feeds

  let get_actor_feeds ?session ?host ~actor ?limit ?cursor () : generators =
    Client.Client.get_json ?session ?host "app.bsky.feed.getActorFeeds"
      ((("actor", actor) :: Client.Client.opt_int "limit" limit)
      @ Client.Client.opt_pair "cursor" cursor)
    |> parse_generators

  let get_suggested_feeds ?session ?host ?limit ?cursor () : generators =
    Client.Client.get_json ?session ?host "app.bsky.feed.getSuggestedFeeds"
      (Client.Client.opt_int "limit" limit
      @ Client.Client.opt_pair "cursor" cursor)
    |> parse_generators

  let get_author_feed_page ?session ?host ~actor ?limit ?cursor ?filter
      ?include_pins () : timeline =
    Client.Client.get_json ?session ?host "app.bsky.feed.getAuthorFeed"
      ((("actor", actor) :: Client.Client.opt_int "limit" limit)
      @ Client.Client.opt_pair "cursor" cursor
      @ Client.Client.opt_pair "filter" filter
      @ Client.Client.opt_bool "includePins" include_pins)
    |> parse_timeline

  let get_list_feed ?session ?host ~list ?limit ?cursor () : timeline =
    Client.Client.get_json ?session ?host "app.bsky.feed.getListFeed"
      ((("list", list) :: Client.Client.opt_int "limit" limit)
      @ Client.Client.opt_pair "cursor" cursor)
    |> parse_timeline

  let get_feed_skeleton_parsed ?session ?host ~feed ?limit ?cursor () :
      feed_skeleton =
    Client.Client.get_json ?session ?host "app.bsky.feed.getFeedSkeleton"
      ((("feed", feed) :: Client.Client.opt_int "limit" limit)
      @ Client.Client.opt_pair "cursor" cursor)
    |> parse_feed_skeleton

  let describe_feed_generator ?session ?host () : describe_feed_generator =
    Client.Client.get_json ?session ?host "app.bsky.feed.describeFeedGenerator"
      []
    |> parse_describe_feed_generator

  let search_posts ?session ?host ~q ?sort ?since ?until ?mentions ?author ?lang
      ?domain ?url ?limit ?cursor () : search_posts =
    Client.Client.get_json ?session ?host "app.bsky.feed.searchPosts"
      ((("q", q) :: Client.Client.opt_pair "sort" sort)
      @ Client.Client.opt_pair "since" since
      @ Client.Client.opt_pair "until" until
      @ Client.Client.opt_pair "mentions" mentions
      @ Client.Client.opt_pair "author" author
      @ Client.Client.opt_pair "lang" lang
      @ Client.Client.opt_pair "domain" domain
      @ Client.Client.opt_pair "url" url
      @ Client.Client.opt_int "limit" limit
      @ Client.Client.opt_pair "cursor" cursor)
    |> parse_search_posts

  let search_posts_v2 ?session ?host ?query ?sort ?(authors = [])
      ?(mentions = []) ?(domains = []) ?(urls = []) ?(embedded_at_uris = [])
      ?(hashtags = []) ?(exclude_authors = []) ?(exclude_mentions = [])
      ?(exclude_domains = []) ?(exclude_urls = [])
      ?(exclude_embedded_at_uris = []) ?(exclude_hashtags = []) ?since ?until
      ?all_time ?(languages = []) ?(exclude_languages = []) ?has_media
      ?has_video ?reply_parent_uri ?thread_root_uri ?exclude_replies
      ?replies_only ?following ?query_language ?limit ?cursor () :
      search_posts_v2 =
    Client.Client.get_json ?session ?host "app.bsky.feed.searchPostsV2"
      (Client.Client.opt_pair "query" query
      @ Client.Client.opt_pair "sort" sort
      @ Client.Client.repeat_param "authors" authors
      @ Client.Client.repeat_param "mentions" mentions
      @ Client.Client.repeat_param "domains" domains
      @ Client.Client.repeat_param "urls" urls
      @ Client.Client.repeat_param "embeddedAtUris" embedded_at_uris
      @ Client.Client.repeat_param "hashtags" hashtags
      @ Client.Client.repeat_param "excludeAuthors" exclude_authors
      @ Client.Client.repeat_param "excludeMentions" exclude_mentions
      @ Client.Client.repeat_param "excludeDomains" exclude_domains
      @ Client.Client.repeat_param "excludeUrls" exclude_urls
      @ Client.Client.repeat_param "excludeEmbeddedAtUris"
          exclude_embedded_at_uris
      @ Client.Client.repeat_param "excludeHashtags" exclude_hashtags
      @ Client.Client.opt_pair "since" since
      @ Client.Client.opt_pair "until" until
      @ Client.Client.opt_bool "allTime" all_time
      @ Client.Client.repeat_param "languages" languages
      @ Client.Client.repeat_param "excludeLanguages" exclude_languages
      @ Client.Client.opt_bool "hasMedia" has_media
      @ Client.Client.opt_bool "hasVideo" has_video
      @ Client.Client.opt_pair "replyParentUri" reply_parent_uri
      @ Client.Client.opt_pair "threadRootUri" thread_root_uri
      @ Client.Client.opt_bool "excludeReplies" exclude_replies
      @ Client.Client.opt_bool "repliesOnly" replies_only
      @ Client.Client.opt_bool "following" following
      @ Client.Client.opt_pair "queryLanguage" query_language
      @ Client.Client.opt_int "limit" limit
      @ Client.Client.opt_pair "cursor" cursor)
    |> parse_search_posts_v2

  let get_quotes ?session ?host ~uri ?cid ?limit ?cursor () : quotes =
    Client.Client.get_json ?session ?host "app.bsky.feed.getQuotes"
      ((("uri", uri) :: Client.Client.opt_pair "cid" cid)
      @ Client.Client.opt_int "limit" limit
      @ Client.Client.opt_pair "cursor" cursor)
    |> parse_quotes

  let get_actor_likes ?session ?host ~actor ?limit ?cursor () : timeline =
    Client.Client.get_json ?session ?host "app.bsky.feed.getActorLikes"
      ((("actor", actor) :: Client.Client.opt_int "limit" limit)
      @ Client.Client.opt_pair "cursor" cursor)
    |> parse_timeline

  let send_interactions (s : Session.session) ?feed interactions : unit =
    ignore
      (Client.Client.post_json ~session:s "app.bsky.feed.sendInteractions"
         (Yojson.Safe.to_string (send_interactions_body ?feed interactions)))
end
