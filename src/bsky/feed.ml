open Session
open Cohttp_client
open App
open Actor
open Notification
open Facet
open Embed

module Feed = struct
  (* Authors feed comes with either "post" "post"+"reply" or "post"+"reason"
   * Depending on get_post_thread results, might want a type for each
   * combination ie. type feed_post, type feed_reply, feed_repost, feed_like?, feed_follow?
   * *)

  type thread_record = {
    text : string;
    record_type : string;
    reply : Notification.reply;
    created_at : string;
  }

  type post_record = {
    text : string;
    record_type : string;
    langs : string list option;
    facets : Facet.facet list option;
    embed : Embed.embed option;
    tags : string list option;
    self_labels : string list option;
    created_at : string;
  }

  type reply_record = {
    text : string;
    record_type : string;
    langs : string list option;
    reply : Notification.reply;
    created_at : string;
  }

  type repost_record = {
    text : string;
    record_type : string;
    created_at : string;
  }

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

  type post = {
    uri : string;
    cid : string;
    author : Actor.typeahead_profile;
    record : post_record;
    reply_count : int;
    repost_count : int;
    like_count : int;
    indexed_at : string;
    viewer : feed_viewer;
    labels : string list option;
  }

  type reply_post = {
    uri : string;
    cid : string;
    author : Actor.typeahead_profile;
    record : reply_record;
    reply_count : int;
    repost_count : int;
    like_count : int;
    indexed_at : string;
    viewer : feed_viewer;
    labels : string list option;
  }

  type repost_post = {
    uri : string;
    cid : string;
    author : Actor.typeahead_profile;
    record : repost_record;
    reply_count : int;
    repost_count : int;
    like_count : int;
    indexed_at : string;
    viewer : feed_viewer;
    labels : string list option;
  }

  type thread_post = {
    uri : string;
    cid : string;
    author : Actor.typeahead_profile;
    record : thread_record;
    reply_count : int;
    repost_count : int;
    like_count : int;
    indexed_at : string;
    viewer : feed_viewer;
    labels : string list option;
  }

  (* lies *)
  type reply = { root : repost_post; parent : repost_post }
  type reply_feed = { post : reply_post; reply : reply }
  type replies = { replies_type : string; post : reply_post }

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

  let parse_post_record json : post_record =
    let open Yojson.Safe.Util in
    let text = json |> member "text" |> to_string in
    let record_type = json |> member "$type" |> to_string in
    let langs = extract_langs_option json in
    let facets = extract_facets_option json in
    let embed = Embed.parse_embed_option json in
    let tags = extract_tags_option json in
    let self_labels = extract_self_labels_option json in
    let created_at = json |> member "createdAt" |> to_string in
    { text; record_type; langs; facets; embed; tags; self_labels; created_at }

  let parse_reply_record json : reply_record =
    let open Yojson.Safe.Util in
    let text = json |> member "text" |> to_string in
    let record_type = json |> member "$type" |> to_string in
    let langs = extract_langs_option json in
    let reply = json |> member "reply" |> Notification.parse_reply in
    let created_at = json |> member "createdAt" |> to_string in
    { text; record_type; langs; reply; created_at }

  let parse_thread_record json : thread_record =
    let open Yojson.Safe.Util in
    let text = json |> member "text" |> to_string in
    let record_type = json |> member "$type" |> to_string in
    (* MAYBE REPLY NOW ALWAYS HERE *)
    let reply = json |> member "reply" |> Notification.parse_reply in
    let created_at = json |> member "createdAt" |> to_string in
    { text; record_type; reply; created_at }

  let parse_repost_record json : repost_record =
    let open Yojson.Safe.Util in
    let text = json |> member "text" |> to_string in
    let record_type = json |> member "$type" |> to_string in
    let created_at = json |> member "createdAt" |> to_string in
    { text; record_type; created_at }

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

  let parse_post json : post =
    let open Yojson.Safe.Util in
    let uri = json |> member "uri" |> to_string in
    let cid = json |> member "cid" |> to_string in
    let author = json |> member "author" |> Actor.parse_typeahead_profile in
    let record = json |> member "record" |> parse_post_record in
    let reply_count = json |> member "replyCount" |> to_int in
    let repost_count = json |> member "repostCount" |> to_int in
    let like_count = json |> member "likeCount" |> to_int in
    let indexed_at = json |> member "indexedAt" |> to_string in
    let viewer = json |> member "viewer" |> parse_feed_viewer in
    let labels = Label.Label.parse_label_values (json |> member "labels") in
    {
      uri;
      cid;
      author;
      record;
      reply_count;
      repost_count;
      like_count;
      indexed_at;
      viewer;
      labels;
    }

  let parse_reply_post json : reply_post =
    let open Yojson.Safe.Util in
    let uri = json |> member "uri" |> to_string in
    let cid = json |> member "cid" |> to_string in
    let author = json |> member "author" |> Actor.parse_typeahead_profile in
    let record = json |> member "record" |> parse_reply_record in
    let reply_count = json |> member "replyCount" |> to_int in
    let repost_count = json |> member "repostCount" |> to_int in
    let like_count = json |> member "likeCount" |> to_int in
    let indexed_at = json |> member "indexedAt" |> to_string in
    let viewer = json |> member "viewer" |> parse_feed_viewer in
    let labels = Label.Label.parse_label_values (json |> member "labels") in
    {
      uri;
      cid;
      author;
      record;
      reply_count;
      repost_count;
      like_count;
      indexed_at;
      viewer;
      labels;
    }

  let parse_thread_post json : thread_post =
    let open Yojson.Safe.Util in
    let uri = json |> member "uri" |> to_string in
    let cid = json |> member "cid" |> to_string in
    let author = json |> member "author" |> Actor.parse_typeahead_profile in
    let record = json |> member "record" |> parse_thread_record in
    let reply_count = json |> member "replyCount" |> to_int in
    let repost_count = json |> member "repostCount" |> to_int in
    let like_count = json |> member "likeCount" |> to_int in
    let indexed_at = json |> member "indexedAt" |> to_string in
    let viewer = json |> member "viewer" |> parse_feed_viewer in
    let labels = Label.Label.parse_label_values (json |> member "labels") in
    {
      uri;
      cid;
      author;
      record;
      reply_count;
      repost_count;
      like_count;
      indexed_at;
      viewer;
      labels;
    }

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
    let cursor = json |> member "cursor" |> to_string in
    let likes = json |> member "likes" |> to_list |> List.map parse_like in
    { uri; cid; cursor; likes }

  let parse_reason json : reason =
    let open Yojson.Safe.Util in
    let reason_type = json |> member "$type" |> to_string in
    let by = json |> member "by" |> Actor.parse_typeahead_profile in
    let indexed_at = json |> member "indexedAt" |> to_string in
    { reason_type; by; indexed_at }

  let parse_repost_post json : repost_post =
    let open Yojson.Safe.Util in
    let uri = json |> member "uri" |> to_string in
    let cid = json |> member "cid" |> to_string in
    let author = json |> member "author" |> Actor.parse_typeahead_profile in
    let record = json |> member "record" |> parse_repost_record in
    let reply_count = json |> member "replyCount" |> to_int in
    let repost_count = json |> member "repostCount" |> to_int in
    let like_count = json |> member "likeCount" |> to_int in
    let indexed_at = json |> member "indexedAt" |> to_string in
    let viewer = json |> member "viewer" |> parse_feed_viewer in
    let labels = Label.Label.parse_label_values (json |> member "labels") in
    {
      uri;
      cid;
      author;
      record;
      reply_count;
      repost_count;
      like_count;
      indexed_at;
      viewer;
      labels;
    }

  let parse_reply json : reply =
    let open Yojson.Safe.Util in
    let root = json |> member "root" |> parse_repost_post in
    let parent = json |> member "parent" |> parse_repost_post in
    { root; parent }

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

  let parse_replies json : replies =
    let open Yojson.Safe.Util in
    let replies_type = json |> member "$type" |> to_string in
    let post = json |> member "post" |> parse_reply_post in
    { replies_type; post }

  type thread_parent = {
    thread_type : string;
    post : repost_post;
    replies : replies list;
  }

  type thread = {
    thread_type : string;
    post : thread_post;
    parent : thread_parent;
    replies : replies list;
  }

  type thread_feed = { thread : thread }

  let parse_thread_parent json : thread_parent =
    let open Yojson.Safe.Util in
    let thread_type = json |> member "$type" |> to_string in
    let post = json |> member "post" |> parse_repost_post in
    let replies =
      json |> member "replies" |> to_list |> List.map parse_replies
    in
    { thread_type; post; replies }

  let parse_thread json : thread =
    let open Yojson.Safe.Util in
    let thread_type = json |> member "$type" |> to_string in
    let post = json |> member "post" |> parse_thread_post in
    let parent = json |> member "parent" |> parse_thread_parent in
    let replies =
      json |> member "replies" |> to_list |> List.map parse_replies
    in
    { thread_type; post; parent; replies }

  let parse_thread_feed json : thread_feed =
    let open Yojson.Safe.Util in
    let thread = json |> member "thread" |> parse_thread in
    { thread }

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
    let cursor = json |> member "cursor" |> to_string in
    { uri; cid; reposted_by; cursor }

  let convert_body_to_json (body : string) : Yojson.Safe.t =
    let json = Yojson.Safe.from_string body in
    json

  let create_feed_endpoint (query_name : string) : string =
    "app.bsky.feed" ^ "." ^ query_name

  let get_author_feed (s : Session.session) (actor : string) (limit : int) :
      feed list =
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
        [ ("actor", actor); ("limit", string_of_int limit) ]
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
    like_count : int option;
    quote_count : int option;
    original : Yojson.Safe.t;
  }

  type search_posts = {
    cursor : string option;
    hits_total : int option;
    posts : post_view list;
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
      like_count =
        (match json |> member "likeCount" with `Int n -> Some n | _ -> None);
      quote_count =
        (match json |> member "quoteCount" with `Int n -> Some n | _ -> None);
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
