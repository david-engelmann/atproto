open Session
open Cohttp_client
open App
open Actor
open Notification
open Facet

module Feed = struct
  (* Authors feed comes with either "post" "post"+"reply" or "post"+"reason"
   * Depending on get_post_thread results, might want a type for each
   * combination ie. type feed_post, type feed_reply, feed_repost, feed_like?, feed_follow?
   * *)

  type thread_record =
    {
      text : string;
      record_type : string;
      reply : Notification.reply;
      created_at : string;
    }

  type post_record =
    {
      text : string;
      record_type : string;
      langs : (string list) option;
      facets : (Facet.facet list) option;
      created_at : string;
    }

  type reply_record =
    {
      text : string;
      record_type : string;
      langs : (string list) option;
      reply : Notification.reply;
      created_at : string;
    }

  type repost_record =
    {
      text : string;
      record_type : string;
      created_at : string;
    }

  type like_viewer =
    {
      like : string;
    }

  type repost_viewer =
    {
      repost : string;
      like : string;
    }

  type feed_viewer =
    [
    | `LikeViewer of like_viewer
    | `ViewerStatus of Actor.viewer_status
    | `RepostViewer of repost_viewer
    | `EmptyViewer
    ]

  type like =
    {
      created_at : string;
      indexed_at : string;
      actor : Actor.short_profile;
    }

  type likes =
    {
      uri : string;
      cid : string;
      cursor : string;
      likes : like list;
    }

  type post =
    {
      uri : string;
      cid : string;
      author : Actor.typeahead_profile;
      record : post_record;
      reply_count : int;
      repost_count : int;
      like_count : int;
      indexed_at : string;
      viewer : feed_viewer;
      labels : (string list) option;
    }

  type reply_post =
    {
      uri : string;
      cid : string;
      author : Actor.typeahead_profile;
      record : reply_record;
      reply_count : int;
      repost_count : int;
      like_count : int;
      indexed_at : string;
      viewer : feed_viewer;
      labels : (string list) option;
    }

  type repost_post =
    {
      uri : string;
      cid : string;
      author : Actor.typeahead_profile;
      record : repost_record;
      reply_count : int;
      repost_count : int;
      like_count : int;
      indexed_at : string;
      viewer : feed_viewer;
      labels : (string list) option;
    }

  type thread_post =
    {
      uri : string;
      cid : string;
      author : Actor.typeahead_profile;
      record : thread_record;
      reply_count : int;
      repost_count : int;
      like_count : int;
      indexed_at : string;
      viewer : feed_viewer;
      labels : (string list) option;
    }


  (* lies *)
  type reply =
    {
      root : repost_post;
      parent : repost_post;
    }

  type reply_feed =
    {
      post : reply_post;
      reply : reply;

    }

  type replies =
    {
      replies_type : string;
      post : reply_post;
    }

  type reason =
    {
      reason_type : string;
      by : Actor.typeahead_profile;
      indexed_at : string;
    }

  type repost_feed =
    {
      post : repost_post;
      reason : reason;
    }

  type post_feed =
    {
      post : repost_post;
    }

  type get_post_feed =
    {
      post : reply_post;
    }

  type feed =
    [
    | `Post of post_feed
    | `Reply of reply_feed
    | `Repost of repost_feed
    ]

  type posts_feed =
    {
      posts : reply_post list;
    }

  type reposted_by_feed =
    {
      uri : string;
      cid : string;
      reposted_by : Actor.short_profile_without_description list;
      cursor : string;
    }

  let check_for_field field json =
    match json with
    | `Assoc fields -> List.exists (fun (key, _) -> key = field) fields
    | _ -> false


  let extract_langs_option json : (string list) option =
    let open Yojson.Safe.Util in
    try
      Some (json |> member "langs" |> to_list |> List.map to_string)
    with
      Type_error _ -> None

  let extract_facets_option json : (Facet.facet list) option =
    let open Yojson.Safe.Util in
    try
      Some (json |> member "facets" |> to_list |> List.map Facet.parse_facet)
    with
      Type_error _ -> None

  let parse_post_record json : post_record =
    let open Yojson.Safe.Util in
    let text = json |> member "text" |> to_string in
    let record_type = json |> member "$type" |> to_string in
    let langs = extract_langs_option json in
    let facets = extract_facets_option json in
    let created_at = json |> member "createdAt" |> to_string in
    { text; record_type; langs; facets; created_at }

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
    | false ->
      match like_check with
      | true -> `LikeViewer (parse_like_viewer json)
      | false ->
          match muted_check with
          | true -> `ViewerStatus (Actor.parse_viewer_status json)
          | false -> `EmptyViewer

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
    let labels =
      match json |> member "labels" with
      | `Null -> None
      | `List labels_json -> Some (labels_json |> List.map to_string)
      | _ -> None
    in
    { uri; cid; author; record; reply_count; repost_count; like_count;
      indexed_at; viewer; labels }

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
    let labels =
      match json |> member "labels" with
      | `Null -> None
      | `List labels_json -> Some (labels_json |> List.map to_string)
      | _ -> None
    in
    { uri; cid; author; record; reply_count; repost_count; like_count;
      indexed_at; viewer; labels }

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
    let labels =
      match json |> member "labels" with
      | `Null -> None
      | `List labels_json -> Some (labels_json |> List.map to_string)
      | _ -> None
    in
    { uri; cid; author; record; reply_count; repost_count; like_count;
      indexed_at; viewer; labels }

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
    let labels =
      match json |> member "labels" with
      | `Null -> None
      | `List labels_json -> Some (labels_json |> List.map to_string)
      | _ -> None
    in
    { uri; cid; author; record; reply_count; repost_count; like_count;
      indexed_at; viewer; labels }

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
    | false ->
      match reply_field_check with
      | true -> `Reply (parse_reply_feed json)
      | false -> `Post (parse_post_feed json)

  let parse_replies json : replies =
    let open Yojson.Safe.Util in
    let replies_type = json |> member "$type" |> to_string in
    let post = json |> member "post" |> parse_reply_post in
    { replies_type; post }

  type thread_parent =
    {
     thread_type : string;
     post : repost_post;
     replies : replies list;
    }

  type thread =
    {
      thread_type : string;
      post : thread_post;
      parent : thread_parent;
      replies : replies list;
    }

  type thread_feed =
    {
      thread : thread;
    }

  let parse_thread_parent json : thread_parent =
    let open Yojson.Safe.Util in
    let thread_type = json |> member "$type" |> to_string in
    let post = json |> member "post" |> parse_repost_post in
    let replies = json |> member "replies" |> to_list |> List.map parse_replies in
    { thread_type; post; replies }

  let parse_thread json : thread =
    let open Yojson.Safe.Util in
    let thread_type = json |> member "$type" |> to_string in
    let post = json |> member "post" |> parse_thread_post in
    let parent = json |> member "parent" |> parse_thread_parent in
    let replies = json |> member "replies" |> to_list |> List.map parse_replies in
    { thread_type; post; parent; replies }

  let parse_thread_feed json : thread_feed =
    let open Yojson.Safe.Util in
    let thread = json |> member "thread" |> parse_thread in
    { thread }

  let parse_posts_feed json : posts_feed =
    let open Yojson.Safe.Util in
    let posts = json |> member "posts" |> to_list |> List.map parse_reply_post in
    { posts }

  let parse_reposted_by_feed json : reposted_by_feed =
    let open Yojson.Safe.Util in
    let uri = json |> member "uri" |> to_string in
    let cid = json |> member "cid" |> to_string in
    let reposted_by = json |> member "repostedBy" |> to_list |> List.map Actor.parse_short_profile_without_description in
    let cursor = json |> member "cursor" |> to_string in
    { uri; cid; reposted_by; cursor }

  let convert_body_to_json (body : string) : Yojson.Safe.t =
    let json = Yojson.Safe.from_string body in
    json

  let create_feed_endpoint (query_name : string) : string =
    "app.bsky.feed" ^ "." ^ query_name

  let get_author_feed (s : Session.session) (actor : string) (limit : int) : feed list =
    let open Yojson.Safe.Util in
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_author_feed_url = App.create_endpoint_url base_url (create_feed_endpoint "getAuthorFeed") in
    let body = Cohttp_client.create_body_from_pairs [("actor", actor); ("limit", string_of_int limit)] in
    let author_feed = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_author_feed_url body headers) in
    let feed = author_feed |> convert_body_to_json |> member "feed" in
    feed |> to_list |> List.map parse_feed

  let get_likes (s : Session.session) (uri : string) (cid : string) (limit : int) : likes =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_likes_url = App.create_endpoint_url base_url (create_feed_endpoint "getLikes") in
    let body = Cohttp_client.create_body_from_pairs [("uri", uri); ("cid", cid); ("limit", string_of_int limit)] in
    let likes = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_likes_url body headers) in
    likes |> convert_body_to_json |> parse_likes

  let get_post_thread (s : Session.session) (uri : string) (depth : int) : thread_feed =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_post_thread_url = App.create_endpoint_url base_url (create_feed_endpoint "getPostThread") in
    let body = Cohttp_client.create_body_from_pairs [("uri", uri); ("depth", string_of_int depth)] in
    let post_thread = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_post_thread_url body headers) in
    post_thread |> convert_body_to_json |> parse_thread_feed

  let get_posts (s : Session.session) (uris: string list) : posts_feed =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_posts_url = App.create_endpoint_url base_url (create_feed_endpoint "getPosts") in
    let body = Cohttp_client.add_query_params "uris" uris in
    let posts = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_posts_url body headers) in
    posts |> convert_body_to_json |> parse_posts_feed (* used function name *)

  let get_reposted_by (s : Session.session) (uri : string) (cid : string) (limit : int) : reposted_by_feed =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_reposted_by_url = App.create_endpoint_url base_url (create_feed_endpoint "getRepostedBy") in
    let body = Cohttp_client.create_body_from_pairs [("uri", uri); ("cid", cid); ("limit", string_of_int limit)] in
    let reposted_by = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_reposted_by_url body headers) in
    reposted_by |> convert_body_to_json |> parse_reposted_by_feed

  let get_timeline (s : Session.session) (algorithm : string) (limit: int) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_timeline_url = App.create_endpoint_url base_url (create_feed_endpoint "getTimeline") in
    let body = Cohttp_client.create_body_from_pairs [("algorithm", algorithm); ("limit", string_of_int limit)] in
    let timeline = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_timeline_url body headers) in
    timeline

  let get_feed_skeleton (s : Session.session) (feed : string) (limit : int) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_feed_skeleton_url = App.create_endpoint_url base_url (create_feed_endpoint "getFeedSkeleton") in
    let body = Cohttp_client.create_body_from_pairs [("feed", feed); ("limit", string_of_int limit)] in
    let feed_skeleton = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_feed_skeleton_url body headers) in
    feed_skeleton

end
