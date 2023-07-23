open Session
open Cohttp_client
open App
open Actor
open Notification

module Feed = struct
  (* Authors feed comes with either "post" "post"+"reply" or "post"+"reason"
   * Depending on get_post_thread results, might want a type for each
   * combination ie. type feed_post, type feed_reply, feed_repost, feed_like?, feed_follow?
   * *)
  type feed =
    [
    | `Post
    | `Reply
    ]

  type post_record =
    {
      text : string;
      record_type : string;
      langs : string list;
      created_at : string;
    }

  type reply_record =
    {
      text : string;
      record_type : string;
      langs : string list;
      reply : Notification.reply;
      created_at : string;
    }

  type like_viewer =
    {
      like : string;
    }

  type feed_viewer =
    [
    | `LikeViewer of like_viewer
    | `ViewerStatus of Actor.viewer_status
    | `EmptyViewer
    ]

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

  let check_for_field field json =
    match json with
    | `Assoc fields -> List.exists (fun (key, _) -> key = field) fields
    | _ -> false

  let parse_post_record json : post_record =
    let open Yojson.Safe.Util in
    let text = json |> member "text" |> to_string in
    let record_type = json |> member "$type" |> to_string in
    let langs = json |> member "langs" |> to_list |> List.map to_string in
    let created_at = json |> member "createdAt" |> to_string in
    { text; record_type; langs; created_at }

  let parse_reply_record json : reply_record =
    let open Yojson.Safe.Util in
    let text = json |> member "text" |> to_string in
    let record_type = json |> member "$type" |> to_string in
    let langs = json |> member "langs" |> to_list |> List.map to_string in
    let reply = json |> member "reply" |> Notification.parse_reply in
    let created_at = json |> member "createdAt" |> to_string in
    { text; record_type; langs; reply; created_at }

  let parse_like_viewer json : like_viewer =
    let open Yojson.Safe.Util in
    let like = json |> member "like" |> to_string in
    { like }

  let parse_feed_viewer json : feed_viewer =
    let like_check = check_for_field "like" json in
    let muted_check = check_for_field "muted" json in
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

  let create_feed_endpoint (query_name : string) : string =
    "app.bsky.feed" ^ "." ^ query_name

  let get_author_feed (s : Session.session) (actor : string) (limit : int) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_author_feed_url = App.create_endpoint_url base_url (create_feed_endpoint "getAuthorFeed") in
    let body = Cohttp_client.create_body_from_pairs [("actor", actor); ("limit", string_of_int limit)] in
    let author_feed = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_author_feed_url body headers) in
    Printf.printf "Author Feed in func: %s\n" author_feed;
    author_feed

  let get_likes (s : Session.session) (uri : string) (cid : string) (limit : int) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_likes_url = App.create_endpoint_url base_url (create_feed_endpoint "getLikes") in
    let body = Cohttp_client.create_body_from_pairs [("uri", uri); ("cid", cid); ("limit", string_of_int limit)] in
    let likes = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_likes_url body headers) in
    likes

  let get_post_thread (s : Session.session) (uri : string) (depth : int) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_post_thread_url = App.create_endpoint_url base_url (create_feed_endpoint "getPostThread") in
    let body = Cohttp_client.create_body_from_pairs [("uri", uri); ("depth", string_of_int depth)] in
    let post_thread = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_post_thread_url body headers) in
    post_thread

  let get_posts (s : Session.session) (uris: string list) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_posts_url = App.create_endpoint_url base_url (create_feed_endpoint "getPosts") in
    let body = Cohttp_client.add_query_params "uris" uris in
    let posts = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_posts_url body headers) in
    posts

  let get_reposted_by (s : Session.session) (uri : string) (cid : string) (limit : int) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_reposted_by_url = App.create_endpoint_url base_url (create_feed_endpoint "getRepostedBy") in
    let body = Cohttp_client.create_body_from_pairs [("uri", uri); ("cid", cid); ("limit", string_of_int limit)] in
    let reposted_by = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_reposted_by_url body headers) in
    reposted_by

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
