open Session
open Cohttp_client
open App

module Feed = struct
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
    let posts = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_post_urls body headers) in
    posts

end
