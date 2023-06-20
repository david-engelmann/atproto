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
end