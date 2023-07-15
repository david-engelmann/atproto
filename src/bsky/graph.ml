open Session
open Cohttp_client
open App

module Graph = struct
  let create_graph_endpoint (query_name : string) : string =
    "app.bsky.graph" ^ "." ^ query_name

  let get_blocks (s : Session.session) (limit : int) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_blocks_url = App.create_endpoint_url base_url (create_sync_endpoint "getBlocks") in
    let body = Cohttp_client.create_body_from_pairs [("limit", string_of_int limit)] in
    let blocks = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_blocks_url body headers) in
    blocks

  let get_followers (s : Session.session) (actor : string) (limit : int) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_followers_url = App.create_endpoint_url base_url (create_sync_endpoint "getFollowers") in
    let body = Cohttp_client.create_body_from_pairs [("actor", actor); ("limit", string_of_int limit)] in
    let followers = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_followers_url body headers) in
    followers

  let get_follows (s : Session.session) (actor : string) (limit : int) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_follows_url = App.create_endpoint_url base_url (create_sync_endpoint "getFollows") in
    let body = Cohttp_client.create_body_from_pairs [("actor", actor); ("limit", string_of_int limit)] in
    let follows = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_follows_url body headers) in
    follows

  let get_mutes (s : Session.session) (limit : int) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_mutes_url = App.create_endpoint_url base_url (create_sync_endpoint "getMutes") in
    let body = Cohttp_client.create_body_from_pairs [("limit", string_of_int limit)] in
    let mutes = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_mutes_url body headers) in
    mutes

  let mute_actor (s : Session.session) (actor : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_muted_actor_url = App.create_endpoint_url base_url (create_sync_endpoint "muteActor") in
    let body = Cohttp_client.create_body_from_pairs [("actor", actor)] in
    let muted_actor = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_muted_actor_url body headers) in
    muted_actor
    
end
