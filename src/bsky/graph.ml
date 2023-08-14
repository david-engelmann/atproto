open Session
open Cohttp_client
open App
open Actor

module Graph = struct
  type followers =
    {
      subject : Actor.short_profile;
      followers : Actor.short_profile list;
    }

  type blocks =
    {
      blocks : Actor.block_profile list;
      cursor : string;
    }

  let create_graph_endpoint (query_name : string) : string =
    "app.bsky.graph" ^ "." ^ query_name

  let convert_body_to_json (body : string) : Yojson.Safe.t =
    let json = Yojson.Safe.from_string body in
    json

  let parse_followers json : followers =
    let open Yojson.Safe.Util in
    let subject = json |> member "subject" |> Actor.parse_short_profile in
    let followers = json |> member "followers" |> to_list |> List.map Actor.parse_short_profile in
    { subject; followers }

  let parse_blocks json : blocks =
    let open Yojson.Safe.Util in
    let blocks = json |> member "blocks" |> to_list |> List.map Actor.parse_block_profile in
    let cursor = json |> member "cursor" |> to_string in
    { blocks; cursor }

  let get_blocks (s : Session.session) (limit : int) : blocks =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_blocks_url = App.create_endpoint_url base_url (create_graph_endpoint "getBlocks") in
    let body = Cohttp_client.create_body_from_pairs [("limit", string_of_int limit)] in
    let blocks = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_blocks_url body headers) in
    blocks |> convert_body_to_json |> parse_blocks

  let get_followers (s : Session.session) (actor : string) (limit : int) : followers =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_followers_url = App.create_endpoint_url base_url (create_graph_endpoint "getFollowers") in
    let body = Cohttp_client.create_body_from_pairs [("actor", actor); ("limit", string_of_int limit)] in
    let followers = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_followers_url body headers) in
    followers |> convert_body_to_json |> parse_followers

  let get_follows (s : Session.session) (actor : string) (limit : int) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_follows_url = App.create_endpoint_url base_url (create_graph_endpoint "getFollows") in
    let body = Cohttp_client.create_body_from_pairs [("actor", actor); ("limit", string_of_int limit)] in
    let follows = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_follows_url body headers) in
    follows

  let get_mutes (s : Session.session) (limit : int) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_mutes_url = App.create_endpoint_url base_url (create_graph_endpoint "getMutes") in
    let body = Cohttp_client.create_body_from_pairs [("limit", string_of_int limit)] in
    let mutes = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_mutes_url body headers) in
    mutes

  let mute_actor (s : Session.session) (actor : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_muted_actor_url = App.create_endpoint_url base_url (create_graph_endpoint "muteActor") in
    let data = Printf.sprintf "{\"actor\": \"%s\"}" actor in
    let muted_actor = Lwt_main.run (Cohttp_client.post_data_with_headers get_muted_actor_url data headers) in
    muted_actor

  let unmute_actor (s : Session.session) (actor : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_unmuted_actor_url = App.create_endpoint_url base_url (create_graph_endpoint "unmuteActor") in
    let data = Printf.sprintf "{\"actor\": \"%s\"}" actor in
    let unmuted_actor = Lwt_main.run (Cohttp_client.post_data_with_headers get_unmuted_actor_url data headers) in
    unmuted_actor
end
