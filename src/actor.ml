open Session
open Cohttp_client
open App

module Actor = struct
  let create_actor_endpoint (query_name : string) : string =
    "app.bsky.actor" ^ "." ^ query_name

  let get_profile (s : Session.session) (actor : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_profile_url = App.create_endpoint_url base_url (create_actor_endpoint "getProfile") in
    let body = Cohttp_client.create_body_from_pairs [("actor", actor)] in
    let profile = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_profile_url body headers) in
    profile

  let get_profiles (s : Session.session) (actors : string list) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_profiles_url = App.create_endpoint_url base_url (create_actor_endpoint "getProfiles") in
    let body = Cohttp_client.add_query_params "actors" actors in
    let profiles = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_profiles_url body headers) in
    profiles

  let get_suggestions (s : Session.session) (limit : int) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_suggestions_url = App.create_endpoint_url base_url (create_actor_endpoint "getSuggestions") in
    let body = Cohttp_client.create_body_from_pairs [("limit", string_of_int limit)] in
    let suggestions = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_suggestions_url body headers) in
    suggestions

  let search_actors (s : Session.session) (term : string) (limit : int) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let search_actors_url = App.create_endpoint_url base_url (create_actor_endpoint "searchActors") in
    let body = Cohttp_client.create_body_from_pairs [("term", term); ("limit", string_of_int limit)] in
    let profiles = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers search_actors_url body headers) in
    profiles

  let search_actors_typeahead (s : Session.session) (term : string) (limit : int) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let search_actors_typeahead_url = App.create_endpoint_url base_url (create_actor_endpoint "searchActorsTypeahead") in
    let body = Cohttp_client.create_body_from_pairs [("term", term); ("limit", string_of_int limit)] in
    let profiles = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers search_actors_typeahead_url body headers) in
    profiles

end
