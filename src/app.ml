open Session
open Cohttp_client

module App = struct
  let base_endpoint_from_env : string =
    let base_endpoint = try Sys.getenv "BASE_ENDPOINT" with Not_found -> "xrpc" in
    base_endpoint

  let get_base_endpoint : string =
    let base_endpoint = base_endpoint_from_env in
    let base_endpoint = if String.get base_endpoint (String.length base_endpoint - 1) = '/' then base_endpoint else base_endpoint ^ "/" in
    base_endpoint

  let create_endpoint_url (url : string) (endpoint : string) : string =
    let url = if String.get url (String.length url - 1) = '/' then url else url ^ "/" in
    url ^ endpoint

  let create_base_url (s : Session.session) : string =
    let base_endpoint = get_base_endpoint in
    "https://" ^ s.atp_host ^ "/" ^ base_endpoint

  let get_profile (s : Session.session) (actor : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = create_base_url s in
    let get_profile_url = create_endpoint_url base_url "app.bsky.actor.getProfile" in
    let body = Cohttp_client.create_body_from_pairs [("actor", actor)] in
    let profile = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_profile_url body headers) in
    profile

  let get_profiles (s : Session.session) (actors : string list) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = create_base_url s in
    let get_profiles_url = create_endpoint_url base_url "app.bsky.actor.getProfiles" in
    let body = Cohttp_client.add_query_params "actors" actors in
    let profiles = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_profiles_url body headers) in
    profiles

  let get_suggestions (s : Session.session) (limit : int) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = create_base_url s in
    let get_suggestions_url = create_endpoint_url base_url "app.bsky.actor.getSuggestions" in
    let body = Cohttp_client.create_body_from_pairs [("limit", string_of_int limit)] in
    let suggestions = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_suggestions_url body headers) in
    suggestions

end
