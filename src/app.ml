open Session
open Cohttp_client
open Auth

module App = struct
  let base_endpoint_from_env : string =
    let base_endpoint = try Sys.getenv "BASE_ENDPOINT" with Not_found -> "xrpc" in
    base_endpoint

  let get_profile (s : session) (actor : string) =
    let base_endpoint = base_endpoint_from_env in
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let get_profile_url = s.atp_host ^ "/" ^ base_endpoint ^ "app.bsky.actor.getProfile" in
    let body = Cohttp_client.create_body_from_pairs [("actor", actor)] in
    let results = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_profile_url body headers) in
    results
end
