open Cohttp_client
open App
open Session

module Server = struct
  let create_server_endpoint (query_name : string) : string =
    "com.atproto.server" ^ "." ^ query_name

  let describe_server (s : Session.session) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let describe_server_url = App.create_endpoint_url base_url (create_server_endpoint "describeServer") in
    let server_description = Lwt_main.run (Cohttp_client.get_request_with_headers describe_server_url headers) in
    server_description
end

