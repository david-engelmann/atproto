open Auth
open Cohttp_client

module Session = struct
  type session =
      {
        username : string;
        password : string;
        atp_host : string;
        auth : Auth.auth;
      }

  let atp_host_from_env : string =
      let atp_host = try Sys.getenv "ATP_HOST" with Not_found -> "bsky.social" in
      atp_host

  let create_session (username : string) (password: string) : session =
    let atp_host = atp_host_from_env in
    let body = Auth.make_auth_token_request username password atp_host in
    let session_auth = body |> Auth.convert_body_to_json |> Auth.parse_auth in
    { username; password; atp_host; auth=session_auth }

  let bearer_token_from_session (s : session) : (string * string) =
      let bearer_header = "Bearer " ^ s.auth.token in
      ("Authorization", bearer_header)

  let refresh_session_auth (s : session) : session =
    if Auth.is_token_expired s.auth then
      let username = s.username in
      let password = s.password in
      let atp_host = s.atp_host in
      let body = Auth.refresh_auth_token_request s.auth.token (Option.get s.auth.refresh_token) username s.auth.did atp_host in
      let session_auth = body |> Auth.convert_body_to_json |> Auth.parse_auth in
      { username; password; atp_host; auth=session_auth }
    else
      s

  let get_session_request (s : session) : string =
    let get_session_url = Printf.sprintf "https://%s/xrpc/com.atproto.server.getSession" s.atp_host in
    let bearer_token = bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let session = Lwt_main.run (Cohttp_client.get_request_with_headers get_session_url headers) in
    session
end
