open Auth

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
    Auth.print_auth s.auth;
    if Auth.is_token_expired s.auth then
      create_session s.username s.password
    else
      s
end
