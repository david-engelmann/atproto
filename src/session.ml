open Auth

module Session = struct
  type session =
      {
        username : string;
        password : string;
        atp_host : string;
        auth : Auth.auth;
      }

  let create_session (username : string) (password: string) : session =
    let atp_host = "bsky.social" in
    let body = Auth.make_auth_token_request username password atp_host in
    let session_auth = body |> Auth.convert_body_to_json |> Auth.parse_auth in
    { username; password; atp_host; auth=session_auth }

  let bearer_token_from_session (s : session) : (string * string) =
      let bearer_header = "Bearer " ^ s.atp_auth_token in
      ("Authorization", bearer_header)

  let refresh_session_auth (s : session) : session =
    if Auth.is_token_expired s.auth then
      create_session s.username s.password
    else
      s
end
