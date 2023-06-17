open Auth

module Session = struct
    type session =
        {
          username : string;
          password : string;
          atp_host : string;
          atp_auth_token : string;
          did : string;
        }

    let create_session (username : string) (password: string) : session =
      let atp_host = "bsky.social" in
      let body = Auth.make_auth_token_request username password atp_host in
      let session_auth = body |> Auth.convert_body_to_json |> Auth.parse_auth in
      let atp_auth_token = session_auth.token in
      let did = session_auth.did in
      { username; password; atp_host; atp_auth_token; did }

    let bearer_token_from_session (s : session) : (string * string) =
        let bearer_header = "Bearer " ^ s.atp_auth_token in
        ("Authorization", bearer_header)

end
