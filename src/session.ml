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

    let create_session (username : string) (password: string) : unit =
        let atp_host : string = "bsky.social" in
        let session_auth = Auth.make_auth_token_request username password atp_host |> Auth.convert_body_to_json |> Auth.parse_auth in
        print_endline username;
        print_endline password;
        print_endline atp_host;
        print_endline session_auth.token;
        print_endline session_auth.did;
        (*{ username; password; atp_host; session_auth.token; session_auth.did }*)
end
