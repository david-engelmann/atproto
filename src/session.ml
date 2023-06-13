module Session = struct
    type session =
        {
          username : string;
          password : string;
          atp_host : string;
          atp_auth_token : string;
          did : string;
        }

    let login (username : string) (password: string) : unit =
        let login_data : (string * string) list = [("identifier", username); ("password", password)] in
        let atp_host : string = "https://bsky.social" in
        (* resp = make_get_request(...)
         * atp_auth_token = resp.get("accessJwt")
         * did = resp.get("did")
         *)
        match login_data with
         | (key, _) :: _ ->
          print_endline key;
         | _ ->
          print_endline "NoKey";
        print_endline atp_host;
end
