module Session = struct
    type session =
        {
          username : string;
          password : string;
          atp_host : string;
          atp_auth_token : string;
          did : string;
        }

    let login (username : string) (password: string) : session =
        let login_data (string, string) list = [("identifier", username); ("password", password)] in
        
end
