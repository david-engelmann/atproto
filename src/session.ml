module Session = struct
    type session =
        {
          username : string;
          password : string;
          atp_host : string;
          atp_auth_token : string;
          did : string;
        }
end
