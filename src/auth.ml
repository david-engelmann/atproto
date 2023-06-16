open Cohttp_client
module Auth = struct
    type auth =
        {
          exp : int;
          iat : int;
          scope : string;
          did : string;
          jti : string option;
        }
    let make_auth_token_request (username : string) (password : string) (personal_data_server : string) : string =
      let url = Printf.sprintf "http://%s/xrpc/com.atproto.server.createSession" personal_data_server in
      let data = Printf.sprintf "{\"identifier\": \"%s\", \"password\": \"%s\"}" username password in
      let body = Lwt_main.run (Cohttp_client.post_data url data) in
      body;
end
