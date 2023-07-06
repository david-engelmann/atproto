open Cohttp_client

open Jose.Jwt

module Server = struct
    let create_server_endpoint (query_name : string) : string =
      "com.atproto.server" ^ "." ^ query_name
end
