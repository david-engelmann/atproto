open Cohttp_client

open Jose
(*open Jose.Jwt*)

module Auth = struct
    type auth =
        {
          exp : int;
          iat : int;
          scope : string;
          did : string;
          jti : string option;
        }

    let parse_auth json : auth =
      let open Yojson.Safe.Util in
      let jwt = json |> member "accessJwt" |> to_string in
      match unsafe_of_string jwt with
      | Ok jwt ->
        let claims = jwt.payload in
        let exp = claims |> member "exp" |> to_int in
        let iat = claims |> member "iat" |> to_int in
        let scope = claims |> member "scope" |> to_string in
        let did = claims |> member "sub" |> to_string in
        let jti = 
          try Some (json |> member "refreshJwt" |> to_string |> from_string |> member "jti" |> to_string)
          with _ -> None
        in
        { exp; iat; scope; did; jti }
      | Error _ -> failwith "Invalid JWT token"
    (*
    let parse_auth json : auth =
      let open Yojson.Basic.Util in
      let jwt = json |> member "accessJwt" |> Jwt.to_string in
      match Jwt.of_string jwt with
      | Ok jwt ->
        (* essentially *)
        (*
        let exp = Jwt.get_int_claim jwt "exp" in
        let iat = Jwt.get_int_claim jwt "iat" in
        let scope = Jwt.get_string_claim jwt "scope" in
        let did = Jwt.get_string_claim jwt "sub" in
        let jti = Jwt.get_string_claim jwt "refreshJwt" in
        *)
        (*let claims = Jwt.claims jwt in
        let exp = claims |> member "exp" |> to_int in
        let iat = claims |> member "iat" |> to_int in
        let scope = claims |> member "scope" |> to_string in
        let did = claims |> member "sub" |> to_string in
        let jti =
          try Some (json |> member "refreshJwt" |> to_string |> from_string |> member "jti" |> to_string)
          with _ -> None
        *)
        in
        { exp; iat; scope; did; jti }
      | Error _ -> failwith "Invalid JWT token"
    *)

    let make_auth_token_request (username : string) (password : string) (personal_data_server : string) : string =
      let url = Printf.sprintf "https://%s/xrpc/com.atproto.server.createSession" personal_data_server in
      let data = Printf.sprintf "{\"identifier\": \"%s\", \"password\": \"%s\"}" username password in
      let body = Lwt_main.run (Cohttp_client.post_data url data) in
      body;
end
