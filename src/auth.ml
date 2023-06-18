open Cohttp_client

open Jose.Jwt

module Auth = struct
    type auth =
        {
          exp : int;
          iat : int;
          scope : string;
          did : string;
          jti : string option;
          token : string;
        }

    let parse_auth json : auth =
      let open Yojson.Safe.Util in
      let token = json |> member "accessJwt" |> to_string in
      match unsafe_of_string token with
      | Ok jwt ->
        let claims = jwt.payload in
        let exp = claims |> member "exp" |> to_int in
        let iat = claims |> member "iat" |> to_int in
        let scope = claims |> member "scope" |> to_string in
        let did = claims |> member "sub" |> to_string in
        let jti =
          try
            let refresh_jwt = json |> member "refreshJwt" |> to_string in
            match unsafe_of_string refresh_jwt with
            | Ok jwt -> Some ( jwt.payload |> member "jti" |> to_string)
            | Error _ -> None
          with _ -> None
        in
        { exp; iat; scope; did; jti; token }
      | Error _ -> failwith "Invalid JWT token"

    let convert_body_to_json (body : string) : Yojson.Safe.t =
      let json = Yojson.Safe.from_string body in
      json

    let make_auth_token_request (username : string) (password : string) (personal_data_server : string) : string =
      let url = Printf.sprintf "https://%s/xrpc/com.atproto.server.createSession" personal_data_server in
      let data = Printf.sprintf "{\"identifier\": \"%s\", \"password\": \"%s\"}" username password in
      let body = Lwt_main.run (Cohttp_client.post_data url data) in
      body

    (*
    let is_token_expired (a : auth) : bool option =
      let expired_at = Ptime.of_float_s (float_of_int a.exp) |> Option.get in
      match expired_at with
      | None -> None
      | Some expired_at ->

        let expired_at = Ptime.add_span expired_at (Ptime.Span.of_int_s (-15 * 60)) in (* subtract 15 minutes *)
        let datetime_now = Unix.gettimeofday () |> Ptime.of_float_s |> Option.get in
        match datetime_now with
        | None -> None
        | Some datetime_now -> Some(Ptime.is_later datetime_now expired_at)
    *)
    let is_token_expired (a : auth) : bool =
      let expired_at = Ptime.of_float_s (float_of_int a.exp) |> Option.get in
      let expired_at = Ptime.add_span expired_at (Ptime.Span.of_int_s (-15 * 60)) |> Option.get in (* subtract 15 minutes *)
      let datetime_now = Unix.gettimeofday () |> Ptime.of_float_s |> Option.get in
      Ptime.is_later ~than:expired_at datetime_now

end
