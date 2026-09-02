open Auth
open Cohttp_client
open Error

(** [com.atproto.server.createSession] / [getSession] and the live session record. *)
module Session = struct
  type session = {
    username : string;
    password : string;
    atp_host : string;
    auth : Auth.auth;
    did_doc : Yojson.Safe.t option;
  }

  (* com.atproto.server.getSession / createSession output extras. *)
  type session_request = {
    handle : string;
    did : string;
    email : string option;
    email_confirmed : bool option;
    email_auth_factor : bool option;
    active : bool option;
    status : string option;
    did_doc : Yojson.Safe.t option;
  }

  let parse_session_request json : session_request =
    let open Yojson.Safe.Util in
    let handle = json |> member "handle" |> to_string in
    let did = json |> member "did" |> to_string in
    let email =
      match json |> member "email" with `String s -> Some s | _ -> None
    in
    let email_confirmed =
      match json |> member "emailConfirmed" with `Bool b -> Some b | _ -> None
    in
    let email_auth_factor =
      match json |> member "emailAuthFactor" with
      | `Bool b -> Some b
      | _ -> None
    in
    let active =
      match json |> member "active" with `Bool b -> Some b | _ -> None
    in
    let status =
      match json |> member "status" with `String s -> Some s | _ -> None
    in
    let did_doc =
      match json |> member "didDoc" with
      | `Null | `String _ -> None
      | (`Assoc _ | `List _) as d -> Some d
      | _ -> None
    in
    {
      handle;
      did;
      email;
      email_confirmed;
      email_auth_factor;
      active;
      status;
      did_doc;
    }

  let atp_host_from_env : string =
    let atp_host =
      try Sys.getenv "ATP_HOST" with Not_found -> "bsky.social"
    in
    atp_host

  (** Create a password session ([com.atproto.server.createSession]) on
      [ATP_HOST] (default [bsky.social]). Optional [auth_factor_token] and
      [allow_takendown] map to the lexicon inputs. *)
  let create_session ?auth_factor_token ?allow_takendown (username : string)
      (password : string) : session =
    let atp_host = atp_host_from_env in
    let body =
      Auth.make_auth_token_request ?auth_factor_token ?allow_takendown username
        password atp_host
    in
    let json = Auth.convert_body_to_json body in
    let session_auth = Auth.parse_auth json in
    let did_doc =
      match Yojson.Safe.Util.member "didDoc" json with
      | `Assoc _ as d -> Some d
      | _ -> None
    in
    { username; password; atp_host; auth = session_auth; did_doc }

  (** [Authorization: Bearer] header pair from the session access JWT. *)
  let bearer_token_from_session (s : session) : string * string =
    let bearer_header = "Bearer " ^ s.auth.token in
    ("Authorization", bearer_header)

  let refresh_token_from_session (s : session) : string * string =
    let bearer_header = "Bearer " ^ Option.get s.auth.refresh_token in
    ("Authorization", bearer_header)

  let get_session_request (s : session) : string =
    let base_endpoint = Auth.get_base_endpoint in
    let get_session_endpoint = Auth.create_server_endpoint "getSession" in
    let get_session_url =
      Printf.sprintf "%s/%s%s"
        (Auth.origin_of_host s.atp_host)
        base_endpoint get_session_endpoint
    in
    let bearer_token = bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let session =
      Lwt_main.run
        (Cohttp_client.get_request_with_headers get_session_url headers)
    in
    session

  (** Current account info for [s] via [com.atproto.server.getSession]
      (handle, DID, email flags, active/status). *)
  let get_session (s : session) : session_request =
    Yojson.Safe.from_string (get_session_request s) |> parse_session_request

  (** Rotate JWTs via [com.atproto.server.refreshSession] using
      [refreshJwt]. Fails if the session has no refresh token. *)
  let refresh_session (s : session) : session =
    match s.auth.refresh_token with
    | None -> failwith "Session.refresh_session: missing refreshJwt"
    | Some refresh ->
        let body =
          Auth.refresh_auth_token_request s.auth.token refresh s.username
            s.auth.did s.atp_host
        in
        let json = Auth.convert_body_to_json body in
        (match Error.check_for_error json with
        | Some _ ->
            failwith
              ("Session.refresh_session: "
              ^ Error.to_string (Error.of_json json))
        | None -> ());
        let session_auth = Auth.parse_auth json in
        let did_doc =
          match Yojson.Safe.Util.member "didDoc" json with
          | `Assoc _ as d -> Some d
          | _ -> s.did_doc
        in
        { s with auth = session_auth; did_doc }

  (** Refresh [s] when [Auth.is_token_expired]; otherwise return [s]. *)
  let refresh_session_auth (s : session) : session =
    if Auth.is_token_expired s.auth then refresh_session s else s

  (** End the session via [com.atproto.server.deleteSession] (Bearer
      [refreshJwt]). *)
  let delete_session (s : session) : string =
    let bearer_token = refresh_token_from_session s in
    let headers = Cohttp_client.create_headers_from_pairs [ bearer_token ] in
    let base_endpoint = Auth.get_base_endpoint in
    let delete_session_endpoint = Auth.create_server_endpoint "deleteSession" in
    let delete_session_url =
      Printf.sprintf "%s/%s%s"
        (Auth.origin_of_host s.atp_host)
        base_endpoint delete_session_endpoint
    in
    Lwt_main.run
      (Cohttp_client.post_request_with_headers delete_session_url headers)
end
