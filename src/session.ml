open Auth
open Cohttp_client
open Error

module Session = struct
  type session =
      {
        username : string;
        password : string;
        atp_host : string;
        auth : Auth.auth;
      }

  type session_request =
      {
        handle : string;
        did : string;
        email : string;
      }

  let parse_session_request json : session_request =
    let open Yojson.Safe.Util in
    let handle = json |> member "handle" |> to_string in
    let did = json |> member "did" |> to_string in
    let email = json |> member "email" |> to_string in
    { handle; did; email }

  let atp_host_from_env : string =
      let atp_host = try Sys.getenv "ATP_HOST" with Not_found -> "bsky.social" in
      atp_host

  let create_session (username : string) (password: string) : session =
    let atp_host = atp_host_from_env in
    let body = Auth.make_auth_token_request username password atp_host in
    let session_json = body |> Auth.convert_body_to_json in
    let session_error = session_json |> Auth.check_for_error in
    match session_error with
    | Some _ ->
        Error.handle_error (Error.parse_error session_json)
    | _ ->
        let session_auth = session_json |> Auth.parse_auth in
        { username; password; atp_host; auth=session_auth }


  let bearer_token_from_session (s : session) : (string * string) =
    let bearer_header = "Bearer " ^ s.auth.token in
    ("Authorization", bearer_header)

  let refresh_token_from_session (s : session) : (string * string) =
    let bearer_header = "Bearer " ^ Option.get s.auth.refresh_token in
    ("Authorization", bearer_header)

  let get_session_request (s : session) : string =
    let base_endpoint = Auth.get_base_endpoint in
    let get_session_endpoint = Auth.create_server_endpoint "getSession" in
    let get_session_url = Printf.sprintf "https://%s/%s%s" s.atp_host base_endpoint get_session_endpoint in
    let bearer_token = bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let session = Lwt_main.run (Cohttp_client.get_request_with_headers get_session_url headers) in
    session

  let refresh_session_auth (s : session) : session =
    if Auth.is_token_expired s.auth then
      let current_session = Yojson.Safe.from_string (get_session_request s) |> parse_session_request in
      let username = s.username in
      let password = s.password in
      let atp_host = s.atp_host in
      let body = Auth.refresh_auth_token_request s.auth.token (Option.get s.auth.refresh_token) current_session.handle current_session.did atp_host in
      let session_auth = body |> Auth.convert_body_to_json |> Auth.parse_auth in
      { username; password; atp_host; auth=session_auth }
    else
      s

  let delete_session (s : session) : string =
    let bearer_token = refresh_token_from_session s in
    let headers = Cohttp_client.create_headers_from_pairs [bearer_token] in
    let base_endpoint = Auth.get_base_endpoint in
    let delete_session_endpoint = Auth.create_server_endpoint "deleteSession" in
    let delete_session_url = Printf.sprintf "https://%s/%s%s" s.atp_host base_endpoint delete_session_endpoint in
    print_endline delete_session_url;
    let deleted_session = Lwt_main.run (Cohttp_client.post_request_with_headers delete_session_url headers) in
    Printf.printf "Delete Session: %s\n" deleted_session;
    deleted_session

end
