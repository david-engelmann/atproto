open Cohttp_client
open App
open Session

module Server = struct
  let create_server_endpoint (query_name : string) : string =
    "com.atproto.server" ^ "." ^ query_name

  let describe_server (s : Session.session) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let describe_server_url = App.create_endpoint_url base_url (create_server_endpoint "describeServer") in
    let server_description = Lwt_main.run (Cohttp_client.get_request_with_headers describe_server_url headers) in
    server_description

  let create_account (s : Session.session) (handle : string) (email : string) (password : string) ?(invite_code : string) ?(recovery_key : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let create_account_url = App.create_endpoint_url base_url (create_server_endpoint "createAccount") in
    match invite_code with
    | Some actual_invite_code ->
        match recovery_key with
        | Some actual_recovery_key ->
            let data = Printf.sprintf "{\"email\": \"%s\", \"handle\": \"%s\", \"password\": \"%s\", \"inviteCode\": \"%s\", \"recoveryKey\": \"%s\"}" email handle password actual_invite_code actual_recovery_key in
        | None ->
            let data = Printf.sprintf "{\"email\": \"%s\", \"handle\": \"%s\", \"password\": \"%s\", \"inviteCode\": \"%s\"}" email handle password actual_invite_code in
    | None ->
        match recovery_key with
        | Some actual_recovery_key ->
            let data = Printf.sprintf "{\"email\": \"%s\", \"handle\": \"%s\", \"password\": \"%s\", \"recoveryKey\": \"%s\"}" email handle password actual_invite_code actual_recovery_key in
        | None ->
            let data = Printf.sprintf "{\"email\": \"%s\", \"handle\": \"%s\", \"password\": \"%s\"}" email handle password actual_invite_code in
    let created_account = Lwt_main.run (Cohttp_client.post_data_with_headers data create_account_url) in
    created_account

end

