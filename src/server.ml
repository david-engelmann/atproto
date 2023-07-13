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

  let create_account (s : Session.session) (handle : string) (email : string) ?invite_code ?recovery_key (password : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let create_account_url = App.create_endpoint_url base_url (create_server_endpoint "createAccount") in
    let data =
      match invite_code, recovery_key with
      | Some actual_invite_code, Some actual_recovery_key -> Printf.sprintf "{\"email\": \"%s\", \"handle\": \"%s\", \"password\": \"%s\", \"inviteCode\": \"%s\", \"recoveryKey\": \"%s\"}" email handle password actual_invite_code actual_recovery_key
      | Some actual_invite_code, None -> Printf.sprintf "{\"email\": \"%s\", \"handle\": \"%s\", \"password\": \"%s\", \"inviteCode\": \"%s\"}" email handle password actual_invite_code
      | None, Some actual_recovery_key -> Printf.sprintf "{\"email\": \"%s\", \"handle\": \"%s\", \"password\": \"%s\", \"recoveryKey\": \"%s\"}" email handle password actual_recovery_key
      | None, None -> Printf.sprintf "{\"email\": \"%s\", \"handle\": \"%s\", \"password\": \"%s\"}" email handle password
    in
    let created_account = Lwt_main.run (Cohttp_client.post_data_with_headers data create_account_url headers) in
    created_account

  let create_app_password (s : Session.session) (name : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let create_app_password_url = App.create_endpoint_url base_url (create_server_endpoint "createAppPassword") in
    let data = Printf.sprintf "{\"name\": \"%s\"}" name in
    let created_app_password = Lwt_main.run (Cohttp_client.post_data_with_headers data create_app_password_url headers) in
    created_app_password

  let get_account_invite_codes (s : Session.session) (include_used : bool) (create_available : bool) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_account_invite_codes_url = App.create_endpoint_url base_url (create_server_endpoint "getAccountInviteCodes") in
    let body = Cohttp_client.create_body_from_pairs [("includeUsed", string_of_bool include_used); ("createAvailable", string_of_bool create_available)] in
    let account_invite_codes = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_account_invite_codes_url body headers) in
    account_invite_codes

  let list_app_passwords (s : Session.session) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let list_app_passwords_url = App.create_endpoint_url base_url (create_server_endpoint "listAppPasswords") in
    let app_passwords = Lwt_main.run (Cohttp_client.get_request_with_headers list_app_passwords_url headers) in
    app_passwords

end

