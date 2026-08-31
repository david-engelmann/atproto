open Cohttp_client
open App
open Session

module Server = struct
  let create_server_endpoint (query_name : string) : string =
    "com.atproto.server" ^ "." ^ query_name

  let describe_server (s : Session.session) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let describe_server_url =
      App.create_endpoint_url base_url (create_server_endpoint "describeServer")
    in
    let server_description =
      Lwt_main.run
        (Cohttp_client.get_request_with_headers describe_server_url headers)
    in
    server_description

  let create_account (s : Session.session) (handle : string) (email : string)
      ?invite_code ?recovery_key (password : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let create_account_url =
      App.create_endpoint_url base_url (create_server_endpoint "createAccount")
    in
    let data =
      match (invite_code, recovery_key) with
      | Some actual_invite_code, Some actual_recovery_key ->
          Printf.sprintf
            "{\"email\": \"%s\", \"handle\": \"%s\", \"password\": \"%s\", \
             \"inviteCode\": \"%s\", \"recoveryKey\": \"%s\"}"
            email handle password actual_invite_code actual_recovery_key
      | Some actual_invite_code, None ->
          Printf.sprintf
            "{\"email\": \"%s\", \"handle\": \"%s\", \"password\": \"%s\", \
             \"inviteCode\": \"%s\"}"
            email handle password actual_invite_code
      | None, Some actual_recovery_key ->
          Printf.sprintf
            "{\"email\": \"%s\", \"handle\": \"%s\", \"password\": \"%s\", \
             \"recoveryKey\": \"%s\"}"
            email handle password actual_recovery_key
      | None, None ->
          Printf.sprintf
            "{\"email\": \"%s\", \"handle\": \"%s\", \"password\": \"%s\"}"
            email handle password
    in
    let created_account =
      Lwt_main.run
        (Cohttp_client.post_data_with_headers create_account_url data headers)
    in
    created_account

  let create_app_password (s : Session.session) (name : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let create_app_password_url =
      App.create_endpoint_url base_url
        (create_server_endpoint "createAppPassword")
    in
    let data = Printf.sprintf "{\"name\": \"%s\"}" name in
    let created_app_password =
      Lwt_main.run
        (Cohttp_client.post_data_with_headers create_app_password_url data
           headers)
    in
    created_app_password

  let get_account_invite_codes (s : Session.session) (include_used : bool)
      (create_available : bool) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let get_account_invite_codes_url =
      App.create_endpoint_url base_url
        (create_server_endpoint "getAccountInviteCodes")
    in
    let body =
      Cohttp_client.create_body_from_pairs
        [
          ("includeUsed", string_of_bool include_used);
          ("createAvailable", string_of_bool create_available);
        ]
    in
    let account_invite_codes =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers
           get_account_invite_codes_url body headers)
    in
    account_invite_codes

  let create_invite_code (s : Session.session) (use_count : int) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let create_invite_code_url =
      App.create_endpoint_url base_url
        (create_server_endpoint "createInviteCode")
    in
    let body =
      Cohttp_client.create_body_from_pairs
        [ ("useCount", string_of_int use_count) ]
    in
    let account_invite_code =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers create_invite_code_url
           body headers)
    in
    account_invite_code

  let create_invite_codes (s : Session.session) (code_count : int)
      (use_count : int) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let create_invite_codes_url =
      App.create_endpoint_url base_url
        (create_server_endpoint "createInviteCodes")
    in
    let body =
      Cohttp_client.create_body_from_pairs
        [
          ("codeCount", string_of_int code_count);
          ("useCount", string_of_int use_count);
        ]
    in
    let account_invite_codes =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers create_invite_codes_url
           body headers)
    in
    account_invite_codes

  let list_app_passwords (s : Session.session) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let list_app_passwords_url =
      App.create_endpoint_url base_url
        (create_server_endpoint "listAppPasswords")
    in
    let app_passwords =
      Lwt_main.run
        (Cohttp_client.get_request_with_headers list_app_passwords_url headers)
    in
    app_passwords

  let request_account_delete (s : Session.session) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let request_account_delete_url =
      App.create_endpoint_url base_url
        (create_server_endpoint "requestAccountDelete")
    in
    let account_delete =
      Lwt_main.run
        (Cohttp_client.get_request_with_headers request_account_delete_url
           headers)
    in
    account_delete

  let request_password_reset (s : Session.session) (email : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let request_password_reset_url =
      App.create_endpoint_url base_url
        (create_server_endpoint "requestPasswordReset")
    in
    let body = Cohttp_client.create_body_from_pairs [ ("email", email) ] in
    let password_reset =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers
           request_password_reset_url body headers)
    in
    password_reset

  let delete_account (s : Session.session) (did : string) (password : string)
      (token : string) =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let delete_account_url =
      App.create_endpoint_url base_url (create_server_endpoint "deleteAccount")
    in
    let body =
      Cohttp_client.create_body_from_pairs
        [ ("did", did); ("password", password); ("token", token) ]
    in
    let delete_account =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers delete_account_url body
           headers)
    in
    delete_account

  let reset_password (s : Session.session) (token : string) (password : string)
      : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let reset_password_url =
      App.create_endpoint_url base_url (create_server_endpoint "resetPassword")
    in
    let body =
      Cohttp_client.create_body_from_pairs
        [ ("token", token); ("password", password) ]
    in
    let reset_password =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers reset_password_url body
           headers)
    in
    reset_password

  let revoke_app_password (s : Session.session) (name : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let revoke_app_password_url =
      App.create_endpoint_url base_url
        (create_server_endpoint "revokeAppPassword")
    in
    let body = Cohttp_client.create_body_from_pairs [ ("name", name) ] in
    let revoke_app_password =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers revoke_app_password_url
           body headers)
    in
    revoke_app_password

  type service_auth = { token : string }

  let parse_service_auth json : service_auth =
    let open Yojson.Safe.Util in
    { token = json |> member "token" |> to_string }

  let get_service_auth_url ~aud ?lxm ?exp (s : Session.session) : string =
    let pairs =
      ("aud", aud)
      :: (match lxm with Some n -> [ ("lxm", n) ] | None -> [])
      @
      match exp with
      | Some n -> [ ("exp", Int64.to_string n) ]
      | None -> []
    in
    let base =
      App.create_endpoint_url (App.create_base_url s)
        (create_server_endpoint "getServiceAuth")
    in
    let qs = Cohttp_client.create_body_from_pairs pairs in
    if qs = "" then base else base ^ "?" ^ qs

  let get_service_auth (s : Session.session) ~aud ?lxm ?exp () : service_auth =
    let _ = Xrpc.Xrpc.service_auth_body ~aud ?lxm ?exp () in
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let url = get_service_auth_url ~aud ?lxm ?exp s in
    let body =
      Lwt_main.run (Cohttp_client.get_request_with_headers url headers)
    in
    parse_service_auth (Yojson.Safe.from_string body)

  type account_status = {
    activated : bool option;
    valid_did : bool option;
    expected_blobs : int option;
    imported_blobs : int option;
  }

  let parse_account_status json : account_status =
    let open Yojson.Safe.Util in
    {
      activated =
        (match json |> member "activated" with `Bool b -> Some b | _ -> None);
      valid_did =
        (match json |> member "validDid" with `Bool b -> Some b | _ -> None);
      expected_blobs =
        (match json |> member "expectedBlobs" with `Int n -> Some n | _ -> None);
      imported_blobs =
        (match json |> member "importedBlobs" with `Int n -> Some n | _ -> None);
    }

  let deactivate_account_url (s : Session.session) : string =
    App.create_endpoint_url (App.create_base_url s)
      (create_server_endpoint "deactivateAccount")

  let activate_account_url (s : Session.session) : string =
    App.create_endpoint_url (App.create_base_url s)
      (create_server_endpoint "activateAccount")

  let check_account_status_url (s : Session.session) : string =
    App.create_endpoint_url (App.create_base_url s)
      (create_server_endpoint "checkAccountStatus")
end
