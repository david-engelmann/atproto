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

  let create_account_body ~handle ?email ?did ?invite_code ?verification_code
      ?verification_phone ?password ?recovery_key ?plc_op () : Yojson.Safe.t =
    let fields =
      ("handle", `String handle)
      :: (match email with Some s -> [ ("email", `String s) ] | None -> [])
      @ (match did with Some s -> [ ("did", `String s) ] | None -> [])
      @ (match invite_code with
        | Some s -> [ ("inviteCode", `String s) ]
        | None -> [])
      @ (match verification_code with
        | Some s -> [ ("verificationCode", `String s) ]
        | None -> [])
      @ (match verification_phone with
        | Some s -> [ ("verificationPhone", `String s) ]
        | None -> [])
      @ (match password with
        | Some s -> [ ("password", `String s) ]
        | None -> [])
      @ (match recovery_key with
        | Some s -> [ ("recoveryKey", `String s) ]
        | None -> [])
      @ match plc_op with Some v -> [ ("plcOp", v) ] | None -> []
    in
    `Assoc fields

  let create_account (s : Session.session) (handle : string) (email : string)
      ?invite_code ?recovery_key ?did ?verification_code ?verification_phone
      ?plc_op (password : string) : string =
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
      Yojson.Safe.to_string
        (create_account_body ~handle ~email ?did ?invite_code ?verification_code
           ?verification_phone ~password ?recovery_key ?plc_op ())
    in
    let created_account =
      Lwt_main.run
        (Cohttp_client.post_data_with_headers create_account_url data headers)
    in
    created_account

  let create_app_password_body ~name ?privileged () : Yojson.Safe.t =
    let fields =
      ("name", `String name)
      ::
      (match privileged with
      | Some b -> [ ("privileged", `Bool b) ]
      | None -> [])
    in
    `Assoc fields

  let create_app_password (s : Session.session) ?privileged (name : string) :
      string =
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
    let data =
      Yojson.Safe.to_string (create_app_password_body ~name ?privileged ())
    in
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
      (("aud", aud) :: (match lxm with Some n -> [ ("lxm", n) ] | None -> []))
      @ match exp with Some n -> [ ("exp", Int64.to_string n) ] | None -> []
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
        (match json |> member "expectedBlobs" with
        | `Int n -> Some n
        | _ -> None);
      imported_blobs =
        (match json |> member "importedBlobs" with
        | `Int n -> Some n
        | _ -> None);
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

  type server_links = {
    privacy_policy : string option;
    terms_of_service : string option;
  }

  type server_description = {
    did : string;
    available_user_domains : string list;
    invite_code_required : bool option;
    phone_verification_required : bool option;
    blob_upload_limit : int option;
    links : server_links;
    contact_email : string option;
  }

  let parse_describe_server json : server_description =
    let open Yojson.Safe.Util in
    let safe_member obj field =
      match obj with `Assoc _ -> obj |> member field | _ -> `Null
    in
    let links = json |> member "links" in
    let contact = json |> member "contact" in
    {
      did = (match json |> member "did" with `String s -> s | _ -> "");
      available_user_domains =
        (match json |> member "availableUserDomains" with
        | `List items ->
            List.filter_map (function `String s -> Some s | _ -> None) items
        | _ -> []);
      invite_code_required =
        (match json |> member "inviteCodeRequired" with
        | `Bool b -> Some b
        | _ -> None);
      phone_verification_required =
        (match json |> member "phoneVerificationRequired" with
        | `Bool b -> Some b
        | _ -> None);
      blob_upload_limit =
        (match json |> member "blobUploadLimit" with
        | `Int n -> Some n
        | _ -> None);
      links =
        {
          privacy_policy =
            (match safe_member links "privacyPolicy" with
            | `String s -> Some s
            | _ -> None);
          terms_of_service =
            (match safe_member links "termsOfService" with
            | `String s -> Some s
            | _ -> None);
        };
      contact_email =
        (match safe_member contact "email" with
        | `String s -> Some s
        | _ -> None);
    }

  let describe_server_parsed ?session ?host () : server_description =
    Client.Client.get_json ?session ?host "com.atproto.server.describeServer" []
    |> parse_describe_server

  let reserve_signing_key_body ?did () : Yojson.Safe.t =
    match did with Some d -> `Assoc [ ("did", `String d) ] | None -> `Assoc []

  type reserved_signing_key = { signing_key : string }

  let parse_reserved_signing_key json : reserved_signing_key =
    {
      signing_key =
        (match Yojson.Safe.Util.member "signingKey" json with
        | `String s -> s
        | _ -> "");
    }

  let reserve_signing_key ?session ?host ?did () : reserved_signing_key =
    Client.Client.post_json ?session ?host
      "com.atproto.server.reserveSigningKey"
      (Yojson.Safe.to_string (reserve_signing_key_body ?did ()))
    |> parse_reserved_signing_key

  let confirm_email_body ~email ~token : Yojson.Safe.t =
    `Assoc [ ("email", `String email); ("token", `String token) ]

  let update_email_body ~email ?token ?email_auth_factor () : Yojson.Safe.t =
    let fields =
      ("email", `String email)
      :: (match token with Some t -> [ ("token", `String t) ] | None -> [])
      @
      match email_auth_factor with
      | Some b -> [ ("emailAuthFactor", `Bool b) ]
      | None -> []
    in
    `Assoc fields

  let request_email_update_body () : Yojson.Safe.t = `Assoc []

  let confirm_email (s : Session.session) ~email ~token () : unit =
    ignore
      (Client.Client.post_json ~session:s "com.atproto.server.confirmEmail"
         (Yojson.Safe.to_string (confirm_email_body ~email ~token)))

  let request_email_confirmation (s : Session.session) : unit =
    ignore
      (Client.Client.post_json ~session:s
         "com.atproto.server.requestEmailConfirmation" "{}")

  type email_update = { token_required : bool }

  let parse_email_update json : email_update =
    {
      token_required =
        (match Yojson.Safe.Util.member "tokenRequired" json with
        | `Bool b -> b
        | _ -> false);
    }

  let request_email_update (s : Session.session) : email_update =
    Client.Client.post_json ~session:s "com.atproto.server.requestEmailUpdate"
      (Yojson.Safe.to_string (request_email_update_body ()))
    |> parse_email_update

  let update_email (s : Session.session) ~email ?token ?email_auth_factor () :
      unit =
    ignore
      (Client.Client.post_json ~session:s "com.atproto.server.updateEmail"
         (Yojson.Safe.to_string
            (update_email_body ~email ?token ?email_auth_factor ())))

  let deactivate_account_body ?delete_after () : Yojson.Safe.t =
    match delete_after with
    | Some t -> `Assoc [ ("deleteAfter", `String t) ]
    | None -> `Assoc []

  let deactivate_account (s : Session.session) ?delete_after () : unit =
    ignore
      (Client.Client.post_json ~session:s "com.atproto.server.deactivateAccount"
         (Yojson.Safe.to_string (deactivate_account_body ?delete_after ())))

  let activate_account (s : Session.session) : unit =
    ignore
      (Client.Client.post_json ~session:s "com.atproto.server.activateAccount"
         "{}")

  let check_account_status (s : Session.session) : account_status =
    Client.Client.get_json ~session:s "com.atproto.server.checkAccountStatus" []
    |> parse_account_status
end
