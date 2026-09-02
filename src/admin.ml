open Session
open Client

(** com.atproto.admin — protocol-client admin XRPC (not a hosted PDS). *)
module Admin = struct
  type status_attr = { applied : bool; ref_ : string option }

  type subject =
    | Repo of { did : string }
    | Record of { uri : string; cid : string }
    | Blob of { did : string; cid : string; record_uri : string option }

  type subject_status = {
    subject : subject;
    takedown : status_attr option;
    deactivated : status_attr option;
    original : Yojson.Safe.t;
  }

  type threat_signature = { property : string; value : string }

  type account_info = {
    did : string;
    handle : string;
    email : string option;
    indexed_at : string;
    invites_disabled : bool option;
    email_confirmed_at : string option;
    deactivated_at : string option;
    invite_note : string option;
    invited_by_code : string option;
    threat_signatures : threat_signature list;
    original : Yojson.Safe.t;
  }

  type accounts = { cursor : string option; accounts : account_info list }

  let parse_status_attr json : status_attr =
    {
      applied = Client.bool_member json "applied";
      ref_ = Client.string_opt json "ref";
    }

  let status_attr_json (s : status_attr) : Yojson.Safe.t =
    let fields =
      ("applied", `Bool s.applied)
      :: (match s.ref_ with Some r -> [ ("ref", `String r) ] | None -> [])
    in
    `Assoc fields

  let parse_subject json : subject =
    let ty = Client.string_opt json "$type" in
    match ty with
    | Some t when t = "com.atproto.repo.strongRef" ->
        Record
          {
            uri = Client.string_member json "uri";
            cid = Client.string_member json "cid";
          }
    | Some t when t = "com.atproto.admin.defs#repoBlobRef" ->
        Blob
          {
            did = Client.string_member json "did";
            cid = Client.string_member json "cid";
            record_uri = Client.string_opt json "recordUri";
          }
    | _ -> Repo { did = Client.string_member json "did" }

  let subject_json = function
    | Repo { did } ->
        `Assoc
          [
            ("$type", `String "com.atproto.admin.defs#repoRef");
            ("did", `String did);
          ]
    | Record { uri; cid } ->
        `Assoc
          [
            ("$type", `String "com.atproto.repo.strongRef");
            ("uri", `String uri);
            ("cid", `String cid);
          ]
    | Blob { did; cid; record_uri } ->
        `Assoc
          ([
             ("$type", `String "com.atproto.admin.defs#repoBlobRef");
             ("did", `String did);
             ("cid", `String cid);
           ]
          @
          match record_uri with
          | Some u -> [ ("recordUri", `String u) ]
          | None -> [])

  let parse_subject_status json : subject_status =
    {
      subject = parse_subject (Yojson.Safe.Util.member "subject" json);
      takedown =
        (match Yojson.Safe.Util.member "takedown" json with
        | `Assoc _ as t -> Some (parse_status_attr t)
        | _ -> None);
      deactivated =
        (match Yojson.Safe.Util.member "deactivated" json with
        | `Assoc _ as d -> Some (parse_status_attr d)
        | _ -> None);
      original = json;
    }

  let parse_threat_signature json : threat_signature =
    {
      property = Client.string_member json "property";
      value = Client.string_member json "value";
    }

  let parse_account_info json : account_info =
    let invited_by_code =
      match Yojson.Safe.Util.member "invitedBy" json with
      | `Assoc _ as inv -> Client.string_opt inv "code"
      | `String s -> Some s
      | _ -> None
    in
    {
      did = Client.string_member json "did";
      handle = Client.string_member json "handle";
      email = Client.string_opt json "email";
      indexed_at = Client.string_member json "indexedAt";
      invites_disabled = Client.bool_opt json "invitesDisabled";
      email_confirmed_at = Client.string_opt json "emailConfirmedAt";
      deactivated_at = Client.string_opt json "deactivatedAt";
      invite_note = Client.string_opt json "inviteNote";
      invited_by_code;
      threat_signatures =
        List.map parse_threat_signature
          (Client.list_member json "threatSignatures");
      original = json;
    }

  let parse_accounts json : accounts =
    {
      cursor = Client.string_opt json "cursor";
      accounts =
        List.map parse_account_info
          (match Client.list_member json "accounts" with
          | [] -> Client.list_member json "infos"
          | xs -> xs);
    }

  let update_subject_status_body ~subject ?takedown ?deactivated () :
      Yojson.Safe.t =
    let fields =
      [ ("subject", subject_json subject) ]
      @ (match takedown with
        | Some t -> [ ("takedown", status_attr_json t) ]
        | None -> [])
      @
      match deactivated with
      | Some d -> [ ("deactivated", status_attr_json d) ]
      | None -> []
    in
    `Assoc fields

  (** JSON body for [com.atproto.admin.enableAccountInvites]. *)
  let enable_invites_body ~account ?note () : Yojson.Safe.t =
    let fields =
      ("account", `String account)
      :: (match note with Some n -> [ ("note", `String n) ] | None -> [])
    in
    `Assoc fields

  (** JSON body for [com.atproto.admin.disableAccountInvites]. *)
  let disable_invites_body ~account ?note () : Yojson.Safe.t =
    enable_invites_body ~account ?note ()

  (** JSON body for [com.atproto.admin.sendEmail]. *)
  let send_email_body ~recipient_did ~content ?subject ?sender_did () :
      Yojson.Safe.t =
    let fields =
      [ ("recipientDid", `String recipient_did); ("content", `String content) ]
      @ (match subject with Some s -> [ ("subject", `String s) ] | None -> [])
      @
      match sender_did with
      | Some d -> [ ("senderDid", `String d) ]
      | None -> []
    in
    `Assoc fields

  (** Subject status (takedown / deactivated) via
      [com.atproto.admin.getSubjectStatus]. Pass [did], [uri], or [blob]. *)
  let get_subject_status (s : Session.session) ?did ?uri ?blob () :
      subject_status =
    Client.get_json ~session:s "com.atproto.admin.getSubjectStatus"
      (Client.opt_pair "did" did @ Client.opt_pair "uri" uri
      @ Client.opt_pair "blob" blob)
    |> parse_subject_status

  (** Update takedown / deactivated flags via
      [com.atproto.admin.updateSubjectStatus]. *)
  let update_subject_status (s : Session.session) ~subject ?takedown
      ?deactivated () : subject_status =
    Client.post_json ~session:s "com.atproto.admin.updateSubjectStatus"
      (Yojson.Safe.to_string
         (update_subject_status_body ~subject ?takedown ?deactivated ()))
    |> parse_subject_status

  (** Account view for [did] via [com.atproto.admin.getAccountInfo]. *)
  let get_account_info (s : Session.session) ~did () : account_info =
    Client.get_json ~session:s "com.atproto.admin.getAccountInfo"
      [ ("did", did) ]
    |> parse_account_info

  (** Account views for [dids] via [com.atproto.admin.getAccountInfos]. *)
  let get_account_infos (s : Session.session) ~dids () : account_info list =
    Client.get_json ~session:s "com.atproto.admin.getAccountInfos"
      (Client.repeat_param "dids" dids)
    |> fun json ->
    List.map parse_account_info
      (match Client.list_member json "infos" with
      | [] -> Client.list_member json "accounts"
      | xs -> xs)

  (** Search accounts via [com.atproto.admin.searchAccounts]. Optional
      [email] / [cursor] map to the lexicon query. *)
  let search_accounts (s : Session.session) ?email ?cursor () : accounts =
    Client.get_json ~session:s "com.atproto.admin.searchAccounts"
      (Client.opt_pair "email" email @ Client.opt_pair "cursor" cursor)
    |> parse_accounts

  (** Enable invites for [account] via
      [com.atproto.admin.enableAccountInvites]. *)
  let enable_account_invites (s : Session.session) ~account ?note () : unit =
    ignore
      (Client.post_json ~session:s "com.atproto.admin.enableAccountInvites"
         (Yojson.Safe.to_string (enable_invites_body ~account ?note ())))

  (** Disable invites for [account] via
      [com.atproto.admin.disableAccountInvites]. *)
  let disable_account_invites (s : Session.session) ~account ?note () : unit =
    ignore
      (Client.post_json ~session:s "com.atproto.admin.disableAccountInvites"
         (Yojson.Safe.to_string (disable_invites_body ~account ?note ())))

  (** Send an admin email via [com.atproto.admin.sendEmail]. *)
  let send_email (s : Session.session) ~recipient_did ~content ?subject
      ?sender_did () : Yojson.Safe.t =
    Client.post_json ~session:s "com.atproto.admin.sendEmail"
      (Yojson.Safe.to_string
         (send_email_body ~recipient_did ~content ?subject ?sender_did ()))

  type invite_code_use = { used_by : string; used_at : string }

  type invite_code = {
    code : string;
    available : int;
    disabled : bool;
    for_account : string;
    created_by : string;
    created_at : string;
    uses : invite_code_use list;
    original : Yojson.Safe.t;
  }

  type invite_codes = { cursor : string option; codes : invite_code list }

  let parse_invite_code_use json : invite_code_use =
    {
      used_by = Client.string_member json "usedBy";
      used_at = Client.string_member json "usedAt";
    }

  let parse_invite_code json : invite_code =
    {
      code = Client.string_member json "code";
      available = Client.int_member json "available";
      disabled = Client.bool_member json "disabled";
      for_account = Client.string_member json "forAccount";
      created_by = Client.string_member json "createdBy";
      created_at = Client.string_member json "createdAt";
      uses = List.map parse_invite_code_use (Client.list_member json "uses");
      original = json;
    }

  let parse_invite_codes json : invite_codes =
    {
      cursor = Client.string_opt json "cursor";
      codes = List.map parse_invite_code (Client.list_member json "codes");
    }

  (** JSON body for [com.atproto.admin.disableInviteCodes]. *)
  let disable_invite_codes_body ?(codes = []) ?(accounts = []) () :
      Yojson.Safe.t =
    let fields =
      (match codes with
      | [] -> []
      | xs -> [ ("codes", `List (List.map (fun s -> `String s) xs)) ])
      @
      match accounts with
      | [] -> []
      | xs -> [ ("accounts", `List (List.map (fun s -> `String s) xs)) ]
    in
    `Assoc fields

  (** JSON body for [com.atproto.admin.deleteAccount]. *)
  let delete_account_body ~did () : Yojson.Safe.t =
    `Assoc [ ("did", `String did) ]

  (** JSON body for [com.atproto.admin.updateAccountEmail]. *)
  let update_account_email_body ~account ~email () : Yojson.Safe.t =
    `Assoc [ ("account", `String account); ("email", `String email) ]

  (** JSON body for [com.atproto.admin.updateAccountHandle]. *)
  let update_account_handle_body ~did ~handle () : Yojson.Safe.t =
    `Assoc [ ("did", `String did); ("handle", `String handle) ]

  (** JSON body for [com.atproto.admin.updateAccountPassword]. *)
  let update_account_password_body ~did ~password () : Yojson.Safe.t =
    `Assoc [ ("did", `String did); ("password", `String password) ]

  (** JSON body for [com.atproto.admin.updateAccountSigningKey]. *)
  let update_account_signing_key_body ~did ~signing_key () : Yojson.Safe.t =
    `Assoc [ ("did", `String did); ("signingKey", `String signing_key) ]

  (** Invite codes via [com.atproto.admin.getInviteCodes]. Optional
      [sort] / [limit] / [cursor] map to the lexicon query. *)
  let get_invite_codes (s : Session.session) ?sort ?limit ?cursor () :
      invite_codes =
    Client.get_json ~session:s "com.atproto.admin.getInviteCodes"
      (Client.opt_pair "sort" sort
      @ Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor)
    |> parse_invite_codes

  (** Disable invite [codes] or all codes for [accounts] via
      [com.atproto.admin.disableInviteCodes]. *)
  let disable_invite_codes (s : Session.session) ?(codes = []) ?(accounts = [])
      () : unit =
    ignore
      (Client.post_json ~session:s "com.atproto.admin.disableInviteCodes"
         (Yojson.Safe.to_string (disable_invite_codes_body ~codes ~accounts ())))

  (** Delete [did] via [com.atproto.admin.deleteAccount]. *)
  let delete_account (s : Session.session) ~did () : unit =
    ignore
      (Client.post_json ~session:s "com.atproto.admin.deleteAccount"
         (Yojson.Safe.to_string (delete_account_body ~did ())))

  (** Update [account] email via [com.atproto.admin.updateAccountEmail]. *)
  let update_account_email (s : Session.session) ~account ~email () : unit =
    ignore
      (Client.post_json ~session:s "com.atproto.admin.updateAccountEmail"
         (Yojson.Safe.to_string (update_account_email_body ~account ~email ())))

  (** Update [did] handle via [com.atproto.admin.updateAccountHandle]. *)
  let update_account_handle (s : Session.session) ~did ~handle () : unit =
    ignore
      (Client.post_json ~session:s "com.atproto.admin.updateAccountHandle"
         (Yojson.Safe.to_string (update_account_handle_body ~did ~handle ())))

  (** Update [did] password via [com.atproto.admin.updateAccountPassword]. *)
  let update_account_password (s : Session.session) ~did ~password () : unit =
    ignore
      (Client.post_json ~session:s "com.atproto.admin.updateAccountPassword"
         (Yojson.Safe.to_string
            (update_account_password_body ~did ~password ())))

  (** Update [did] signing key via
      [com.atproto.admin.updateAccountSigningKey]. *)
  let update_account_signing_key (s : Session.session) ~did ~signing_key () :
      unit =
    ignore
      (Client.post_json ~session:s "com.atproto.admin.updateAccountSigningKey"
         (Yojson.Safe.to_string
            (update_account_signing_key_body ~did ~signing_key ())))
end
