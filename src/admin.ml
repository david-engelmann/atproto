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

  type account_info = {
    did : string;
    handle : string;
    email : string option;
    indexed_at : string;
    invites_disabled : bool option;
    email_confirmed_at : string option;
    deactivated_at : string option;
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

  let parse_account_info json : account_info =
    {
      did = Client.string_member json "did";
      handle = Client.string_member json "handle";
      email = Client.string_opt json "email";
      indexed_at = Client.string_member json "indexedAt";
      invites_disabled = Client.bool_opt json "invitesDisabled";
      email_confirmed_at = Client.string_opt json "emailConfirmedAt";
      deactivated_at = Client.string_opt json "deactivatedAt";
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

  let enable_invites_body ~account ?note () : Yojson.Safe.t =
    let fields =
      ("account", `String account)
      :: (match note with Some n -> [ ("note", `String n) ] | None -> [])
    in
    `Assoc fields

  let disable_invites_body ~account ?note () : Yojson.Safe.t =
    enable_invites_body ~account ?note ()

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

  let get_subject_status (s : Session.session) ?did ?uri ?blob () :
      subject_status =
    Client.get_json ~session:s "com.atproto.admin.getSubjectStatus"
      (Client.opt_pair "did" did @ Client.opt_pair "uri" uri
      @ Client.opt_pair "blob" blob)
    |> parse_subject_status

  let update_subject_status (s : Session.session) ~subject ?takedown
      ?deactivated () : subject_status =
    Client.post_json ~session:s "com.atproto.admin.updateSubjectStatus"
      (Yojson.Safe.to_string
         (update_subject_status_body ~subject ?takedown ?deactivated ()))
    |> parse_subject_status

  let get_account_info (s : Session.session) ~did () : account_info =
    Client.get_json ~session:s "com.atproto.admin.getAccountInfo"
      [ ("did", did) ]
    |> parse_account_info

  let get_account_infos (s : Session.session) ~dids () : account_info list =
    Client.get_json ~session:s "com.atproto.admin.getAccountInfos"
      (Client.repeat_param "dids" dids)
    |> fun json ->
    List.map parse_account_info
      (match Client.list_member json "infos" with
      | [] -> Client.list_member json "accounts"
      | xs -> xs)

  let search_accounts (s : Session.session) ?email ?cursor () : accounts =
    Client.get_json ~session:s "com.atproto.admin.searchAccounts"
      (Client.opt_pair "email" email @ Client.opt_pair "cursor" cursor)
    |> parse_accounts

  let enable_account_invites (s : Session.session) ~account ?note () : unit =
    ignore
      (Client.post_json ~session:s "com.atproto.admin.enableAccountInvites"
         (Yojson.Safe.to_string (enable_invites_body ~account ?note ())))

  let disable_account_invites (s : Session.session) ~account ?note () : unit =
    ignore
      (Client.post_json ~session:s "com.atproto.admin.disableAccountInvites"
         (Yojson.Safe.to_string (disable_invites_body ~account ?note ())))

  let send_email (s : Session.session) ~recipient_did ~content ?subject
      ?sender_did () : Yojson.Safe.t =
    Client.post_json ~session:s "com.atproto.admin.sendEmail"
      (Yojson.Safe.to_string
         (send_email_body ~recipient_did ~content ?subject ?sender_did ()))
end
