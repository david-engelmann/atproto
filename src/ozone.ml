open Session
open Client
open Xrpc

(** tools.ozone.* — Ozone moderation client. Always send atproto-proxy. *)
module Ozone = struct
  let labeler_proxy (did : string) : Xrpc.proxy = Xrpc.labeler_proxy did
  let proxy_headers proxy = [ Xrpc.proxy_header proxy ]

  type subject_status = {
    subject : Yojson.Safe.t;
    subject_repo_handle : string option;
    updated_at : string option;
    created_at : string option;
    review_state : string option;
    comment : string option;
    priority_score : int option;
    original : Yojson.Safe.t;
  }

  type statuses = {
    cursor : string option;
    subject_statuses : subject_status list;
  }

  type mod_event = {
    id : int option;
    event : Yojson.Safe.t;
    subject : Yojson.Safe.t;
    created_by : string option;
    created_at : string option;
    original : Yojson.Safe.t;
  }

  type events = { cursor : string option; events : mod_event list }

  type repo_view = {
    did : string;
    handle : string option;
    related_records : Yojson.Safe.t list;
    indexed_at : string option;
    moderation : Yojson.Safe.t option;
    original : Yojson.Safe.t;
  }

  type record_view = {
    uri : string;
    cid : string option;
    value : Yojson.Safe.t option;
    original : Yojson.Safe.t;
  }

  type server_config = {
    viewer_role : string option;
    appview : string option;
    original : Yojson.Safe.t;
  }

  let parse_subject_status json : subject_status =
    {
      subject = Yojson.Safe.Util.member "subject" json;
      subject_repo_handle = Client.string_opt json "subjectRepoHandle";
      updated_at = Client.string_opt json "updatedAt";
      created_at = Client.string_opt json "createdAt";
      review_state = Client.string_opt json "reviewState";
      comment = Client.string_opt json "comment";
      priority_score = Client.int_opt json "priorityScore";
      original = json;
    }

  let parse_statuses json : statuses =
    {
      cursor = Client.string_opt json "cursor";
      subject_statuses =
        List.map parse_subject_status
          (Client.list_member json "subjectStatuses");
    }

  let parse_mod_event json : mod_event =
    {
      id = Client.int_opt json "id";
      event = Yojson.Safe.Util.member "event" json;
      subject = Yojson.Safe.Util.member "subject" json;
      created_by = Client.string_opt json "createdBy";
      created_at = Client.string_opt json "createdAt";
      original = json;
    }

  let parse_events json : events =
    {
      cursor = Client.string_opt json "cursor";
      events = List.map parse_mod_event (Client.list_member json "events");
    }

  let parse_repo json : repo_view =
    {
      did = Client.string_member json "did";
      handle = Client.string_opt json "handle";
      related_records = Client.list_member json "relatedRecords";
      indexed_at = Client.string_opt json "indexedAt";
      moderation =
        (match Yojson.Safe.Util.member "moderation" json with
        | `Null -> None
        | other -> Some other);
      original = json;
    }

  let parse_record json : record_view =
    {
      uri = Client.string_member json "uri";
      cid = Client.string_opt json "cid";
      value =
        (match Yojson.Safe.Util.member "value" json with
        | `Null -> None
        | other -> Some other);
      original = json;
    }

  let parse_server_config json : server_config =
    {
      viewer_role = Client.string_opt json "viewer";
      appview =
        (match Yojson.Safe.Util.member "appview" json with
        | `Assoc _ as a -> Client.string_opt a "url"
        | _ -> Client.string_opt json "appview");
      original = json;
    }

  let repo_ref did : Yojson.Safe.t =
    `Assoc
      [
        ("$type", `String "com.atproto.admin.defs#repoRef"); ("did", `String did);
      ]

  let strong_ref ~uri ~cid : Yojson.Safe.t =
    `Assoc
      [
        ("$type", `String "com.atproto.repo.strongRef");
        ("uri", `String uri);
        ("cid", `String cid);
      ]

  let emit_event_body ~event ~subject ~created_by ?subject_blob_cids
      ?external_id () : Yojson.Safe.t =
    let fields =
      [
        ("event", event); ("subject", subject); ("createdBy", `String created_by);
      ]
      @ (match subject_blob_cids with
        | Some cids ->
            [ ("subjectBlobCids", `List (List.map (fun c -> `String c) cids)) ]
        | None -> [])
      @
      match external_id with
      | Some id -> [ ("externalId", `String id) ]
      | None -> []
    in
    `Assoc fields

  let comment_event ?(sticky = false) comment : Yojson.Safe.t =
    `Assoc
      [
        ("$type", `String "tools.ozone.moderation.defs#modEventComment");
        ("comment", `String comment);
        ("sticky", `Bool sticky);
      ]

  let acknowledge_event ?comment () : Yojson.Safe.t =
    let fields =
      [ ("$type", `String "tools.ozone.moderation.defs#modEventAcknowledge") ]
      @ match comment with Some c -> [ ("comment", `String c) ] | None -> []
    in
    `Assoc fields

  let takedown_event ?comment ?(acknowledge_account_subjects = false) () :
      Yojson.Safe.t =
    let fields =
      [
        ("$type", `String "tools.ozone.moderation.defs#modEventTakedown");
        ("acknowledgeAccountSubjects", `Bool acknowledge_account_subjects);
      ]
      @ match comment with Some c -> [ ("comment", `String c) ] | None -> []
    in
    `Assoc fields

  let query_statuses (s : Session.session) ~proxy ?subject ?comment
      ?review_state ?limit ?cursor () : statuses =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.queryStatuses"
      (Client.opt_pair "subject" subject
      @ Client.opt_pair "comment" comment
      @ Client.opt_pair "reviewState" review_state
      @ Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor)
    |> parse_statuses

  let query_events (s : Session.session) ~proxy ?types ?created_by ?subject
      ?limit ?cursor () : events =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.queryEvents"
      (Client.repeat_param "types" (Option.value types ~default:[])
      @ Client.opt_pair "createdBy" created_by
      @ Client.opt_pair "subject" subject
      @ Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor)
    |> parse_events

  let emit_event (s : Session.session) ~proxy ~event ~subject ~created_by
      ?subject_blob_cids ?external_id () : mod_event =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.emitEvent"
      (Yojson.Safe.to_string
         (emit_event_body ~event ~subject ~created_by ?subject_blob_cids
            ?external_id ()))
    |> parse_mod_event

  let get_event (s : Session.session) ~proxy ~id () : mod_event =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.getEvent"
      [ ("id", string_of_int id) ]
    |> parse_mod_event

  let get_repo (s : Session.session) ~proxy ~did () : repo_view =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.getRepo"
      [ ("did", did) ]
    |> parse_repo

  let get_record (s : Session.session) ~proxy ~uri ?cid () : record_view =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.getRecord"
      (("uri", uri) :: Client.opt_pair "cid" cid)
    |> parse_record

  let get_config (s : Session.session) ~proxy () : server_config =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.server.getConfig" []
    |> parse_server_config
end
