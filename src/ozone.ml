open Session
open Client
open Xrpc
open Actor

(** tools.ozone.* — Ozone moderation client.
    Password [at+jwt] sessions send [atproto-proxy] through the PDS.
    OAuth DPoP cannot be proxied: mint [getServiceAuth] ([aud] = Ozone DID)
    and call the Ozone host with that JWT ([emit_event_service] / …). *)
module Ozone = struct
  (** [atproto-proxy] for Ozone ([did#atproto_labeler]). *)
  let labeler_proxy (did : string) : Xrpc.proxy = Xrpc.labeler_proxy did

  (** [atproto-proxy] header list for [proxy]. *)
  let proxy_headers proxy = [ Xrpc.proxy_header proxy ]

  type repo_ref = { did : string }
  type strong_ref = { uri : string; cid : string }
  type message_ref = { did : string; convo_id : string; message_id : string }
  type convo_ref = { did : string; convo_id : string }

  type subject =
    [ `Repo_ref of repo_ref
    | `Strong_ref of strong_ref
    | `Message_ref of message_ref
    | `Convo_ref of convo_ref
    | `Unknown of Yojson.Safe.t ]

  type comment_event = { comment : string; sticky : bool option }

  type acknowledge_event = {
    comment : string option;
    acknowledge_account_subjects : bool option;
  }

  type takedown_event = {
    comment : string option;
    duration_in_hours : int option;
    acknowledge_account_subjects : bool option;
    policies : string list;
  }

  type comment_only_event = { comment : string option }
  type report_event = { comment : string option; report_type : string }

  type label_event = {
    comment : string option;
    create_label_vals : string list;
    negate_label_vals : string list;
    duration_in_hours : int option;
  }

  type mute_event = { comment : string option; duration_in_hours : int option }

  type email_event = {
    subject_line : string;
    content : string option;
    comment : string option;
  }

  type tag_event = {
    add : string list;
    remove : string list;
    comment : string option;
  }

  type priority_score_event = { comment : string option; score : int }
  type unknown_event = { type_ : string; original : Yojson.Safe.t }

  type mute_reporter_event = {
    comment : string option;
    duration_in_hours : int option;
  }

  type divert_event = { comment : string option }

  type account_lifecycle_event = {
    comment : string option;
    active : bool;
    status : string option;
    timestamp : string;
  }

  type identity_lifecycle_event = {
    comment : string option;
    handle : string option;
    pds_host : string option;
    tombstone : bool option;
    timestamp : string;
  }

  type record_lifecycle_event = {
    comment : string option;
    op : string;
    cid : string option;
    timestamp : string;
  }

  type age_assurance_event = {
    created_at : string;
    attempt_id : string;
    status : string;
    country_code : string option;
    region_code : string option;
    init_ip : string option;
    init_ua : string option;
    complete_ip : string option;
    complete_ua : string option;
  }

  type age_assurance_override_event = { comment : string; status : string }
  type comment_required_event = { comment : string }

  type schedule_takedown_event = {
    comment : string option;
    execute_at : string option;
    execute_after : string option;
    execute_until : string option;
  }

  type mod_tool = { name : string; meta : Yojson.Safe.t option }

  type event =
    [ `Comment of comment_event
    | `Acknowledge of acknowledge_event
    | `Takedown of takedown_event
    | `Reverse_takedown of comment_only_event
    | `Report of report_event
    | `Label of label_event
    | `Escalate of comment_only_event
    | `Mute of mute_event
    | `Unmute of comment_only_event
    | `Mute_reporter of mute_reporter_event
    | `Unmute_reporter of comment_only_event
    | `Email of email_event
    | `Tag of tag_event
    | `Resolve_appeal of comment_only_event
    | `Priority_score of priority_score_event
    | `Divert of divert_event
    | `Account of account_lifecycle_event
    | `Identity of identity_lifecycle_event
    | `Record of record_lifecycle_event
    | `Age_assurance of age_assurance_event
    | `Age_assurance_override of age_assurance_override_event
    | `Age_assurance_purge of comment_required_event
    | `Revoke_account_credentials of comment_required_event
    | `Schedule_takedown of schedule_takedown_event
    | `Cancel_scheduled_takedown of comment_only_event
    | `Unknown of unknown_event ]

  type subject_status = {
    id : int option;
    subject : subject;
    subject_repo_handle : string option;
    updated_at : string option;
    created_at : string option;
    review_state : string option;
    comment : string option;
    priority_score : int option;
    hosting : Yojson.Safe.t option;
    subject_blob_cids : string list;
    mute_until : string option;
    mute_reporting_until : string option;
    last_reviewed_by : string option;
    last_reviewed_at : string option;
    last_reported_at : string option;
    last_appealed_at : string option;
    takendown : bool option;
    appealed : bool option;
    suspend_until : string option;
    tags : string list;
    age_assurance_state : string option;
    age_assurance_updated_by : string option;
    original : Yojson.Safe.t;
  }

  type statuses = {
    cursor : string option;
    subject_statuses : subject_status list;
  }

  type mod_event = {
    id : int option;
    event : event;
    subject : subject;
    created_by : string option;
    created_at : string option;
    creator_handle : string option;
    subject_handle : string option;
    subject_blob_cids : string list;
    mod_tool : mod_tool option;
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
    pds : string option;
    blob_divert : string option;
    chat : string option;
    verifier_did : string option;
    original : Yojson.Safe.t;
  }

  let string_list json field =
    List.filter_map
      (function `String s -> Some s | _ -> None)
      (Client.list_member json field)

  let type_name json =
    match Yojson.Safe.Util.member "$type" json with `String s -> s | _ -> ""

  let ends_with suffix s =
    let n = String.length suffix in
    let m = String.length s in
    m >= n && String.sub s (m - n) n = suffix

  let parse_subject json : subject =
    let t = type_name json in
    let has_did =
      match Yojson.Safe.Util.member "did" json with
      | `String _ -> true
      | _ -> false
    in
    let has_uri =
      match Yojson.Safe.Util.member "uri" json with
      | `String _ -> true
      | _ -> false
    in
    let has_message =
      match Yojson.Safe.Util.member "messageId" json with
      | `String _ -> true
      | _ -> false
    in
    let has_convo =
      match Yojson.Safe.Util.member "convoId" json with
      | `String _ -> true
      | _ -> false
    in
    if ends_with "messageRef" t || (has_message && has_convo) then
      `Message_ref
        {
          did = Client.string_member json "did";
          convo_id = Client.string_member json "convoId";
          message_id = Client.string_member json "messageId";
        }
    else if ends_with "convoRef" t || (has_convo && has_did && not has_message)
    then
      `Convo_ref
        {
          did = Client.string_member json "did";
          convo_id = Client.string_member json "convoId";
        }
    else if ends_with "strongRef" t || (has_uri && not has_did) then
      `Strong_ref
        {
          uri = Client.string_member json "uri";
          cid = Client.string_member json "cid";
        }
    else if ends_with "repoRef" t || has_did then
      `Repo_ref { did = Client.string_member json "did" }
    else `Unknown json

  let parse_event json : event =
    let t = type_name json in
    let comment = Client.string_opt json "comment" in
    if ends_with "modEventComment" t then
      `Comment
        {
          comment = Option.value comment ~default:"";
          sticky = Client.bool_opt json "sticky";
        }
    else if ends_with "modEventAcknowledge" t then
      `Acknowledge
        {
          comment;
          acknowledge_account_subjects =
            Client.bool_opt json "acknowledgeAccountSubjects";
        }
    else if ends_with "modEventTakedown" t then
      `Takedown
        {
          comment;
          duration_in_hours = Client.int_opt json "durationInHours";
          acknowledge_account_subjects =
            Client.bool_opt json "acknowledgeAccountSubjects";
          policies = string_list json "policies";
        }
    else if ends_with "modEventReverseTakedown" t then
      `Reverse_takedown { comment }
    else if ends_with "modEventReport" t then
      `Report { comment; report_type = Client.string_member json "reportType" }
    else if ends_with "modEventLabel" t then
      `Label
        {
          comment;
          create_label_vals = string_list json "createLabelVals";
          negate_label_vals = string_list json "negateLabelVals";
          duration_in_hours = Client.int_opt json "durationInHours";
        }
    else if ends_with "modEventEscalate" t then `Escalate { comment }
    else if ends_with "modEventMute" t then
      `Mute
        { comment; duration_in_hours = Client.int_opt json "durationInHours" }
    else if ends_with "modEventUnmute" t then `Unmute { comment }
    else if ends_with "modEventEmail" t then
      `Email
        {
          subject_line = Client.string_member json "subjectLine";
          content = Client.string_opt json "content";
          comment;
        }
    else if ends_with "modEventTag" t then
      `Tag
        {
          add = string_list json "add";
          remove = string_list json "remove";
          comment;
        }
    else if ends_with "modEventResolveAppeal" t then `Resolve_appeal { comment }
    else if ends_with "modEventPriorityScore" t then
      `Priority_score { comment; score = Client.int_member json "score" }
    else if ends_with "modEventMuteReporter" t then
      `Mute_reporter
        { comment; duration_in_hours = Client.int_opt json "durationInHours" }
    else if ends_with "modEventUnmuteReporter" t then
      `Unmute_reporter { comment }
    else if ends_with "modEventDivert" t then `Divert { comment }
    else if ends_with "accountEvent" t then
      `Account
        {
          comment;
          active = Client.bool_member json "active";
          status = Client.string_opt json "status";
          timestamp = Client.string_member json "timestamp";
        }
    else if ends_with "identityEvent" t then
      `Identity
        {
          comment;
          handle = Client.string_opt json "handle";
          pds_host = Client.string_opt json "pdsHost";
          tombstone = Client.bool_opt json "tombstone";
          timestamp = Client.string_member json "timestamp";
        }
    else if ends_with "recordEvent" t then
      `Record
        {
          comment;
          op = Client.string_member json "op";
          cid = Client.string_opt json "cid";
          timestamp = Client.string_member json "timestamp";
        }
    else if ends_with "ageAssuranceEvent" t then
      `Age_assurance
        {
          created_at = Client.string_member json "createdAt";
          attempt_id = Client.string_member json "attemptId";
          status = Client.string_member json "status";
          country_code = Client.string_opt json "countryCode";
          region_code = Client.string_opt json "regionCode";
          init_ip = Client.string_opt json "initIp";
          init_ua = Client.string_opt json "initUa";
          complete_ip = Client.string_opt json "completeIp";
          complete_ua = Client.string_opt json "completeUa";
        }
    else if ends_with "ageAssuranceOverrideEvent" t then
      `Age_assurance_override
        {
          comment = Option.value comment ~default:"";
          status = Client.string_member json "status";
        }
    else if ends_with "ageAssurancePurgeEvent" t then
      `Age_assurance_purge { comment = Option.value comment ~default:"" }
    else if ends_with "revokeAccountCredentialsEvent" t then
      `Revoke_account_credentials { comment = Option.value comment ~default:"" }
    else if ends_with "scheduleTakedownEvent" t then
      `Schedule_takedown
        {
          comment;
          execute_at = Client.string_opt json "executeAt";
          execute_after = Client.string_opt json "executeAfter";
          execute_until = Client.string_opt json "executeUntil";
        }
    else if ends_with "cancelScheduledTakedownEvent" t then
      `Cancel_scheduled_takedown { comment }
    else `Unknown { type_ = t; original = json }

  let parse_subject_status json : subject_status =
    {
      id = Client.int_opt json "id";
      subject =
        (match Yojson.Safe.Util.member "subject" json with
        | `Assoc _ as s -> parse_subject s
        | other -> `Unknown other);
      subject_repo_handle = Client.string_opt json "subjectRepoHandle";
      updated_at = Client.string_opt json "updatedAt";
      created_at = Client.string_opt json "createdAt";
      review_state = Client.string_opt json "reviewState";
      comment = Client.string_opt json "comment";
      priority_score = Client.int_opt json "priorityScore";
      hosting =
        (match Yojson.Safe.Util.member "hosting" json with
        | `Null -> None
        | other -> Some other);
      subject_blob_cids = string_list json "subjectBlobCids";
      mute_until = Client.string_opt json "muteUntil";
      mute_reporting_until = Client.string_opt json "muteReportingUntil";
      last_reviewed_by = Client.string_opt json "lastReviewedBy";
      last_reviewed_at = Client.string_opt json "lastReviewedAt";
      last_reported_at = Client.string_opt json "lastReportedAt";
      last_appealed_at = Client.string_opt json "lastAppealedAt";
      takendown = Client.bool_opt json "takendown";
      appealed = Client.bool_opt json "appealed";
      suspend_until = Client.string_opt json "suspendUntil";
      tags = string_list json "tags";
      age_assurance_state = Client.string_opt json "ageAssuranceState";
      age_assurance_updated_by = Client.string_opt json "ageAssuranceUpdatedBy";
      original = json;
    }

  let parse_statuses json : statuses =
    {
      cursor = Client.string_opt json "cursor";
      subject_statuses =
        List.map parse_subject_status
          (Client.list_member json "subjectStatuses");
    }

  let parse_mod_tool json : mod_tool =
    {
      name = Client.string_member json "name";
      meta =
        (match Yojson.Safe.Util.member "meta" json with
        | `Null -> None
        | other -> Some other);
    }

  let parse_mod_event json : mod_event =
    {
      id = Client.int_opt json "id";
      event =
        (match Yojson.Safe.Util.member "event" json with
        | `Assoc _ as e -> parse_event e
        | other -> `Unknown { type_ = ""; original = other });
      subject =
        (match Yojson.Safe.Util.member "subject" json with
        | `Assoc _ as s -> parse_subject s
        | other -> `Unknown other);
      created_by = Client.string_opt json "createdBy";
      created_at = Client.string_opt json "createdAt";
      creator_handle = Client.string_opt json "creatorHandle";
      subject_handle = Client.string_opt json "subjectHandle";
      subject_blob_cids = string_list json "subjectBlobCids";
      mod_tool =
        (match Yojson.Safe.Util.member "modTool" json with
        | `Assoc _ as t -> Some (parse_mod_tool t)
        | _ -> None);
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

  let service_url json field : string option =
    match Yojson.Safe.Util.member field json with
    | `Assoc _ as a -> Client.string_opt a "url"
    | `String s -> Some s
    | _ -> None

  let parse_server_config json : server_config =
    {
      viewer_role =
        (match Yojson.Safe.Util.member "viewer" json with
        | `Assoc _ as v -> Client.string_opt v "role"
        | _ -> Client.string_opt json "viewer");
      appview = service_url json "appview";
      pds = service_url json "pds";
      blob_divert = service_url json "blobDivert";
      chat = service_url json "chat";
      verifier_did = Client.string_opt json "verifierDid";
      original = json;
    }

  (** [com.atproto.admin.defs#repoRef] subject ([did]). *)
  let repo_ref did : Yojson.Safe.t =
    `Assoc
      [
        ("$type", `String "com.atproto.admin.defs#repoRef"); ("did", `String did);
      ]

  (** [com.atproto.repo.strongRef] subject ([uri] / [cid]). *)
  let strong_ref ~uri ~cid : Yojson.Safe.t =
    `Assoc
      [
        ("$type", `String "com.atproto.repo.strongRef");
        ("uri", `String uri);
        ("cid", `String cid);
      ]

  (** [tools.ozone.moderation.emitEvent#reportAction] payload ([ids] /
      [types] / [all] / [note]). *)
  let report_action ?ids ?(types = []) ?all ?note () : Yojson.Safe.t =
    `Assoc
      ((match ids with
       | Some xs -> [ ("ids", `List (List.map (fun n -> `Int n) xs)) ]
       | None -> [])
      @ (match types with
        | [] -> []
        | xs -> [ ("types", `List (List.map (fun s -> `String s) xs)) ])
      @ (match all with Some b -> [ ("all", `Bool b) ] | None -> [])
      @ match note with Some n -> [ ("note", `String n) ] | None -> [])

  (** JSON body for [tools.ozone.moderation.emitEvent]. *)
  let emit_event_body ~event ~subject ~created_by ?subject_blob_cids
      ?external_id ?mod_tool ?report_action () : Yojson.Safe.t =
    let fields =
      [
        ("event", event); ("subject", subject); ("createdBy", `String created_by);
      ]
      @ (match subject_blob_cids with
        | Some cids ->
            [ ("subjectBlobCids", `List (List.map (fun c -> `String c) cids)) ]
        | None -> [])
      @ (match external_id with
        | Some id -> [ ("externalId", `String id) ]
        | None -> [])
      @ (match mod_tool with Some t -> [ ("modTool", t) ] | None -> [])
      @
      match report_action with Some r -> [ ("reportAction", r) ] | None -> []
    in
    `Assoc fields

  (** [tools.ozone.moderation.defs#modEventComment] event. Optional
      [sticky]. *)
  let comment_event ?(sticky = false) comment : Yojson.Safe.t =
    `Assoc
      [
        ("$type", `String "tools.ozone.moderation.defs#modEventComment");
        ("comment", `String comment);
        ("sticky", `Bool sticky);
      ]

  (** [tools.ozone.moderation.defs#modEventAcknowledge] event. *)
  let acknowledge_event ?comment () : Yojson.Safe.t =
    let fields =
      [ ("$type", `String "tools.ozone.moderation.defs#modEventAcknowledge") ]
      @ match comment with Some c -> [ ("comment", `String c) ] | None -> []
    in
    `Assoc fields

  (** [tools.ozone.moderation.defs#modEventTakedown] event. *)
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

  (** Subject statuses via [tools.ozone.moderation.queryStatuses]. *)
  let query_statuses (s : Session.session) ~proxy ?host ?subject ?comment
      ?review_state ?limit ?cursor () : statuses =
    Client.get_json ~session:s ?host ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.queryStatuses"
      (Client.opt_pair "subject" subject
      @ Client.opt_pair "comment" comment
      @ Client.opt_pair "reviewState" review_state
      @ Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor)
    |> parse_statuses

  (** Moderation events via [tools.ozone.moderation.queryEvents]. Password
      [at+jwt] sessions send [atproto-proxy] through the PDS. *)
  let query_events (s : Session.session) ~proxy ?host ?types ?created_by
      ?subject ?limit ?cursor () : events =
    Client.get_json ~session:s ?host ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.queryEvents"
      (Client.repeat_param "types" (Option.value types ~default:[])
      @ Client.opt_pair "createdBy" created_by
      @ Client.opt_pair "subject" subject
      @ Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor)
    |> parse_events

  (** Emit a moderation event via [tools.ozone.moderation.emitEvent]. Password
      [at+jwt] sessions send [atproto-proxy] through the PDS. *)
  let emit_event (s : Session.session) ~proxy ?host ~event ~subject ~created_by
      ?subject_blob_cids ?external_id ?mod_tool ?report_action () : mod_event =
    Client.post_json ~session:s ?host ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.emitEvent"
      (Yojson.Safe.to_string
         (emit_event_body ~event ~subject ~created_by ?subject_blob_cids
            ?external_id ?mod_tool ?report_action ()))
    |> parse_mod_event

  (** Emit a moderation event via [tools.ozone.moderation.emitEvent] on the
      Ozone host with a PDS-minted service-auth JWT (OAuth DPoP
      [getServiceAuth]). No [atproto-proxy] — DPoP cannot be proxied, and
      Ozone rejects the PDS access token. *)
  let emit_event_service ~bearer ~host ~event ~subject ~created_by
      ?subject_blob_cids ?external_id ?mod_tool ?report_action () : mod_event =
    Client.post_json ~bearer ~host "tools.ozone.moderation.emitEvent"
      (Yojson.Safe.to_string
         (emit_event_body ~event ~subject ~created_by ?subject_blob_cids
            ?external_id ?mod_tool ?report_action ()))
    |> parse_mod_event

  (** Moderation events via [tools.ozone.moderation.queryEvents] on the Ozone
      host with a service-auth JWT (no [atproto-proxy]). *)
  let query_events_service ~bearer ~host ?types ?created_by ?subject ?limit
      ?cursor () : events =
    Client.get_json ~bearer ~host "tools.ozone.moderation.queryEvents"
      (Client.repeat_param "types" (Option.value types ~default:[])
      @ Client.opt_pair "createdBy" created_by
      @ Client.opt_pair "subject" subject
      @ Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor)
    |> parse_events

  (** Ozone config via [tools.ozone.server.getConfig] on the Ozone host with
      a service-auth JWT. *)
  let get_config_service ~bearer ~host () : server_config =
    Client.get_json ~bearer ~host "tools.ozone.server.getConfig" []
    |> parse_server_config

  (** One event by [id] via [tools.ozone.moderation.getEvent]. *)
  let get_event (s : Session.session) ~proxy ~id () : mod_event =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.getEvent"
      [ ("id", string_of_int id) ]
    |> parse_mod_event

  (** Repo view for [did] via [tools.ozone.moderation.getRepo]. *)
  let get_repo (s : Session.session) ~proxy ~did () : repo_view =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.getRepo"
      [ ("did", did) ]
    |> parse_repo

  (** Record view for [uri] via [tools.ozone.moderation.getRecord]. *)
  let get_record (s : Session.session) ~proxy ~uri ?cid () : record_view =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.getRecord"
      (("uri", uri) :: Client.opt_pair "cid" cid)
    |> parse_record

  (** Ozone server config via [tools.ozone.server.getConfig] ([appview] /
      [pds] / [chat] / [viewer.role]). *)
  let get_config (s : Session.session) ~proxy ?host () : server_config =
    Client.get_json ~session:s ?host ~extra:(proxy_headers proxy)
      "tools.ozone.server.getConfig" []
    |> parse_server_config

  type subject_view = { subject : string; original : Yojson.Safe.t }
  type subjects = { subjects : subject_view list }

  type reporter_stat = {
    did : string;
    account_report_count : int option;
    record_report_count : int option;
    reported_account_count : int option;
    original : Yojson.Safe.t;
  }

  type reporter_stats = { stats : reporter_stat list }

  type timeline_summary = {
    event_subject_type : string;
    event_type : string;
    count : int;
  }

  type timeline_item = { day : string; summary : timeline_summary list }
  type account_timeline = { timeline : timeline_item list }
  type account_preferences = { preferences : Actor.preference list }

  type scheduled_action = {
    id : string option;
    subject : string option;
    status : string option;
    original : Yojson.Safe.t;
  }

  type scheduled_actions = {
    cursor : string option;
    actions : scheduled_action list;
  }

  type failed_item = {
    subject : string;
    error : string;
    error_code : string option;
  }

  type batch_result = { succeeded : string list; failed : failed_item list }

  type scheduling = {
    execute_at : string option;
    execute_after : string option;
    execute_until : string option;
  }

  let parse_subject_view json : subject_view =
    {
      subject =
        (match Client.string_opt json "subject" with
        | Some s -> s
        | None -> Client.string_member json "did");
      original = json;
    }

  let parse_subjects json : subjects =
    {
      subjects =
        List.map parse_subject_view (Client.list_member json "subjects");
    }

  let parse_reporter_stat json : reporter_stat =
    {
      did = Client.string_member json "did";
      account_report_count = Client.int_opt json "accountReportCount";
      record_report_count = Client.int_opt json "recordReportCount";
      reported_account_count = Client.int_opt json "reportedAccountCount";
      original = json;
    }

  let parse_reporter_stats json : reporter_stats =
    { stats = List.map parse_reporter_stat (Client.list_member json "stats") }

  let parse_timeline_summary json : timeline_summary =
    {
      event_subject_type = Client.string_member json "eventSubjectType";
      event_type = Client.string_member json "eventType";
      count = Client.int_member json "count";
    }

  let parse_timeline_item json : timeline_item =
    {
      day = Client.string_member json "day";
      summary =
        List.map parse_timeline_summary (Client.list_member json "summary");
    }

  let parse_account_timeline json : account_timeline =
    {
      timeline =
        List.map parse_timeline_item (Client.list_member json "timeline");
    }

  let parse_account_preferences json : account_preferences =
    {
      preferences =
        List.map Actor.parse_preference (Client.list_member json "preferences");
    }

  let parse_scheduled_action json : scheduled_action =
    {
      id = Client.string_opt json "id";
      subject =
        (match Client.string_opt json "subject" with
        | Some s -> Some s
        | None -> Client.string_opt json "did");
      status = Client.string_opt json "status";
      original = json;
    }

  let parse_scheduled_actions json : scheduled_actions =
    {
      cursor = Client.string_opt json "cursor";
      actions =
        List.map parse_scheduled_action (Client.list_member json "actions");
    }

  let parse_failed_item json : failed_item =
    {
      subject =
        (match Client.string_opt json "subject" with
        | Some s -> s
        | None -> Client.string_member json "did");
      error = Client.string_member json "error";
      error_code = Client.string_opt json "errorCode";
    }

  let parse_batch_result json : batch_result =
    {
      succeeded =
        List.filter_map
          (function `String s -> Some s | _ -> None)
          (Client.list_member json "succeeded");
      failed = List.map parse_failed_item (Client.list_member json "failed");
    }

  (** Record views for [uris] via [tools.ozone.moderation.getRecords]. *)
  let get_records (s : Session.session) ~proxy ~uris () : record_view list =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.getRecords"
      (Client.repeat_param "uris" uris)
    |> fun json -> List.map parse_record (Client.list_member json "records")

  (** Repo views for [dids] via [tools.ozone.moderation.getRepos]. *)
  let get_repos (s : Session.session) ~proxy ~dids () : repo_view list =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.getRepos"
      (Client.repeat_param "dids" dids)
    |> fun json -> List.map parse_repo (Client.list_member json "repos")

  (** Subject views via [tools.ozone.moderation.getSubjects]. *)
  let get_subjects (s : Session.session) ~proxy ~subjects () : subjects =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.getSubjects"
      (Client.repeat_param "subjects" subjects)
    |> parse_subjects

  (** Search repos via [tools.ozone.moderation.searchRepos] ([q] or [term]). *)
  let search_repos (s : Session.session) ~proxy ?q ?term ?limit ?cursor () :
      repo_view list * string option =
    let json =
      Client.get_json ~session:s ~extra:(proxy_headers proxy)
        "tools.ozone.moderation.searchRepos"
        (Client.opt_pair "q" q
        @ Client.opt_pair "term" term
        @ Client.opt_int "limit" limit
        @ Client.opt_pair "cursor" cursor)
    in
    ( List.map parse_repo (Client.list_member json "repos"),
      Client.string_opt json "cursor" )

  (** Event timeline for [did] via
      [tools.ozone.moderation.getAccountTimeline]. *)
  let get_account_timeline (s : Session.session) ~proxy ~did () :
      account_timeline =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.getAccountTimeline"
      [ ("did", did) ]
    |> parse_account_timeline

  (** Private preferences for [did] via
      [tools.ozone.moderation.getAccountPreferences]
      (moderator or admin auth). Same [app.bsky.actor.defs#preferences]
      union as [Actor.parse_preferences]. *)
  let get_account_preferences (s : Session.session) ~proxy ~did () :
      account_preferences =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.getAccountPreferences"
      [ ("did", did) ]
    |> parse_account_preferences

  (** Reporter stats for [dids] via
      [tools.ozone.moderation.getReporterStats]. *)
  let get_reporter_stats (s : Session.session) ~proxy ~dids () : reporter_stats
      =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.getReporterStats"
      (Client.repeat_param "dids" dids)
    |> parse_reporter_stats

  let scheduling_to_json (c : scheduling) : Yojson.Safe.t =
    `Assoc
      ((match c.execute_at with
       | Some s -> [ ("executeAt", `String s) ]
       | None -> [])
      @ (match c.execute_after with
        | Some s -> [ ("executeAfter", `String s) ]
        | None -> [])
      @
      match c.execute_until with
      | Some s -> [ ("executeUntil", `String s) ]
      | None -> [])

  (** [tools.ozone.moderation.scheduleAction#takedown] action. *)
  let takedown_action ?comment ?duration_in_hours
      ?(acknowledge_account_subjects = false) () : Yojson.Safe.t =
    let fields =
      [
        ("$type", `String "tools.ozone.moderation.scheduleAction#takedown");
        ("acknowledgeAccountSubjects", `Bool acknowledge_account_subjects);
      ]
      @ (match comment with Some c -> [ ("comment", `String c) ] | None -> [])
      @
      match duration_in_hours with
      | Some n -> [ ("durationInHours", `Int n) ]
      | None -> []
    in
    `Assoc fields

  (** JSON body for [tools.ozone.moderation.scheduleAction]. *)
  let schedule_action_body ~action ~subjects ~created_by ~scheduling ?mod_tool
      () : Yojson.Safe.t =
    let fields =
      [
        ("action", action);
        ("subjects", `List (List.map (fun s -> `String s) subjects));
        ("createdBy", `String created_by);
        ("scheduling", scheduling_to_json scheduling);
      ]
      @ match mod_tool with Some t -> [ ("modTool", t) ] | None -> []
    in
    `Assoc fields

  (** Schedule a future action via [tools.ozone.moderation.scheduleAction]. *)
  let schedule_action (s : Session.session) ~proxy ~action ~subjects ~created_by
      ~scheduling ?mod_tool () : batch_result =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.scheduleAction"
      (Yojson.Safe.to_string
         (schedule_action_body ~action ~subjects ~created_by ~scheduling
            ?mod_tool ()))
    |> parse_batch_result

  (** Scheduled actions via [tools.ozone.moderation.listScheduledActions]. *)
  let list_scheduled_actions (s : Session.session) ~proxy ~statuses
      ?starts_after ?ends_before ?subjects ?limit ?cursor () : scheduled_actions
      =
    let body =
      `Assoc
        ([ ("statuses", `List (List.map (fun s -> `String s) statuses)) ]
        @ (match starts_after with
          | Some t -> [ ("startsAfter", `String t) ]
          | None -> [])
        @ (match ends_before with
          | Some t -> [ ("endsBefore", `String t) ]
          | None -> [])
        @ (match subjects with
          | Some xs ->
              [ ("subjects", `List (List.map (fun s -> `String s) xs)) ]
          | None -> [])
        @ (match limit with Some n -> [ ("limit", `Int n) ] | None -> [])
        @ match cursor with Some c -> [ ("cursor", `String c) ] | None -> [])
    in
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.listScheduledActions"
      (Yojson.Safe.to_string body)
    |> parse_scheduled_actions

  (** Cancel scheduled actions via
      [tools.ozone.moderation.cancelScheduledActions]. *)
  let cancel_scheduled_actions (s : Session.session) ~proxy ~subjects ?comment
      () : batch_result =
    let fields =
      [ ("subjects", `List (List.map (fun s -> `String s) subjects)) ]
      @ match comment with Some c -> [ ("comment", `String c) ] | None -> []
    in
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.cancelScheduledActions"
      (Yojson.Safe.to_string (`Assoc fields))
    |> parse_batch_result

  type template = {
    id : string;
    name : string;
    content_markdown : string;
    subject : string option;
    lang : string option;
    disabled : bool option;
    original : Yojson.Safe.t;
  }

  type templates = { templates : template list }

  let parse_template json : template =
    {
      id = Client.string_member json "id";
      name = Client.string_member json "name";
      content_markdown = Client.string_member json "contentMarkdown";
      subject = Client.string_opt json "subject";
      lang = Client.string_opt json "lang";
      disabled = Client.bool_opt json "disabled";
      original = json;
    }

  let parse_templates json : templates =
    {
      templates =
        List.map parse_template
          (match Client.list_member json "communicationTemplates" with
          | [] -> Client.list_member json "templates"
          | xs -> xs);
    }

  (** Communication templates via [tools.ozone.communication.listTemplates]. *)
  let list_templates (s : Session.session) ~proxy () : templates =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.communication.listTemplates" []
    |> parse_templates

  (** JSON body for [tools.ozone.communication.createTemplate]. *)
  let create_template_body ~name ~content_markdown ?subject ?lang ?created_by ()
      : Yojson.Safe.t =
    `Assoc
      ([ ("name", `String name); ("contentMarkdown", `String content_markdown) ]
      @ (match subject with Some s -> [ ("subject", `String s) ] | None -> [])
      @ (match lang with Some s -> [ ("lang", `String s) ] | None -> [])
      @
      match created_by with
      | Some d -> [ ("createdBy", `String d) ]
      | None -> [])

  (** Create a template via [tools.ozone.communication.createTemplate]. *)
  let create_template (s : Session.session) ~proxy ~name ~content_markdown
      ?subject ?lang ?created_by () : template =
    let created_by = Option.value created_by ~default:s.auth.did in
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.communication.createTemplate"
      (Yojson.Safe.to_string
         (create_template_body ~name ~content_markdown ?subject ?lang
            ~created_by ()))
    |> parse_template

  (** Update template [id] via [tools.ozone.communication.updateTemplate]. *)
  let update_template (s : Session.session) ~proxy ~id ?name ?content_markdown
      ?subject ?disabled ?updated_by () : template =
    let updated_by = Option.value updated_by ~default:s.auth.did in
    let fields =
      ("id", `String id)
      :: ("updatedBy", `String updated_by)
      :: (match name with Some n -> [ ("name", `String n) ] | None -> [])
      @ (match content_markdown with
        | Some c -> [ ("contentMarkdown", `String c) ]
        | None -> [])
      @ (match subject with Some s -> [ ("subject", `String s) ] | None -> [])
      @ match disabled with Some b -> [ ("disabled", `Bool b) ] | None -> []
    in
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.communication.updateTemplate"
      (Yojson.Safe.to_string (`Assoc fields))
    |> parse_template

  (** Delete template [id] via [tools.ozone.communication.deleteTemplate]. *)
  let delete_template (s : Session.session) ~proxy ~id () : unit =
    ignore
      (Client.post_json ~session:s ~extra:(proxy_headers proxy)
         "tools.ozone.communication.deleteTemplate"
         (Yojson.Safe.to_string (`Assoc [ ("id", `String id) ])))

  type set_view = {
    name : string;
    description : string option;
    set_size : int option;
    original : Yojson.Safe.t;
  }

  type sets = { cursor : string option; sets : set_view list }

  type set_values = {
    name : string;
    values : string list;
    cursor : string option;
  }

  let parse_set_view json : set_view =
    {
      name = Client.string_member json "name";
      description = Client.string_opt json "description";
      set_size = Client.int_opt json "setSize";
      original = json;
    }

  let parse_sets json : sets =
    {
      cursor = Client.string_opt json "cursor";
      sets = List.map parse_set_view (Client.list_member json "sets");
    }

  let parse_set_values json : set_values =
    {
      name = Client.string_member json "name";
      values =
        List.filter_map
          (function `String s -> Some s | _ -> None)
          (Client.list_member json "values");
      cursor = Client.string_opt json "cursor";
    }

  (** Sets via [tools.ozone.set.querySets]. *)
  let query_sets (s : Session.session) ~proxy ?limit ?cursor ?name_prefix
      ?sort_by ?sort_direction () : sets =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.set.querySets"
      (Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor
      @ Client.opt_pair "namePrefix" name_prefix
      @ Client.opt_pair "sortBy" sort_by
      @ Client.opt_pair "sortDirection" sort_direction)
    |> parse_sets

  (** Create or update set [name] via [tools.ozone.set.upsertSet]. *)
  let upsert_set (s : Session.session) ~proxy ~name ?description () : set_view =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.set.upsertSet"
      (Yojson.Safe.to_string
         (`Assoc
           (("name", `String name)
           ::
           (match description with
           | Some d -> [ ("description", `String d) ]
           | None -> []))))
    |> parse_set_view

  (** Values in set [name] via [tools.ozone.set.getValues]. *)
  let get_set_values (s : Session.session) ~proxy ~name ?limit ?cursor () :
      set_values =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.set.getValues"
      ((("name", name) :: Client.opt_int "limit" limit)
      @ Client.opt_pair "cursor" cursor)
    |> parse_set_values

  (** Add [values] to set [name] via [tools.ozone.set.addValues]. *)
  let add_set_values (s : Session.session) ~proxy ~name ~values () : unit =
    ignore
      (Client.post_json ~session:s ~extra:(proxy_headers proxy)
         "tools.ozone.set.addValues"
         (Yojson.Safe.to_string
            (`Assoc
              [
                ("name", `String name);
                ("values", `List (List.map (fun v -> `String v) values));
              ])))

  (** Remove [values] from set [name] via [tools.ozone.set.deleteValues]. *)
  let delete_set_values (s : Session.session) ~proxy ~name ~values () : unit =
    ignore
      (Client.post_json ~session:s ~extra:(proxy_headers proxy)
         "tools.ozone.set.deleteValues"
         (Yojson.Safe.to_string
            (`Assoc
              [
                ("name", `String name);
                ("values", `List (List.map (fun v -> `String v) values));
              ])))

  (** Delete set [name] via [tools.ozone.set.deleteSet]. *)
  let delete_set (s : Session.session) ~proxy ~name () : unit =
    ignore
      (Client.post_json ~session:s ~extra:(proxy_headers proxy)
         "tools.ozone.set.deleteSet"
         (Yojson.Safe.to_string (`Assoc [ ("name", `String name) ])))

  type setting_option = {
    key : string;
    scope : string option;
    value : Yojson.Safe.t;
    description : string option;
    original : Yojson.Safe.t;
  }

  type setting_options = { options : setting_option list }

  let parse_setting_option json : setting_option =
    {
      key = Client.string_member json "key";
      scope = Client.string_opt json "scope";
      value = Yojson.Safe.Util.member "value" json;
      description = Client.string_opt json "description";
      original = json;
    }

  let parse_setting_options json : setting_options =
    {
      options =
        List.map parse_setting_option
          (match Client.list_member json "options" with
          | [] -> Client.list_member json "settings"
          | xs -> xs);
    }

  (** Settings via [tools.ozone.setting.listOptions]. *)
  let list_options (s : Session.session) ~proxy ?prefix ?scope ?limit ?cursor ()
      : setting_options =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.setting.listOptions"
      (Client.opt_pair "prefix" prefix
      @ Client.opt_pair "scope" scope
      @ Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor)
    |> parse_setting_options

  (** Create or update setting [key] via [tools.ozone.setting.upsertOption]. *)
  let upsert_option (s : Session.session) ~proxy ~key ~scope ~value ?description
      () : setting_option =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.setting.upsertOption"
      (Yojson.Safe.to_string
         (`Assoc
           ([ ("key", `String key); ("scope", `String scope); ("value", value) ]
           @
           match description with
           | Some d -> [ ("description", `String d) ]
           | None -> [])))
    |> parse_setting_option

  (** Remove setting [keys] via [tools.ozone.setting.removeOptions]. *)
  let remove_options (s : Session.session) ~proxy ~keys ~scope () : unit =
    ignore
      (Client.post_json ~session:s ~extra:(proxy_headers proxy)
         "tools.ozone.setting.removeOptions"
         (Yojson.Safe.to_string
            (`Assoc
              [
                ("keys", `List (List.map (fun k -> `String k) keys));
                ("scope", `String scope);
              ])))

  type team_member = {
    did : string;
    role : string option;
    disabled : bool option;
    original : Yojson.Safe.t;
  }

  type team_members = { cursor : string option; members : team_member list }

  let parse_team_member json : team_member =
    {
      did = Client.string_member json "did";
      role = Client.string_opt json "role";
      disabled = Client.bool_opt json "disabled";
      original = json;
    }

  let parse_team_members json : team_members =
    {
      cursor = Client.string_opt json "cursor";
      members = List.map parse_team_member (Client.list_member json "members");
    }

  (** Team members via [tools.ozone.team.listMembers]. *)
  let list_members (s : Session.session) ~proxy ?q ?disabled ?(roles = [])
      ?limit ?cursor () : team_members =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.team.listMembers"
      (Client.opt_pair "q" q
      @ Client.opt_bool "disabled" disabled
      @ Client.repeat_param "roles" roles
      @ Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor)
    |> parse_team_members

  (** Add team member [did] via [tools.ozone.team.addMember]. *)
  let add_member (s : Session.session) ~proxy ~did ~role () : team_member =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.team.addMember"
      (Yojson.Safe.to_string
         (`Assoc [ ("did", `String did); ("role", `String role) ]))
    |> parse_team_member

  (** Update team member [did] via [tools.ozone.team.updateMember]. *)
  let update_member (s : Session.session) ~proxy ~did ?role ?disabled () :
      team_member =
    let fields =
      ("did", `String did)
      :: (match role with Some r -> [ ("role", `String r) ] | None -> [])
      @ match disabled with Some b -> [ ("disabled", `Bool b) ] | None -> []
    in
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.team.updateMember"
      (Yojson.Safe.to_string (`Assoc fields))
    |> parse_team_member

  (** Remove team member [did] via [tools.ozone.team.deleteMember]. *)
  let delete_member (s : Session.session) ~proxy ~did () : unit =
    ignore
      (Client.post_json ~session:s ~extra:(proxy_headers proxy)
         "tools.ozone.team.deleteMember"
         (Yojson.Safe.to_string (`Assoc [ ("did", `String did) ])))

  type url_rule = {
    url : string;
    pattern_type : string option;
    action : string option;
    reason : string option;
    created_by : string option;
    original : Yojson.Safe.t;
  }

  type url_rules = { cursor : string option; rules : url_rule list }

  let parse_url_rule json : url_rule =
    {
      url = Client.string_member json "url";
      (* Official tools.ozone.safelink.defs#urlRule uses `pattern`. *)
      pattern_type =
        (match Client.string_opt json "pattern" with
        | Some s -> Some s
        | None -> Client.string_opt json "patternType");
      action = Client.string_opt json "action";
      reason = Client.string_opt json "reason";
      created_by = Client.string_opt json "createdBy";
      original = json;
    }

  let parse_url_rules json : url_rules =
    {
      cursor = Client.string_opt json "cursor";
      rules =
        List.map parse_url_rule
          (match Client.list_member json "rules" with
          | [] -> Client.list_member json "events"
          | xs -> xs);
    }

  (** URL rules via [tools.ozone.safelink.queryRules]. *)
  let query_safelink_rules (s : Session.session) ~proxy ?cursor ?limit
      ?(urls = []) ?pattern_type ?(actions = []) ?reason ?created_by
      ?sort_direction () : url_rules =
    let body =
      `Assoc
        ((match cursor with Some c -> [ ("cursor", `String c) ] | None -> [])
        @ (match limit with Some n -> [ ("limit", `Int n) ] | None -> [])
        @ (match urls with
          | [] -> []
          | xs -> [ ("urls", `List (List.map (fun u -> `String u) xs)) ])
        @ (match pattern_type with
          | Some p -> [ ("patternType", `String p) ]
          | None -> [])
        @ (match actions with
          | [] -> []
          | xs -> [ ("actions", `List (List.map (fun a -> `String a) xs)) ])
        @ (match reason with Some r -> [ ("reason", `String r) ] | None -> [])
        @ (match created_by with
          | Some d -> [ ("createdBy", `String d) ]
          | None -> [])
        @
        match sort_direction with
        | Some d -> [ ("sortDirection", `String d) ]
        | None -> [])
    in
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.safelink.queryRules"
      (Yojson.Safe.to_string body)
    |> parse_url_rules

  (* Official addRule / updateRule / removeRule take `pattern` (not
     patternType) and return tools.ozone.safelink.defs#event. *)

  (** JSON body for [tools.ozone.safelink.addRule]. *)
  let add_safelink_rule_body ~url ~pattern ~action ~reason ?comment ?created_by
      () : Yojson.Safe.t =
    `Assoc
      (("url", `String url)
       :: ("pattern", `String pattern)
       :: ("action", `String action)
       :: ("reason", `String reason)
       ::
       (match comment with Some c -> [ ("comment", `String c) ] | None -> [])
      @
      match created_by with
      | Some d -> [ ("createdBy", `String d) ]
      | None -> [])

  (** JSON body for [tools.ozone.safelink.updateRule]. *)
  let update_safelink_rule_body ~url ~pattern ~action ~reason ?comment
      ?created_by () : Yojson.Safe.t =
    add_safelink_rule_body ~url ~pattern ~action ~reason ?comment ?created_by ()

  (** JSON body for [tools.ozone.safelink.removeRule]. *)
  let remove_safelink_rule_body ~url ~pattern ?comment ?created_by () :
      Yojson.Safe.t =
    `Assoc
      (("url", `String url)
       :: ("pattern", `String pattern)
       ::
       (match comment with Some c -> [ ("comment", `String c) ] | None -> [])
      @
      match created_by with
      | Some d -> [ ("createdBy", `String d) ]
      | None -> [])

  type safelink_event = {
    id : int;
    event_type : string;
    url : string;
    pattern : string option;
    action : string option;
    reason : string option;
    created_by : string;
    created_at : string;
    comment : string option;
    original : Yojson.Safe.t;
  }

  type safelink_events = {
    cursor : string option;
    events : safelink_event list;
  }

  let parse_safelink_event json : safelink_event =
    {
      id = Client.int_member json "id";
      event_type = Client.string_member json "eventType";
      url = Client.string_member json "url";
      pattern =
        (match Client.string_opt json "pattern" with
        | Some s -> Some s
        | None -> Client.string_opt json "patternType");
      action = Client.string_opt json "action";
      reason = Client.string_opt json "reason";
      created_by = Client.string_member json "createdBy";
      created_at = Client.string_member json "createdAt";
      comment = Client.string_opt json "comment";
      original = json;
    }

  let parse_safelink_events json : safelink_events =
    {
      cursor = Client.string_opt json "cursor";
      events = List.map parse_safelink_event (Client.list_member json "events");
    }

  (** Add a URL rule via [tools.ozone.safelink.addRule]. *)
  let add_safelink_rule (s : Session.session) ~proxy ~url ~pattern ~action
      ~reason ?comment ?created_by () : safelink_event =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.safelink.addRule"
      (Yojson.Safe.to_string
         (add_safelink_rule_body ~url ~pattern ~action ~reason ?comment
            ?created_by ()))
    |> parse_safelink_event

  (** Update a URL rule via [tools.ozone.safelink.updateRule]. *)
  let update_safelink_rule (s : Session.session) ~proxy ~url ~pattern ~action
      ~reason ?comment ?created_by () : safelink_event =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.safelink.updateRule"
      (Yojson.Safe.to_string
         (update_safelink_rule_body ~url ~pattern ~action ~reason ?comment
            ?created_by ()))
    |> parse_safelink_event

  (** Remove a URL rule via [tools.ozone.safelink.removeRule]. *)
  let remove_safelink_rule (s : Session.session) ~proxy ~url ~pattern ?comment
      ?created_by () : safelink_event =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.safelink.removeRule"
      (Yojson.Safe.to_string
         (remove_safelink_rule_body ~url ~pattern ?comment ?created_by ()))
    |> parse_safelink_event

  (** JSON body for [tools.ozone.safelink.queryEvents]. *)
  let query_safelink_events_body ?cursor ?limit ?(urls = []) ?pattern_type
      ?sort_direction () : Yojson.Safe.t =
    `Assoc
      ((match cursor with Some c -> [ ("cursor", `String c) ] | None -> [])
      @ (match limit with Some n -> [ ("limit", `Int n) ] | None -> [])
      @ (match urls with
        | [] -> []
        | xs -> [ ("urls", `List (List.map (fun u -> `String u) xs)) ])
      @ (match pattern_type with
        | Some p -> [ ("patternType", `String p) ]
        | None -> [])
      @
      match sort_direction with
      | Some d -> [ ("sortDirection", `String d) ]
      | None -> [])

  (** Safelink events via [tools.ozone.safelink.queryEvents]. *)
  let query_safelink_events (s : Session.session) ~proxy ?cursor ?limit
      ?(urls = []) ?pattern_type ?sort_direction () : safelink_events =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.safelink.queryEvents"
      (Yojson.Safe.to_string
         (query_safelink_events_body ?cursor ?limit ~urls ?pattern_type
            ?sort_direction ()))
    |> parse_safelink_events

  type related_account = { did : string; original : Yojson.Safe.t }

  type related_accounts = {
    cursor : string option;
    accounts : related_account list;
  }

  let parse_related_account json : related_account =
    {
      did =
        (match Client.string_opt json "did" with
        | Some s -> s
        | None -> Client.string_member json "account");
      original = json;
    }

  let parse_related_accounts json : related_accounts =
    {
      cursor = Client.string_opt json "cursor";
      accounts =
        List.map parse_related_account
          (match Client.list_member json "accounts" with
          | [] -> Client.list_member json "relatedAccounts"
          | xs -> xs);
    }

  (** Signature correlation for [dids] via
      [tools.ozone.signature.findCorrelation]. *)
  let find_correlation (s : Session.session) ~proxy ~dids () : Yojson.Safe.t =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.signature.findCorrelation"
      (Client.repeat_param "dids" dids)

  (** Related accounts for [did] via
      [tools.ozone.signature.findRelatedAccounts]. *)
  let find_related_accounts (s : Session.session) ~proxy ~did ?limit ?cursor ()
      : related_accounts =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.signature.findRelatedAccounts"
      ((("did", did) :: Client.opt_int "limit" limit)
      @ Client.opt_pair "cursor" cursor)
    |> parse_related_accounts

  (** Accounts matching signature [values] via
      [tools.ozone.signature.searchAccounts]. *)
  let search_accounts_by_signature (s : Session.session) ~proxy ?(values = [])
      ?limit ?cursor () : related_accounts =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.signature.searchAccounts"
      (Client.repeat_param "values" values
      @ Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor)
    |> parse_related_accounts

  type verification_view = {
    uri : string;
    issuer : string option;
    subject : string option;
    handle : string option;
    display_name : string option;
    created_at : string option;
    revoke_reason : string option;
    revoked_at : string option;
    revoked_by : string option;
    original : Yojson.Safe.t;
  }

  type verifications = {
    cursor : string option;
    verifications : verification_view list;
  }

  type grant_error = { error : string; subject : string }
  type revoke_error = { uri : string; error : string }

  type grant_verifications_result = {
    verifications : verification_view list;
    failed_verifications : grant_error list;
  }

  type revoke_verifications_result = {
    revoked_verifications : string list;
    failed_revocations : revoke_error list;
  }

  let parse_verification_view json : verification_view =
    {
      uri = Client.string_member json "uri";
      issuer = Client.string_opt json "issuer";
      subject = Client.string_opt json "subject";
      handle = Client.string_opt json "handle";
      display_name = Client.string_opt json "displayName";
      created_at = Client.string_opt json "createdAt";
      revoke_reason = Client.string_opt json "revokeReason";
      revoked_at = Client.string_opt json "revokedAt";
      revoked_by = Client.string_opt json "revokedBy";
      original = json;
    }

  let parse_verifications json : verifications =
    {
      cursor = Client.string_opt json "cursor";
      verifications =
        List.map parse_verification_view
          (Client.list_member json "verifications");
    }

  let parse_grant_error json : grant_error =
    {
      error = Client.string_member json "error";
      subject = Client.string_member json "subject";
    }

  let parse_revoke_error json : revoke_error =
    {
      uri = Client.string_member json "uri";
      error = Client.string_member json "error";
    }

  let parse_grant_verifications json : grant_verifications_result =
    {
      verifications =
        List.map parse_verification_view
          (Client.list_member json "verifications");
      failed_verifications =
        List.map parse_grant_error
          (Client.list_member json "failedVerifications");
    }

  let parse_revoke_verifications json : revoke_verifications_result =
    {
      revoked_verifications =
        List.filter_map
          (function `String s -> Some s | _ -> None)
          (Client.list_member json "revokedVerifications");
      failed_revocations =
        List.map parse_revoke_error
          (Client.list_member json "failedRevocations");
    }

  (** [tools.ozone.verification.grantVerifications#verificationInput]
      item ([subject] / [handle] / [display_name]). *)
  let verification_input ~subject ~handle ~display_name ?created_at () :
      Yojson.Safe.t =
    `Assoc
      (("subject", `String subject)
      :: ("handle", `String handle)
      :: ("displayName", `String display_name)
      ::
      (match created_at with
      | Some t -> [ ("createdAt", `String t) ]
      | None -> []))

  (** JSON body for [tools.ozone.verification.grantVerifications]. *)
  let grant_verifications_body ~verifications () : Yojson.Safe.t =
    `Assoc [ ("verifications", `List verifications) ]

  (** JSON body for [tools.ozone.verification.revokeVerifications]. *)
  let revoke_verifications_body ~uris ?revoke_reason () : Yojson.Safe.t =
    `Assoc
      (("uris", `List (List.map (fun u -> `String u) uris))
      ::
      (match revoke_reason with
      | Some r -> [ ("revokeReason", `String r) ]
      | None -> []))

  (** Verifications via [tools.ozone.verification.listVerifications]. *)
  let list_verifications (s : Session.session) ~proxy ?cursor ?limit
      ?(issuers = []) ?(subjects = []) () : verifications =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.verification.listVerifications"
      (Client.opt_pair "cursor" cursor
      @ Client.opt_int "limit" limit
      @ Client.repeat_param "issuers" issuers
      @ Client.repeat_param "subjects" subjects)
    |> parse_verifications

  (** Grant verifications via [tools.ozone.verification.grantVerifications]. *)
  let grant_verifications (s : Session.session) ~proxy ~verifications () :
      grant_verifications_result =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.verification.grantVerifications"
      (Yojson.Safe.to_string (grant_verifications_body ~verifications ()))
    |> parse_grant_verifications

  (** Revoke verification [uris] via
      [tools.ozone.verification.revokeVerifications]. *)
  let revoke_verifications (s : Session.session) ~proxy ~uris ?revoke_reason ()
      : revoke_verifications_result =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.verification.revokeVerifications"
      (Yojson.Safe.to_string
         (revoke_verifications_body ~uris ?revoke_reason ()))
    |> parse_revoke_verifications

  type account_history_event = {
    created_at : string option;
    event : Yojson.Safe.t;
  }

  type account_history = {
    cursor : string option;
    events : account_history_event list;
  }

  let parse_account_history_event json : account_history_event =
    {
      created_at = Client.string_opt json "createdAt";
      event =
        (match Yojson.Safe.Util.member "event" json with
        | `Null -> json
        | other -> other);
    }

  let parse_account_history json : account_history =
    {
      cursor = Client.string_opt json "cursor";
      events =
        List.map parse_account_history_event (Client.list_member json "events");
    }

  (** Hosting history for [did] via [tools.ozone.hosting.getAccountHistory]. *)
  let get_account_history (s : Session.session) ~proxy ~did ?events ?cursor
      ?limit () : account_history =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.hosting.getAccountHistory"
      (("did", did)
       :: Client.repeat_param "events" (Option.value events ~default:[])
      @ Client.opt_pair "cursor" cursor
      @ Client.opt_int "limit" limit)
    |> parse_account_history

  (* tools.ozone.queue.* and tools.ozone.report.* operator workflows. *)

  let int_list json field =
    List.filter_map
      (function
        | `Int n -> Some n
        | `Intlit s -> ( try Some (int_of_string s) with _ -> None)
        | _ -> None)
      (Client.list_member json field)

  let repeat_int key values =
    Client.repeat_param key (List.map string_of_int values)

  let opt_json_int k = function Some n -> [ (k, `Int n) ] | None -> []
  let opt_json_str k = function Some s -> [ (k, `String s) ] | None -> []
  let opt_json_bool k = function Some b -> [ (k, `Bool b) ] | None -> []
  let json_strings xs = `List (List.map (fun s -> `String s) xs)
  let json_ints xs = `List (List.map (fun n -> `Int n) xs)

  type queue_stats = {
    pending_count : int option;
    actioned_count : int option;
    escalated_count : int option;
    inbound_count : int option;
    action_rate : int option;
    avg_handling_time_sec : int option;
    last_updated : string option;
  }

  type queue_view = {
    id : int;
    name : string;
    subject_types : string list;
    collection : string option;
    report_types : string list;
    description : string option;
    recommended_policies : string list;
    created_by : string option;
    created_at : string option;
    updated_at : string option;
    enabled : bool option;
    deleted_at : string option;
    stats : queue_stats option;
    original : Yojson.Safe.t;
  }

  type queues = { cursor : string option; queues : queue_view list }

  type assignment_view = {
    id : int option;
    did : string;
    report_id : int option;
    start_at : string option;
    end_at : string option;
    queue : queue_view option;
    moderator : team_member option;
    original : Yojson.Safe.t;
  }

  type assignments = {
    cursor : string option;
    assignments : assignment_view list;
  }

  type delete_queue_result = { deleted : bool; reports_migrated : int option }
  type route_reports_result = { assigned : int; unmatched : int }

  let parse_queue_stats json : queue_stats =
    {
      pending_count = Client.int_opt json "pendingCount";
      actioned_count = Client.int_opt json "actionedCount";
      escalated_count = Client.int_opt json "escalatedCount";
      inbound_count = Client.int_opt json "inboundCount";
      action_rate = Client.int_opt json "actionRate";
      avg_handling_time_sec = Client.int_opt json "avgHandlingTimeSec";
      last_updated = Client.string_opt json "lastUpdated";
    }

  let parse_queue_view json : queue_view =
    {
      id = Client.int_member json "id";
      name = Client.string_member json "name";
      subject_types = string_list json "subjectTypes";
      collection = Client.string_opt json "collection";
      report_types = string_list json "reportTypes";
      description = Client.string_opt json "description";
      recommended_policies = string_list json "recommendedPolicies";
      created_by = Client.string_opt json "createdBy";
      created_at = Client.string_opt json "createdAt";
      updated_at = Client.string_opt json "updatedAt";
      enabled = Client.bool_opt json "enabled";
      deleted_at = Client.string_opt json "deletedAt";
      stats =
        (match Yojson.Safe.Util.member "stats" json with
        | `Assoc _ as s -> Some (parse_queue_stats s)
        | _ -> None);
      original = json;
    }

  let parse_queue_result json : queue_view =
    match Yojson.Safe.Util.member "queue" json with
    | `Assoc _ as q -> parse_queue_view q
    | _ -> parse_queue_view json

  let parse_queues json : queues =
    {
      cursor = Client.string_opt json "cursor";
      queues = List.map parse_queue_view (Client.list_member json "queues");
    }

  let parse_assignment_view json : assignment_view =
    {
      id = Client.int_opt json "id";
      did = Client.string_member json "did";
      report_id = Client.int_opt json "reportId";
      start_at = Client.string_opt json "startAt";
      end_at = Client.string_opt json "endAt";
      queue =
        (match Yojson.Safe.Util.member "queue" json with
        | `Assoc _ as q -> Some (parse_queue_view q)
        | _ -> None);
      moderator =
        (match Yojson.Safe.Util.member "moderator" json with
        | `Assoc _ as m -> Some (parse_team_member m)
        | _ -> None);
      original = json;
    }

  let parse_assignments json : assignments =
    {
      cursor = Client.string_opt json "cursor";
      assignments =
        List.map parse_assignment_view (Client.list_member json "assignments");
    }

  let parse_delete_queue_result json : delete_queue_result =
    {
      deleted = Client.bool_member json "deleted";
      reports_migrated = Client.int_opt json "reportsMigrated";
    }

  let parse_route_reports_result json : route_reports_result =
    {
      assigned = Client.int_member json "assigned";
      unmatched = Client.int_member json "unmatched";
    }

  (** JSON body for [tools.ozone.queue.createQueue]. *)
  let create_queue_body ~name ?(subject_types = []) ?collection
      ?(report_types = []) ?description ?(recommended_policies = []) () :
      Yojson.Safe.t =
    `Assoc
      (("name", `String name)
       ::
       (match subject_types with
       | [] -> []
       | xs -> [ ("subjectTypes", json_strings xs) ])
      @ opt_json_str "collection" collection
      @ (match report_types with
        | [] -> []
        | xs -> [ ("reportTypes", json_strings xs) ])
      @ opt_json_str "description" description
      @
      match recommended_policies with
      | [] -> []
      | xs -> [ ("recommendedPolicies", json_strings xs) ])

  (** JSON body for [tools.ozone.queue.updateQueue]. *)
  let update_queue_body ~queue_id ?name ?enabled ?description
      ?recommended_policies () : Yojson.Safe.t =
    `Assoc
      ((("queueId", `Int queue_id) :: opt_json_str "name" name)
      @ opt_json_bool "enabled" enabled
      @ opt_json_str "description" description
      @
      match recommended_policies with
      | Some xs -> [ ("recommendedPolicies", json_strings xs) ]
      | None -> [])

  (** JSON body for [tools.ozone.queue.deleteQueue]. *)
  let delete_queue_body ~queue_id ?migrate_to_queue_id () : Yojson.Safe.t =
    `Assoc
      (("queueId", `Int queue_id)
      :: opt_json_int "migrateToQueueId" migrate_to_queue_id)

  (** Moderation queues via [tools.ozone.queue.listQueues]. *)
  let list_queues (s : Session.session) ~proxy ?enabled ?subject_type
      ?collection ?(report_types = []) ?limit ?cursor () : queues =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.queue.listQueues"
      (Client.opt_bool "enabled" enabled
      @ Client.opt_pair "subjectType" subject_type
      @ Client.opt_pair "collection" collection
      @ Client.repeat_param "reportTypes" report_types
      @ Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor)
    |> parse_queues

  (** Create a queue via [tools.ozone.queue.createQueue]. *)
  let create_queue (s : Session.session) ~proxy ~name ?subject_types ?collection
      ?report_types ?description ?recommended_policies () : queue_view =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.queue.createQueue"
      (Yojson.Safe.to_string
         (create_queue_body ~name ?subject_types ?collection ?report_types
            ?description ?recommended_policies ()))
    |> parse_queue_result

  (** Update queue [queue_id] via [tools.ozone.queue.updateQueue]. *)
  let update_queue (s : Session.session) ~proxy ~queue_id ?name ?enabled
      ?description ?recommended_policies () : queue_view =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.queue.updateQueue"
      (Yojson.Safe.to_string
         (update_queue_body ~queue_id ?name ?enabled ?description
            ?recommended_policies ()))
    |> parse_queue_result

  (** Delete queue [queue_id] via [tools.ozone.queue.deleteQueue]. *)
  let delete_queue (s : Session.session) ~proxy ~queue_id ?migrate_to_queue_id
      () : delete_queue_result =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.queue.deleteQueue"
      (Yojson.Safe.to_string
         (delete_queue_body ~queue_id ?migrate_to_queue_id ()))
    |> parse_delete_queue_result

  (** Assign [did] to [queue_id] via [tools.ozone.queue.assignModerator]. *)
  let assign_queue_moderator (s : Session.session) ~proxy ~queue_id ~did () :
      assignment_view =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.queue.assignModerator"
      (Yojson.Safe.to_string
         (`Assoc [ ("queueId", `Int queue_id); ("did", `String did) ]))
    |> parse_assignment_view

  (** Unassign [did] from [queue_id] via
      [tools.ozone.queue.unassignModerator]. *)
  let unassign_queue_moderator (s : Session.session) ~proxy ~queue_id ~did () :
      unit =
    ignore
      (Client.post_json ~session:s ~extra:(proxy_headers proxy)
         "tools.ozone.queue.unassignModerator"
         (Yojson.Safe.to_string
            (`Assoc [ ("queueId", `Int queue_id); ("did", `String did) ])))

  (** Queue assignments via [tools.ozone.queue.getAssignments]. *)
  let get_queue_assignments (s : Session.session) ~proxy ?only_active
      ?(queue_ids = []) ?(dids = []) ?limit ?cursor () : assignments =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.queue.getAssignments"
      (Client.opt_bool "onlyActive" only_active
      @ repeat_int "queueIds" queue_ids
      @ Client.repeat_param "dids" dids
      @ Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor)
    |> parse_assignments

  (** Route a report-id range via [tools.ozone.queue.routeReports]. *)
  let route_reports (s : Session.session) ~proxy ~start_report_id ~end_report_id
      () : route_reports_result =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.queue.routeReports"
      (Yojson.Safe.to_string
         (`Assoc
           [
             ("startReportId", `Int start_report_id);
             ("endReportId", `Int end_report_id);
           ]))
    |> parse_route_reports_result

  type report_activity =
    [ `Queue of string option
    | `Assignment of string option
    | `Escalation of string option
    | `Close of string option
    | `Reopen of string option
    | `Note
    | `Unknown of unknown_event ]

  type report_assignment = {
    did : string;
    assigned_at : string option;
    original : Yojson.Safe.t;
  }

  type report_view = {
    id : int;
    event_id : int option;
    status : string;
    subject : Yojson.Safe.t;
    report_type : string;
    reported_by : string option;
    comment : string option;
    created_at : string option;
    updated_at : string option;
    queued_at : string option;
    action_event_ids : int list;
    action_note : string option;
    related_report_count : int option;
    assignment : report_assignment option;
    queue : queue_view option;
    is_muted : bool option;
    is_automated : bool option;
    original : Yojson.Safe.t;
  }

  type reports = { cursor : string option; reports : report_view list }

  type report_activity_view = {
    id : int option;
    report_id : int option;
    activity : report_activity;
    internal_note : string option;
    public_note : string option;
    meta : Yojson.Safe.t option;
    is_automated : bool option;
    created_by : string option;
    created_at : string option;
    original : Yojson.Safe.t;
  }

  type report_activities = {
    cursor : string option;
    activities : report_activity_view list;
  }

  type report_stats = {
    date : string option;
    pending_count : int option;
    actioned_count : int option;
    escalated_count : int option;
    inbound_count : int option;
    action_rate : int option;
    avg_handling_time_sec : int option;
    last_updated : string option;
    computed_at : string option;
    original : Yojson.Safe.t;
  }

  type historical_stats = { cursor : string option; stats : report_stats list }
  type close_reports_result = { closed_count : int; report_ids : int list }

  (** Prefix [name] as [tools.ozone.report.defs#name]. *)
  let report_reason name = "tools.ozone.report.defs#" ^ name

  (** [tools.ozone.report.defs#reasonAppeal] report reason. *)
  let reason_appeal = report_reason "reasonAppeal"

  (** [tools.ozone.report.defs#reasonOther] report reason. *)
  let reason_other = report_reason "reasonOther"

  (** [tools.ozone.report.defs#reasonViolenceThreats] report reason. *)
  let reason_violence_threats = report_reason "reasonViolenceThreats"

  (** [tools.ozone.report.defs#reasonMisleadingSpam] report reason. *)
  let reason_misleading_spam = report_reason "reasonMisleadingSpam"

  let parse_report_activity json : report_activity =
    let t = type_name json in
    let previous = Client.string_opt json "previousStatus" in
    if ends_with "queueActivity" t then `Queue previous
    else if ends_with "assignmentActivity" t then `Assignment previous
    else if ends_with "escalationActivity" t then `Escalation previous
    else if ends_with "closeActivity" t then `Close previous
    else if ends_with "reopenActivity" t then `Reopen previous
    else if ends_with "noteActivity" t then `Note
    else `Unknown { type_ = t; original = json }

  let parse_report_assignment json : report_assignment =
    {
      did = Client.string_member json "did";
      assigned_at = Client.string_opt json "assignedAt";
      original = json;
    }

  let parse_report_view json : report_view =
    {
      id = Client.int_member json "id";
      event_id = Client.int_opt json "eventId";
      status = Client.string_member json "status";
      subject = Yojson.Safe.Util.member "subject" json;
      report_type = Client.string_member json "reportType";
      reported_by = Client.string_opt json "reportedBy";
      comment = Client.string_opt json "comment";
      created_at = Client.string_opt json "createdAt";
      updated_at = Client.string_opt json "updatedAt";
      queued_at = Client.string_opt json "queuedAt";
      action_event_ids = int_list json "actionEventIds";
      action_note = Client.string_opt json "actionNote";
      related_report_count = Client.int_opt json "relatedReportCount";
      assignment =
        (match Yojson.Safe.Util.member "assignment" json with
        | `Assoc _ as a -> Some (parse_report_assignment a)
        | _ -> None);
      queue =
        (match Yojson.Safe.Util.member "queue" json with
        | `Assoc _ as q -> Some (parse_queue_view q)
        | _ -> None);
      is_muted = Client.bool_opt json "isMuted";
      is_automated = Client.bool_opt json "isAutomated";
      original = json;
    }

  let parse_report_result json : report_view =
    match Yojson.Safe.Util.member "report" json with
    | `Assoc _ as r -> parse_report_view r
    | _ -> parse_report_view json

  let parse_reports json : reports =
    {
      cursor = Client.string_opt json "cursor";
      reports = List.map parse_report_view (Client.list_member json "reports");
    }

  let parse_report_activity_view json : report_activity_view =
    {
      id = Client.int_opt json "id";
      report_id = Client.int_opt json "reportId";
      activity =
        (match Yojson.Safe.Util.member "activity" json with
        | `Assoc _ as a -> parse_report_activity a
        | other -> `Unknown { type_ = ""; original = other });
      internal_note = Client.string_opt json "internalNote";
      public_note = Client.string_opt json "publicNote";
      meta =
        (match Yojson.Safe.Util.member "meta" json with
        | `Null -> None
        | other -> Some other);
      is_automated = Client.bool_opt json "isAutomated";
      created_by = Client.string_opt json "createdBy";
      created_at = Client.string_opt json "createdAt";
      original = json;
    }

  let parse_activity_result json : report_activity_view =
    match Yojson.Safe.Util.member "activity" json with
    | `Assoc _ as a -> parse_report_activity_view a
    | _ -> parse_report_activity_view json

  let parse_report_activities json : report_activities =
    {
      cursor = Client.string_opt json "cursor";
      activities =
        List.map parse_report_activity_view
          (Client.list_member json "activities");
    }

  let parse_report_stats json : report_stats =
    {
      date = Client.string_opt json "date";
      pending_count = Client.int_opt json "pendingCount";
      actioned_count = Client.int_opt json "actionedCount";
      escalated_count = Client.int_opt json "escalatedCount";
      inbound_count = Client.int_opt json "inboundCount";
      action_rate = Client.int_opt json "actionRate";
      avg_handling_time_sec = Client.int_opt json "avgHandlingTimeSec";
      last_updated = Client.string_opt json "lastUpdated";
      computed_at = Client.string_opt json "computedAt";
      original = json;
    }

  let parse_live_stats json : report_stats =
    match Yojson.Safe.Util.member "stats" json with
    | `Assoc _ as s -> parse_report_stats s
    | _ -> parse_report_stats json

  let parse_historical_stats json : historical_stats =
    {
      cursor = Client.string_opt json "cursor";
      stats = List.map parse_report_stats (Client.list_member json "stats");
    }

  let parse_close_reports_result json : close_reports_result =
    {
      closed_count = Client.int_member json "closedCount";
      report_ids = int_list json "reportIds";
    }

  let activity_json kind ?previous_status () : Yojson.Safe.t =
    `Assoc
      (("$type", `String ("tools.ozone.report.defs#" ^ kind))
      :: opt_json_str "previousStatus" previous_status)

  (** [tools.ozone.report.defs#queueActivity] activity. *)
  let queue_activity ?previous_status () =
    activity_json "queueActivity" ?previous_status ()

  (** [tools.ozone.report.defs#assignmentActivity] activity. *)
  let assignment_activity ?previous_status () =
    activity_json "assignmentActivity" ?previous_status ()

  (** [tools.ozone.report.defs#escalationActivity] activity. *)
  let escalation_activity ?previous_status () =
    activity_json "escalationActivity" ?previous_status ()

  (** [tools.ozone.report.defs#closeActivity] activity. *)
  let close_activity ?previous_status () =
    activity_json "closeActivity" ?previous_status ()

  (** [tools.ozone.report.defs#reopenActivity] activity. *)
  let reopen_activity ?previous_status () =
    activity_json "reopenActivity" ?previous_status ()

  (** [tools.ozone.report.defs#noteActivity] activity. *)
  let note_activity () = activity_json "noteActivity" ()

  (** JSON body for [tools.ozone.report.createActivity]. *)
  let create_activity_body ~activity ?report_id ?event_id ?internal_note
      ?public_note ?is_automated () : Yojson.Safe.t =
    `Assoc
      ((("activity", activity) :: opt_json_int "reportId" report_id)
      @ opt_json_int "eventId" event_id
      @ opt_json_str "internalNote" internal_note
      @ opt_json_str "publicNote" public_note
      @ opt_json_bool "isAutomated" is_automated)

  (** Reports via [tools.ozone.report.queryReports] ([status] required). *)
  let query_reports (s : Session.session) ~proxy ~status ?queue_id
      ?(report_types = []) ?subject ?did ?subject_type ?(collections = [])
      ?reported_after ?reported_before ?is_muted ?assigned_to ?sort_field
      ?sort_direction ?limit ?cursor () : reports =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.report.queryReports"
      ((("status", status) :: Client.opt_int "queueId" queue_id)
      @ Client.repeat_param "reportTypes" report_types
      @ Client.opt_pair "subject" subject
      @ Client.opt_pair "did" did
      @ Client.opt_pair "subjectType" subject_type
      @ Client.repeat_param "collections" collections
      @ Client.opt_pair "reportedAfter" reported_after
      @ Client.opt_pair "reportedBefore" reported_before
      @ Client.opt_bool "isMuted" is_muted
      @ Client.opt_pair "assignedTo" assigned_to
      @ Client.opt_pair "sortField" sort_field
      @ Client.opt_pair "sortDirection" sort_direction
      @ Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor)
    |> parse_reports

  (** Report [id] via [tools.ozone.report.getReport]. *)
  let get_report (s : Session.session) ~proxy ~id () : report_view =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.report.getReport"
      [ ("id", string_of_int id) ]
    |> parse_report_result

  (** Latest report via [tools.ozone.report.getLatestReport]. *)
  let get_latest_report (s : Session.session) ~proxy () : report_view =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.report.getLatestReport" []
    |> parse_report_result

  (** Assign a moderator via [tools.ozone.report.assignModerator]. *)
  let assign_report_moderator (s : Session.session) ~proxy ~report_id ?queue_id
      ?did ?is_permanent () : assignment_view =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.report.assignModerator"
      (Yojson.Safe.to_string
         (`Assoc
           ((("reportId", `Int report_id) :: opt_json_int "queueId" queue_id)
           @ opt_json_str "did" did
           @ opt_json_bool "isPermanent" is_permanent)))
    |> parse_assignment_view

  (** Unassign the moderator via [tools.ozone.report.unassignModerator]. *)
  let unassign_report_moderator (s : Session.session) ~proxy ~report_id () :
      assignment_view =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.report.unassignModerator"
      (Yojson.Safe.to_string (`Assoc [ ("reportId", `Int report_id) ]))
    |> parse_assignment_view

  (** Report assignments via [tools.ozone.report.getAssignments]. *)
  let get_report_assignments (s : Session.session) ~proxy ?only_active
      ?(report_ids = []) ?(dids = []) ?limit ?cursor () : assignments =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.report.getAssignments"
      (Client.opt_bool "onlyActive" only_active
      @ repeat_int "reportIds" report_ids
      @ Client.repeat_param "dids" dids
      @ Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor)
    |> parse_assignments

  (** Record activity via [tools.ozone.report.createActivity]. *)
  let create_activity (s : Session.session) ~proxy ~activity ?report_id
      ?event_id ?internal_note ?public_note ?is_automated () :
      report_activity_view =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.report.createActivity"
      (Yojson.Safe.to_string
         (create_activity_body ~activity ?report_id ?event_id ?internal_note
            ?public_note ?is_automated ()))
    |> parse_activity_result

  (** Activities for [report_id] via [tools.ozone.report.listActivities]. *)
  let list_activities (s : Session.session) ~proxy ~report_id ?limit ?cursor ()
      : report_activities =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.report.listActivities"
      ((("reportId", string_of_int report_id) :: Client.opt_int "limit" limit)
      @ Client.opt_pair "cursor" cursor)
    |> parse_report_activities

  (** Activities via [tools.ozone.report.queryActivities]. *)
  let query_activities (s : Session.session) ~proxy ?(activity_types = [])
      ?created_after ?created_before ?sort_direction ?limit ?cursor () :
      report_activities =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.report.queryActivities"
      (Client.repeat_param "activityTypes" activity_types
      @ Client.opt_pair "createdAfter" created_after
      @ Client.opt_pair "createdBefore" created_before
      @ Client.opt_pair "sortDirection" sort_direction
      @ Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor)
    |> parse_report_activities

  (** Move [report_id] to [queue_id] via [tools.ozone.report.reassignQueue]. *)
  let reassign_queue (s : Session.session) ~proxy ~report_id ~queue_id ?comment
      () : report_view =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.report.reassignQueue"
      (Yojson.Safe.to_string
         (`Assoc
           (("reportId", `Int report_id)
           :: ("queueId", `Int queue_id)
           :: opt_json_str "comment" comment)))
    |> parse_report_result

  (** Recompute stats via [tools.ozone.report.refreshStats]. *)
  let refresh_stats (s : Session.session) ~proxy ~start_date ~end_date
      ?queue_ids () : unit =
    ignore
      (Client.post_json ~session:s ~extra:(proxy_headers proxy)
         "tools.ozone.report.refreshStats"
         (Yojson.Safe.to_string
            (`Assoc
              (("startDate", `String start_date)
              :: ("endDate", `String end_date)
              ::
              (match queue_ids with
              | Some xs -> [ ("queueIds", json_ints xs) ]
              | None -> [])))))

  (** Close reports for [subject] via [tools.ozone.report.closeReports]. *)
  let close_reports (s : Session.session) ~proxy ~subject ?(report_types = [])
      ?internal_note ?is_automated () : close_reports_result =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.report.closeReports"
      (Yojson.Safe.to_string
         (`Assoc
           (("subject", `String subject)
            ::
            (match report_types with
            | [] -> []
            | xs -> [ ("reportTypes", json_strings xs) ])
           @ opt_json_str "internalNote" internal_note
           @ opt_json_bool "isAutomated" is_automated)))
    |> parse_close_reports_result

  (** Live report stats via [tools.ozone.report.getLiveStats]. *)
  let get_live_stats (s : Session.session) ~proxy ?queue_id ?moderator_did
      ?(report_types = []) () : report_stats =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.report.getLiveStats"
      (Client.opt_int "queueId" queue_id
      @ Client.opt_pair "moderatorDid" moderator_did
      @ Client.repeat_param "reportTypes" report_types)
    |> parse_live_stats

  (** Historical report stats via [tools.ozone.report.getHistoricalStats]. *)
  let get_historical_stats (s : Session.session) ~proxy ?queue_id ?moderator_did
      ?(report_types = []) ?start_date ?end_date ?limit ?cursor () :
      historical_stats =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.report.getHistoricalStats"
      (Client.opt_int "queueId" queue_id
      @ Client.opt_pair "moderatorDid" moderator_did
      @ Client.repeat_param "reportTypes" report_types
      @ Client.opt_pair "startDate" start_date
      @ Client.opt_pair "endDate" end_date
      @ Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor)
    |> parse_historical_stats
end
