open Session
open Client
open Xrpc

(** tools.ozone.* — Ozone moderation client. Always send atproto-proxy. *)
module Ozone = struct
  let labeler_proxy (did : string) : Xrpc.proxy = Xrpc.labeler_proxy did
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
    | `Email of email_event
    | `Tag of tag_event
    | `Resolve_appeal of comment_only_event
    | `Priority_score of priority_score_event
    | `Unknown of unknown_event ]

  type subject_status = {
    subject : subject;
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
    event : event;
    subject : subject;
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
    else `Unknown { type_ = t; original = json }

  let parse_subject_status json : subject_status =
    {
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

  let get_records (s : Session.session) ~proxy ~uris () : record_view list =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.getRecords"
      (Client.repeat_param "uris" uris)
    |> fun json -> List.map parse_record (Client.list_member json "records")

  let get_repos (s : Session.session) ~proxy ~dids () : repo_view list =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.getRepos"
      (Client.repeat_param "dids" dids)
    |> fun json -> List.map parse_repo (Client.list_member json "repos")

  let get_subjects (s : Session.session) ~proxy ~subjects () : subjects =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.getSubjects"
      (Client.repeat_param "subjects" subjects)
    |> parse_subjects

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

  let get_account_timeline (s : Session.session) ~proxy ~did () :
      account_timeline =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.getAccountTimeline"
      [ ("did", did) ]
    |> parse_account_timeline

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

  let schedule_action (s : Session.session) ~proxy ~action ~subjects ~created_by
      ~scheduling ?mod_tool () : batch_result =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.moderation.scheduleAction"
      (Yojson.Safe.to_string
         (schedule_action_body ~action ~subjects ~created_by ~scheduling
            ?mod_tool ()))
    |> parse_batch_result

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

  (* Remaining operator namespaces: communication, set, setting, team,
     safelink, signature, verification, hosting. *)

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

  let list_templates (s : Session.session) ~proxy () : templates =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.communication.listTemplates" []
    |> parse_templates

  let create_template_body ~name ~content_markdown ?subject ?lang () :
      Yojson.Safe.t =
    `Assoc
      ([ ("name", `String name); ("contentMarkdown", `String content_markdown) ]
      @ (match subject with Some s -> [ ("subject", `String s) ] | None -> [])
      @ match lang with Some s -> [ ("lang", `String s) ] | None -> [])

  let create_template (s : Session.session) ~proxy ~name ~content_markdown
      ?subject ?lang () : template =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.communication.createTemplate"
      (Yojson.Safe.to_string
         (create_template_body ~name ~content_markdown ?subject ?lang ()))
    |> parse_template

  let update_template (s : Session.session) ~proxy ~id ?name ?content_markdown
      ?subject ?disabled () : template =
    let fields =
      ("id", `String id)
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

  let get_set_values (s : Session.session) ~proxy ~name ?limit ?cursor () :
      set_values =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.set.getValues"
      ((("name", name) :: Client.opt_int "limit" limit)
      @ Client.opt_pair "cursor" cursor)
    |> parse_set_values

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

  let list_options (s : Session.session) ~proxy ?prefix ?scope ?limit ?cursor ()
      : setting_options =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.setting.listOptions"
      (Client.opt_pair "prefix" prefix
      @ Client.opt_pair "scope" scope
      @ Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor)
    |> parse_setting_options

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

  let add_member (s : Session.session) ~proxy ~did ~role () : team_member =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.team.addMember"
      (Yojson.Safe.to_string
         (`Assoc [ ("did", `String did); ("role", `String role) ]))
    |> parse_team_member

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
      url =
        (match Client.string_opt json "url" with
        | Some s -> s
        | None -> Client.string_member json "pattern");
      pattern_type = Client.string_opt json "patternType";
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

  let add_safelink_rule (s : Session.session) ~proxy ~url ~pattern_type ~action
      ?reason ?comment () : url_rule =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.safelink.addRule"
      (Yojson.Safe.to_string
         (`Assoc
           ([
              ("url", `String url);
              ("patternType", `String pattern_type);
              ("action", `String action);
            ]
           @ (match reason with
             | Some r -> [ ("reason", `String r) ]
             | None -> [])
           @
           match comment with
           | Some c -> [ ("comment", `String c) ]
           | None -> [])))
    |> parse_url_rule

  let update_safelink_rule (s : Session.session) ~proxy ~url ~pattern_type
      ?action ?reason ?comment () : url_rule =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.safelink.updateRule"
      (Yojson.Safe.to_string
         (`Assoc
           (("url", `String url)
            :: ("patternType", `String pattern_type)
            ::
            (match action with
            | Some a -> [ ("action", `String a) ]
            | None -> [])
           @ (match reason with
             | Some r -> [ ("reason", `String r) ]
             | None -> [])
           @
           match comment with
           | Some c -> [ ("comment", `String c) ]
           | None -> [])))
    |> parse_url_rule

  let remove_safelink_rule (s : Session.session) ~proxy ~url ~pattern_type
      ?comment () : url_rule =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.safelink.removeRule"
      (Yojson.Safe.to_string
         (`Assoc
           (("url", `String url)
           :: ("patternType", `String pattern_type)
           ::
           (match comment with
           | Some c -> [ ("comment", `String c) ]
           | None -> []))))
    |> parse_url_rule

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

  let find_correlation (s : Session.session) ~proxy ~dids () : Yojson.Safe.t =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.signature.findCorrelation"
      (Client.repeat_param "dids" dids)

  let find_related_accounts (s : Session.session) ~proxy ~did ?limit ?cursor ()
      : related_accounts =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.signature.findRelatedAccounts"
      ((("did", did) :: Client.opt_int "limit" limit)
      @ Client.opt_pair "cursor" cursor)
    |> parse_related_accounts

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
    revoked_at : string option;
    original : Yojson.Safe.t;
  }

  type verifications = {
    cursor : string option;
    verifications : verification_view list;
  }

  let parse_verification_view json : verification_view =
    {
      uri = Client.string_member json "uri";
      issuer = Client.string_opt json "issuer";
      subject = Client.string_opt json "subject";
      handle = Client.string_opt json "handle";
      revoked_at = Client.string_opt json "revokedAt";
      original = json;
    }

  let parse_verifications json : verifications =
    {
      cursor = Client.string_opt json "cursor";
      verifications =
        List.map parse_verification_view
          (Client.list_member json "verifications");
    }

  let list_verifications (s : Session.session) ~proxy ?cursor ?limit
      ?(issuers = []) ?(subjects = []) () : verifications =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.verification.listVerifications"
      (Client.opt_pair "cursor" cursor
      @ Client.opt_int "limit" limit
      @ Client.repeat_param "issuers" issuers
      @ Client.repeat_param "subjects" subjects)
    |> parse_verifications

  let grant_verifications (s : Session.session) ~proxy ~verifications () :
      verifications =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.verification.grantVerifications"
      (Yojson.Safe.to_string
         (`Assoc [ ("verifications", `List verifications) ]))
    |> parse_verifications

  let revoke_verifications (s : Session.session) ~proxy ~uris ?revoke_reason ()
      : verifications =
    Client.post_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.verification.revokeVerifications"
      (Yojson.Safe.to_string
         (`Assoc
           ([ ("uris", `List (List.map (fun u -> `String u) uris)) ]
           @
           match revoke_reason with
           | Some r -> [ ("revokeReason", `String r) ]
           | None -> [])))
    |> parse_verifications

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

  let get_account_history (s : Session.session) ~proxy ~did ?events ?cursor
      ?limit () : account_history =
    Client.get_json ~session:s ~extra:(proxy_headers proxy)
      "tools.ozone.hosting.getAccountHistory"
      (("did", did)
       :: Client.repeat_param "events" (Option.value events ~default:[])
      @ Client.opt_pair "cursor" cursor
      @ Client.opt_int "limit" limit)
    |> parse_account_history
end
