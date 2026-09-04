open Client
open Session

(** [com.atproto.moderation.createReport] — user reports (not Ozone operator tools). *)
module Moderation = struct
  type strong_ref = { uri : string; cid : string }
  type repo_ref = { did : string }
  type mod_tool = { name : string; meta : Yojson.Safe.t option }

  type report_response = {
    id : int;
    created_at : string;
    reason_type : string;
    reported_by : string;
    reason : string option;
    subject : Yojson.Safe.t;
    mod_tool : mod_tool option;
  }

  (** [com.atproto.moderation.defs#reasonSpam] report reason. *)
  let reason_spam = "com.atproto.moderation.defs#reasonSpam"

  (** [com.atproto.moderation.defs#reasonViolation] report reason. *)
  let reason_violation = "com.atproto.moderation.defs#reasonViolation"

  (** [com.atproto.moderation.defs#reasonMisleading] report reason. *)
  let reason_misleading = "com.atproto.moderation.defs#reasonMisleading"

  (** [com.atproto.moderation.defs#reasonSexual] report reason. *)
  let reason_sexual = "com.atproto.moderation.defs#reasonSexual"

  (** [com.atproto.moderation.defs#reasonRude] report reason. *)
  let reason_rude = "com.atproto.moderation.defs#reasonRude"

  (** [com.atproto.moderation.defs#reasonOther] report reason. *)
  let reason_other = "com.atproto.moderation.defs#reasonOther"

  (** [com.atproto.moderation.defs#reasonAppeal] report reason. *)
  let reason_appeal = "com.atproto.moderation.defs#reasonAppeal"

  let parse_mod_tool json : mod_tool option =
    match Yojson.Safe.Util.member "modTool" json with
    | `Assoc _ as t ->
        Some
          {
            name =
              (match Yojson.Safe.Util.member "name" t with
              | `String s -> s
              | _ -> "");
            meta =
              (match Yojson.Safe.Util.member "meta" t with
              | `Null -> None
              | other -> Some other);
          }
    | _ -> None

  let mod_tool_json (t : mod_tool) : Yojson.Safe.t =
    let fields =
      ("name", `String t.name)
      :: (match t.meta with Some m -> [ ("meta", m) ] | None -> [])
    in
    `Assoc fields

  let create_moderation_endpoint (query_name : string) : string =
    "com.atproto.moderation" ^ "." ^ query_name

  let convert_body_to_json (body : string) : Yojson.Safe.t =
    let json = Yojson.Safe.from_string body in
    json

  let parse_report_response json : report_response =
    let open Yojson.Safe.Util in
    let id = json |> member "id" |> to_int in
    let created_at = json |> member "createdAt" |> to_string in
    let reason_type = json |> member "reasonType" |> to_string in
    let reported_by = json |> member "reportedBy" |> to_string in
    let reason =
      match json |> member "reason" with `String s -> Some s | _ -> None
    in
    let subject = json |> member "subject" in
    {
      id;
      created_at;
      reason_type;
      reported_by;
      reason;
      subject;
      mod_tool = parse_mod_tool json;
    }

  let create_subject_from_strong_ref (ref : strong_ref) =
    let subject =
      `Assoc
        [
          ("$type", `String "com.atproto.repo.strongRef");
          ("uri", `String ref.uri);
          ("cid", `String ref.cid);
        ]
    in
    subject

  let create_subject_from_repo_ref (ref : repo_ref) =
    let subject =
      `Assoc
        [
          ("$type", `String "com.atproto.admin.defs#repoRef");
          ("did", `String ref.did);
        ]
    in
    subject

  let report_fields reason_type ?reason ?mod_tool subject =
    [ ("reasonType", `String reason_type); ("subject", subject) ]
    @ (match reason with Some r -> [ ("reason", `String r) ] | None -> [])
    @
    match mod_tool with
    | Some t -> [ ("modTool", mod_tool_json t) ]
    | None -> []

  (** Yojson body for [com.atproto.moderation.createReport] on a record
      ([com.atproto.repo.strongRef]). Shares [report_fields] with the
      string wrappers. *)
  let create_report_body_from_strong_ref (reason_type : string) ?reason
      ?mod_tool (subject : strong_ref) : Yojson.Safe.t =
    let subject = create_subject_from_strong_ref subject in
    `Assoc (report_fields reason_type ?reason ?mod_tool subject)

  (** Yojson body for [com.atproto.moderation.createReport] on an
      account ([com.atproto.admin.defs#repoRef]). Shares [report_fields]
      with the string wrappers. *)
  let create_report_body_from_repo_ref (reason_type : string) ?reason ?mod_tool
      (subject : repo_ref) : Yojson.Safe.t =
    let subject = create_subject_from_repo_ref subject in
    `Assoc (report_fields reason_type ?reason ?mod_tool subject)

  (** JSON body for [com.atproto.moderation.createReport] on a record
      ([com.atproto.repo.strongRef]). *)
  let create_report_data_from_strong_ref (reason_type : string) ?reason
      ?mod_tool (subject : strong_ref) : string =
    Yojson.Safe.to_string
      (create_report_body_from_strong_ref reason_type ?reason ?mod_tool subject)

  (** JSON body for [com.atproto.moderation.createReport] on an
      account ([com.atproto.admin.defs#repoRef]). *)
  let create_report_data_from_repo_ref (reason_type : string) ?reason ?mod_tool
      (subject : repo_ref) : string =
    Yojson.Safe.to_string
      (create_report_body_from_repo_ref reason_type ?reason ?mod_tool subject)

  (** User report via [com.atproto.moderation.createReport] on a record
      ([com.atproto.repo.strongRef]). Not an Ozone operator tool. *)
  let create_report_with_strong_ref (s : Session.session) (reason_type : string)
      ?reason ?mod_tool (subject : strong_ref) : report_response =
    create_report_body_from_strong_ref reason_type ?reason ?mod_tool subject
    |> Yojson.Safe.to_string
    |> Client.post_json ~session:s "com.atproto.moderation.createReport"
    |> parse_report_response

  (** User report via [com.atproto.moderation.createReport] on an
      account ([com.atproto.admin.defs#repoRef]). *)
  let create_report_with_repo_ref (s : Session.session) (reason_type : string)
      ?reason ?mod_tool (subject : repo_ref) : report_response =
    create_report_body_from_repo_ref reason_type ?reason ?mod_tool subject
    |> Yojson.Safe.to_string
    |> Client.post_json ~session:s "com.atproto.moderation.createReport"
    |> parse_report_response
end
