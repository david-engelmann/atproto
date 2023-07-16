open Cohttp_client
open App
open Session

module Moderation = struct
  type strong_ref =
    {
      uri : string;
      cid : string;
    }

  type repo_ref =
    {
      did : string;
    }

  type report_response =
    {
      id : int;
      created_at : string;
      reason_type : string;
      reported_by : string;
      subject : Yojson.Safe.t;
    }

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
    let subject = json |> member "subject" in
    { id; created_at; reason_type; reported_by; subject; }

  let create_subject_from_strong_ref (ref : strong_ref) =
    let subject = `Assoc [
      ("$type", `String "com.atproto.repo.strongRef");
      ("uri", `String ref.uri);
      ("cid", `String ref.cid);
    ] in
    subject

  let create_subject_from_repo_ref (ref : repo_ref) =
    let subject = `Assoc [
      ("$type", `String "com.atproto.admin.defs#repoRef");
      ("did", `String ref.did);
    ] in
    subject

  let create_report_data_from_strong_ref (reason_type : string) ?reason (subject : strong_ref) : string =
    let subject = create_subject_from_strong_ref subject in
    let report_data =
      match reason with
      | Some r -> `Assoc [
            ("reasonType", `String reason_type);
            ("reason", `String r);
            ("subject", subject);
        ]
      | None -> `Assoc [
            ("reasonType", `String reason_type);
            ("subject", subject);
        ]
    in
    Yojson.Basic.to_string report_data

  let create_report_data_from_repo_ref (reason_type : string) ?reason (subject : repo_ref) : string =
    let subject = create_subject_from_repo_ref subject in
    let report_data =
      match reason with
      | Some r -> `Assoc [
            ("reasonType", `String reason_type);
            ("reason", `String r);
            ("subject", subject);
        ]
      | None -> `Assoc [
            ("reasonType", `String reason_type);
            ("subject", subject);
        ]
    in
    Yojson.Basic.to_string report_data

  let create_report_with_strong_ref (s : Session.session) (reason_type : string) ?reason (subject : strong_ref) : report_response =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let create_report_url = App.create_endpoint_url base_url (create_moderation_endpoint "createReport") in
    let data = create_report_data_from_strong_ref reason_type ?reason subject in
    let created_report = Lwt_main.run (Cohttp_client.post_data_with_headers create_report_url data headers) in
    created_report |> convert_body_to_json |> parse_report_response

  let create_report_with_repo_ref (s : Session.session) (reason_type : string) ?reason (subject : repo_ref) : report_response =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let create_report_url = App.create_endpoint_url base_url (create_moderation_endpoint "createReport") in
    let data = create_report_data_from_repo_ref reason_type ?reason subject in
    let created_report = Lwt_main.run (Cohttp_client.post_data_with_headers create_report_url data headers) in
    created_report |> convert_body_to_json |> parse_report_response
end
