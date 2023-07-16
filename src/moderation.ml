open Cohttp_client
open App
open Session
open Yojson

module Moderation = struct
  type strong_ref =
  {
    uri : string;
    cid : string;
  }

  let create_moderation_endpoint (query_name : string) : string =
    "com.atproto.moderation" ^ "." ^ query_name

  let create_subject_from_strong_ref (ref : strong_ref) : string =
    let json_subject = `Assoc [
      ("$type", `String "com.atproto.repo.strongRef");
      ("uri", `String ref.uri);
      ("cid", `String ref.cid);
    ]
    let subject = Yojson.Basic.to_string json_subject in
    subject

  let create_report_with_strong_ref (s : Session.session) (reason_type : string) ?reason (subject : strong_ref) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let create_report_url = App.create_endpoint_url base_url (create_moderation_endpoint "createReport") in
    let data =
      match reason with
      | Some r -> Printf.sprintf "{\"reasonType\": \"%s\", \"reason\": \"%s\", \"subject\": \"%s\"}" reason_type r (create_subject_from_strong_ref subject)
      | None -> Printf.sprintf "{\"reasonType\": \"%s\", \"subject\": \"%s\"}" reason_type (create_subject_from_strong_ref subject)
    in
    let created_report = Lwt_main.run (Cohttp_client.post_data_with_headers create_report_url data headers) in
    created_report
end
