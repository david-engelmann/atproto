open Cohttp_client
open App
open Session

module Moderation = struct
  let create_moderation_endpoint (query_name : string) : string =
    "com.atproto.moderation" ^ "." ^ query_name

  let create_report (s : Session.session) (reason_type : string) ?reason (subject : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let create_report_url = App.create_endpoint_url base_url (create_moderation_endpoint "createReport") in
    let data =
      match reason with
      | Some r -> Printf.sprintf "{\"reasonType\": \"%s\", \"reason\": \"%s\", \"subject\": \"%s\"}" reason_type r subject
      | None -> Printf.sprintf "{\"reasonType\": \"%s\", \"subject\": \"%s\"}" reason_type subject
    in
    let created_report = Lwt_main.run (Cohttp_client.post_data_with_headers data create_report_url headers) in
    created_report
end
