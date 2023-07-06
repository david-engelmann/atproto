open Session
open Cohttp_client
open App

module Label = struct
  let create_label_endpoint (query_name : string) : string =
    "com.atproto.label" ^ "." ^ query_name

  (* List of AT URI patterns to match (boolean 'OR'). Each may
   * be a prefix (ending with '*'; will match inclusive of the string leading to
   * '*'), or a full URI *)
  let query_labels (s : Session.session) (uri_patterns : string list) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let query_labels_url = App.create_endpoint_url base_url (create_label_endpoint "queryLabels") in
    let body = Cohttp_client.add_query_params "uriPatterns" uri_patterns in
    let labels = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers query_labels_url body headers) in
    labels
end
