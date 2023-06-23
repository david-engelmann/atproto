open Session
open Cohttp_client
open App

module Sync = struct
  let create_sync_endpoint (query_name : string) : string =
    "com.atproto.sync" ^ "." ^ query_name

  let get_blob (s : Session.session) (did : string) (cid: string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_blob_url = App.create_endpoint_url base_url (create_sync_endpoint "getBlob") in
    let body = Cohttp_client.create_body_from_pairs [("did", did); ("cid", cid)] in
    let blob = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_blob_url body headers) in
    blob

end
