open Session
open Cohttp_client
open App

module Sync = struct
  let create_sync_endpoint (query_name : string) : string =
    "com.atproto.sync" ^ "." ^ query_name

  let get_blob (s : Session.session) (did : string) (cid : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_blob_url = App.create_endpoint_url base_url (create_sync_endpoint "getBlob") in
    let body = Cohttp_client.create_body_from_pairs [("did", did); ("cid", cid)] in
    let content_type = Lwt_main.run (Cohttp_client.get_content_type_with_body_headers get_blob_url body headers) in
    Printf.printf "Content-Type: %s\n" content_type;
    let blob = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_blob_url body headers) in
    blob

  let download_image (s : Session.session) (did : string) (cid : string) (filename : string) : unit Lwt.t =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_blob_url = App.create_endpoint_url base_url (create_sync_endpoint "getBlob") in
    let body = Cohttp_client.create_body_from_pairs [("did", did); ("cid", cid)] in
    let content_type = Lwt_main.run (Cohttp_client.get_content_type_with_body_headers get_blob_url body headers) in
    let blob = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_blob_url body headers) in
    match content_type with
    | ct when List.mem ct ["image/jpeg"; "image/png"; "image/gif"; "image/bmp"; "image/webp"; "image/svg+xml"; "image/tiff"] ->
      Lwt_io.with_file ~mode:Lwt_io.Output filename (fun oc -> Lwt_io.write oc blob)
    | _ -> Lwt.return ()

end
