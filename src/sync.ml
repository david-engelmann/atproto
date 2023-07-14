open Session
open Cohttp_client
open App

module Sync = struct
  let create_sync_endpoint (query_name : string) : string =
    "com.atproto.sync" ^ "." ^ query_name

  let get_blob (s : Session.session) (did : string) (cid : string) : string Lwt.t =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_blob_url = App.create_endpoint_url base_url (create_sync_endpoint "getBlob") in
    let body = Cohttp_client.create_body_from_pairs [("did", did); ("cid", cid)] in
    let blob = Cohttp_client.get_request_with_body_and_headers get_blob_url body headers in
    blob

  let download_image (s : Session.session) (did : string) (cid : string) (filename : string) : unit Lwt.t =
    let open Lwt.Infix in
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_blob_url = App.create_endpoint_url base_url (create_sync_endpoint "getBlob") in
    let body = Cohttp_client.create_body_from_pairs [("did", did); ("cid", cid)] in
    Cohttp_client.get_content_type_with_body_headers get_blob_url body headers >>= fun content_type ->
    match content_type with
    | ct when List.mem ct ["image/jpeg"; "image/png"; "image/gif"; "image/bmp"; "image/webp"; "image/svg+xml"; "image/tiff"] ->
      get_blob s did cid >>= fun blob ->
      Lwt_io.with_file ~mode:Lwt_io.Output filename (fun oc -> Lwt_io.write oc blob)
    | _ -> Lwt.return ()

  let get_blocks (s : Session.session) (did : string) (cids : string list) : string Lwt.t =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_block_url = App.create_endpoint_url base_url (create_sync_endpoint "getBlocks") in
    let body = Cohttp_client.create_body_from_pairs [("did", did); ("cids",
                                                                    Yojson.Basic.to_string
                                                                      (`List
                                                                        (List.map
                                                                        (fun s
                                                                             ->
                                                                        `String
                                                                        s)
                                                                        cids
                                                                        )))] in
    let block = Cohttp_client.get_request_with_body_and_headers get_block_url body headers in
    block
end
