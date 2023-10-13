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
    let get_blocks_url = App.create_endpoint_url base_url (create_sync_endpoint "getBlocks") in
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
    let blocks = Cohttp_client.get_request_with_body_and_headers get_blocks_url body headers in
    blocks

  let get_checkout (s : Session.session) (did : string) (commit : string option) =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_checkout_url = App.create_endpoint_url base_url (create_sync_endpoint "getCheckout") in
    match commit with
    | None ->
        let body = Cohttp_client.create_body_from_pairs [("did", did); ("commit", "HEAD")] in
        let checkout = Lwt_main.run (Cohttp_client.get_stream_request_with_body_and_headers get_checkout_url body headers) in
        checkout
    | Some c ->
        let body = Cohttp_client.create_body_from_pairs [("did", did); ("commit", c)] in
        let checkout = Lwt_main.run (Cohttp_client.get_stream_request_with_body_and_headers get_checkout_url body headers) in
        checkout

  let get_commit_path (s : Session.session) (did : string) (latest : string) (earliest : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_commit_path_url = App.create_endpoint_url base_url (create_sync_endpoint "getCommitPath") in
    let body = Cohttp_client.create_body_from_pairs [("did", did); ("latest", latest); ("earliest", earliest)] in
    let commit_path = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_commit_path_url body headers) in
    commit_path

  let get_head (s : Session.session) (did : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_head_url = App.create_endpoint_url base_url (create_sync_endpoint "getHead") in
    let body = Cohttp_client.create_body_from_pairs [("did", did)] in
    let head = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_head_url body headers) in
    head

  let get_record (s : Session.session) (did : string) (collection : string) (rkey : string) (commit : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_record_url = App.create_endpoint_url base_url (create_sync_endpoint "getRecord") in
    let body = Cohttp_client.create_body_from_pairs [("did", did); ("collection", collection); ("rkey", rkey); ("commit", commit)] in
    let record = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_record_url body headers) in
    record

  let get_repo (s : Session.session) (did : string) (earliest : string) (latest : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_repo_url = App.create_endpoint_url base_url (create_sync_endpoint "getRepo") in
    let body = Cohttp_client.create_body_from_pairs [("did", did); ("earliest", earliest); ("latest", latest)] in
    let repo = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_repo_url body headers) in
    repo

  let list_blobs (s : Session.session) (did : string) (earliest : string) (latest : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let list_blobs_url = App.create_endpoint_url base_url (create_sync_endpoint "listBlobs") in
    let body = Cohttp_client.create_body_from_pairs [("did", did); ("earliest", earliest); ("latest", latest)] in
    let blobs = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers list_blobs_url body headers) in
    blobs

  let list_repos (s : Session.session) (limit : int) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let list_repos_url = App.create_endpoint_url base_url (create_sync_endpoint "listRepos") in
    let body = Cohttp_client.create_body_from_pairs [("limit", string_of_int limit)] in
    let repos = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers list_repos_url body headers) in
    repos

  let notify_of_update (s : Session.session) (hostname : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let notify_of_update_url = App.create_endpoint_url base_url (create_sync_endpoint "notifyOfUpdate") in
    let body = Cohttp_client.create_body_from_pairs [("hostname", hostname)] in
    let notify_response = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers notify_of_update_url body headers) in
    notify_response

  let request_crawl (s : Session.session) (hostname : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let request_crawl_url = App.create_endpoint_url base_url (create_sync_endpoint "requestCrawl") in
    let body = Cohttp_client.create_body_from_pairs [("hostname", hostname)] in
    let crawl_response = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers request_crawl_url body headers) in
    crawl_response
end
