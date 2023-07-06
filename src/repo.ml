open Session
open Cohttp_client
open App

module Repo = struct
  let create_repo_endpoint (query_name : string) : string =
    "com.atproto.repo" ^ "." ^ query_name

  let describe_repo (s : Session.session) (repo : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let describe_repo_url = App.create_endpoint_url base_url (create_repo_endpoint "describeRepo") in
    let body = Cohttp_client.create_body_from_pairs [("repo", repo)] in
    let repo_description = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers describe_repo_url body headers) in
    repo_description

  let get_record (s : Session.session) (repo : string) (collection : string) (rkey : string) (cid : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_record_url = App.create_endpoint_url base_url (create_repo_endpoint "getRecord") in
    let body = Cohttp_client.create_body_from_pairs [("repo", repo); ("collection", collection); ("rkey", rkey); ("cid", cid)] in
    let record = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_record_url body headers) in
    record

  let list_records (s : Session.session) (repo : string) (collection : string) (limit : int) (reverse : boolean) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let list_records_url = App.create_endpoint_url base_url (create_repo_endpoint "listRecords") in
    let body = Cohttp_client.create_body_from_pairs [("repo", repo); ("collection", collection); ("limit", limit); ("reverse", reverse)] in
    let records = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers list_records_url body headers) in
    records

end