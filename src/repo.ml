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

  let list_records (s : Session.session) (repo : string) (collection : string) (limit : int) (reverse : bool) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let list_records_url = App.create_endpoint_url base_url (create_repo_endpoint "listRecords") in
    let body = Cohttp_client.create_body_from_pairs [("repo", repo); ("collection", collection); ("limit", string_of_int limit); ("reverse", string_of_bool reverse)] in
    let records = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers list_records_url body headers) in
    records

  let create_record (s : Session.session) (repo : string) (collection : string) record ?rkey ?(validate = true) ?swap_commit : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let create_record_url = App.create_endpoint_url base_url (create_repo_endpoint "createRecord") in
    (*
    let use_rkey = Option.is_some rkey in
    let use_swap_commit = Option.is_some swap_commit in
    let use_validate = Option.is_some validate in
    let json_data =
      match (use_rkey, use_swap_commit, use_validate) with
      | (true, true, true) ->
          `Assoc [("repo", `String repo);
                  ("collection", `String collection);
                  ("record", record);
                  ("rkey", `String (Option.get rkey));
                  ("validate", `Bool (Option.get validate));
                  ("swapCommit", `String (Option.get swap_commit));]
     | (true, false, true) ->
          `Assoc [("repo", `String repo);
                  ("collection", `String collection);
                  ("record", record);
                  ("rkey", `String (Option.get rkey));
                  ("validate", `Bool (Option.get validate));]
    | (false, true, true) ->
          `Assoc [("repo", `String repo);
                  ("collection", `String collection);
                  ("record", record);
                  ("validate", `Bool (Option.get validate));
                  ("swapCommit", `String (Option.get swap_commit));]
    | (false, false, true) ->
          `Assoc [("repo", `String repo);
                  ("collection", `String collection);
                  ("record", record);
                  ("validate", `Bool (Option.get validate));]
    | (true, true, false) ->
        `Assoc [("repo", `String repo);
              ("collection", `String collection);
              ("record", record);
              ("rkey", `String (Option.get rkey));
              ("swapCommit", `String (Option.get swap_commit));]
    | (true, false, false) ->
        `Assoc [("repo", `String repo);
              ("collection", `String collection);
              ("record", record);
              ("rkey", `String (Option.get rkey));
              ]
    | (false, true, false) ->
        `Assoc [("repo", `String repo);
              ("collection", `String collection);
              ("record", record);
              ("swapCommit", `String (Option.get swap_commit));]
    | (false, false, false) ->
        `Assoc [("repo", `String repo);
              ("collection", `String collection);
              ("record", record);
              ]
    *)
    let fields =
      [
        Some ("repo", `String repo);
        Some ("collection", `String collection);
        Some ("record", record);
        Option.map (fun rkey -> ("rkey",  `String rkey)) rkey;
        Some ("validate", `Bool validate);
        Option.map (fun swap_commit -> ("swapCommit", `String swap_commit)) swap_commit;
      ] in
    let json_data = `Assoc (List.filter_map Fun.id fields) in
    let data = Yojson.Basic.to_string json_data in
    let created_record = Lwt_main.run (Cohttp_client.post_data_with_headers create_record_url data headers) in
    created_record

end
