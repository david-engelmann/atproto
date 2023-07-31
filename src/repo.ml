open Session
open Cohttp_client
open App
open Embed
open Facet
open Feed
open Notification

module Repo = struct
  type post_record =
    {
      text : string;
      record_type : string;
      embed : Embed.embed option;
      facets : (Facet.facet list) option;
      langs : (string list) option;
      reply : Notification.reply option;
      created_at : string;
    }

  let parse_post_record json : post_record =
    let open Yojson.Safe.Util in
    let text = json |> member "text" |> to_string in
    let record_type = json |> member "$type" |> to_string in
    let embed = Embed.parse_embed_option json in
    let facets = Feed.extract_facets_option json in
    let langs = Feed.extract_langs_option json in
    let reply = Notification.parse_reply_option json in
    let created_at = json |> member "createdAt" |> to_string in
    { text; record_type; embed; facets; langs; reply; created_at }

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

  let create_record (s : Session.session) (repo : string) (collection : string) ?rkey ?(validate = true) ?swap_commit (record : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let create_record_url = App.create_endpoint_url base_url (create_repo_endpoint "createRecord") in
    let fields =
      [
        Some ("repo", `String repo);
        Some ("collection", `String collection);
        Some ("record", `String record);
        Option.map (fun rkey -> ("rkey",  `String rkey)) rkey;
        Some ("validate", `Bool validate);
        Option.map (fun swap_commit -> ("swapCommit", `String swap_commit)) swap_commit;
      ] in
    let json_data = `Assoc (List.filter_map Fun.id fields) in
    let data = Yojson.Basic.to_string json_data in
    let created_record = Lwt_main.run (Cohttp_client.post_data_with_headers create_record_url data headers) in
    created_record

  let put_record (s : Session.session) (repo : string) (collection : string) ?rkey ?(validate = true) ?swap_record ?swap_commit (record : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let put_record_url = App.create_endpoint_url base_url (create_repo_endpoint "putRecord") in
    let fields =
      [
        Some ("repo", `String repo);
        Some ("collection", `String collection);
        Some ("record", `String record);
        Option.map (fun rkey -> ("rkey",  `String rkey)) rkey;
        Some ("validate", `Bool validate);
        Option.map (fun swap_record -> ("swapRecord", `String swap_record)) swap_record;
        Option.map (fun swap_commit -> ("swapCommit", `String swap_commit)) swap_commit;
      ] in
    let json_data = `Assoc (List.filter_map Fun.id fields) in
    let data = Yojson.Basic.to_string json_data in
    let puted_record = Lwt_main.run (Cohttp_client.post_data_with_headers put_record_url data headers) in
    puted_record

  let delete_record (s : Session.session) (repo : string) (collection : string) ?swap_record ?swap_commit (rkey : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let delete_record_url = App.create_endpoint_url base_url (create_repo_endpoint "deleteRecord") in
    let fields =
      [
        Some ("repo", `String repo);
        Some ("collection", `String collection);
        Some ("rkey", `String rkey);
        Option.map (fun swap_record -> ("swapRecord", `String swap_record)) swap_record;
        Option.map (fun swap_commit -> ("swapCommit", `String swap_commit)) swap_commit;
      ] in
    let json_data = `Assoc (List.filter_map Fun.id fields) in
    let data = Yojson.Basic.to_string json_data in
    let deleted_record = Lwt_main.run (Cohttp_client.post_data_with_headers delete_record_url data headers) in
    deleted_record

end

