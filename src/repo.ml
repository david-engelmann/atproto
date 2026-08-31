open Session
open Cohttp_client
open App
open Embed
open Facet
open Feed
open Notification

module Repo = struct
  type post_record = {
    text : string;
    record_type : string;
    embed : Embed.embed option;
    facets : Facet.facet list option;
    langs : string list option;
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
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let describe_repo_url =
      App.create_endpoint_url base_url (create_repo_endpoint "describeRepo")
    in
    let body = Cohttp_client.create_body_from_pairs [ ("repo", repo) ] in
    let repo_description =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers describe_repo_url body
           headers)
    in
    repo_description

  let get_record (s : Session.session) (repo : string) (collection : string)
      (rkey : string) (cid : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let get_record_url =
      App.create_endpoint_url base_url (create_repo_endpoint "getRecord")
    in
    let body =
      Cohttp_client.create_body_from_pairs
        [
          ("repo", repo);
          ("collection", collection);
          ("rkey", rkey);
          ("cid", cid);
        ]
    in
    let record =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers get_record_url body
           headers)
    in
    record

  let list_records (s : Session.session) (repo : string) (collection : string)
      (limit : int) (reverse : bool) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let list_records_url =
      App.create_endpoint_url base_url (create_repo_endpoint "listRecords")
    in
    let body =
      Cohttp_client.create_body_from_pairs
        [
          ("repo", repo);
          ("collection", collection);
          ("limit", string_of_int limit);
          ("reverse", string_of_bool reverse);
        ]
    in
    let records =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers list_records_url body
           headers)
    in
    records

  let create_record (s : Session.session) (repo : string) (collection : string)
      ?rkey ?(validate = true) ?swap_commit (record : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let create_record_url =
      App.create_endpoint_url base_url (create_repo_endpoint "createRecord")
    in
    let fields =
      [
        Some ("repo", `String repo);
        Some ("collection", `String collection);
        Some
          ( "record",
            try Yojson.Basic.from_string record with _ -> `String record );
        Option.map (fun rkey -> ("rkey", `String rkey)) rkey;
        Some ("validate", `Bool validate);
        Option.map
          (fun swap_commit -> ("swapCommit", `String swap_commit))
          swap_commit;
      ]
    in
    let json_data = `Assoc (List.filter_map Fun.id fields) in
    let data = Yojson.Basic.to_string json_data in
    let created_record =
      Lwt_main.run
        (Cohttp_client.post_data_with_headers create_record_url data headers)
    in
    created_record

  let put_record (s : Session.session) (repo : string) (collection : string)
      ?rkey ?(validate = true) ?swap_record ?swap_commit (record : string) :
      string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let put_record_url =
      App.create_endpoint_url base_url (create_repo_endpoint "putRecord")
    in
    let fields =
      [
        Some ("repo", `String repo);
        Some ("collection", `String collection);
        Some
          ( "record",
            try Yojson.Basic.from_string record with _ -> `String record );
        Option.map (fun rkey -> ("rkey", `String rkey)) rkey;
        Some ("validate", `Bool validate);
        Option.map
          (fun swap_record -> ("swapRecord", `String swap_record))
          swap_record;
        Option.map
          (fun swap_commit -> ("swapCommit", `String swap_commit))
          swap_commit;
      ]
    in
    let json_data = `Assoc (List.filter_map Fun.id fields) in
    let data = Yojson.Basic.to_string json_data in
    let puted_record =
      Lwt_main.run
        (Cohttp_client.post_data_with_headers put_record_url data headers)
    in
    puted_record

  let delete_record (s : Session.session) (repo : string) (collection : string)
      ?swap_record ?swap_commit (rkey : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let delete_record_url =
      App.create_endpoint_url base_url (create_repo_endpoint "deleteRecord")
    in
    let fields =
      [
        Some ("repo", `String repo);
        Some ("collection", `String collection);
        Some ("rkey", `String rkey);
        Option.map
          (fun swap_record -> ("swapRecord", `String swap_record))
          swap_record;
        Option.map
          (fun swap_commit -> ("swapCommit", `String swap_commit))
          swap_commit;
      ]
    in
    let json_data = `Assoc (List.filter_map Fun.id fields) in
    let data = Yojson.Basic.to_string json_data in
    let deleted_record =
      Lwt_main.run
        (Cohttp_client.post_data_with_headers delete_record_url data headers)
    in
    deleted_record

  type write_op =
    | Create of {
        collection : string;
        rkey : string option;
        value : Yojson.Safe.t;
      }
    | Update of { collection : string; rkey : string; value : Yojson.Safe.t }
    | Delete of { collection : string; rkey : string }

  let write_op_to_json = function
    | Create { collection; rkey; value } ->
        `Assoc
          ([
             ("$type", `String "com.atproto.repo.applyWrites#create");
             ("collection", `String collection);
             ("value", value);
           ]
          @ match rkey with Some r -> [ ("rkey", `String r) ] | None -> [])
    | Update { collection; rkey; value } ->
        `Assoc
          [
            ("$type", `String "com.atproto.repo.applyWrites#update");
            ("collection", `String collection);
            ("rkey", `String rkey);
            ("value", value);
          ]
    | Delete { collection; rkey } ->
        `Assoc
          [
            ("$type", `String "com.atproto.repo.applyWrites#delete");
            ("collection", `String collection);
            ("rkey", `String rkey);
          ]

  let apply_writes_body ~repo ~writes ?(validate = true) ?swap_commit () :
      Yojson.Safe.t =
    let fields =
      [
        ("repo", `String repo);
        ("validate", `Bool validate);
        ("writes", `List (List.map write_op_to_json writes));
      ]
      @
      match swap_commit with
      | Some cid -> [ ("swapCommit", `String cid) ]
      | None -> []
    in
    `Assoc fields

  let apply_writes (s : Session.session) ~repo ~writes ?validate ?swap_commit ()
      : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let url =
      App.create_endpoint_url base_url (create_repo_endpoint "applyWrites")
    in
    let data =
      Yojson.Safe.to_string
        (apply_writes_body ~repo ~writes ?validate ?swap_commit ())
    in
    Lwt_main.run (Cohttp_client.post_data_with_headers url data headers)

  type blob_ref = {
    cid : string;
    mime_type : string;
    size : int;
    original : Yojson.Safe.t;
  }

  let parse_blob_ref json : blob_ref =
    let open Yojson.Safe.Util in
    let blob = match json |> member "blob" with `Null -> json | b -> b in
    let cid =
      match blob |> member "ref" with
      | `Assoc _ as ref_ -> (
          match ref_ |> member "$link" with `String s -> s | _ -> "")
      | `String s -> s
      | _ -> ( match blob |> member "cid" with `String s -> s | _ -> "")
    in
    {
      cid;
      mime_type =
        (match blob |> member "mimeType" with `String s -> s | _ -> "");
      size = (match blob |> member "size" with `Int n -> n | _ -> 0);
      original = blob;
    }

  let upload_blob_url (s : Session.session) : string =
    App.create_endpoint_url (App.create_base_url s)
      (create_repo_endpoint "uploadBlob")

  let upload_blob (s : Session.session)
      ?(content_type = "application/octet-stream") (bytes : string) : blob_ref =
    let bearer_token = Session.bearer_token_from_session s in
    let headers =
      Cohttp_client.create_headers_from_pairs
        [ ("Content-Type", content_type); bearer_token ]
    in
    let url = upload_blob_url s in
    let body =
      Lwt_main.run (Cohttp_client.post_data_with_headers url bytes headers)
    in
    parse_blob_ref (Yojson.Safe.from_string body)

  type missing_blob = { cid : string; record_uri : string }
  type list_missing_blobs = { cursor : string option; blobs : missing_blob list }

  let parse_missing_blob json : missing_blob =
    let open Yojson.Safe.Util in
    {
      cid = (match json |> member "cid" with `String s -> s | _ -> "");
      record_uri =
        (match json |> member "recordUri" with `String s -> s | _ -> "");
    }

  let parse_list_missing_blobs json : list_missing_blobs =
    let open Yojson.Safe.Util in
    {
      cursor =
        (match json |> member "cursor" with `String s -> Some s | _ -> None);
      blobs =
        (match json |> member "blobs" with
        | `List items -> List.map parse_missing_blob items
        | _ -> []);
    }

  let list_missing_blobs (s : Session.session) ?cursor ?limit () :
      list_missing_blobs =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let url =
      App.create_endpoint_url (App.create_base_url s)
        (create_repo_endpoint "listMissingBlobs")
    in
    let pairs =
      (match cursor with Some c -> [ ("cursor", c) ] | None -> [])
      @ match limit with Some n -> [ ("limit", string_of_int n) ] | None -> []
    in
    let body = Cohttp_client.create_body_from_pairs pairs in
    let resp =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers url body headers)
    in
    parse_list_missing_blobs (Yojson.Safe.from_string resp)

  let import_repo_url (s : Session.session) : string =
    App.create_endpoint_url (App.create_base_url s)
      (create_repo_endpoint "importRepo")

  let import_repo (s : Session.session) (car_bytes : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let headers =
      Cohttp_client.create_headers_from_pairs
        [ ("Content-Type", "application/vnd.ipld.car"); bearer_token ]
    in
    let url = import_repo_url s in
    Lwt_main.run (Cohttp_client.post_data_with_headers url car_bytes headers)
end
