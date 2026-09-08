open Session
open Cohttp_client
open App
open Embed
open Facet
open Feed
open Notification

(** [com.atproto.repo] — create/put/delete/applyWrites plus typed record parsers. *)
module Repo = struct
  type post_record = {
    text : string;
    record_type : string;
    embed : Embed.embed option;
    facets : Facet.facet list option;
    langs : string list option;
    reply : Notification.reply option;
    tags : string list option;
    self_labels : string list option;
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
    let tags = Feed.extract_tags_option json in
    let self_labels = Feed.extract_self_labels_option json in
    let created_at = json |> member "createdAt" |> to_string in
    {
      text;
      record_type;
      embed;
      facets;
      langs;
      reply;
      tags;
      self_labels;
      created_at;
    }

  type commit_meta = { cid : string; rev : string }
  type record_get = { uri : string; cid : string option; value : Yojson.Safe.t }
  type listed_record = { uri : string; cid : string; value : Yojson.Safe.t }
  type listed_records = { cursor : string option; records : listed_record list }

  type repo_description = {
    handle : string;
    did : string;
    did_doc : Yojson.Safe.t option;
    collections : string list;
    handle_is_correct : bool;
  }

  type write_result = {
    uri : string;
    cid : string;
    commit : commit_meta option;
    validation_status : string option;
  }

  type apply_writes_result = {
    commit : commit_meta option;
    results : Yojson.Safe.t list;
  }

  let parse_commit_meta json : commit_meta option =
    match json with
    | `Assoc _ ->
        Some
          {
            cid =
              (match Yojson.Safe.Util.member "cid" json with
              | `String s -> s
              | _ -> "");
            rev =
              (match Yojson.Safe.Util.member "rev" json with
              | `String s -> s
              | _ -> "");
          }
    | _ -> None

  let parse_record_get json : record_get =
    let open Yojson.Safe.Util in
    {
      uri = (match json |> member "uri" with `String s -> s | _ -> "");
      cid = (match json |> member "cid" with `String s -> Some s | _ -> None);
      value = json |> member "value";
    }

  let parse_listed_record json : listed_record =
    let open Yojson.Safe.Util in
    {
      uri = (match json |> member "uri" with `String s -> s | _ -> "");
      cid = (match json |> member "cid" with `String s -> s | _ -> "");
      value = json |> member "value";
    }

  let parse_listed_records json : listed_records =
    let open Yojson.Safe.Util in
    {
      cursor =
        (match json |> member "cursor" with `String s -> Some s | _ -> None);
      records =
        (match json |> member "records" with
        | `List xs -> List.map parse_listed_record xs
        | _ -> []);
    }

  let parse_repo_description json : repo_description =
    let open Yojson.Safe.Util in
    {
      handle = (match json |> member "handle" with `String s -> s | _ -> "");
      did = (match json |> member "did" with `String s -> s | _ -> "");
      did_doc =
        (match json |> member "didDoc" with
        | `Null -> None
        | other -> Some other);
      collections =
        (match json |> member "collections" with
        | `List xs ->
            List.filter_map (function `String s -> Some s | _ -> None) xs
        | _ -> []);
      handle_is_correct =
        (match json |> member "handleIsCorrect" with
        | `Bool b -> b
        | _ -> false);
    }

  let parse_write_result json : write_result =
    let open Yojson.Safe.Util in
    {
      uri = (match json |> member "uri" with `String s -> s | _ -> "");
      cid = (match json |> member "cid" with `String s -> s | _ -> "");
      commit = parse_commit_meta (json |> member "commit");
      validation_status =
        (match json |> member "validationStatus" with
        | `String s -> Some s
        | _ -> None);
    }

  let parse_apply_writes_result json : apply_writes_result =
    let open Yojson.Safe.Util in
    {
      commit = parse_commit_meta (json |> member "commit");
      results = (match json |> member "results" with `List xs -> xs | _ -> []);
    }

  (** Query-string pairs for [com.atproto.repo.describeRepo]. *)
  let describe_repo_body ~repo : (string * string) list = [ ("repo", repo) ]

  (** Query-string pairs for [com.atproto.repo.getRecord]. Optional [cid]
      maps to the lexicon query. *)
  let get_record_body ~repo ~collection ~rkey ?cid () : (string * string) list =
    [ ("repo", repo); ("collection", collection); ("rkey", rkey) ]
    @ Client.Client.opt_pair "cid" cid

  (** Query-string pairs for [com.atproto.repo.listRecords]. Optional
      [limit] / [cursor] / [reverse] map to the lexicon query. *)
  let list_records_body ~repo ~collection ?limit ?cursor ?reverse () :
      (string * string) list =
    [ ("repo", repo); ("collection", collection) ]
    @ Client.Client.opt_int "limit" limit
    @ Client.Client.opt_pair "cursor" cursor
    @ Client.Client.opt_bool "reverse" reverse

  (** Parsed [com.atproto.repo.describeRepo] (handle, DID, collections).
      Works without a session. *)
  let describe_repo_parsed ?session ?host ~repo () : repo_description =
    Client.Client.get_json ?session ?host "com.atproto.repo.describeRepo"
      (describe_repo_body ~repo)
    |> parse_repo_description

  (** Parsed [com.atproto.repo.getRecord] ([uri], optional [cid], [value]).
      Works without a session. *)
  let get_record_parsed ?session ?host ~repo ~collection ~rkey ?cid () :
      record_get =
    Client.Client.get_json ?session ?host "com.atproto.repo.getRecord"
      (get_record_body ~repo ~collection ~rkey ?cid ())
    |> parse_record_get

  (** Parsed [com.atproto.repo.listRecords]. Optional [limit] / [cursor] /
      [reverse] map to the lexicon query. Works without a session. *)
  let list_records_parsed ?session ?host ~repo ~collection ?limit ?cursor
      ?reverse () : listed_records =
    Client.Client.get_json ?session ?host "com.atproto.repo.listRecords"
      (list_records_body ~repo ~collection ?limit ?cursor ?reverse ())
    |> parse_listed_records

  let create_repo_endpoint (query_name : string) : string =
    "com.atproto.repo" ^ "." ^ query_name

  (** Raw JSON from [com.atproto.repo.describeRepo] for [repo]. Shares
      [describe_repo_body] with [describe_repo_parsed]. *)
  let describe_repo (s : Session.session) (repo : string) : string =
    Client.Client.get_text ~session:s "com.atproto.repo.describeRepo"
      (describe_repo_body ~repo)

  (** Fetch a record via [com.atproto.repo.getRecord]. Returns the raw JSON
      body. Shares [get_record_body] with [get_record_parsed]. *)
  let get_record (s : Session.session) (repo : string) (collection : string)
      (rkey : string) (cid : string) : string =
    Client.Client.get_text ~session:s "com.atproto.repo.getRecord"
      (get_record_body ~repo ~collection ~rkey ~cid ())

  (** List records via [com.atproto.repo.listRecords]. Returns the raw JSON
      body. Shares [list_records_body] with [list_records_parsed]. *)
  let list_records (s : Session.session) (repo : string) (collection : string)
      (limit : int) (reverse : bool) : string =
    Client.Client.get_text ~session:s "com.atproto.repo.listRecords"
      (list_records_body ~repo ~collection ~limit ~reverse ())

  let record_json_of_string (record : string) : Yojson.Safe.t =
    try Yojson.Safe.from_string record with _ -> `String record

  (** JSON body for [com.atproto.repo.createRecord]. [record] is Yojson. *)
  let create_record_body ~repo ~collection ?rkey ?(validate = true) ?swap_commit
      (record : Yojson.Safe.t) : Yojson.Safe.t =
    let fields =
      [
        Some ("repo", `String repo);
        Some ("collection", `String collection);
        Some ("record", record);
        Option.map (fun rkey -> ("rkey", `String rkey)) rkey;
        Some ("validate", `Bool validate);
        Option.map
          (fun swap_commit -> ("swapCommit", `String swap_commit))
          swap_commit;
      ]
    in
    `Assoc (List.filter_map Fun.id fields)

  (** JSON body for [com.atproto.repo.putRecord]. [record] is Yojson. *)
  let put_record_body ~repo ~collection ?rkey ?(validate = true) ?swap_record
      ?swap_commit (record : Yojson.Safe.t) : Yojson.Safe.t =
    let fields =
      [
        Some ("repo", `String repo);
        Some ("collection", `String collection);
        Some ("record", record);
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
    `Assoc (List.filter_map Fun.id fields)

  (* Empty procedure output stays [""] for existing string write callers. *)
  let json_body_string (json : Yojson.Safe.t) : string =
    match json with `Assoc [] -> "" | j -> Yojson.Safe.to_string j

  let post_repo_write (s : Session.session) (query_name : string)
      (body : Yojson.Safe.t) : string =
    Client.Client.post_json ~session:s
      (create_repo_endpoint query_name)
      (Yojson.Safe.to_string body)
    |> json_body_string

  (** Create a record via [com.atproto.repo.createRecord]. [record] is a JSON
      object string; optional [rkey] and [swap_commit] map to the lexicon
      inputs. Shares [create_record_body] with [create_record_json]. *)
  let create_record (s : Session.session) (repo : string) (collection : string)
      ?rkey ?(validate = true) ?swap_commit (record : string) : string =
    post_repo_write s "createRecord"
      (create_record_body ~repo ~collection ?rkey ~validate ?swap_commit
         (record_json_of_string record))

  (** Create a record via [com.atproto.repo.createRecord] from Yojson.
      Same optional labels as [create_record]. Returns the parsed write
      result ([uri] / [cid] / optional [commit]) via [parse_write_result]. *)
  let create_record_json (s : Session.session) (repo : string)
      (collection : string) ?rkey ?(validate = true) ?swap_commit
      (record : Yojson.Safe.t) : write_result =
    post_repo_write s "createRecord"
      (create_record_body ~repo ~collection ?rkey ~validate ?swap_commit record)
    |> Yojson.Safe.from_string |> parse_write_result

  (** Put a record via [com.atproto.repo.putRecord]. [record] is a JSON
      object string; optional [rkey], [swap_record], and [swap_commit] map
      to the lexicon inputs. Shares [put_record_body] with [put_record_json]. *)
  let put_record (s : Session.session) (repo : string) (collection : string)
      ?rkey ?(validate = true) ?swap_record ?swap_commit (record : string) :
      string =
    post_repo_write s "putRecord"
      (put_record_body ~repo ~collection ?rkey ~validate ?swap_record
         ?swap_commit
         (record_json_of_string record))

  (** Put a record via [com.atproto.repo.putRecord] from Yojson. Same
      optional labels as [put_record]. Returns the parsed write result
      ([uri] / [cid] / optional [commit]) via [parse_write_result]. *)
  let put_record_json (s : Session.session) (repo : string)
      (collection : string) ?rkey ?(validate = true) ?swap_record ?swap_commit
      (record : Yojson.Safe.t) : write_result =
    post_repo_write s "putRecord"
      (put_record_body ~repo ~collection ?rkey ~validate ?swap_record
         ?swap_commit record)
    |> Yojson.Safe.from_string |> parse_write_result

  (** JSON body for [com.atproto.repo.deleteRecord]. Same field names as
      [delete_record] ([repo] / [collection] / [rkey] / optional
      [swapRecord] / [swapCommit]). *)
  let delete_record_body ~repo ~collection ~rkey ?swap_record ?swap_commit () :
      Yojson.Safe.t =
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
    `Assoc (List.filter_map Fun.id fields)

  (** Delete a record via [com.atproto.repo.deleteRecord]. Optional
      [swap_record] / [swap_commit] map to the lexicon inputs. Shares
      [delete_record_body] with callers that need the JSON. *)
  let delete_record (s : Session.session) (repo : string) (collection : string)
      ?swap_record ?swap_commit (rkey : string) : string =
    post_repo_write s "deleteRecord"
      (delete_record_body ~repo ~collection ~rkey ?swap_record ?swap_commit ())

  type write_op =
    | Create of {
        collection : string;
        rkey : string option;
        value : Yojson.Safe.t;
      }
    | Update of { collection : string; rkey : string; value : Yojson.Safe.t }
    | Delete of { collection : string; rkey : string }

  (** JSON for one [com.atproto.repo.applyWrites] create / update / delete
      op. *)
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

  (** JSON body for [com.atproto.repo.applyWrites]. *)
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

  (** Apply a batch of create/update/delete ops via
      [com.atproto.repo.applyWrites]. Returns the raw JSON body. *)
  let apply_writes (s : Session.session) ~repo ~writes ?validate ?swap_commit ()
      : string =
    post_repo_write s "applyWrites"
      (apply_writes_body ~repo ~writes ?validate ?swap_commit ())

  (** Apply a batch of create/update/delete ops via
      [com.atproto.repo.applyWrites] and parse the response. Same
      arguments as [apply_writes]. Returns [apply_writes_result]
      ([commit] / [results]) via [parse_apply_writes_result]. *)
  let apply_writes_parsed (s : Session.session) ~repo ~writes ?validate
      ?swap_commit () : apply_writes_result =
    apply_writes s ~repo ~writes ?validate ?swap_commit ()
    |> Yojson.Safe.from_string |> parse_apply_writes_result

  type blob_ref = {
    cid : string;
    mime_type : string;
    size : int;
    original : Yojson.Safe.t;
  }

  (** Blob CID for [bytes] (CIDv1 raw + SHA-256). Optional [expected]
      must match. *)
  let verify_blob_bytes ?expected (bytes : string) : Cid.Cid.t =
    Cid.Cid.verify_blob ?expected bytes

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

  (** Encode [blob_ref] as a lexicon blob object
      ([{ "$type": "blob", "ref": { "$link": cid }, "mimeType", "size" }]).
      Reuses [.original] when it is already a well-formed blob Assoc
      with those fields; otherwise builds from [cid] / [mime_type] /
      [size] via [Embed.blob_to_json]. Does not invent leftover unused
      fields. *)
  let blob_ref_to_json (b : blob_ref) : Yojson.Safe.t =
    let well_formed =
      match b.original with
      | `Assoc _ as json -> (
          let open Yojson.Safe.Util in
          (match json |> member "$type" with
          | `String "blob" -> true
          | _ -> false)
          && (match json |> member "ref" with
             | `Assoc _ as ref_ -> (
                 match ref_ |> member "$link" with
                 | `String s -> s = b.cid
                 | _ -> false)
             | _ -> false)
          && (match json |> member "mimeType" with
             | `String s -> s = b.mime_type
             | _ -> false)
          &&
          match json |> member "size" with `Int n -> n = b.size | _ -> false)
      | _ -> false
    in
    if well_formed then b.original
    else Embed.blob_to_json ~cid:b.cid ~mime_type:b.mime_type ~size:b.size ()

  let upload_blob_url (s : Session.session) : string =
    App.create_endpoint_url (App.create_base_url s)
      (create_repo_endpoint "uploadBlob")

  (** Upload bytes via [com.atproto.repo.uploadBlob]. Optional
      [content_type] defaults to [application/octet-stream]. *)
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

  type list_missing_blobs = {
    cursor : string option;
    blobs : missing_blob list;
  }

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

  (** Query-string pairs for [com.atproto.repo.listMissingBlobs]. Optional
      [cursor] / [limit] map to the lexicon query. *)
  let list_missing_blobs_body ?cursor ?limit () : (string * string) list =
    Client.Client.opt_pair "cursor" cursor @ Client.Client.opt_int "limit" limit

  (** Missing blobs via [com.atproto.repo.listMissingBlobs]. Optional
      [cursor] / [limit] map to the lexicon query. *)
  let list_missing_blobs (s : Session.session) ?cursor ?limit () :
      list_missing_blobs =
    Client.Client.get_json ~session:s "com.atproto.repo.listMissingBlobs"
      (list_missing_blobs_body ?cursor ?limit ())
    |> parse_list_missing_blobs

  let import_repo_url (s : Session.session) : string =
    App.create_endpoint_url (App.create_base_url s)
      (create_repo_endpoint "importRepo")

  (** Import a CAR via [com.atproto.repo.importRepo]. *)
  let import_repo (s : Session.session) (car_bytes : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let headers =
      Cohttp_client.create_headers_from_pairs
        [ ("Content-Type", "application/vnd.ipld.car"); bearer_token ]
    in
    let url = import_repo_url s in
    Lwt_main.run (Cohttp_client.post_data_with_headers url car_bytes headers)
end
