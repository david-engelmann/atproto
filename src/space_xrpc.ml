open Auth
open Syntax
open Base64url
open Label
open Oauth
open Space_commit
open Space_credential

(** Experimental [com.atproto.space.*] XRPC client from AT Protocol
    proposal 0016 (permissioned data / spaces).

    Tracks
    {{:https://github.com/bluesky-social/proposals/blob/main/0016-permissioned-data/README.md}0016
    § XRPC API} and the draft lexicons on
    {{:https://github.com/bluesky-social/atproto/pull/5187}bluesky-social/atproto\#5187}.
    Typed query / JSON body builders, response parsers, and
    [Client.get_json] / [post_json] / [get_text] wrappers. Credential
    exchange and DPoP headers reuse [Space_credential]; signed-commit
    JSON maps onto [Space_commit.t].

    Auth, per the draft:

    - [getDelegationToken] and writes ([createRecord] / [putRecord] /
      [deleteRecord] / [applyWrites] / [listSpaces]): OAuth / session
      on the user's PDS
    - [getSpaceCredential]: delegation Bearer + exchange DPoP on the
      space host
    - repo / host reads and notify registration: OAuth {e or} a space
      credential + resource DPoP ([dpop])
    - [notifyWrite] / [notifySpaceDeleted]: service-auth Bearer
      (host / syncer, not a typical app session)

    The proposal is not final; this module may change and is {e not} a
    stable spaces product API. This repo does not start or stub a
    space host. Live calls skip unless [ATP_SPACE] is set and
    [ATP_SPACE_HOST] names a real host that serves these draft NSIDs.

    Deferred: [com.atproto.simplespace.*] management;
    [registerNotify] [repo] (proposal prose; not in the \#5187
    lexicon); [space:] OAuth scope grammar; applying [listRepoOps] /
    two-root CAR locally (the XRPC wrappers land). *)
module Space_xrpc = struct
  let nsid_prefix = "com.atproto.space."
  let get_delegation_token_nsid = Space_credential.get_delegation_token_nsid
  let get_space_credential_nsid = Space_credential.get_space_credential_nsid
  let get_record_nsid = "com.atproto.space.getRecord"
  let list_records_nsid = "com.atproto.space.listRecords"
  let get_blob_nsid = "com.atproto.space.getBlob"
  let list_blobs_nsid = "com.atproto.space.listBlobs"
  let get_latest_commit_nsid = "com.atproto.space.getLatestCommit"
  let get_repo_nsid = "com.atproto.space.getRepo"
  let list_repo_ops_nsid = "com.atproto.space.listRepoOps"
  let list_repos_nsid = "com.atproto.space.listRepos"
  let list_spaces_nsid = "com.atproto.space.listSpaces"
  let create_record_nsid = "com.atproto.space.createRecord"
  let put_record_nsid = "com.atproto.space.putRecord"
  let delete_record_nsid = "com.atproto.space.deleteRecord"
  let apply_writes_nsid = "com.atproto.space.applyWrites"
  let register_notify_nsid = "com.atproto.space.registerNotify"
  let unregister_notify_nsid = "com.atproto.space.unregisterNotify"
  let notify_write_nsid = "com.atproto.space.notifyWrite"
  let notify_space_deleted_nsid = "com.atproto.space.notifySpaceDeleted"

  let create_space_endpoint (query_name : string) : string =
    nsid_prefix ^ query_name

  exception Invalid of string

  let fail msg = raise (Invalid msg)

  let env_truthy name =
    match Sys.getenv_opt name with
    | Some v ->
        let v = String.lowercase_ascii (String.trim v) in
        List.mem v [ "1"; "true"; "yes"; "on" ]
    | None -> false

  (** Space host from [ATP_SPACE_HOST] (no scheme). [None] when unset
      or blank — there is no invented default host. *)
  let space_host_from_env : string option =
    match Sys.getenv_opt "ATP_SPACE_HOST" with
    | Some h ->
        let h = String.trim h in
        if h = "" then None else Some h
    | None -> None

  (** True when live space XRPC hops may run: [ATP_SPACE] is truthy
      and [ATP_SPACE_HOST] is set. Default off so CI never invents a
      space host. *)
  let live_enabled : bool =
    env_truthy "ATP_SPACE" && Option.is_some space_host_from_env

  type dpop = {
    credential : string;
    priv : Mirage_crypto_ec.P256.Dsa.priv;
    pub : Mirage_crypto_ec.P256.Dsa.pub;
  }

  type record = { uri : string; cid : string; value : Yojson.Safe.t }

  type listed_record = {
    collection : string;
    rkey : string;
    cid : string;
    value : Yojson.Safe.t option;
  }

  type listed_records = { cursor : string option; records : listed_record list }
  type listed_blobs = { cursor : string option; cids : string list }
  type repo_view = { did : string; rev : string; hash : string }
  type listed_repos = { cursor : string option; repos : repo_view list }
  type space_view = { uri : string }
  type listed_spaces = { cursor : string option; spaces : space_view list }

  type write_result = {
    uri : string;
    cid : string;
    validation_status : string option;
  }

  type write_op =
    | Create of {
        collection : string;
        rkey : string option;
        value : Yojson.Safe.t;
      }
    | Update of { collection : string; rkey : string; value : Yojson.Safe.t }
    | Delete of { collection : string; rkey : string }

  type apply_writes_result = { results : Yojson.Safe.t list }

  type op_entry = {
    rev : string;
    collection : string;
    rkey : string;
    cid : string option;
    prev : string option;
    value : Yojson.Safe.t option;
  }

  type listed_ops = {
    ops : op_entry list;
    commit : Space_commit.t option;
    cursor : string option;
  }

  let string_opt json field =
    match Yojson.Safe.Util.member field json with
    | `String s -> Some s
    | _ -> None

  let string_member json field =
    Option.value ~default:"" (string_opt json field)

  let int_member json field =
    match Yojson.Safe.Util.member field json with
    | `Int n -> n
    | `Intlit s -> ( try int_of_string s with _ -> 0)
    | _ -> 0

  let bytes_to_json (raw : string) : Yojson.Safe.t =
    `Assoc [ ("$bytes", `String (Base64url.encode_std raw)) ]

  let bytes_of_json_exn label json =
    match Label.bytes_of_json json with
    | Some b -> b
    | None -> fail ("missing or invalid $bytes field " ^ label)

  let ensure_space_uri label uri =
    match At_uri.Space.of_string uri with
    | At_uri.Space.Space _ -> ()
    | At_uri.Space.Record _ ->
        fail (label ^ " must be a space URI (through skey), not a record URI")
    | exception At_uri.Space.Invalid msg -> fail (label ^ ": " ^ msg)

  let ensure_did label did =
    if not (Syntax.is_valid_did did) then fail (label ^ " must be a DID")

  let ensure_nsid label nsid =
    if not (Syntax.is_valid_nsid nsid) then fail (label ^ " must be an NSID")

  let host_of ?session ?host () =
    match host with
    | Some h -> h
    | None -> (
        match session with
        | Some s -> s.Auth.atp_host
        | None -> (
            match space_host_from_env with
            | Some h -> h
            | None -> Auth.atp_host_from_env))

  (** RFC 9449 [htu] for [nsid] on [host] (scheme from [ATP_SCHEME]). *)
  let xrpc_htu ~host nsid =
    Oauth.htu_of_url (Oauth.url_on (Auth.origin_of_host host) ("/xrpc/" ^ nsid))

  let resource_extra ~host ~nsid ~htm (d : dpop) =
    let htu = xrpc_htu ~host nsid in
    let proof =
      Space_credential.resource_dpop_proof ~priv:d.priv ~pub:d.pub ~htm ~htu
        ~credential:d.credential ()
    in
    Space_credential.resource_headers ~credential:d.credential ~dpop:proof

  let get_json ?session ?host ?bearer ?dpop nsid pairs =
    let host = host_of ?session ?host () in
    let extra =
      match dpop with
      | Some d -> resource_extra ~host ~nsid ~htm:"GET" d
      | None -> []
    in
    Client.Client.get_json ?session ~host ?bearer ~extra nsid pairs

  let get_text ?session ?host ?bearer ?dpop nsid pairs =
    let host = host_of ?session ?host () in
    let extra =
      match dpop with
      | Some d -> resource_extra ~host ~nsid ~htm:"GET" d
      | None -> []
    in
    Client.Client.get_text ?session ~host ?bearer ~extra nsid pairs

  let post_json ?session ?host ?bearer ?dpop nsid data =
    let host = host_of ?session ?host () in
    let extra =
      match dpop with
      | Some d -> resource_extra ~host ~nsid ~htm:"POST" d
      | None -> []
    in
    Client.Client.post_json ?session ~host ?bearer ~extra nsid data

  (** Query pairs for [com.atproto.space.getDelegationToken]. Reuses
      [Space_credential.get_delegation_token_body]. *)
  let get_delegation_token_body ~space =
    Space_credential.get_delegation_token_body ~space

  (** JSON body for [com.atproto.space.getSpaceCredential]. Reuses
      [Space_credential.get_space_credential_body]. *)
  let get_space_credential_body ~space ?client_attestation () =
    Space_credential.get_space_credential_body ~space ?client_attestation ()

  (** Query pairs for [com.atproto.space.getRecord]. *)
  let get_record_body ~space ~repo ~collection ~rkey : (string * string) list =
    ensure_space_uri "getRecord space" space;
    ensure_did "getRecord repo" repo;
    ensure_nsid "getRecord collection" collection;
    [
      ("space", space);
      ("repo", repo);
      ("collection", collection);
      ("rkey", rkey);
    ]

  (** Query pairs for [com.atproto.space.listRecords]. *)
  let list_records_body ~space ~repo ?collection ?limit ?cursor ?reverse
      ?exclude_values () : (string * string) list =
    ensure_space_uri "listRecords space" space;
    ensure_did "listRecords repo" repo;
    ("space", space) :: ("repo", repo)
    :: Client.Client.opt_pair "collection" collection
    @ Client.Client.opt_int "limit" limit
    @ Client.Client.opt_pair "cursor" cursor
    @ Client.Client.opt_bool "reverse" reverse
    @ Client.Client.opt_bool "excludeValues" exclude_values

  (** Query pairs for [com.atproto.space.getBlob]. *)
  let get_blob_body ~space ~repo ~cid : (string * string) list =
    ensure_space_uri "getBlob space" space;
    ensure_did "getBlob repo" repo;
    [ ("space", space); ("repo", repo); ("cid", cid) ]

  (** Query pairs for [com.atproto.space.listBlobs]. *)
  let list_blobs_body ~space ~repo ?since ?limit ?cursor () :
      (string * string) list =
    ensure_space_uri "listBlobs space" space;
    ensure_did "listBlobs repo" repo;
    (("space", space) :: ("repo", repo) :: Client.Client.opt_pair "since" since)
    @ Client.Client.opt_int "limit" limit
    @ Client.Client.opt_pair "cursor" cursor

  (** Query pairs for [com.atproto.space.getLatestCommit]. *)
  let get_latest_commit_body ~space ~repo : (string * string) list =
    ensure_space_uri "getLatestCommit space" space;
    ensure_did "getLatestCommit repo" repo;
    [ ("space", space); ("repo", repo) ]

  (** Query pairs for [com.atproto.space.getRepo]. *)
  let get_repo_body ~space ~repo ?exclude_values () : (string * string) list =
    ensure_space_uri "getRepo space" space;
    ensure_did "getRepo repo" repo;
    ("space", space) :: ("repo", repo)
    :: Client.Client.opt_bool "excludeValues" exclude_values

  (** Query pairs for [com.atproto.space.listRepoOps]. *)
  let list_repo_ops_body ~space ~repo ?since ?limit ?cursor ?exclude_values () :
      (string * string) list =
    ensure_space_uri "listRepoOps space" space;
    ensure_did "listRepoOps repo" repo;
    (("space", space) :: ("repo", repo) :: Client.Client.opt_pair "since" since)
    @ Client.Client.opt_int "limit" limit
    @ Client.Client.opt_pair "cursor" cursor
    @ Client.Client.opt_bool "excludeValues" exclude_values

  (** Query pairs for [com.atproto.space.listRepos] (writer set). *)
  let list_repos_body ~space ?limit ?cursor () : (string * string) list =
    ensure_space_uri "listRepos space" space;
    (("space", space) :: Client.Client.opt_int "limit" limit)
    @ Client.Client.opt_pair "cursor" cursor

  (** Query pairs for [com.atproto.space.listSpaces]. Parameter [type]
      is the space-type NSID ([space_type]). *)
  let list_spaces_body ?space_type ?did ?limit ?cursor () :
      (string * string) list =
    (match space_type with
    | Some t -> ensure_nsid "listSpaces type" t
    | None -> ());
    (match did with Some d -> ensure_did "listSpaces did" d | None -> ());
    Client.Client.opt_pair "type" space_type
    @ Client.Client.opt_pair "did" did
    @ Client.Client.opt_int "limit" limit
    @ Client.Client.opt_pair "cursor" cursor

  (** JSON body for [com.atproto.space.createRecord]. *)
  let create_record_body ~space ~repo ~collection ?rkey ?validate
      (record : Yojson.Safe.t) : Yojson.Safe.t =
    ensure_space_uri "createRecord space" space;
    ensure_did "createRecord repo" repo;
    ensure_nsid "createRecord collection" collection;
    let fields =
      [
        Some ("space", `String space);
        Some ("repo", `String repo);
        Some ("collection", `String collection);
        Some ("record", record);
        Option.map (fun rkey -> ("rkey", `String rkey)) rkey;
        Option.map (fun v -> ("validate", `Bool v)) validate;
      ]
    in
    `Assoc (List.filter_map Fun.id fields)

  (** JSON body for [com.atproto.space.putRecord]. [rkey] is required. *)
  let put_record_body ~space ~repo ~collection ~rkey ?validate
      (record : Yojson.Safe.t) : Yojson.Safe.t =
    ensure_space_uri "putRecord space" space;
    ensure_did "putRecord repo" repo;
    ensure_nsid "putRecord collection" collection;
    let fields =
      [
        Some ("space", `String space);
        Some ("repo", `String repo);
        Some ("collection", `String collection);
        Some ("rkey", `String rkey);
        Some ("record", record);
        Option.map (fun v -> ("validate", `Bool v)) validate;
      ]
    in
    `Assoc (List.filter_map Fun.id fields)

  (** JSON body for [com.atproto.space.deleteRecord]. *)
  let delete_record_body ~space ~repo ~collection ~rkey : Yojson.Safe.t =
    ensure_space_uri "deleteRecord space" space;
    ensure_did "deleteRecord repo" repo;
    ensure_nsid "deleteRecord collection" collection;
    `Assoc
      [
        ("space", `String space);
        ("repo", `String repo);
        ("collection", `String collection);
        ("rkey", `String rkey);
      ]

  (** JSON for one [com.atproto.space.applyWrites] create / update /
      delete op. *)
  let write_op_to_json = function
    | Create { collection; rkey; value } ->
        `Assoc
          ([
             ("$type", `String "com.atproto.space.applyWrites#create");
             ("collection", `String collection);
             ("value", value);
           ]
          @ match rkey with Some r -> [ ("rkey", `String r) ] | None -> [])
    | Update { collection; rkey; value } ->
        `Assoc
          [
            ("$type", `String "com.atproto.space.applyWrites#update");
            ("collection", `String collection);
            ("rkey", `String rkey);
            ("value", value);
          ]
    | Delete { collection; rkey } ->
        `Assoc
          [
            ("$type", `String "com.atproto.space.applyWrites#delete");
            ("collection", `String collection);
            ("rkey", `String rkey);
          ]

  (** JSON body for [com.atproto.space.applyWrites]. *)
  let apply_writes_body ~space ~repo ~writes ?validate () : Yojson.Safe.t =
    ensure_space_uri "applyWrites space" space;
    ensure_did "applyWrites repo" repo;
    let fields =
      [
        Some ("space", `String space);
        Some ("repo", `String repo);
        Some ("writes", `List (List.map write_op_to_json writes));
        Option.map (fun v -> ("validate", `Bool v)) validate;
      ]
    in
    `Assoc (List.filter_map Fun.id fields)

  (** JSON body for [com.atproto.space.registerNotify]. Lexicon fields
      are [space] + [service] only (no [repo]). *)
  let register_notify_body ~space ~service : Yojson.Safe.t =
    ensure_space_uri "registerNotify space" space;
    if service = "" then fail "registerNotify service must be non-empty";
    `Assoc [ ("space", `String space); ("service", `String service) ]

  (** JSON body for [com.atproto.space.unregisterNotify]. *)
  let unregister_notify_body ~space ~service : Yojson.Safe.t =
    ensure_space_uri "unregisterNotify space" space;
    if service = "" then fail "unregisterNotify service must be non-empty";
    `Assoc [ ("space", `String space); ("service", `String service) ]

  (** JSON body for [com.atproto.space.notifyWrite]. [hash] is the
      32-byte commit digest. *)
  let notify_write_body ~space ~repo ~rev ~hash : Yojson.Safe.t =
    ensure_space_uri "notifyWrite space" space;
    ensure_did "notifyWrite repo" repo;
    `Assoc
      [
        ("space", `String space);
        ("repo", `String repo);
        ("rev", `String rev);
        ("hash", bytes_to_json hash);
      ]

  (** JSON body for [com.atproto.space.notifySpaceDeleted]. *)
  let notify_space_deleted_body ~space : Yojson.Safe.t =
    ensure_space_uri "notifySpaceDeleted space" space;
    `Assoc [ ("space", `String space) ]

  (** Lexicon JSON for [com.atproto.space.defs#signedCommit]. *)
  let signed_commit_to_json (c : Space_commit.t) : Yojson.Safe.t =
    `Assoc
      [
        ("ver", `Int c.Space_commit.ver);
        ("hash", bytes_to_json c.Space_commit.hash);
        ("ikm", bytes_to_json c.Space_commit.ikm);
        ("sig", bytes_to_json c.Space_commit.sig_);
        ("mac", bytes_to_json c.Space_commit.mac);
        ("rev", `String c.Space_commit.rev);
      ]

  let parse_signed_commit json : Space_commit.t =
    match json with
    | `Assoc _ ->
        let hash =
          bytes_of_json_exn "hash" (Yojson.Safe.Util.member "hash" json)
        in
        let ikm =
          bytes_of_json_exn "ikm" (Yojson.Safe.Util.member "ikm" json)
        in
        let sig_ =
          bytes_of_json_exn "sig" (Yojson.Safe.Util.member "sig" json)
        in
        let mac =
          bytes_of_json_exn "mac" (Yojson.Safe.Util.member "mac" json)
        in
        {
          Space_commit.ver = int_member json "ver";
          hash;
          ikm;
          sig_;
          mac;
          rev = string_member json "rev";
        }
    | _ -> fail "signedCommit must be an object"

  let parse_signed_commit_opt json : Space_commit.t option =
    match json with `Assoc _ -> Some (parse_signed_commit json) | _ -> None

  let parse_record json : record =
    {
      uri = string_member json "uri";
      cid = string_member json "cid";
      value = Yojson.Safe.Util.member "value" json;
    }

  let parse_listed_record json : listed_record =
    {
      collection = string_member json "collection";
      rkey = string_member json "rkey";
      cid = string_member json "cid";
      value =
        (match Yojson.Safe.Util.member "value" json with
        | `Null -> None
        | other -> Some other);
    }

  let parse_listed_records json : listed_records =
    {
      cursor = string_opt json "cursor";
      records =
        (match Yojson.Safe.Util.member "records" json with
        | `List xs -> List.map parse_listed_record xs
        | _ -> []);
    }

  let parse_listed_blobs json : listed_blobs =
    {
      cursor = string_opt json "cursor";
      cids =
        (match Yojson.Safe.Util.member "cids" json with
        | `List xs ->
            List.filter_map (function `String s -> Some s | _ -> None) xs
        | _ -> []);
    }

  let parse_repo_view json : repo_view =
    {
      did = string_member json "did";
      rev = string_member json "rev";
      hash =
        (match Label.bytes_of_json (Yojson.Safe.Util.member "hash" json) with
        | Some b -> b
        | None -> "");
    }

  let parse_listed_repos json : listed_repos =
    {
      cursor = string_opt json "cursor";
      repos =
        (match Yojson.Safe.Util.member "repos" json with
        | `List xs -> List.map parse_repo_view xs
        | _ -> []);
    }

  let parse_listed_spaces json : listed_spaces =
    {
      cursor = string_opt json "cursor";
      spaces =
        (match Yojson.Safe.Util.member "spaces" json with
        | `List xs -> List.map (fun x -> { uri = string_member x "uri" }) xs
        | _ -> []);
    }

  let parse_write_result json : write_result =
    {
      uri = string_member json "uri";
      cid = string_member json "cid";
      validation_status = string_opt json "validationStatus";
    }

  let parse_apply_writes_result json : apply_writes_result =
    {
      results =
        (match Yojson.Safe.Util.member "results" json with
        | `List xs -> xs
        | _ -> []);
    }

  let parse_op_entry json : op_entry =
    {
      rev = string_member json "rev";
      collection = string_member json "collection";
      rkey = string_member json "rkey";
      cid = string_opt json "cid";
      prev = string_opt json "prev";
      value =
        (match Yojson.Safe.Util.member "value" json with
        | `Null -> None
        | other -> Some other);
    }

  let parse_listed_ops json : listed_ops =
    {
      ops =
        (match Yojson.Safe.Util.member "ops" json with
        | `List xs -> List.map parse_op_entry xs
        | _ -> []);
      commit = parse_signed_commit_opt (Yojson.Safe.Util.member "commit" json);
      cursor = string_opt json "cursor";
    }

  let parse_token_field json field =
    match string_opt json field with
    | Some t when t <> "" -> t
    | _ -> fail ("missing " ^ field)

  (** [com.atproto.space.getDelegationToken] on the user's PDS. *)
  let get_delegation_token ?session ?host ~space () : string =
    get_json ?session ?host get_delegation_token_nsid
      (get_delegation_token_body ~space)
    |> fun json -> parse_token_field json "token"

  (** [com.atproto.space.getSpaceCredential] on the space host.
      [host] is required (no invented space host). Delegation Bearer +
      exchange DPoP (no [ath]). *)
  let get_space_credential ~host ~delegation ~priv ~pub ~space
      ?client_attestation () : string =
    ensure_space_uri "getSpaceCredential space" space;
    let htu = xrpc_htu ~host get_space_credential_nsid in
    let proof = Space_credential.exchange_dpop_proof ~priv ~pub ~htu () in
    let extra = Space_credential.exchange_headers ~delegation ~dpop:proof in
    Client.Client.post_json ~host ~extra get_space_credential_nsid
      (Yojson.Safe.to_string
         (get_space_credential_body ~space ?client_attestation ()))
    |> fun json -> parse_token_field json "credential"

  (** [com.atproto.space.getRecord]. OAuth session or [dpop]. *)
  let get_record ?session ?host ?bearer ?dpop ~space ~repo ~collection ~rkey ()
      : record =
    get_json ?session ?host ?bearer ?dpop get_record_nsid
      (get_record_body ~space ~repo ~collection ~rkey)
    |> parse_record

  (** [com.atproto.space.listRecords]. *)
  let list_records ?session ?host ?bearer ?dpop ~space ~repo ?collection ?limit
      ?cursor ?reverse ?exclude_values () : listed_records =
    get_json ?session ?host ?bearer ?dpop list_records_nsid
      (list_records_body ~space ~repo ?collection ?limit ?cursor ?reverse
         ?exclude_values ())
    |> parse_listed_records

  (** [com.atproto.space.getBlob] — raw blob bytes. *)
  let get_blob ?session ?host ?bearer ?dpop ~space ~repo ~cid () : string =
    get_text ?session ?host ?bearer ?dpop get_blob_nsid
      (get_blob_body ~space ~repo ~cid)

  (** [com.atproto.space.listBlobs]. *)
  let list_blobs ?session ?host ?bearer ?dpop ~space ~repo ?since ?limit ?cursor
      () : listed_blobs =
    get_json ?session ?host ?bearer ?dpop list_blobs_nsid
      (list_blobs_body ~space ~repo ?since ?limit ?cursor ())
    |> parse_listed_blobs

  (** [com.atproto.space.getLatestCommit]. *)
  let get_latest_commit ?session ?host ?bearer ?dpop ~space ~repo () :
      Space_commit.t =
    let json =
      get_json ?session ?host ?bearer ?dpop get_latest_commit_nsid
        (get_latest_commit_body ~space ~repo)
    in
    parse_signed_commit (Yojson.Safe.Util.member "commit" json)

  (** [com.atproto.space.getRepo] — CARv1 bytes (two roots: signed
      commit, then DRISL index). Does not fold or apply the CAR. *)
  let get_repo ?session ?host ?bearer ?dpop ~space ~repo ?exclude_values () :
      string =
    get_text ?session ?host ?bearer ?dpop get_repo_nsid
      (get_repo_body ~space ~repo ?exclude_values ())

  (** [com.atproto.space.listRepoOps] — incremental oplog. Does not
      apply ops to a local copy. *)
  let list_repo_ops ?session ?host ?bearer ?dpop ~space ~repo ?since ?limit
      ?cursor ?exclude_values () : listed_ops =
    get_json ?session ?host ?bearer ?dpop list_repo_ops_nsid
      (list_repo_ops_body ~space ~repo ?since ?limit ?cursor ?exclude_values ())
    |> parse_listed_ops

  (** [com.atproto.space.listRepos] — writer set from the space host. *)
  let list_repos ?session ?host ?bearer ?dpop ~space ?limit ?cursor () :
      listed_repos =
    get_json ?session ?host ?bearer ?dpop list_repos_nsid
      (list_repos_body ~space ?limit ?cursor ())
    |> parse_listed_repos

  (** [com.atproto.space.listSpaces] — spaces the caller has written
      to. OAuth on the user's PDS. *)
  let list_spaces ?session ?host ?space_type ?did ?limit ?cursor () :
      listed_spaces =
    get_json ?session ?host list_spaces_nsid
      (list_spaces_body ?space_type ?did ?limit ?cursor ())
    |> parse_listed_spaces

  (** [com.atproto.space.createRecord] (OAuth / session). *)
  let create_record ?session ?host ~space ~repo ~collection ?rkey ?validate
      ~record () : write_result =
    post_json ?session ?host create_record_nsid
      (Yojson.Safe.to_string
         (create_record_body ~space ~repo ~collection ?rkey ?validate record))
    |> parse_write_result

  (** [com.atproto.space.putRecord] (OAuth / session). *)
  let put_record ?session ?host ~space ~repo ~collection ~rkey ?validate ~record
      () : write_result =
    post_json ?session ?host put_record_nsid
      (Yojson.Safe.to_string
         (put_record_body ~space ~repo ~collection ~rkey ?validate record))
    |> parse_write_result

  (** [com.atproto.space.deleteRecord] (OAuth / session). *)
  let delete_record ?session ?host ~space ~repo ~collection ~rkey () : unit =
    ignore
      (post_json ?session ?host delete_record_nsid
         (Yojson.Safe.to_string
            (delete_record_body ~space ~repo ~collection ~rkey)))

  (** [com.atproto.space.applyWrites] (OAuth / session). *)
  let apply_writes ?session ?host ~space ~repo ~writes ?validate () :
      apply_writes_result =
    post_json ?session ?host apply_writes_nsid
      (Yojson.Safe.to_string
         (apply_writes_body ~space ~repo ~writes ?validate ()))
    |> parse_apply_writes_result

  (** [com.atproto.space.registerNotify] on the space host. *)
  let register_notify ?session ?host ?bearer ?dpop ~space ~service () : string =
    let json =
      post_json ?session ?host ?bearer ?dpop register_notify_nsid
        (Yojson.Safe.to_string (register_notify_body ~space ~service))
    in
    match string_opt json "expiresAt" with
    | Some t when t <> "" -> t
    | _ -> fail "registerNotify missing expiresAt"

  (** [com.atproto.space.unregisterNotify] on the space host. *)
  let unregister_notify ?session ?host ?bearer ?dpop ~space ~service () : unit =
    ignore
      (post_json ?session ?host ?bearer ?dpop unregister_notify_nsid
         (Yojson.Safe.to_string (unregister_notify_body ~space ~service)))

  (** [com.atproto.space.notifyWrite] (service-auth [bearer]). *)
  let notify_write ?host ?bearer ~space ~repo ~rev ~hash () : unit =
    ignore
      (post_json ?host ?bearer notify_write_nsid
         (Yojson.Safe.to_string (notify_write_body ~space ~repo ~rev ~hash)))

  (** [com.atproto.space.notifySpaceDeleted] (service-auth [bearer]). *)
  let notify_space_deleted ?host ?bearer ~space () : unit =
    ignore
      (post_json ?host ?bearer notify_space_deleted_nsid
         (Yojson.Safe.to_string (notify_space_deleted_body ~space)))
end
