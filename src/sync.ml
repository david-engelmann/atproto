open Session
open Cohttp_client
open App
open Car

module Sync = struct
  type latest_commit = { cid : string; rev : string }

  type repo_list_item = {
    did : string;
    head : string;
    rev : string;
    active : bool option;
    status : string option;
  }

  type list_repos = {
    cursor : string option;
    repos : repo_list_item list;
  }

  type list_blobs = {
    cursor : string option;
    cids : string list;
  }

  let create_sync_endpoint (query_name : string) : string =
    "com.atproto.sync." ^ query_name

  let host_of ?host (s : Session.session option) =
    match host with
    | Some h -> h
    | None -> (
        match s with
        | Some sess -> sess.atp_host
        | None -> Session.atp_host_from_env)

  let headers_of (s : Session.session option) =
    let application_json = Cohttp_client.application_json_setting_tuple in
    match s with
    | Some sess ->
        Cohttp_client.create_headers_from_pairs
          [ application_json; Session.bearer_token_from_session sess ]
    | None -> Cohttp_client.create_headers_from_pairs [ application_json ]

  let request_json ?host ?session endpoint pairs =
    let host = host_of ?host session in
    let base_url = App.create_public_base_url ~host () in
    let url = App.create_endpoint_url base_url endpoint in
    let body = Cohttp_client.create_body_from_pairs pairs in
    let headers = headers_of session in
    let resp =
      Lwt_main.run (Cohttp_client.get_request_with_body_and_headers url body headers)
    in
    match Error.Error.of_body resp with
    | Some e -> failwith ("Sync: " ^ Error.Error.to_string e)
    | None -> Yojson.Safe.from_string resp

  let request_bytes ?host ?session endpoint pairs =
    let host = host_of ?host session in
    let base_url = App.create_public_base_url ~host () in
    let url = App.create_endpoint_url base_url endpoint in
    let body = Cohttp_client.create_body_from_pairs pairs in
    let headers = headers_of session in
    Lwt_main.run (Cohttp_client.get_request_with_body_and_headers url body headers)

  let string_opt json field =
    match Yojson.Safe.Util.member field json with
    | `String s -> Some s
    | _ -> None

  let parse_latest_commit json : latest_commit =
    let open Yojson.Safe.Util in
    {
      cid = json |> member "cid" |> to_string;
      rev = json |> member "rev" |> to_string;
    }

  let parse_repo_list_item json : repo_list_item =
    let open Yojson.Safe.Util in
    {
      did = json |> member "did" |> to_string;
      head =
        (match json |> member "head" with
        | `String s -> s
        | _ -> (
            match json |> member "cid" with
            | `String s -> s
            | _ -> ""));
      rev = (match json |> member "rev" with `String s -> s | _ -> "");
      active =
        (match json |> member "active" with
        | `Bool b -> Some b
        | _ -> None);
      status = string_opt json "status";
    }

  let parse_list_repos json : list_repos =
    let open Yojson.Safe.Util in
    {
      cursor = string_opt json "cursor";
      repos =
        (match json |> member "repos" with
        | `List items -> List.map parse_repo_list_item items
        | _ -> []);
    }

  let parse_list_blobs json : list_blobs =
    let open Yojson.Safe.Util in
    {
      cursor = string_opt json "cursor";
      cids =
        (match json |> member "cids" with
        | `List items ->
            List.filter_map (function `String s -> Some s | _ -> None) items
        | _ -> []);
    }

  let optional_pairs pairs =
    List.filter_map (fun (k, v) -> Option.map (fun x -> (k, x)) v) pairs

  let get_latest_commit ?host ?session (did : string) : latest_commit =
    request_json ?host ?session
      (create_sync_endpoint "getLatestCommit")
      [ ("did", did) ]
    |> parse_latest_commit

  let get_repo ?host ?session ?since (did : string) : string =
    request_bytes ?host ?session
      (create_sync_endpoint "getRepo")
      (("did", did) :: optional_pairs [ ("since", since) ])

  let get_repo_car ?host ?session ?since (did : string) : Car.t =
    Car.parse (get_repo ?host ?session ?since did)

  let get_blob (s : Session.session) (did : string) (cid : string) : string Lwt.t =
    let host = s.atp_host in
    let base_url = App.create_public_base_url ~host () in
    let url = App.create_endpoint_url base_url (create_sync_endpoint "getBlob") in
    let body = Cohttp_client.create_body_from_pairs [ ("did", did); ("cid", cid) ] in
    let headers = headers_of (Some s) in
    Cohttp_client.get_request_with_body_and_headers url body headers

  let download_image (s : Session.session) (did : string) (cid : string)
      (filename : string) : unit Lwt.t =
    let open Lwt.Infix in
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let get_blob_url =
      App.create_endpoint_url base_url (create_sync_endpoint "getBlob")
    in
    let body = Cohttp_client.create_body_from_pairs [ ("did", did); ("cid", cid) ] in
    Cohttp_client.get_content_type_with_body_headers get_blob_url body headers
    >>= fun content_type ->
    match content_type with
    | ct
      when List.mem ct
             [
               "image/jpeg";
               "image/png";
               "image/gif";
               "image/bmp";
               "image/webp";
               "image/svg+xml";
               "image/tiff";
             ] ->
        get_blob s did cid >>= fun blob ->
        Lwt_io.with_file ~mode:Lwt_io.Output filename (fun oc ->
            Lwt_io.write oc blob)
    | _ -> Lwt.return ()

  let get_blocks (s : Session.session) (did : string) (cids : string list) :
      string Lwt.t =
    let host = s.atp_host in
    let base_url = App.create_public_base_url ~host () in
    let url = App.create_endpoint_url base_url (create_sync_endpoint "getBlocks") in
    let body =
      Cohttp_client.create_body_from_pairs [ ("did", did) ]
      ^
      if cids = [] then ""
      else "&" ^ Cohttp_client.add_query_params "cids" cids
    in
    let headers = headers_of (Some s) in
    Cohttp_client.get_request_with_body_and_headers url body headers

  let get_record ?host ?session ?commit (did : string) (collection : string)
      (rkey : string) : string =
    request_bytes ?host ?session
      (create_sync_endpoint "getRecord")
      (("did", did)
      :: ("collection", collection)
      :: ("rkey", rkey)
      :: optional_pairs [ ("commit", commit) ])

  let list_blobs ?host ?session ?since ?cursor ?limit (did : string) : list_blobs =
    request_json ?host ?session
      (create_sync_endpoint "listBlobs")
      (("did", did)
      :: optional_pairs
           [
             ("since", since);
             ("cursor", cursor);
             ("limit", Option.map string_of_int limit);
           ])
    |> parse_list_blobs

  let list_repos ?host ?session ?cursor ?limit () : list_repos =
    request_json ?host ?session
      (create_sync_endpoint "listRepos")
      (optional_pairs
         [ ("cursor", cursor); ("limit", Option.map string_of_int limit) ])
    |> parse_list_repos

  let request_crawl ?host ?session (hostname : string) : string =
    let host = host_of ?host session in
    let base_url = App.create_public_base_url ~host () in
    let url =
      App.create_endpoint_url base_url (create_sync_endpoint "requestCrawl")
    in
    let headers = headers_of session in
    let data = Printf.sprintf "{\"hostname\": \"%s\"}" hostname in
    Lwt_main.run (Cohttp_client.post_data_with_headers url data headers)

  (* Deprecated 2023 endpoints kept as thin wrappers so older call sites compile. *)
  let get_head (s : Session.session) (did : string) : string =
    let c = get_latest_commit ~session:s did in
    Yojson.Safe.to_string (`Assoc [ ("cid", `String c.cid); ("rev", `String c.rev) ])

  let get_checkout (s : Session.session) (did : string) (_commit : string) : string =
    get_repo ~session:s did

  let get_commit_path (_s : Session.session) (_did : string) (_latest : string)
      (_earliest : string) : string =
    failwith
      "com.atproto.sync.getCommitPath was removed; use Sync.get_repo ?since"

  let get_repo_legacy (s : Session.session) (did : string) (_earliest : string)
      (_latest : string) : string =
    get_repo ~session:s did

  let list_blobs_legacy (s : Session.session) (did : string) (_earliest : string)
      (_latest : string) : string =
    let blobs = list_blobs ~session:s did in
    Yojson.Safe.to_string
      (`Assoc [ ("cids", `List (List.map (fun c -> `String c) blobs.cids)) ])

  let list_repos_legacy (s : Session.session) (limit : int) : string =
    let repos = list_repos ~session:s ~limit () in
    Yojson.Safe.to_string
      (`Assoc
         [
           ( "repos",
             `List
               (List.map
                  (fun (r : repo_list_item) ->
                    `Assoc
                      [
                        ("did", `String r.did);
                        ("head", `String r.head);
                        ("rev", `String r.rev);
                      ])
                  repos.repos) );
         ])

  let notify_of_update (_s : Session.session) (_hostname : string) : string =
    failwith "com.atproto.sync.notifyOfUpdate is deprecated; use request_crawl"
end
