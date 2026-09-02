open Cohttp_client
open App
open Session

(** com.atproto.identity — handle and DID resolution (no auth required). *)
module Identity = struct
  type resolved_handle = { did : string }

  type resolved_identity = {
    did : string;
    handle : string option;
    pds : string option;
  }

  let create_identity_endpoint (query_name : string) : string =
    "com.atproto.identity." ^ query_name

  let parse_resolved_handle json : resolved_handle =
    let open Yojson.Safe.Util in
    match json |> member "did" with
    | `String did -> { did }
    | _ -> failwith "Identity.resolve_handle: missing did"

  let host_from ?host ?session () =
    match host with
    | Some h -> h
    | None -> (
        match session with
        | Some s -> s.Session.atp_host
        | None -> Session.atp_host_from_env)

  let host_is_local host =
    let bare =
      match String.split_on_char ':' host with h :: _ -> h | [] -> host
    in
    let bare = String.lowercase_ascii bare in
    bare = "localhost" || bare = "127.0.0.1" || bare = "[::1]" || bare = "::1"

  (* PLC_ORIGIN, or local @atproto/dev-env :2582 when talking to a local PDS. *)
  let plc_directory ?host ?session () : string option =
    match Sys.getenv_opt "PLC_ORIGIN" with
    | Some o when String.trim o <> "" -> Some (String.trim o)
    | _ ->
        if host_is_local (host_from ?host ?session ()) then
          Some "http://localhost:2582"
        else None

  let get_body ?host ?session endpoint pairs =
    let host = host_from ?host ?session () in
    let base_url = App.create_public_base_url ~host () in
    let url = App.create_endpoint_url base_url endpoint in
    let body = Cohttp_client.create_body_from_pairs pairs in
    let headers =
      match session with
      | Some s ->
          Cohttp_client.create_headers_from_pairs
            [
              Cohttp_client.application_json_setting_tuple;
              Session.bearer_token_from_session s;
            ]
      | None ->
          Cohttp_client.create_headers_from_pairs
            [ Cohttp_client.application_json_setting_tuple ]
    in
    Lwt_main.run
      (Cohttp_client.get_request_with_body_and_headers url body headers)

  let get_json ?host ?session endpoint pairs =
    let resp = get_body ?host ?session endpoint pairs in
    match Error.Error.of_body resp with
    | Some e -> failwith ("Identity: " ^ Error.Error.to_string e)
    | None -> Yojson.Safe.from_string resp

  let get_json_allow_error ?host ?session endpoint pairs =
    Yojson.Safe.from_string (get_body ?host ?session endpoint pairs)

  let raise_xrpc json =
    failwith ("Identity: " ^ Error.Error.to_string (Error.Error.of_json json))

  let host_of_service_endpoint (url : string) : string =
    let strip prefix =
      let plen = String.length prefix in
      if String.length url >= plen && String.sub url 0 plen = prefix then
        String.sub url plen (String.length url - plen)
      else url
    in
    let rest =
      let after_https = strip "https://" in
      if after_https = url then strip "http://" else after_https
    in
    match String.index_opt rest '/' with
    | None -> rest
    | Some i -> String.sub rest 0 i

  (** Resolve [handle] to a DID via [com.atproto.identity.resolveHandle].
      Optional [host] / [session] select the PDS; otherwise [ATP_HOST] or
      [bsky.social]. *)
  let resolve_handle ?host ?session (handle : string) : resolved_handle =
    get_json ?host ?session
      (create_identity_endpoint "resolveHandle")
      [ ("handle", handle) ]
    |> parse_resolved_handle

  type did_resolution = {
    did_doc : Yojson.Safe.t;
    document : Did_plc.Did_plc.did_document option;
  }

  let parse_did_resolution json : did_resolution =
    let open Yojson.Safe.Util in
    let did_doc =
      match json |> member "didDoc" with `Null -> json | other -> other
    in
    let document =
      try Some (Did_plc.Did_plc.parse_document did_doc) with _ -> None
    in
    { did_doc; document }

  let document_json_of_did ?host ?session (did : string) : Yojson.Safe.t =
    let directory = plc_directory ?host ?session () in
    if Did_plc.Did_plc.is_plc_did did then
      Did_plc.Did_plc.resolve_json ?directory did
    else if Did_web.Did_web.is_web_did did then Did_web.Did_web.resolve_json did
    else
      failwith
        "Identity.resolve_did: directory fallback supports did:plc and did:web"

  (* Official resolveDid output is { didDoc }. PDS 0.5.x and production
     entryway currently 501 MethodNotImplemented; fall back to PLC / did:web. *)
  let resolve_did_via_directory ?host ?session (did : string) : Yojson.Safe.t =
    `Assoc [ ("didDoc", document_json_of_did ?host ?session did) ]

  (** Fetch a DID document via [com.atproto.identity.resolveDid].
      Falls back to the PLC directory or did:web when the PDS returns
      [MethodNotImplemented] (common on PDS 0.5.x). *)
  let resolve_did ?host ?session (did : string) : Yojson.Safe.t =
    let json =
      get_json_allow_error ?host ?session
        (create_identity_endpoint "resolveDid")
        [ ("did", did) ]
    in
    if Error.Error.is_not_implemented_json json then
      resolve_did_via_directory ?host ?session did
    else
      match Error.Error.check_for_error json with
      | Some _ -> raise_xrpc json
      | None -> json

  let resolve_did_parsed ?host ?session (did : string) : did_resolution =
    resolve_did ?host ?session did |> parse_did_resolution

  let resolve ?host ?session (actor : string) : resolved_identity =
    let directory = plc_directory ?host ?session () in
    if String.length actor >= 4 && String.sub actor 0 4 = "did:" then
      let doc =
        if Did_plc.Did_plc.is_plc_did actor then
          Did_plc.Did_plc.resolve ?directory actor
        else if Did_web.Did_web.is_web_did actor then
          Did_web.Did_web.resolve actor
        else if Did_key.Did_key.is_did_key actor then
          {
            id = actor;
            also_known_as = [];
            verification_method = [];
            service = [];
          }
        else
          failwith
            "Identity.resolve: only did:plc, did:web, and did:key are \
             supported for DID input"
      in
      {
        did = doc.Did_plc.Did_plc.id;
        handle = Did_plc.Did_plc.handle_of_document doc;
        pds = Did_plc.Did_plc.pds_endpoint doc;
      }
    else
      let resolved = resolve_handle ?host ?session actor in
      let did = resolved.did in
      if Did_plc.Did_plc.is_plc_did did then
        let doc = Did_plc.Did_plc.resolve ?directory did in
        {
          did;
          handle = Did_plc.Did_plc.handle_of_document doc;
          pds = Did_plc.Did_plc.pds_endpoint doc;
        }
      else if Did_web.Did_web.is_web_did did then
        let doc = Did_web.Did_web.resolve did in
        {
          did;
          handle = Did_plc.Did_plc.handle_of_document doc;
          pds = Did_plc.Did_plc.pds_endpoint doc;
        }
      else { did; handle = Some actor; pds = None }

  type identity_info = {
    did : string;
    handle : string;
    did_doc : Yojson.Safe.t option;
  }

  let parse_identity_info json : identity_info =
    let open Yojson.Safe.Util in
    {
      did = json |> member "did" |> to_string;
      handle = (match json |> member "handle" with `String s -> s | _ -> "");
      did_doc =
        (match json |> member "didDoc" with
        | `Null -> None
        | `Assoc _ as doc -> Some doc
        | _ -> None);
    }

  let resolve_identity_via_directory ?host ?session (identifier : string) :
      identity_info =
    let did, handle =
      if String.length identifier >= 4 && String.sub identifier 0 4 = "did:"
      then
        let doc_json = document_json_of_did ?host ?session identifier in
        let doc = Did_plc.Did_plc.parse_document doc_json in
        ( doc.Did_plc.Did_plc.id,
          Option.value ~default:"handle.invalid"
            (Did_plc.Did_plc.handle_of_document doc) )
      else
        let resolved = resolve_handle ?host ?session identifier in
        (resolved.did, identifier)
    in
    {
      did;
      handle;
      did_doc =
        (try Some (document_json_of_did ?host ?session did) with _ -> None);
    }

  (** Resolve a handle or DID to [did], [handle], and optional [didDoc].
      Same XRPC-then-directory fallback as {!resolve_did}. *)
  let resolve_identity ?host ?session (identifier : string) : identity_info =
    let json =
      get_json_allow_error ?host ?session
        (create_identity_endpoint "resolveIdentity")
        [ ("identifier", identifier) ]
    in
    if Error.Error.is_not_implemented_json json then
      resolve_identity_via_directory ?host ?session identifier
    else
      match Error.Error.check_for_error json with
      | Some _ -> raise_xrpc json
      | None -> parse_identity_info json

  let update_handle_body (handle : string) : Yojson.Safe.t =
    `Assoc [ ("handle", `String handle) ]

  type recommended_did_credentials = {
    rotation_keys : string list;
    also_known_as : string list;
    verification_methods : Yojson.Safe.t;
    services : Yojson.Safe.t;
  }

  let parse_recommended_did_credentials json : recommended_did_credentials =
    let open Yojson.Safe.Util in
    let strings field =
      match json |> member field with
      | `List items ->
          List.filter_map (function `String s -> Some s | _ -> None) items
      | _ -> []
    in
    {
      rotation_keys = strings "rotationKeys";
      also_known_as = strings "alsoKnownAs";
      verification_methods = json |> member "verificationMethods";
      services = json |> member "services";
    }

  let sign_plc_operation_body ?token ?rotation_keys ?also_known_as
      ?verification_methods ?services () : Yojson.Safe.t =
    let str_list xs = `List (List.map (fun s -> `String s) xs) in
    let fields =
      (match token with Some t -> [ ("token", `String t) ] | None -> [])
      @ (match rotation_keys with
        | Some ks -> [ ("rotationKeys", str_list ks) ]
        | None -> [])
      @ (match also_known_as with
        | Some aka -> [ ("alsoKnownAs", str_list aka) ]
        | None -> [])
      @ (match verification_methods with
        | Some v -> [ ("verificationMethods", v) ]
        | None -> [])
      @ match services with Some s -> [ ("services", s) ] | None -> []
    in
    `Assoc fields

  let submit_plc_operation_body (operation : Yojson.Safe.t) : Yojson.Safe.t =
    `Assoc [ ("operation", operation) ]

  let handle_txt_name = Syntax.Syntax.handle_txt_name
  let parse_txt_did = Syntax.Syntax.parse_txt_did
  let handle_well_known_url = Syntax.Syntax.handle_well_known_url
  let parse_well_known_did = Syntax.Syntax.parse_well_known_did

  let update_handle (s : Session.session) ~handle () : unit =
    ignore
      (Client.Client.post_json ~session:s "com.atproto.identity.updateHandle"
         (Yojson.Safe.to_string (update_handle_body handle)))

  let get_recommended_did_credentials (s : Session.session) :
      recommended_did_credentials =
    Client.Client.get_json ~session:s
      "com.atproto.identity.getRecommendedDidCredentials" []
    |> parse_recommended_did_credentials

  let sign_plc_operation (s : Session.session) ?token ?rotation_keys
      ?also_known_as ?verification_methods ?services () : Yojson.Safe.t =
    Client.Client.post_json ~session:s "com.atproto.identity.signPlcOperation"
      (Yojson.Safe.to_string
         (sign_plc_operation_body ?token ?rotation_keys ?also_known_as
            ?verification_methods ?services ()))

  let submit_plc_operation (s : Session.session) ~operation () : unit =
    ignore
      (Client.Client.post_json ~session:s
         "com.atproto.identity.submitPlcOperation"
         (Yojson.Safe.to_string (submit_plc_operation_body operation)))

  let request_plc_operation_signature (s : Session.session) : unit =
    ignore
      (Client.Client.post_json ~session:s
         "com.atproto.identity.requestPlcOperationSignature" "")

  let refresh_identity_body ~identifier : Yojson.Safe.t =
    `Assoc [ ("identifier", `String identifier) ]

  let refresh_identity ?session ?host ~identifier () : identity_info =
    Client.Client.post_json ?session ?host
      "com.atproto.identity.refreshIdentity"
      (Yojson.Safe.to_string (refresh_identity_body ~identifier))
    |> parse_identity_info
end
