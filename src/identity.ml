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

  let get_json ?host ?session endpoint pairs =
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
    let resp =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers url body headers)
    in
    match Error.Error.of_body resp with
    | Some e -> failwith ("Identity: " ^ Error.Error.to_string e)
    | None -> Yojson.Safe.from_string resp

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

  let resolve_handle ?host ?session (handle : string) : resolved_handle =
    get_json ?host ?session
      (create_identity_endpoint "resolveHandle")
      [ ("handle", handle) ]
    |> parse_resolved_handle

  let resolve_did ?host ?session (did : string) : Yojson.Safe.t =
    get_json ?host ?session
      (create_identity_endpoint "resolveDid")
      [ ("did", did) ]

  let resolve ?host ?session (actor : string) : resolved_identity =
    if String.length actor >= 4 && String.sub actor 0 4 = "did:" then
      let doc =
        if Did_plc.Did_plc.is_plc_did actor then Did_plc.Did_plc.resolve actor
        else if Did_web.Did_web.is_web_did actor then
          Did_web.Did_web.resolve actor
        else
          failwith
            "Identity.resolve: only did:plc and did:web are supported for DID \
             input"
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
        let doc = Did_plc.Did_plc.resolve did in
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
end
