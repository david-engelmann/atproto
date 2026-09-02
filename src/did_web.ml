open Cohttp_client
open Did_plc

(** did:web resolution — https://w3c-ccg.github.io/did-method-web/ *)
module Did_web = struct
  type did_document = Did_plc.did_document

  let is_web_did (did : string) : bool =
    String.length did > 8 && String.sub did 0 8 = "did:web:"

  let validate_web_did (did : string) : unit =
    if not (is_web_did did) then
      failwith ("Did_web: not a did:web identifier: " ^ did)

  (** HTTPS URL for the [did:web:] document ([/.well-known/did.json] or
      a path [did.json]). *)
  let document_url (did : string) : string =
    validate_web_did did;
    let rest = String.sub did 8 (String.length did - 8) in
    if rest = "" then failwith "Did_web: empty identifier";
    (* Split on literal ':' first so a port encoded as %3A stays in the host. *)
    match String.split_on_char ':' rest with
    | [] -> failwith "Did_web: empty identifier"
    | [ host ] ->
        Printf.sprintf "https://%s/.well-known/did.json" (Uri.pct_decode host)
    | host :: path ->
        Printf.sprintf "https://%s/%s/did.json" (Uri.pct_decode host)
          (String.concat "/" (List.map Uri.pct_decode path))

  let parse_document = Did_plc.parse_document

  (** Fetch the [did:web:] document as JSON. *)
  let resolve_json (did : string) : Yojson.Safe.t =
    let url = document_url did in
    let headers =
      Cohttp_client.create_headers_from_pairs
        [ Cohttp_client.application_json_setting_tuple ]
    in
    let body =
      Lwt_main.run (Cohttp_client.get_request_with_headers url headers)
    in
    match Error.Error.of_body body with
    | Some e -> failwith ("Did_web.resolve: " ^ Error.Error.to_string e)
    | None -> Yojson.Safe.from_string body

  (** Fetch and parse the [did:web:] document. *)
  let resolve (did : string) : did_document = parse_document (resolve_json did)
end
