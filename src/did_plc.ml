open Cohttp_client

(** did:plc documents and directory resolution — https://web.plc.directory/spec/v0.1/did-plc *)
module Did_plc = struct
  type verification_method = {
    id : string;
    type_ : string;
    controller : string;
    public_key_multibase : string option;
  }

  type service = {
    id : string;
    type_ : string;
    service_endpoint : string;
  }

  type did_document = {
    id : string;
    also_known_as : string list;
    verification_method : verification_method list;
    service : service list;
  }

  type operation = {
    type_ : string;
    sig_ : string option;
    prev : string option;
    raw : Yojson.Safe.t;
  }

  let default_directory = "plc.directory"

  let is_plc_did (did : string) : bool =
    String.length did > 8 && String.sub did 0 8 = "did:plc:"

  let validate_plc_did (did : string) : unit =
    if not (is_plc_did did) then failwith ("Did_plc: not a did:plc identifier: " ^ did);
    let suffix = String.sub did 8 (String.length did - 8) in
    if String.length suffix <> 24 then
      failwith "Did_plc: did:plc identifier must be 24 base32 characters after the prefix";
    String.iter
      (function
        | 'a' .. 'z' | '2' .. '7' -> ()
        | _ -> failwith "Did_plc: invalid base32 character in did:plc")
      suffix

  let string_list json field =
    match Yojson.Safe.Util.member field json with
    | `List items ->
        List.filter_map
          (function `String s -> Some s | _ -> None)
          items
    | _ -> []

  let parse_verification_method json : verification_method =
    let open Yojson.Safe.Util in
    {
      id = (match json |> member "id" with `String s -> s | _ -> "");
      type_ = (match json |> member "type" with `String s -> s | _ -> "");
      controller =
        (match json |> member "controller" with `String s -> s | _ -> "");
      public_key_multibase =
        (match json |> member "publicKeyMultibase" with
        | `String s -> Some s
        | _ -> None);
    }

  let parse_service json : service =
    let open Yojson.Safe.Util in
    {
      id = (match json |> member "id" with `String s -> s | _ -> "");
      type_ = (match json |> member "type" with `String s -> s | _ -> "");
      service_endpoint =
        (match json |> member "serviceEndpoint" with
        | `String s -> s
        | _ -> "");
    }

  let parse_document json : did_document =
    let open Yojson.Safe.Util in
    let id =
      match json |> member "id" with
      | `String s -> s
      | _ -> failwith "Did_plc.parse_document: missing id"
    in
    let verification_method =
      match json |> member "verificationMethod" with
      | `List items -> List.map parse_verification_method items
      | _ -> []
    in
    let service =
      match json |> member "service" with
      | `List items -> List.map parse_service items
      | _ -> []
    in
    { id; also_known_as = string_list json "alsoKnownAs"; verification_method; service }

  let parse_operation json : operation =
    let open Yojson.Safe.Util in
    {
      type_ = (match json |> member "type" with `String s -> s | _ -> "");
      sig_ = (match json |> member "sig" with `String s -> Some s | _ -> None);
      prev = (match json |> member "prev" with `String s -> Some s | _ -> None);
      raw = json;
    }

  let handle_of_document (doc : did_document) : string option =
    let rec find = function
      | [] -> None
      | hd :: rest ->
          if String.length hd >= 5 && String.sub hd 0 5 = "at://" then
            Some (String.sub hd 5 (String.length hd - 5))
          else find rest
    in
    find doc.also_known_as

  let pds_endpoint (doc : did_document) : string option =
    let is_pds (s : service) =
      let id = s.id in
      String.length id >= 11
      && (String.sub id (String.length id - 11) 11 = "#atproto_pds"
         || id = "#atproto_pds")
      || s.type_ = "AtprotoPersonalDataServer"
    in
    match List.find_opt is_pds doc.service with
    | Some s -> Some s.service_endpoint
    | None -> None

  let signing_key (doc : did_document) : verification_method option =
    List.find_opt
      (fun (m : verification_method) ->
        let id = m.id in
        String.length id >= 8
        && String.sub id (String.length id - 8) 8 = "#atproto")
      doc.verification_method

  let directory_url ?(directory = default_directory) (did : string) : string =
    Printf.sprintf "https://%s/%s" directory did

  let resolve ?(directory = default_directory) (did : string) : did_document =
    validate_plc_did did;
    let url = directory_url ~directory did in
    let headers =
      Cohttp_client.create_headers_from_pairs
        [ Cohttp_client.application_json_setting_tuple ]
    in
    let body = Lwt_main.run (Cohttp_client.get_request_with_headers url headers) in
    match Error.Error.of_body body with
    | Some e -> failwith ("Did_plc.resolve: " ^ Error.Error.to_string e)
    | None -> parse_document (Yojson.Safe.from_string body)

  let resolve_log ?(directory = default_directory) (did : string) : operation list =
    validate_plc_did did;
    let url = Printf.sprintf "https://%s/%s/log" directory did in
    let headers =
      Cohttp_client.create_headers_from_pairs
        [ Cohttp_client.application_json_setting_tuple ]
    in
    let body = Lwt_main.run (Cohttp_client.get_request_with_headers url headers) in
    match Yojson.Safe.from_string body with
    | `List items -> List.map parse_operation items
    | _ -> failwith "Did_plc.resolve_log: expected a JSON array"
end
