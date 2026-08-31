open Cohttp_client
open Cid
open Dag_cbor
open Base32
open Base64url
open Hash
open Did_key

let ensure_rng = lazy (Mirage_crypto_rng_unix.use_default ())

(** did:plc documents and directory resolution — https://web.plc.directory/spec/v0.1/did-plc *)
module Did_plc = struct
  type verification_method = {
    id : string;
    type_ : string;
    controller : string;
    public_key_multibase : string option;
  }

  type service = { id : string; type_ : string; service_endpoint : string }

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

  type plc_service = { type_ : string; endpoint : string }

  type plc_state = {
    did : string option;
    rotation_keys : string list;
    verification_methods : (string * string) list;
    also_known_as : string list;
    services : (string * plc_service) list;
  }

  type audit_entry = {
    did : string option;
    cid : string option;
    operation : operation;
    nullified : bool;
    created_at : string option;
  }

  let default_directory = "plc.directory"

  let strip_trailing_slash (s : string) : string =
    let n = String.length s in
    if n > 0 && s.[n - 1] = '/' then String.sub s 0 (n - 1) else s

  let starts_with prefix s =
    let n = String.length prefix in
    String.length s >= n && String.sub s 0 n = prefix

  (* Host ("plc.directory") or full origin ("http://localhost:2582"). *)
  let origin_of_directory (directory : string) : string =
    let d = String.trim directory in
    if starts_with "http://" d || starts_with "https://" d then
      strip_trailing_slash d
    else "https://" ^ d

  let origin_from_env () : string =
    match Sys.getenv_opt "PLC_ORIGIN" with
    | Some o when String.trim o <> "" -> strip_trailing_slash (String.trim o)
    | _ -> origin_of_directory default_directory

  let plc_origin ?directory () : string =
    match directory with
    | Some d -> origin_of_directory d
    | None -> origin_from_env ()

  let is_plc_did (did : string) : bool =
    String.length did > 8 && String.sub did 0 8 = "did:plc:"

  let validate_plc_did (did : string) : unit =
    if not (is_plc_did did) then
      failwith ("Did_plc: not a did:plc identifier: " ^ did);
    let suffix = String.sub did 8 (String.length did - 8) in
    if String.length suffix <> 24 then
      failwith
        "Did_plc: did:plc identifier must be 24 base32 characters after the \
         prefix";
    String.iter
      (function
        | 'a' .. 'z' | '2' .. '7' -> ()
        | _ -> failwith "Did_plc: invalid base32 character in did:plc")
      suffix

  let string_list json field =
    match Yojson.Safe.Util.member field json with
    | `List items ->
        List.filter_map (function `String s -> Some s | _ -> None) items
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
        (match json |> member "serviceEndpoint" with `String s -> s | _ -> "");
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
    {
      id;
      also_known_as = string_list json "alsoKnownAs";
      verification_method;
      service;
    }

  let parse_operation json : operation =
    let open Yojson.Safe.Util in
    {
      type_ = (match json |> member "type" with `String s -> s | _ -> "");
      sig_ = (match json |> member "sig" with `String s -> Some s | _ -> None);
      prev =
        (match json |> member "prev" with `String s -> Some s | _ -> None);
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

  let ends_with suffix s =
    let n = String.length s and m = String.length suffix in
    n >= m && String.sub s (n - m) m = suffix

  (* DID document #bsky_chat / BlueskyChatService — chat.bsky atproto-proxy. *)
  let is_chat_service (s : service) : bool =
    s.id = "#bsky_chat" || s.id = "bsky_chat"
    || ends_with "#bsky_chat" s.id
    ||
    let t = String.lowercase_ascii s.type_ in
    t = "blueskychatservice" || t = "bskychatservice"

  let chat_service (doc : did_document) : service option =
    List.find_opt is_chat_service doc.service

  let chat_endpoint (doc : did_document) : string option =
    match chat_service doc with
    | Some s -> Some s.service_endpoint
    | None -> None

  let signing_key (doc : did_document) : verification_method option =
    List.find_opt
      (fun (m : verification_method) ->
        let id = m.id in
        String.length id >= 8
        && String.sub id (String.length id - 8) 8 = "#atproto")
      doc.verification_method

  let did_key_of_method (m : verification_method) : string option =
    match m.public_key_multibase with
    | Some mb when String.length mb > 8 && String.sub mb 0 8 = "did:key:" ->
        Some mb
    | Some mb when String.length mb > 0 && (mb.[0] = 'z' || mb.[0] = 'Z') ->
        Some ("did:key:" ^ mb)
    | _ -> None

  let is_atproto_key_id (id : string) : bool =
    let n = String.length id in
    n >= 8 && String.sub id (n - 8) 8 = "#atproto"

  let atproto_signing_keys (doc : did_document) : string list =
    List.filter_map
      (fun (m : verification_method) ->
        if is_atproto_key_id m.id then did_key_of_method m else None)
      doc.verification_method

  let signing_keys_of_document (doc : did_document) : string list =
    match atproto_signing_keys doc with
    | [] -> List.filter_map did_key_of_method doc.verification_method
    | keys -> keys

  let directory_url ?(directory = default_directory) (did : string) : string =
    origin_of_directory directory ^ "/" ^ did

  let fetch_json (url : string) : Yojson.Safe.t =
    let headers =
      Cohttp_client.create_headers_from_pairs
        [ Cohttp_client.application_json_setting_tuple ]
    in
    let body =
      Lwt_main.run (Cohttp_client.get_request_with_headers url headers)
    in
    match Error.Error.of_body body with
    | Some e -> failwith ("Did_plc.resolve: " ^ Error.Error.to_string e)
    | None -> Yojson.Safe.from_string body

  let resolve_json ?directory (did : string) : Yojson.Safe.t =
    validate_plc_did did;
    fetch_json (plc_origin ?directory () ^ "/" ^ did)

  let resolve ?directory (did : string) : did_document =
    parse_document (resolve_json ?directory did)

  let resolve_log ?directory (did : string) : operation list =
    validate_plc_did did;
    let url = plc_origin ?directory () ^ "/" ^ did ^ "/log" in
    match fetch_json url with
    | `List items -> List.map parse_operation items
    | _ -> failwith "Did_plc.resolve_log: expected a JSON array"

  let json_string_opt json field =
    match Yojson.Safe.Util.member field json with
    | `String s -> Some s
    | _ -> None

  let parse_plc_service json : plc_service =
    {
      type_ =
        (match Yojson.Safe.Util.member "type" json with
        | `String s -> s
        | _ -> "");
      endpoint =
        (match Yojson.Safe.Util.member "endpoint" json with
        | `String s -> s
        | _ -> "");
    }

  let parse_plc_state json : plc_state =
    let open Yojson.Safe.Util in
    let verification_methods =
      match json |> member "verificationMethods" with
      | `Assoc fields ->
          List.filter_map
            (fun (k, v) -> match v with `String s -> Some (k, s) | _ -> None)
            fields
      | _ -> []
    in
    let services =
      match json |> member "services" with
      | `Assoc fields ->
          List.filter_map
            (fun (k, v) ->
              match v with
              | `Assoc _ as obj -> Some (k, parse_plc_service obj)
              | _ -> None)
            fields
      | _ -> []
    in
    {
      did = json_string_opt json "did";
      rotation_keys = string_list json "rotationKeys";
      verification_methods;
      also_known_as = string_list json "alsoKnownAs";
      services;
    }

  let parse_audit_entry json : audit_entry =
    let op_json =
      match Yojson.Safe.Util.member "operation" json with
      | `Assoc _ as op -> op
      | _ -> json
    in
    {
      did = json_string_opt json "did";
      cid = json_string_opt json "cid";
      operation = parse_operation op_json;
      nullified =
        (match Yojson.Safe.Util.member "nullified" json with
        | `Bool b -> b
        | _ -> false);
      created_at = json_string_opt json "createdAt";
    }

  let resolve_data ?directory (did : string) : plc_state =
    validate_plc_did did;
    parse_plc_state
      (fetch_json (plc_origin ?directory () ^ "/" ^ did ^ "/data"))

  let resolve_audit_log ?directory (did : string) : audit_entry list =
    validate_plc_did did;
    let url = plc_origin ?directory () ^ "/" ^ did ^ "/log/audit" in
    match fetch_json url with
    | `List items -> List.map parse_audit_entry items
    | _ -> failwith "Did_plc.resolve_audit_log: expected a JSON array"

  let assoc_strings xs = `Assoc (List.map (fun (k, v) -> (k, `String v)) xs)

  let services_json (services : (string * plc_service) list) : Yojson.Safe.t =
    `Assoc
      (List.map
         (fun (id, s) ->
           ( id,
             `Assoc
               [ ("type", `String s.type_); ("endpoint", `String s.endpoint) ]
           ))
         services)

  let genesis_operation ?(also_known_as = [])
      ?(verification_methods : (string * string) list = [])
      ?(services : (string * plc_service) list = []) ~rotation_keys () :
      Yojson.Safe.t =
    `Assoc
      [
        ("type", `String "plc_operation");
        ("rotationKeys", `List (List.map (fun k -> `String k) rotation_keys));
        ("verificationMethods", assoc_strings verification_methods);
        ("alsoKnownAs", `List (List.map (fun a -> `String a) also_known_as));
        ("services", services_json services);
        ("prev", `Null);
      ]

  let update_operation ?(also_known_as = [])
      ?(verification_methods : (string * string) list = [])
      ?(services : (string * plc_service) list = []) ~rotation_keys ~prev () :
      Yojson.Safe.t =
    `Assoc
      [
        ("type", `String "plc_operation");
        ("rotationKeys", `List (List.map (fun k -> `String k) rotation_keys));
        ("verificationMethods", assoc_strings verification_methods);
        ("alsoKnownAs", `List (List.map (fun a -> `String a) also_known_as));
        ("services", services_json services);
        ("prev", `String prev);
      ]

  let tombstone_operation ~prev () : Yojson.Safe.t =
    `Assoc [ ("type", `String "plc_tombstone"); ("prev", `String prev) ]

  let submit_operation ?directory (did : string) (op : Yojson.Safe.t) : string =
    validate_plc_did did;
    let url = plc_origin ?directory () ^ "/" ^ did in
    let headers =
      Cohttp_client.create_headers_from_pairs
        [ Cohttp_client.application_json_setting_tuple ]
    in
    let status, body =
      Lwt_main.run
        (Cohttp_client.post_with_status url (Yojson.Safe.to_string op) headers)
    in
    if status < 200 || status >= 300 then
      match Error.Error.of_body body with
      | Some e -> failwith ("Did_plc.submit: " ^ Error.Error.to_string e)
      | None ->
          failwith (Printf.sprintf "Did_plc.submit: HTTP %d %s" status body)
    else body

  let rec json_to_cbor : Yojson.Safe.t -> Dag_cbor.value = function
    | `Null -> Dag_cbor.Null
    | `Bool b -> Dag_cbor.Bool b
    | `Int n -> Dag_cbor.Int n
    | `Intlit s -> Dag_cbor.Int64 (Int64.of_string s)
    | `Float f ->
        let n = Int64.of_float f in
        if Float.equal f (Int64.to_float n) then Dag_cbor.Int64 n
        else failwith "Did_plc: DAG-CBOR cannot encode floats"
    | `String s -> Dag_cbor.Text s
    | `List xs -> Dag_cbor.Array (List.map json_to_cbor xs)
    | `Assoc fields ->
        Dag_cbor.Map (List.map (fun (k, v) -> (k, json_to_cbor v)) fields)

  let strip_sig = function
    | `Assoc fields -> `Assoc (List.filter (fun (k, _) -> k <> "sig") fields)
    | other -> other

  let cbor_of_json json = Dag_cbor.encode (json_to_cbor json)
  let unsigned_bytes (op : operation) = cbor_of_json (strip_sig op.raw)
  let signed_bytes (op : operation) = cbor_of_json op.raw

  let genesis_did_of_signed_cbor (signed_cbor : string) : string =
    let hash = Hash.sha256 signed_cbor in
    "did:plc:" ^ String.sub (Base32.encode hash) 0 24

  let genesis_did (op : operation) : string =
    genesis_did_of_signed_cbor (signed_bytes op)

  let cid_of_operation (op : operation) : Cid.t = Cid.create (signed_bytes op)

  let rotation_keys_of_json json =
    match Yojson.Safe.Util.member "rotationKeys" json with
    | `List items ->
        List.filter_map (function `String s -> Some s | _ -> None) items
    | _ ->
        let open Yojson.Safe.Util in
        List.filter_map
          (fun field ->
            match member field json with `String s -> Some s | _ -> None)
          [ "signingKey"; "recoveryKey" ]

  (* NIST P-256 group order n and floor(n/2) for low-S ECDSA. *)
  let p256_n =
    Hash.hex_decode
      "ffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551"

  let p256_n_half =
    Hash.hex_decode
      "7fffffff800000007fffffffffffffffde737d56d38bcf4279dce5617e3192a8"

  let sub_be (n : string) (s : string) : string =
    let out = Bytes.create (String.length n) in
    let borrow = ref 0 in
    for i = String.length n - 1 downto 0 do
      let d = Char.code n.[i] - Char.code s.[i] - !borrow in
      if d < 0 then (
        Bytes.set out i (Char.chr (d + 256));
        borrow := 1)
      else (
        Bytes.set out i (Char.chr d);
        borrow := 0)
    done;
    Bytes.to_string out

  let low_s (s : string) : string =
    if String.compare s p256_n_half > 0 then sub_be p256_n s else s

  let is_low_s (s : string) : bool = String.compare s p256_n_half <= 0

  type sig_status =
    [ `Valid | `Invalid | `Unsupported_curve of string | `Missing ]

  let k256_n = K256.K256.n_octets
  let k256_n_half = K256.K256.n_half_octets
  let low_s_k256 = K256.K256.low_s
  let is_low_s_k256 = K256.K256.is_low_s

  let sign_p256 ~(priv : Mirage_crypto_ec.P256.Dsa.priv) (json : Yojson.Safe.t)
      : Yojson.Safe.t =
    Lazy.force ensure_rng;
    let unsigned = strip_sig json in
    let digest = Hash.sha256 (cbor_of_json unsigned) in
    let r, s = Mirage_crypto_ec.P256.Dsa.sign ~key:priv digest in
    let s = low_s s in
    let sig_b64 = Base64url.encode (r ^ s) in
    match unsigned with
    | `Assoc fields -> `Assoc (fields @ [ ("sig", `String sig_b64) ])
    | _ -> failwith "Did_plc.sign_p256: expected a JSON object"

  let verify_p256 ~(pub : Mirage_crypto_ec.P256.Dsa.pub) (op : operation) :
      sig_status =
    match op.sig_ with
    | None -> `Missing
    | Some b64 ->
        let raw = Base64url.decode b64 in
        if String.length raw <> 64 then `Invalid
        else
          let r = String.sub raw 0 32 in
          let s = String.sub raw 32 32 in
          if not (is_low_s s) then `Invalid
          else
            let digest = Hash.sha256 (unsigned_bytes op) in
            if Mirage_crypto_ec.P256.Dsa.verify ~key:pub (r, s) digest then
              `Valid
            else `Invalid

  let sign_k256 ~(priv : K256.K256.priv) (json : Yojson.Safe.t) : Yojson.Safe.t
      =
    let unsigned = strip_sig json in
    let digest = Hash.sha256 (cbor_of_json unsigned) in
    let r, s = K256.K256.sign ~key:priv digest in
    let sig_b64 = Base64url.encode (r ^ s) in
    match unsigned with
    | `Assoc fields -> `Assoc (fields @ [ ("sig", `String sig_b64) ])
    | _ -> failwith "Did_plc.sign_k256: expected a JSON object"

  let verify_k256 ~(pub : K256.K256.pub) (op : operation) : sig_status =
    match op.sig_ with
    | None -> `Missing
    | Some b64 ->
        let raw = Base64url.decode b64 in
        if String.length raw <> 64 then `Invalid
        else
          let r = String.sub raw 0 32 in
          let s = String.sub raw 32 32 in
          if not (is_low_s_k256 s) then `Invalid
          else
            let digest = Hash.sha256 (unsigned_bytes op) in
            if K256.K256.verify ~key:pub (r, s) digest then `Valid else `Invalid

  let verify_with_rotation_keys (keys : string list) (op : operation) :
      sig_status =
    let parsed =
      List.filter_map
        (fun k -> try Some (Did_key.of_string k) with _ -> None)
        keys
    in
    let rec try_keys = function
      | [] -> (
          let other =
            List.find_map
              (fun k ->
                match k.Did_key.curve with
                | Did_key.Other n -> Some (Printf.sprintf "0x%x" n)
                | _ -> None)
              parsed
          in
          match other with Some c -> `Unsupported_curve c | None -> `Invalid)
      | k :: rest -> (
          match k.Did_key.curve with
          | Did_key.P256 -> (
              match Did_key.p256_pub k with
              | Some pub -> (
                  match verify_p256 ~pub op with
                  | `Valid -> `Valid
                  | _ -> try_keys rest)
              | None -> try_keys rest)
          | Did_key.K256 -> (
              match Did_key.k256_pub k with
              | Some pub -> (
                  match verify_k256 ~pub op with
                  | `Valid -> `Valid
                  | _ -> try_keys rest)
              | None -> try_keys rest)
          | Did_key.Other _ -> try_keys rest)
    in
    try_keys parsed

  type chain_result = {
    genesis_ok : bool;
    prev_links_ok : bool;
    signatures : sig_status list;
  }

  let verify_chain ~(did : string) (ops : operation list) : chain_result =
    match ops with
    | [] -> failwith "Did_plc.verify_chain: empty log"
    | genesis :: rest ->
        let genesis_ok =
          genesis_did genesis = did
          && match genesis.prev with None -> true | Some _ -> false
        in
        let rec walk prev_cid keys acc_ok acc_sigs = function
          | [] -> (acc_ok, List.rev acc_sigs)
          | op :: rest ->
              let prev_ok =
                match op.prev with
                | Some p -> p = Cid.to_string prev_cid
                | None -> false
              in
              let sig_st = verify_with_rotation_keys keys op in
              let keys =
                match rotation_keys_of_json op.raw with [] -> keys | ks -> ks
              in
              walk (cid_of_operation op) keys (acc_ok && prev_ok)
                (sig_st :: acc_sigs) rest
        in
        let keys = rotation_keys_of_json genesis.raw in
        let genesis_sig = verify_with_rotation_keys keys genesis in
        let prev_ok, rest_sigs =
          walk (cid_of_operation genesis) keys true [] rest
        in
        {
          genesis_ok;
          prev_links_ok = prev_ok;
          signatures = genesis_sig :: rest_sigs;
        }
end
