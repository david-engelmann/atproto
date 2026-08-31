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

  let default_directory = "plc.directory"

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
    Printf.sprintf "https://%s/%s" directory did

  let resolve ?(directory = default_directory) (did : string) : did_document =
    validate_plc_did did;
    let url = directory_url ~directory did in
    let headers =
      Cohttp_client.create_headers_from_pairs
        [ Cohttp_client.application_json_setting_tuple ]
    in
    let body =
      Lwt_main.run (Cohttp_client.get_request_with_headers url headers)
    in
    match Error.Error.of_body body with
    | Some e -> failwith ("Did_plc.resolve: " ^ Error.Error.to_string e)
    | None -> parse_document (Yojson.Safe.from_string body)

  let resolve_log ?(directory = default_directory) (did : string) :
      operation list =
    validate_plc_did did;
    let url = Printf.sprintf "https://%s/%s/log" directory did in
    let headers =
      Cohttp_client.create_headers_from_pairs
        [ Cohttp_client.application_json_setting_tuple ]
    in
    let body =
      Lwt_main.run (Cohttp_client.get_request_with_headers url headers)
    in
    match Yojson.Safe.from_string body with
    | `List items -> List.map parse_operation items
    | _ -> failwith "Did_plc.resolve_log: expected a JSON array"

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
