open Session
open Cohttp_client
open App
open Dag_cbor
open Base64url
open Hash
open Did_key

let ensure_rng = lazy (Mirage_crypto_rng_unix.use_default ())

(** [com.atproto.label] — query, parse, and verify signed labels. *)
module Label = struct
  type label = {
    src : string;
    uri : string;
    cid : string option;
    val_ : string;
    neg : bool option;
    cts : string option;
    exp : string option;
    ver : int option;
    sig_ : string option;
  }

  let bytes_of_json json =
    match json with
    | `Assoc fields -> (
        match List.assoc_opt "$bytes" fields with
        | Some (`String s) -> Some (Base64url.decode s)
        | _ -> None)
    | `String s -> Some (Base64url.decode s)
    | _ -> None

  let parse_label json : label =
    let open Yojson.Safe.Util in
    {
      src = (match json |> member "src" with `String s -> s | _ -> "");
      uri = (match json |> member "uri" with `String s -> s | _ -> "");
      cid = (match json |> member "cid" with `String s -> Some s | _ -> None);
      val_ = (match json |> member "val" with `String s -> s | _ -> "");
      neg = (match json |> member "neg" with `Bool b -> Some b | _ -> None);
      cts = (match json |> member "cts" with `String s -> Some s | _ -> None);
      exp = (match json |> member "exp" with `String s -> Some s | _ -> None);
      ver = (match json |> member "ver" with `Int n -> Some n | _ -> None);
      sig_ = bytes_of_json (json |> member "sig");
    }

  let parse_label_cbor (v : Dag_cbor.value) : label =
    let fields = Dag_cbor.get_map v in
    {
      src = Dag_cbor.as_text (Dag_cbor.require "src" fields);
      uri = Dag_cbor.as_text (Dag_cbor.require "uri" fields);
      cid =
        (match Dag_cbor.find "cid" fields with
        | Some (Dag_cbor.Text s) -> Some s
        | _ -> None);
      val_ = Dag_cbor.as_text (Dag_cbor.require "val" fields);
      neg =
        (match Dag_cbor.find "neg" fields with
        | Some b -> Some (Dag_cbor.as_bool b)
        | None -> None);
      cts =
        (match Dag_cbor.find "cts" fields with
        | Some t -> Some (Dag_cbor.as_text t)
        | None -> None);
      exp =
        (match Dag_cbor.find "exp" fields with
        | Some t -> Some (Dag_cbor.as_text t)
        | None -> None);
      ver =
        (match Dag_cbor.find "ver" fields with
        | Some n -> Some (Dag_cbor.as_int n)
        | None -> None);
      sig_ =
        (match Dag_cbor.find "sig" fields with
        | Some (Dag_cbor.Bytes b) -> Some b
        | _ -> None);
    }

  type query_labels = { cursor : string option; labels : label list }

  let parse_query_labels json : query_labels =
    let open Yojson.Safe.Util in
    {
      cursor =
        (match json |> member "cursor" with `String s -> Some s | _ -> None);
      labels =
        (match json |> member "labels" with
        | `List items -> List.map parse_label items
        | _ -> []);
    }

  let parse_label_values json : string list option =
    match json with
    | `Null -> None
    | `List items ->
        let vals =
          List.filter_map
            (function
              | `String s -> Some s
              | `Assoc _ as obj -> (
                  match Yojson.Safe.Util.member "val" obj with
                  | `String s -> Some s
                  | _ -> None)
              | _ -> None)
            items
        in
        if vals = [] then None else Some vals
    | _ -> None

  let self_label_values json : string list =
    match json with
    | `List items ->
        List.filter_map
          (function
            | `String s -> Some s
            | `Assoc _ as obj -> (
                match Yojson.Safe.Util.member "val" obj with
                | `String s -> Some s
                | _ -> None)
            | _ -> None)
          items
    | _ -> []

  (* com.atproto.label.defs#selfLabels — author-applied values on a record. *)
  let parse_self_labels json : string list option =
    match json with
    | `Null -> None
    | `Assoc _ -> (
        match Yojson.Safe.Util.member "values" json with
        | `List _ as values ->
            let vals = self_label_values values in
            if vals = [] then None else Some vals
        | _ -> None)
    | `List _ as values ->
        let vals = self_label_values values in
        if vals = [] then None else Some vals
    | _ -> None

  type label_value_definition_strings = {
    lang : string;
    name : string;
    description : string;
  }

  type label_value_definition = {
    identifier : string;
    severity : string;
    blurs : string;
    default_setting : string option;
    adult_only : bool option;
    locales : label_value_definition_strings list;
  }

  let parse_label_value_definition_strings json : label_value_definition_strings
      =
    let open Yojson.Safe.Util in
    {
      lang = (match json |> member "lang" with `String s -> s | _ -> "");
      name = (match json |> member "name" with `String s -> s | _ -> "");
      description =
        (match json |> member "description" with `String s -> s | _ -> "");
    }

  let parse_label_value_definition json : label_value_definition =
    let open Yojson.Safe.Util in
    {
      identifier =
        (match json |> member "identifier" with `String s -> s | _ -> "");
      severity =
        (match json |> member "severity" with `String s -> s | _ -> "");
      blurs = (match json |> member "blurs" with `String s -> s | _ -> "");
      default_setting =
        (match json |> member "defaultSetting" with
        | `String s -> Some s
        | _ -> None);
      adult_only =
        (match json |> member "adultOnly" with `Bool b -> Some b | _ -> None);
      locales =
        (match json |> member "locales" with
        | `List xs -> List.map parse_label_value_definition_strings xs
        | _ -> []);
    }

  let self_labels_to_json (vals : string list) : Yojson.Safe.t =
    `Assoc
      [
        ("$type", `String "com.atproto.label.defs#selfLabels");
        ( "values",
          `List (List.map (fun v -> `Assoc [ ("val", `String v) ]) vals) );
      ]

  let create_label_endpoint (query_name : string) : string =
    "com.atproto.label" ^ "." ^ query_name

  let query_labels_body ?(uri_patterns = []) ?sources ?limit ?cursor () :
      (string * string) list =
    let pairs =
      List.map (fun p -> ("uriPatterns", p)) uri_patterns
      @ (match sources with
        | Some srcs -> List.map (fun s -> ("sources", s)) srcs
        | None -> [])
      @ (match limit with
        | Some n -> [ ("limit", string_of_int n) ]
        | None -> [])
      @ match cursor with Some c -> [ ("cursor", c) ] | None -> []
    in
    pairs

  (* List of AT URI patterns to match (boolean 'OR'). Each may
   * be a prefix (ending with '*'; will match inclusive of the string leading to
   * '*'), or a full URI *)
  let query_labels (s : Session.session) (uri_patterns : string list) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let query_labels_url =
      App.create_endpoint_url base_url (create_label_endpoint "queryLabels")
    in
    let body = Cohttp_client.add_query_params "uriPatterns" uri_patterns in
    let labels =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers query_labels_url body
           headers)
    in
    labels

  let query_labels_parsed (s : Session.session) ~uri_patterns ?sources ?limit
      ?cursor () : query_labels =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let url =
      App.create_endpoint_url base_url (create_label_endpoint "queryLabels")
    in
    let body =
      Cohttp_client.create_body_from_pairs
        (query_labels_body ~uri_patterns ?sources ?limit ?cursor ())
    in
    let resp =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers url body headers)
    in
    parse_query_labels (Yojson.Safe.from_string resp)

  (* ---- signed labels (com.atproto.label.defs#label) -------------------- *)

  let encode_unsigned (l : label) : string =
    let fields =
      [
        ("src", Dag_cbor.Text l.src);
        ("uri", Dag_cbor.Text l.uri);
        ("val", Dag_cbor.Text l.val_);
        ("ver", Dag_cbor.Int (Option.value ~default:1 l.ver));
      ]
      @ (match l.cid with Some c -> [ ("cid", Dag_cbor.Text c) ] | None -> [])
      @ (match l.neg with
        | Some true -> [ ("neg", Dag_cbor.Bool true) ]
        | _ -> [])
      @ (match l.cts with Some t -> [ ("cts", Dag_cbor.Text t) ] | None -> [])
      @ match l.exp with Some t -> [ ("exp", Dag_cbor.Text t) ] | None -> []
    in
    Dag_cbor.encode (Dag_cbor.Map fields)

  let encode_signed (l : label) : string =
    match l.sig_ with
    | None -> encode_unsigned l
    | Some sig_ ->
        let unsigned = Dag_cbor.decode (encode_unsigned l) in
        let fields = Dag_cbor.get_map unsigned in
        Dag_cbor.encode
          (Dag_cbor.Map (fields @ [ ("sig", Dag_cbor.Bytes sig_) ]))

  type sig_status =
    [ `Valid | `Invalid | `Unsupported_curve of string | `Missing ]

  let sign_p256 ~(priv : Mirage_crypto_ec.P256.Dsa.priv) (l : label) : label =
    Lazy.force ensure_rng;
    let digest = Hash.sha256 (encode_unsigned l) in
    let r, s = Mirage_crypto_ec.P256.Dsa.sign ~key:priv digest in
    let s =
      if String.compare s Did_plc.Did_plc.p256_n_half > 0 then
        Did_plc.Did_plc.sub_be Did_plc.Did_plc.p256_n s
      else s
    in
    { l with ver = Some (Option.value ~default:1 l.ver); sig_ = Some (r ^ s) }

  let sign_k256 ~(priv : K256.K256.priv) (l : label) : label =
    let digest = Hash.sha256 (encode_unsigned l) in
    let r, s = K256.K256.sign ~key:priv digest in
    { l with ver = Some (Option.value ~default:1 l.ver); sig_ = Some (r ^ s) }

  let verify_with_keys ~(keys : string list) (l : label) : sig_status =
    match l.sig_ with
    | None -> `Missing
    | Some raw ->
        if String.length raw <> 64 then `Invalid
        else
          let r = String.sub raw 0 32 in
          let s = String.sub raw 32 32 in
          let digest = Hash.sha256 (encode_unsigned l) in
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
                match other with
                | Some c -> `Unsupported_curve c
                | None -> `Invalid)
            | k :: rest -> (
                match k.Did_key.curve with
                | Did_key.P256 -> (
                    match Did_key.p256_pub k with
                    | Some pub ->
                        if
                          Did_plc.Did_plc.is_low_s s
                          && Mirage_crypto_ec.P256.Dsa.verify ~key:pub (r, s)
                               digest
                        then `Valid
                        else try_keys rest
                    | None -> try_keys rest)
                | Did_key.K256 -> (
                    match Did_key.k256_pub k with
                    | Some pub ->
                        if
                          K256.K256.is_low_s s
                          && K256.K256.verify ~key:pub (r, s) digest
                        then `Valid
                        else try_keys rest
                    | None -> try_keys rest)
                | Did_key.Other _ -> try_keys rest)
          in
          try_keys parsed

  let json_of_label (l : label) : Yojson.Safe.t =
    let fields =
      [
        ("src", `String l.src); ("uri", `String l.uri); ("val", `String l.val_);
      ]
      @ (match l.ver with Some n -> [ ("ver", `Int n) ] | None -> [])
      @ (match l.cid with Some c -> [ ("cid", `String c) ] | None -> [])
      @ (match l.neg with Some b -> [ ("neg", `Bool b) ] | None -> [])
      @ (match l.cts with Some t -> [ ("cts", `String t) ] | None -> [])
      @ (match l.exp with Some t -> [ ("exp", `String t) ] | None -> [])
      @
      match l.sig_ with
      | Some b ->
          [ ("sig", `Assoc [ ("$bytes", `String (Base64url.encode_std b)) ]) ]
      | None -> []
    in
    `Assoc fields

  (* ---- subscribeLabels ------------------------------------------------- *)

  type header = { op : int; t : string option }
  type labels_msg = { seq : int64; labels : label list }
  type info = { name : string; message : string option }

  type message =
    [ `Labels of labels_msg
    | `Info of info
    | `Error of string * string option
    | `Unknown of string * Dag_cbor.value ]

  let host_uses_cleartext (host : string) : bool =
    let bare =
      match String.split_on_char ':' host with h :: _ -> h | [] -> host
    in
    let bare = String.lowercase_ascii bare in
    bare = "localhost" || bare = "127.0.0.1" || bare = "[::1]" || bare = "::1"

  let subscribe_url ?(host = "bsky.network") ?scheme ?cursor () =
    let scheme =
      match scheme with
      | Some s -> s
      | None -> if host_uses_cleartext host then "ws" else "wss"
    in
    let base =
      Printf.sprintf "%s://%s/xrpc/com.atproto.label.subscribeLabels" scheme host
    in
    match cursor with
    | None -> base
    | Some c -> base ^ "?cursor=" ^ Int64.to_string c

  let parse_header (v : Dag_cbor.value) : header =
    let fields = Dag_cbor.get_map v in
    {
      op = Dag_cbor.as_int (Dag_cbor.require "op" fields);
      t =
        (match Dag_cbor.find "t" fields with
        | Some (Dag_cbor.Text s) -> Some s
        | _ -> None);
    }

  let parse_labels_msg (v : Dag_cbor.value) : labels_msg =
    let fields = Dag_cbor.get_map v in
    {
      seq = Dag_cbor.as_int64 (Dag_cbor.require "seq" fields);
      labels =
        (match Dag_cbor.find "labels" fields with
        | Some a -> List.map parse_label_cbor (Dag_cbor.as_array a)
        | None -> []);
    }

  let decode_frame (bytes : string) : header * message =
    match Dag_cbor.decode_sequence bytes with
    | header_v :: body :: _ ->
        let header = parse_header header_v in
        let message =
          if header.op = -1 then
            let fields = Dag_cbor.get_map body in
            let err =
              match Dag_cbor.find "error" fields with
              | Some (Dag_cbor.Text s) -> s
              | _ -> "error"
            in
            let msg =
              match Dag_cbor.find "message" fields with
              | Some (Dag_cbor.Text s) -> Some s
              | _ -> None
            in
            `Error (err, msg)
          else
            match header.t with
            | Some "#labels" -> `Labels (parse_labels_msg body)
            | Some "#info" ->
                let fields = Dag_cbor.get_map body in
                `Info
                  {
                    name = Dag_cbor.as_text (Dag_cbor.require "name" fields);
                    message =
                      (match Dag_cbor.find "message" fields with
                      | Some (Dag_cbor.Text s) -> Some s
                      | _ -> None);
                  }
            | Some other -> `Unknown (other, body)
            | None -> `Unknown ("", body)
        in
        (header, message)
    | _ -> failwith "Label.decode_frame: expected header and body CBOR values"

  let encode_header (h : header) : string =
    let fields =
      ("op", Dag_cbor.Int h.op)
      :: (match h.t with Some t -> [ ("t", Dag_cbor.Text t) ] | None -> [])
    in
    Dag_cbor.encode (Dag_cbor.Map fields)

  let encode_labels_frame (m : labels_msg) : string =
    let header = encode_header { op = 1; t = Some "#labels" } in
    let body =
      Dag_cbor.encode
        (Dag_cbor.Map
           [
             ("seq", Dag_cbor.Int64 m.seq);
             ( "labels",
               Dag_cbor.Array
                 (List.map
                    (fun l -> Dag_cbor.decode (encode_signed l))
                    m.labels) );
           ])
    in
    header ^ body

  let subscribe ?(host = "bsky.network") ?cursor ?max_messages f =
    let url = subscribe_url ~host ?cursor () in
    Websocket.Websocket.with_connection url (fun ws ->
        let rec loop n =
          match max_messages with
          | Some m when n >= m -> ()
          | _ -> (
              match Websocket.Websocket.recv_message ws with
              | Websocket.Websocket.Binary payload
              | Websocket.Websocket.Text payload ->
                  f (decode_frame payload);
                  loop (n + 1)
              | Websocket.Websocket.Close _ -> ()
              | Websocket.Websocket.Ping _ | Websocket.Websocket.Pong _ ->
                  loop n)
        in
        loop 0)

  let subscribe_one ?host ?cursor () : header * message =
    let cell = ref None in
    subscribe ?host ?cursor ~max_messages:1 (fun frame -> cell := Some frame);
    match !cell with
    | Some frame -> frame
    | None -> failwith "Label.subscribe_one: no frame received"
end
