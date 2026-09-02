open Base64url
open Hash

let ensure_rng = lazy (Mirage_crypto_rng_unix.use_default ())

(** XRPC protocol headers and service-auth JWT claims.
    https://atproto.com/specs/xrpc
    https://atproto.com/specs/label#labeler-http-headers *)
module Xrpc = struct
  exception Invalid of string

  let fail msg = raise (Invalid msg)

  let ascii_lower s =
    String.map
      (function 'A' .. 'Z' as c -> Char.chr (Char.code c + 32) | c -> c)
      s

  let trim s =
    let n = String.length s in
    let rec left i =
      if i >= n then n
      else match s.[i] with ' ' | '\t' -> left (i + 1) | _ -> i
    in
    let rec right i =
      if i < 0 then 0
      else match s.[i] with ' ' | '\t' -> right (i - 1) | _ -> i + 1
    in
    let lo = left 0 in
    let hi = right (n - 1) in
    if hi <= lo then "" else String.sub s lo (hi - lo)

  let split_commas s =
    String.split_on_char ',' s |> List.map trim
    |> List.filter (fun p -> p <> "")

  (* ---- atproto-proxy --------------------------------------------------- *)

  type proxy = { did : string; service : string }

  let parse_proxy (value : string) : proxy =
    let v = trim value in
    match String.index_opt v '#' with
    | None -> fail "atproto-proxy must be did#service"
    | Some i ->
        let did = String.sub v 0 i in
        let service = String.sub v (i + 1) (String.length v - i - 1) in
        if not (Syntax.Syntax.is_valid_did did) then
          fail ("atproto-proxy DID is invalid: " ^ did);
        if service = "" then fail "atproto-proxy service id is empty";
        { did; service }

  let proxy_to_string (p : proxy) : string = p.did ^ "#" ^ p.service

  (** [atproto-proxy] header pair ([did#service]). *)
  let proxy_header (p : proxy) : string * string =
    ("atproto-proxy", proxy_to_string p)

  let labeler_proxy (did : string) : proxy =
    { did; service = "atproto_labeler" }

  let chat_proxy : proxy =
    { did = "did:web:api.bsky.chat"; service = "bsky_chat" }

  let appview_proxy : proxy =
    { did = "did:web:api.bsky.app"; service = "bsky_appview" }

  (* ---- atproto-accept-labelers / atproto-content-labelers -------------- *)

  type labeler = { did : string; redact : bool }

  let parse_labeler_item (item : string) : labeler =
    let parts = String.split_on_char ';' item |> List.map trim in
    match parts with
    | [] -> fail "empty labeler item"
    | did :: params ->
        if not (Syntax.Syntax.is_valid_did did) then
          fail ("labeler DID is invalid: " ^ did);
        let redact = List.exists (fun p -> ascii_lower p = "redact") params in
        { did; redact }

  let parse_labelers (value : string) : labeler list =
    let items = split_commas value in
    let parsed = List.map parse_labeler_item items in
    (* de-duplicate by DID, unioning redact flags (spec: combine parameters) *)
    let acc = Hashtbl.create 8 in
    let order = ref [] in
    List.iter
      (fun (l : labeler) ->
        match Hashtbl.find_opt acc l.did with
        | None ->
            Hashtbl.add acc l.did l.redact;
            order := l.did :: !order
        | Some prev -> Hashtbl.replace acc l.did (prev || l.redact))
      parsed;
    List.rev_map (fun did -> { did; redact = Hashtbl.find acc did }) !order

  let labelers_to_string (ls : labeler list) : string =
    String.concat ", "
      (List.map
         (fun (l : labeler) -> if l.redact then l.did ^ ";redact" else l.did)
         ls)

  (** [atproto-accept-labelers] header from [ls] ([did] or [did;redact]). *)
  let accept_labelers_header (ls : labeler list) : string * string =
    ("atproto-accept-labelers", labelers_to_string ls)

  (** [atproto-content-labelers] header from [ls]. *)
  let content_labelers_header (ls : labeler list) : string * string =
    ("atproto-content-labelers", labelers_to_string ls)

  (* ---- Rate-limit and repo-rev headers -------------------------------- *)

  type rate_limit = {
    limit : int option;
    remaining : int option;
    reset : int64 option;
    policy : string option;
  }

  let header_value headers name =
    let lower = ascii_lower name in
    List.find_map
      (fun (k, v) -> if ascii_lower k = lower then Some (trim v) else None)
      headers

  let int_opt s = try Some (int_of_string s) with _ -> None
  let int64_opt s = try Some (Int64.of_string s) with _ -> None

  let parse_rate_limit (headers : (string * string) list) : rate_limit =
    {
      limit =
        (match header_value headers "RateLimit-Limit" with
        | Some s -> int_opt s
        | None -> None);
      remaining =
        (match header_value headers "RateLimit-Remaining" with
        | Some s -> int_opt s
        | None -> None);
      reset =
        (match header_value headers "RateLimit-Reset" with
        | Some s -> int64_opt s
        | None -> None);
      policy = header_value headers "RateLimit-Policy";
    }

  let repo_rev_header (rev : string) : string * string =
    ("atproto-repo-rev", rev)

  (* ---- Service-auth JWT (com.atproto.server.getServiceAuth) ----------- *)
  (* https://atproto.com/specs/xrpc#inter-service-authentication-jwt *)

  type service_auth = {
    alg : string;
    typ : string;
    kid : string;
    iss : string;
    aud : string;
    aud_did : string;
    aud_service : string option;
    exp : int64 option;
    iat : int64 option;
    lxm : string option;
    jti : string option;
    raw : string;
  }

  let default_kid = "#atproto"
  let default_typ = "JWT"
  let recommended_lifetime = 60L

  let split_jwt jwt =
    match String.split_on_char '.' jwt with
    | [ h; p; s ] -> (h, p, s)
    | _ -> fail "service-auth JWT must have three base64url parts"

  let json_int64 json field =
    match Yojson.Safe.Util.member field json with
    | `Int n -> Some (Int64.of_int n)
    | `Intlit s -> Some (Int64.of_string s)
    | _ -> None

  let json_string json field =
    match Yojson.Safe.Util.member field json with
    | `String s -> Some s
    | _ -> None

  let normalize_kid kid =
    if kid = "" then default_kid else if kid.[0] = '#' then kid else "#" ^ kid

  let parse_service_auth (jwt : string) : service_auth =
    let header_b64, payload_b64, _ = split_jwt jwt in
    let header = Yojson.Safe.from_string (Base64url.decode header_b64) in
    let json = Yojson.Safe.from_string (Base64url.decode payload_b64) in
    let iss =
      match json_string json "iss" with
      | Some s -> s
      | None -> fail "service-auth JWT missing iss"
    in
    let aud =
      match json_string json "aud" with
      | Some s -> s
      | None -> fail "service-auth JWT missing aud"
    in
    if not (Syntax.Syntax.is_valid_did iss) then
      fail "service-auth iss must be a DID";
    if not (Syntax.Syntax.is_valid_did_ref aud) then
      fail "service-auth aud must be a DID (optional #service fragment)";
    let aud_ref = Syntax.Syntax.parse_did_ref aud in
    let lxm =
      match json_string json "lxm" with
      | Some s ->
          if not (Syntax.Syntax.is_valid_nsid s) then
            fail "service-auth lxm must be an NSID";
          Some s
      | None -> None
    in
    {
      alg = Option.value ~default:"" (json_string header "alg");
      typ = Option.value ~default:default_typ (json_string header "typ");
      kid =
        normalize_kid
          (Option.value ~default:default_kid (json_string header "kid"));
      iss;
      aud;
      aud_did = aud_ref.did;
      aud_service = aud_ref.fragment;
      exp = json_int64 json "exp";
      iat = json_int64 json "iat";
      lxm;
      jti = json_string json "jti";
      raw = jwt;
    }

  let service_auth_body ~aud ?lxm ?exp () : Yojson.Safe.t =
    if not (Syntax.Syntax.is_valid_did_ref aud) then
      fail "getServiceAuth aud must be a DID (optional #service fragment)";
    (match lxm with
    | Some n ->
        if not (Syntax.Syntax.is_valid_nsid n) then
          fail "getServiceAuth lxm must be an NSID"
    | None -> ());
    let fields =
      [ ("aud", `String aud) ]
      @ (match lxm with Some n -> [ ("lxm", `String n) ] | None -> [])
      @
      match exp with
      | Some n -> [ ("exp", `Intlit (Int64.to_string n)) ]
      | None -> []
    in
    `Assoc fields

  let service_auth_header (jwt : string) : string * string =
    ("Authorization", "Bearer " ^ jwt)

  let random_jti () : string =
    Lazy.force ensure_rng;
    let buf = Bytes.create 16 in
    for i = 0 to 15 do
      Bytes.set buf i (Char.chr (Random.int 256))
    done;
    Hash.hex_encode (Bytes.to_string buf)

  let b64url_json json = Base64url.encode (Yojson.Safe.to_string json)

  let unsigned_jwt ~alg ~typ ~kid ~iss ~aud ~exp ~iat ?lxm ~jti () : string =
    let header =
      `Assoc
        [ ("alg", `String alg); ("typ", `String typ); ("kid", `String kid) ]
    in
    let payload =
      `Assoc
        ([
           ("iss", `String iss);
           ("aud", `String aud);
           ("exp", `Intlit (Int64.to_string exp));
           ("iat", `Intlit (Int64.to_string iat));
           ("jti", `String jti);
         ]
        @ match lxm with Some n -> [ ("lxm", `String n) ] | None -> [])
    in
    b64url_json header ^ "." ^ b64url_json payload

  let finish_jwt unsigned signature =
    unsigned ^ "." ^ Base64url.encode signature

  let sign_service_jwt_p256 ~(priv : Mirage_crypto_ec.P256.Dsa.priv) ~iss ~aud
      ?lxm ?exp ?iat ?jti ?(kid = default_kid) ?(now = Unix.gettimeofday ()) ()
      : string =
    if not (Syntax.Syntax.is_valid_did iss) then
      fail "service-auth iss must be a DID";
    if not (Syntax.Syntax.is_valid_did_ref aud) then
      fail "service-auth aud must be a DID (optional #service fragment)";
    (match lxm with
    | Some n ->
        if not (Syntax.Syntax.is_valid_nsid n) then
          fail "service-auth lxm must be an NSID"
    | None -> ());
    Lazy.force ensure_rng;
    let iat = Option.value iat ~default:(Int64.of_float now) in
    let exp = Option.value exp ~default:(Int64.add iat recommended_lifetime) in
    let jti = Option.value jti ~default:(random_jti ()) in
    let unsigned =
      unsigned_jwt ~alg:"ES256" ~typ:default_typ ~kid:(normalize_kid kid) ~iss
        ~aud ~exp ~iat ?lxm ~jti ()
    in
    let digest = Hash.sha256 unsigned in
    let r, s = Mirage_crypto_ec.P256.Dsa.sign ~key:priv digest in
    let s =
      if String.compare s Did_plc.Did_plc.p256_n_half > 0 then
        Did_plc.Did_plc.sub_be Did_plc.Did_plc.p256_n s
      else s
    in
    finish_jwt unsigned (r ^ s)

  let sign_service_jwt_k256 ~(priv : K256.K256.priv) ~iss ~aud ?lxm ?exp ?iat
      ?jti ?(kid = default_kid) ?(now = Unix.gettimeofday ()) () : string =
    if not (Syntax.Syntax.is_valid_did iss) then
      fail "service-auth iss must be a DID";
    if not (Syntax.Syntax.is_valid_did_ref aud) then
      fail "service-auth aud must be a DID (optional #service fragment)";
    (match lxm with
    | Some n ->
        if not (Syntax.Syntax.is_valid_nsid n) then
          fail "service-auth lxm must be an NSID"
    | None -> ());
    let iat = Option.value iat ~default:(Int64.of_float now) in
    let exp = Option.value exp ~default:(Int64.add iat recommended_lifetime) in
    let jti = Option.value jti ~default:(random_jti ()) in
    let unsigned =
      unsigned_jwt ~alg:"ES256K" ~typ:default_typ ~kid:(normalize_kid kid) ~iss
        ~aud ~exp ~iat ?lxm ~jti ()
    in
    let digest = Hash.sha256 unsigned in
    let r, s = K256.K256.sign ~key:priv digest in
    finish_jwt unsigned (r ^ s)

  type sig_status =
    [ `Valid | `Invalid | `Unsupported_curve of string | `Missing ]

  let verify_service_sig ~(keys : string list) (jwt : string) : sig_status =
    let header_b64, payload_b64, sig_b64 = split_jwt jwt in
    if sig_b64 = "" || sig_b64 = "sig" then `Missing
    else
      let raw =
        try Base64url.decode sig_b64
        with _ -> fail "service-auth signature is not base64url"
      in
      if String.length raw <> 64 then `Invalid
      else
        let r = String.sub raw 0 32 in
        let s = String.sub raw 32 32 in
        let digest = Hash.sha256 (header_b64 ^ "." ^ payload_b64) in
        let parsed =
          List.filter_map
            (fun k -> try Some (Did_key.Did_key.of_string k) with _ -> None)
            keys
        in
        let rec try_keys = function
          | [] -> (
              let other =
                List.find_map
                  (fun k ->
                    match k.Did_key.Did_key.curve with
                    | Did_key.Did_key.Other n -> Some (Printf.sprintf "0x%x" n)
                    | _ -> None)
                  parsed
              in
              match other with
              | Some c -> `Unsupported_curve c
              | None -> `Invalid)
          | k :: rest -> (
              match k.Did_key.Did_key.curve with
              | Did_key.Did_key.P256 -> (
                  match Did_key.Did_key.p256_pub k with
                  | Some pub ->
                      if
                        Did_plc.Did_plc.is_low_s s
                        && Mirage_crypto_ec.P256.Dsa.verify ~key:pub (r, s)
                             digest
                      then `Valid
                      else try_keys rest
                  | None -> try_keys rest)
              | Did_key.Did_key.K256 -> (
                  match Did_key.Did_key.k256_pub k with
                  | Some pub ->
                      if
                        K256.K256.is_low_s s
                        && K256.K256.verify ~key:pub (r, s) digest
                      then `Valid
                      else try_keys rest
                  | None -> try_keys rest)
              | Did_key.Did_key.Other _ -> try_keys rest)
        in
        try_keys parsed

  let audience_matches ~(expected : string) (claims : service_auth) : bool =
    if claims.aud = expected then true
    else
      let want = Syntax.Syntax.parse_did_ref expected in
      claims.aud_did = want.did
      &&
      match want.fragment with
      | None ->
          (* expected bare DID: accept legacy bare aud only, not any service *)
          claims.aud_service = None
      | Some frag -> claims.aud_service = Some frag

  let is_expired ?(now = Unix.gettimeofday ()) ?(leeway = 5L)
      (claims : service_auth) : bool =
    match claims.exp with
    | None -> true
    | Some exp ->
        let now_i = Int64.of_float now in
        Int64.compare now_i (Int64.add exp leeway) > 0

  let verify_service_jwt ~keys ?aud ?lxm ?now ?(require_lxm = false)
      ?(require_jti = false) (jwt : string) : service_auth =
    let claims = parse_service_auth jwt in
    if claims.typ <> default_typ && ascii_lower claims.typ <> "jwt" then
      fail ("service-auth typ is not JWT: " ^ claims.typ);
    (match verify_service_sig ~keys jwt with
    | `Valid -> ()
    | `Missing -> fail "service-auth JWT missing signature"
    | `Invalid -> fail "service-auth JWT signature is invalid"
    | `Unsupported_curve c ->
        fail ("service-auth JWT uses unsupported curve " ^ c));
    (match aud with
    | Some expected ->
        if not (audience_matches ~expected claims) then
          fail
            (Printf.sprintf "service-auth aud %s does not match %s" claims.aud
               expected)
    | None -> ());
    (match (lxm, claims.lxm) with
    | Some expected, Some got when got = expected -> ()
    | Some expected, Some got ->
        fail
          (Printf.sprintf "service-auth lxm %s does not match %s" got expected)
    | Some expected, None ->
        fail ("service-auth JWT missing lxm (expected " ^ expected ^ ")")
    | None, None when require_lxm -> fail "service-auth JWT missing lxm"
    | None, _ -> ());
    if require_jti && claims.jti = None then fail "service-auth JWT missing jti";
    if is_expired ?now claims then fail "service-auth JWT is expired";
    claims

  type jti_cache = {
    seen : (string, int64) Hashtbl.t;
    order : string Queue.t;
    cap : int;
  }

  let create_jti_cache ?(cap = 4096) () : jti_cache =
    { seen = Hashtbl.create cap; order = Queue.create (); cap }

  let remember_jti ?(now = Unix.gettimeofday ()) (c : jti_cache)
      (claims : service_auth) : bool =
    match claims.jti with
    | None -> false
    | Some jti ->
        let exp = Option.value claims.exp ~default:(Int64.of_float now) in
        if Hashtbl.mem c.seen jti then true
        else (
          Hashtbl.add c.seen jti exp;
          Queue.add jti c.order;
          (if Queue.length c.order > c.cap then
             let old = Queue.take c.order in
             Hashtbl.remove c.seen old);
          false)

  let jti_seen (c : jti_cache) (jti : string) : bool = Hashtbl.mem c.seen jti
end
