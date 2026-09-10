open Hash
open Did_plc
open K256
open Syntax
open At_uri
open Base64url
open Xrpc
open Oauth

let ensure_rng = lazy (Mirage_crypto_rng_unix.use_default ())

(** Experimental space-credential materials from AT Protocol proposal
    0016 (permissioned data / spaces).

    Tracks
    {{:https://github.com/bluesky-social/proposals/blob/main/0016-permissioned-data/README.md}0016
    § Access control} — delegation token, optional client attestation,
    space credential, DPoP binding, and the credential-exchange flow.
    Sign/verify reuse the inter-service JWT construction
    ([Xrpc] ES256 / ES256K, IEEE P1363, low-S). DPoP proofs reuse
    [Oauth] (RFC 9449 [dpop+jwt], RFC 7638 [jkt], [ath] =
    base64url([sha256]) of the credential). DPoP server nonces are not
    used.

    The proposal is not final; this module may change and is {e not} a
    stable spaces product API. XRPC wrappers live in [Space]. This
    repo does not start or stub a space host. *)
module Space_credential : sig
  val delegation_typ : string
  (** JWT [typ] for a delegation token
      (["atproto-space-delegation+jwt"]). *)

  val credential_typ : string
  (** JWT [typ] for a space credential
      (["atproto-space-credential+jwt"]). *)

  val attestation_typ : string
  (** JWT [typ] for a client attestation
      (["atproto-client-attestation+jwt"]). *)

  val delegation_kid : string
  (** Delegation [kid] (["#atproto"]; required). *)

  val credential_kid : string
  (** Default credential [kid] (["#atproto"]). An authority with a
      dedicated space key uses [space_key_kid] instead. *)

  val space_key_kid : string
  (** Dedicated space signing-key id (["#atproto_space"]). *)

  val space_host_fragment : string
  (** Service fragment on a space-host audience
      (["atproto_space_host"]). *)

  val delegation_lifetime : int64
  (** Default delegation lifetime (60 seconds). *)

  val credential_lifetime : int64
  (** Default space-credential lifetime (7200 seconds / 2 hours). *)

  val attestation_lifetime : int64
  (** Default client-attestation lifetime (60 seconds). *)

  val clock_skew_sec : int64
  (** Expiry / DPoP [iat] leeway (5 seconds). *)

  val max_dpop_age_sec : int64
  (** Maximum DPoP proof age (60 seconds), plus [clock_skew_sec]. *)

  val get_delegation_token_nsid : string
  val get_space_credential_nsid : string

  type kind = [ `Delegation | `Credential | `Client_attestation ]
  type header = { alg : string; typ : string; kid : string option }

  type payload = {
    iss : string;
    sub : string;
    aud : string option;
    iat : int64;
    exp : int64;
    jti : string option;
    cnf_jkt : string option;
  }

  type t = { kind : kind; header : header; payload : payload; raw : string }
  type signer = [ `P256 of Mirage_crypto_ec.P256.Dsa.priv | `K256 of K256.priv ]

  type sig_status =
    [ `Valid | `Invalid | `Unsupported_curve of string | `Missing ]

  exception Invalid of string

  val typ_of_kind : kind -> string
  val kind_of_typ : string -> kind

  val space_host_aud : string -> string
  (** [did#atproto_space_host] audience for a space authority. *)

  val is_expired : ?now:float -> ?leeway:int64 -> payload -> bool
  (** True when [now] is at or past [exp] minus [leeway] (default
      [clock_skew_sec]). Matches official [CLOCK_SKEW_SEC = 5]. *)

  val parse : kind:kind -> string -> t
  (** Structural parse for [kind]. Checks [typ], required claims, and
      space-URI / client_id shape. Does not verify the signature. *)

  val parse_delegation : string -> t
  val parse_credential : string -> t
  val parse_attestation : string -> t

  val verify_sig : keys:string list -> string -> sig_status
  (** Verify the JWT signature against [did:key] public keys (same
      construction as [Xrpc.verify_service_sig]). *)

  val verify :
    kind:kind ->
    keys:string list ->
    ?aud:string ->
    ?sub:string ->
    ?now:float ->
    string ->
    t
  (** Parse, check expiry / optional [aud] / [sub], and verify the
      signature. *)

  val verify_delegation :
    keys:string list -> ?aud:string -> ?sub:string -> ?now:float -> string -> t

  val verify_credential :
    keys:string list -> ?sub:string -> ?now:float -> string -> t

  val verify_attestation :
    keys:string list -> ?aud:string -> ?now:float -> string -> t

  val sign_delegation :
    sign:signer ->
    iss:string ->
    sub:string ->
    aud:string ->
    ?exp:int64 ->
    ?iat:int64 ->
    ?jti:string ->
    ?now:float ->
    unit ->
    string
  (** Mint a delegation token. [iss] is the user DID, [sub] is the
      space URI (through [skey]), [aud] is the space host
      ([space_host_aud]). [kid] is always ["#atproto"]. Default
      lifetime 60s. No [lxm]. *)

  val sign_credential :
    sign:signer ->
    iss:string ->
    sub:string ->
    dpop_jkt:string ->
    ?kid:string ->
    ?exp:int64 ->
    ?iat:int64 ->
    ?jti:string ->
    ?now:float ->
    unit ->
    string
  (** Mint a space credential. [iss] is the space-authority DID, [sub]
      is the space URI, [dpop_jkt] is the RFC 7638 thumbprint of the
      application's DPoP key ([cnf.jkt]). No [aud]. Default lifetime
      2h. *)

  val sign_attestation :
    sign:signer ->
    client_id:string ->
    aud:string ->
    ?kid:string ->
    ?exp:int64 ->
    ?iat:int64 ->
    ?jti:string ->
    ?now:float ->
    unit ->
    string
  (** Mint a client-attestation JWT. [iss] and [sub] are both
      [client_id]. [aud] is the space host. Offline shape only — no
      JWKS fetch. *)

  val bearer_header : string -> string * string
  (** [Authorization: Bearer] for a delegation token (credential
      exchange). *)

  val dpop_authorization : string -> string * string
  (** [Authorization: DPoP] for a space credential (resource
      request). *)

  val get_delegation_token_body : space:string -> (string * string) list
  (** Query pairs for [com.atproto.space.getDelegationToken]. *)

  val get_space_credential_body :
    space:string -> ?client_attestation:string -> unit -> Yojson.Safe.t
  (** JSON body for [com.atproto.space.getSpaceCredential]
      (procedure / POST). *)

  val get_space_credential_htu : origin:string -> string
  (** RFC 9449 [htu] for the credential-exchange POST (no query /
      fragment). *)

  val exchange_dpop_proof :
    priv:Mirage_crypto_ec.P256.Dsa.priv ->
    pub:Mirage_crypto_ec.P256.Dsa.pub ->
    ?jti:string ->
    ?iat:int64 ->
    htu:string ->
    unit ->
    string
  (** DPoP proof for [getSpaceCredential]. [htm] is [POST]. Must not
      include [ath] (the delegation token is a single-use grant, not
      an access token). *)

  val resource_dpop_proof :
    priv:Mirage_crypto_ec.P256.Dsa.priv ->
    pub:Mirage_crypto_ec.P256.Dsa.pub ->
    htm:string ->
    htu:string ->
    credential:string ->
    ?jti:string ->
    ?iat:int64 ->
    unit ->
    string
  (** DPoP proof for a space-credential resource request. Binds [ath]
      to [credential] (RFC 9449). *)

  val exchange_headers :
    delegation:string -> dpop:string -> (string * string) list
  (** [Authorization: Bearer] + [DPoP] for credential exchange. *)

  val resource_headers :
    credential:string -> dpop:string -> (string * string) list
  (** [Authorization: DPoP] + [DPoP] for a repo-host request. *)

  type dpop_check = {
    jti : string;
    jkt : string;
    htm : string;
    htu : string;
    iat : int64;
    ath : string option;
  }

  val dpop_jkt : Mirage_crypto_ec.P256.Dsa.pub -> string
  (** RFC 7638 JWK thumbprint of a DPoP P-256 key ([Oauth.dpop_jkt]). *)

  val ath_of_credential : string -> string
  (** [base64url(sha256(credential))] ([Oauth.ath_of_access_token]). *)

  val verify_exchange_dpop :
    ?now:float -> htm:string -> htu:string -> string -> dpop_check
  (** Verify a credential-exchange DPoP proof (embedded JWK, no
      [ath]). Returns the bound [jkt] for [cnf.jkt]. *)

  val verify_resource_dpop :
    ?now:float ->
    htm:string ->
    htu:string ->
    credential:string ->
    jkt:string ->
    string ->
    dpop_check
  (** Verify a resource DPoP proof: signature, [ath], [htm]/[htu],
      recent [iat], and [jkt] = credential [cnf.jkt]. *)

  val generate_dpop_pair :
    unit -> Mirage_crypto_ec.P256.Dsa.priv * Mirage_crypto_ec.P256.Dsa.pub
  (** Fresh P-256 key pair. Proposal: generate one per space
      credential. *)
end = struct
  let delegation_typ = "atproto-space-delegation+jwt"
  let credential_typ = "atproto-space-credential+jwt"
  let attestation_typ = "atproto-client-attestation+jwt"
  let delegation_kid = "#atproto"
  let credential_kid = "#atproto"
  let space_key_kid = "#atproto_space"
  let space_host_fragment = "atproto_space_host"
  let delegation_lifetime = 60L
  let credential_lifetime = 7200L
  let attestation_lifetime = 60L
  let clock_skew_sec = 5L
  let max_dpop_age_sec = 60L
  let get_delegation_token_nsid = "com.atproto.space.getDelegationToken"
  let get_space_credential_nsid = "com.atproto.space.getSpaceCredential"

  type kind = [ `Delegation | `Credential | `Client_attestation ]
  type header = { alg : string; typ : string; kid : string option }

  type payload = {
    iss : string;
    sub : string;
    aud : string option;
    iat : int64;
    exp : int64;
    jti : string option;
    cnf_jkt : string option;
  }

  type t = { kind : kind; header : header; payload : payload; raw : string }
  type signer = [ `P256 of Mirage_crypto_ec.P256.Dsa.priv | `K256 of K256.priv ]

  type sig_status =
    [ `Valid | `Invalid | `Unsupported_curve of string | `Missing ]

  exception Invalid of string

  let fail msg = raise (Invalid msg)

  let typ_of_kind = function
    | `Delegation -> delegation_typ
    | `Credential -> credential_typ
    | `Client_attestation -> attestation_typ

  let kind_of_typ = function
    | t when t = delegation_typ -> `Delegation
    | t when t = credential_typ -> `Credential
    | t when t = attestation_typ -> `Client_attestation
    | other -> fail ("unknown space token typ " ^ other)

  let space_host_aud space_did =
    if not (Syntax.is_valid_did space_did) then
      fail ("space host aud requires a DID, got " ^ space_did);
    space_did ^ "#" ^ space_host_fragment

  let json_string json field =
    match Yojson.Safe.Util.member field json with
    | `String s -> Some s
    | _ -> None

  let json_int64 json field =
    match Yojson.Safe.Util.member field json with
    | `Int n -> Some (Int64.of_int n)
    | `Intlit s -> Some (Int64.of_string s)
    | _ -> None

  let require_string json field =
    match json_string json field with
    | Some s when s <> "" -> s
    | Some _ -> fail ("empty token \"" ^ field ^ "\"")
    | None -> fail ("missing token \"" ^ field ^ "\"")

  let require_int64 json field =
    match json_int64 json field with
    | Some n -> n
    | None -> fail ("missing token \"" ^ field ^ "\"")

  let ensure_space_uri label uri =
    match Space.of_string uri with
    | Space.Space _ -> ()
    | Space.Record _ ->
        fail (label ^ " must be a space URI (through skey), not a record URI")
    | exception Space.Invalid msg -> fail (label ^ ": " ^ msg)

  let ensure_user_did label did =
    if not (Syntax.is_valid_did did) then fail (label ^ " must be a DID")

  let ensure_aud aud =
    if not (Syntax.is_valid_did_ref aud) then
      fail "token aud must be a DID (optional #service fragment)"

  let ensure_client_id label id =
    if id = "" then fail (label ^ " must be a non-empty client_id")

  let is_expired ?(now = Unix.gettimeofday ()) ?(leeway = clock_skew_sec)
      (p : payload) : bool =
    let now_i = Int64.of_float now in
    Int64.compare now_i (Int64.add p.exp leeway) >= 0

  let decode_json_part label b64 =
    try Yojson.Safe.from_string (Base64url.decode b64)
    with _ -> fail ("could not parse token " ^ label)

  let parse ~kind (jwt : string) : t =
    let header_b64, payload_b64, _ =
      try Oauth.split_jwt jwt with Failure msg -> fail msg
    in
    let header_json = decode_json_part "header" header_b64 in
    let payload_json = decode_json_part "payload" payload_b64 in
    let typ =
      match json_string header_json "typ" with
      | Some t -> t
      | None -> fail "missing token \"typ\""
    in
    let expected = typ_of_kind kind in
    if typ <> expected then
      fail
        (Printf.sprintf "wrong token type: expected \"%s\", got \"%s\"" expected
           typ);
    let alg =
      match json_string header_json "alg" with
      | Some a when a <> "" -> a
      | _ -> fail "missing token \"alg\""
    in
    let kid = json_string header_json "kid" in
    let iss = require_string payload_json "iss" in
    let sub = require_string payload_json "sub" in
    let aud = json_string payload_json "aud" in
    let iat = require_int64 payload_json "iat" in
    let exp = require_int64 payload_json "exp" in
    let jti = json_string payload_json "jti" in
    let cnf_jkt =
      match Yojson.Safe.Util.member "cnf" payload_json with
      | `Assoc _ as cnf -> json_string cnf "jkt"
      | _ -> None
    in
    (match kind with
    | `Delegation -> (
        ensure_user_did "delegation iss" iss;
        ensure_space_uri "delegation sub" sub;
        (match aud with
        | None -> fail "missing token \"aud\""
        | Some a -> ensure_aud a);
        (match kid with
        | Some k when k = delegation_kid -> ()
        | Some k ->
            fail ("delegation kid must be " ^ delegation_kid ^ ", got " ^ k)
        | None -> fail "delegation token requires kid \"#atproto\"");
        match jti with
        | Some j when j <> "" -> ()
        | _ -> fail "a delegation token requires a \"jti\" to be consumed by")
    | `Credential -> (
        ensure_user_did "credential iss" iss;
        ensure_space_uri "credential sub" sub;
        match cnf_jkt with
        | Some j when j <> "" -> ()
        | _ -> fail "missing token \"cnf.jkt\"")
    | `Client_attestation -> (
        ensure_client_id "attestation iss" iss;
        ensure_client_id "attestation sub" sub;
        if iss <> sub then
          fail
            "client attestation \"iss\" and \"sub\" must both be the client_id";
        (match aud with
        | None -> fail "missing token \"aud\""
        | Some a -> ensure_aud a);
        match jti with
        | Some j when j <> "" -> ()
        | _ ->
            fail
              "a client attestation token requires a \"jti\" to be consumed by"));
    {
      kind;
      header = { alg; typ; kid };
      payload = { iss; sub; aud; iat; exp; jti; cnf_jkt };
      raw = jwt;
    }

  let parse_delegation jwt = parse ~kind:`Delegation jwt
  let parse_credential jwt = parse ~kind:`Credential jwt
  let parse_attestation jwt = parse ~kind:`Client_attestation jwt

  let verify_sig ~keys jwt : sig_status =
    try Xrpc.verify_service_sig ~keys jwt with Xrpc.Invalid msg -> fail msg

  let verify ~kind ~keys ?aud ?sub ?now jwt : t =
    let token = parse ~kind jwt in
    if is_expired ?now token.payload then fail "token expired";
    (match aud with
    | Some expected ->
        if token.payload.aud <> Some expected then
          fail "token audience does not match this service"
    | None -> ());
    (match sub with
    | Some expected ->
        if token.payload.sub <> expected then
          fail "token subject does not match the requested space"
    | None -> ());
    match verify_sig ~keys jwt with
    | `Valid -> token
    | `Missing -> fail "invalid token signature"
    | `Invalid -> fail "invalid token signature"
    | `Unsupported_curve c -> fail ("token uses unsupported curve " ^ c)

  let verify_delegation ~keys ?aud ?sub ?now jwt =
    verify ~kind:`Delegation ~keys ?aud ?sub ?now jwt

  let verify_credential ~keys ?sub ?now jwt =
    verify ~kind:`Credential ~keys ?sub ?now jwt

  let verify_attestation ~keys ?aud ?now jwt =
    verify ~kind:`Client_attestation ~keys ?aud ?now jwt

  let b64url_json json = Base64url.encode (Yojson.Safe.to_string json)

  let unsigned_jwt ~alg ~typ ?kid ~iss ~sub ?aud ?cnf_jkt ~exp ~iat ~jti () :
      string =
    let header_fields =
      [ ("alg", `String alg); ("typ", `String typ) ]
      @ match kid with Some k -> [ ("kid", `String k) ] | None -> []
    in
    let payload_fields =
      [ ("iss", `String iss); ("sub", `String sub) ]
      @ (match aud with Some a -> [ ("aud", `String a) ] | None -> [])
      @ (match cnf_jkt with
        | Some jkt -> [ ("cnf", `Assoc [ ("jkt", `String jkt) ]) ]
        | None -> [])
      @ [
          ("iat", `Intlit (Int64.to_string iat));
          ("exp", `Intlit (Int64.to_string exp));
          ("jti", `String jti);
        ]
    in
    b64url_json (`Assoc header_fields)
    ^ "."
    ^ b64url_json (`Assoc payload_fields)

  let finish_jwt unsigned signature =
    unsigned ^ "." ^ Base64url.encode signature

  let sign_bytes ~(sign : signer) (unsigned : string) : string =
    let digest = Hash.sha256 unsigned in
    match sign with
    | `P256 priv ->
        Lazy.force ensure_rng;
        let r, s = Mirage_crypto_ec.P256.Dsa.sign ~key:priv digest in
        let s = Did_plc.low_s s in
        r ^ s
    | `K256 priv ->
        let r, s = K256.sign ~key:priv digest in
        r ^ s

  let alg_of_signer = function `P256 _ -> "ES256" | `K256 _ -> "ES256K"

  let mint ~kind ~sign ~iss ~sub ?aud ?cnf_jkt ?kid ?exp ?iat ?jti
      ?(now = Unix.gettimeofday ()) () : string =
    let lifetime =
      match kind with
      | `Delegation -> delegation_lifetime
      | `Credential -> credential_lifetime
      | `Client_attestation -> attestation_lifetime
    in
    let iat = Option.value iat ~default:(Int64.of_float now) in
    let exp = Option.value exp ~default:(Int64.add iat lifetime) in
    let jti = Option.value jti ~default:(Xrpc.random_jti ()) in
    let unsigned =
      unsigned_jwt ~alg:(alg_of_signer sign) ~typ:(typ_of_kind kind) ?kid ~iss
        ~sub ?aud ?cnf_jkt ~exp ~iat ~jti ()
    in
    finish_jwt unsigned (sign_bytes ~sign unsigned)

  let sign_delegation ~sign ~iss ~sub ~aud ?exp ?iat ?jti ?now () : string =
    ensure_user_did "delegation iss" iss;
    ensure_space_uri "delegation sub" sub;
    ensure_aud aud;
    mint ~kind:`Delegation ~sign ~iss ~sub ~aud ~kid:delegation_kid ?exp ?iat
      ?jti ?now ()

  let sign_credential ~sign ~iss ~sub ~dpop_jkt ?kid ?exp ?iat ?jti ?now () :
      string =
    ensure_user_did "credential iss" iss;
    ensure_space_uri "credential sub" sub;
    if dpop_jkt = "" then fail "a credential token requires a \"dpopJkt\"";
    let kid = Option.value kid ~default:credential_kid in
    mint ~kind:`Credential ~sign ~iss ~sub ~cnf_jkt:dpop_jkt ~kid ?exp ?iat ?jti
      ?now ()

  let sign_attestation ~sign ~client_id ~aud ?kid ?exp ?iat ?jti ?now () :
      string =
    ensure_client_id "attestation client_id" client_id;
    ensure_aud aud;
    mint ~kind:`Client_attestation ~sign ~iss:client_id ~sub:client_id ~aud ?kid
      ?exp ?iat ?jti ?now ()

  let bearer_header jwt = Xrpc.service_auth_header jwt
  let dpop_authorization jwt = Oauth.authorization_dpop jwt

  let get_delegation_token_body ~space =
    ensure_space_uri "getDelegationToken space" space;
    [ ("space", space) ]

  let get_space_credential_body ~space ?client_attestation () =
    ensure_space_uri "getSpaceCredential space" space;
    let fields =
      [ ("space", `String space) ]
      @
      match client_attestation with
      | Some t -> [ ("clientAttestation", `String t) ]
      | None -> []
    in
    `Assoc fields

  let get_space_credential_htu ~origin =
    Oauth.htu_of_url
      (Oauth.url_on origin ("/xrpc/" ^ get_space_credential_nsid))

  let exchange_dpop_proof ~priv ~pub ?jti ?iat ~htu () : string =
    Oauth.dpop_proof ~priv ~pub ~htm:"POST" ~htu:(Oauth.htu_of_url htu) ?jti
      ?iat ()

  let resource_dpop_proof ~priv ~pub ~htm ~htu ~credential ?jti ?iat () : string
      =
    let ath = Oauth.ath_of_access_token credential in
    Oauth.dpop_proof ~priv ~pub ~htm ~htu:(Oauth.htu_of_url htu) ~ath ?jti ?iat
      ()

  let exchange_headers ~delegation ~dpop =
    [ bearer_header delegation; Oauth.dpop_header dpop ]

  let resource_headers ~credential ~dpop =
    [ dpop_authorization credential; Oauth.dpop_header dpop ]

  type dpop_check = {
    jti : string;
    jkt : string;
    htm : string;
    htu : string;
    iat : int64;
    ath : string option;
  }

  let dpop_jkt pub = Oauth.dpop_jkt pub
  let ath_of_credential credential = Oauth.ath_of_access_token credential
  let generate_dpop_pair () = Oauth.generate_dpop_pair ()

  let p256_pub_of_jwk (jwk : Yojson.Safe.t) : Mirage_crypto_ec.P256.Dsa.pub =
    let open Yojson.Safe.Util in
    let kty = match jwk |> member "kty" with `String s -> s | _ -> "" in
    let crv = match jwk |> member "crv" with `String s -> s | _ -> "" in
    if kty <> "EC" || crv <> "P-256" then fail "DPoP jwk must be EC P-256";
    let x =
      match jwk |> member "x" with
      | `String s -> (
          try Base64url.decode s with _ -> fail "DPoP jwk x is not base64url")
      | _ -> fail "DPoP jwk missing x"
    in
    let y =
      match jwk |> member "y" with
      | `String s -> (
          try Base64url.decode s with _ -> fail "DPoP jwk y is not base64url")
      | _ -> fail "DPoP jwk missing y"
    in
    if String.length x <> 32 || String.length y <> 32 then
      fail "DPoP jwk coordinates must be 32 bytes";
    match Mirage_crypto_ec.P256.Dsa.pub_of_octets ("\x04" ^ x ^ y) with
    | Ok pub -> pub
    | Error _ -> fail "invalid DPoP P-256 jwk"

  let dpop_iat_ok ?(now = Unix.gettimeofday ()) (iat : int64) : bool =
    let now_i = Int64.of_float now in
    let age = Int64.sub now_i iat in
    let ahead = Int64.sub iat now_i in
    Int64.compare age (Int64.add max_dpop_age_sec clock_skew_sec) <= 0
    && Int64.compare ahead clock_skew_sec <= 0

  let verify_dpop_embedded ?now ~htm ~htu (proof : string) : dpop_check =
    let claims = try Oauth.parse_dpop proof with Failure msg -> fail msg in
    if claims.Oauth.typ <> "dpop+jwt" then fail "DPoP typ is not dpop+jwt";
    if claims.Oauth.alg <> "ES256" then fail "DPoP alg must be ES256";
    if claims.Oauth.jti = "" then fail "missing DPoP proof \"jti\"";
    let want_htu = Oauth.htu_of_url htu in
    if claims.Oauth.htm <> htm then
      fail "DPoP proof \"htm\" does not match the request";
    if claims.Oauth.htu <> want_htu then
      fail "DPoP proof \"htu\" does not match the request";
    if not (dpop_iat_ok ?now claims.Oauth.iat) then fail "DPoP proof is expired";
    let pub = p256_pub_of_jwk claims.Oauth.jwk in
    if not (Oauth.verify_dpop ~pub proof) then
      fail "could not verify DPoP proof";
    let jkt =
      try Oauth.jwk_thumbprint claims.Oauth.jwk with Failure msg -> fail msg
    in
    {
      jti = claims.Oauth.jti;
      jkt;
      htm = claims.Oauth.htm;
      htu = claims.Oauth.htu;
      iat = claims.Oauth.iat;
      ath = claims.Oauth.ath;
    }

  let verify_exchange_dpop ?now ~htm ~htu proof =
    let checked = verify_dpop_embedded ?now ~htm ~htu proof in
    (match checked.ath with
    | Some _ ->
        fail "DPoP proof \"ath\" must be omitted when obtaining a credential"
    | None -> ());
    checked

  let verify_resource_dpop ?now ~htm ~htu ~credential ~jkt proof =
    let checked = verify_dpop_embedded ?now ~htm ~htu proof in
    let want = ath_of_credential credential in
    (match checked.ath with
    | Some got when got = want -> ()
    | Some _ -> fail "DPoP proof \"ath\" does not match the credential"
    | None -> fail "DPoP proof \"ath\" does not match the credential");
    if checked.jkt <> jkt then
      fail "DPoP proof is not signed by the key the credential is bound to";
    checked
end
