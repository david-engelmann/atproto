open Hash
open Base64url

let ensure_rng = lazy (Mirage_crypto_rng_unix.use_default ())

(** AT Protocol OAuth core: PKCE (S256), DPoP (ES256 + nonce), client metadata,
    PAR/token request+response, redirect callback, and an injectable HTTP loop.
    Hosting a public client-metadata URL and completing a live browser login
    remain application-level. *)
module Oauth = struct
  type pkce = { verifier : string; challenge : string; method_ : string }

  type dpop_claims = {
    typ : string;
    alg : string;
    jwk : Yojson.Safe.t;
    jti : string;
    htm : string;
    htu : string;
    iat : int64;
    ath : string option;
    nonce : string option;
  }

  let pkce_unreserved =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~"

  let random_verifier ?(len = 64) () : string =
    if len < 43 || len > 128 then
      failwith "Oauth.random_verifier: PKCE verifier must be 43-128 chars";
    Random.self_init ();
    String.init len (fun _ ->
        pkce_unreserved.[Random.int (String.length pkce_unreserved)])

  let pkce_challenge (verifier : string) : string =
    if String.length verifier < 43 || String.length verifier > 128 then
      failwith "Oauth.pkce_challenge: verifier must be 43-128 chars";
    Base64url.encode (Hash.sha256 verifier)

  let pkce_s256 ?(verifier = random_verifier ()) () : pkce =
    { verifier; challenge = pkce_challenge verifier; method_ = "S256" }

  let p256_jwk (pub : Mirage_crypto_ec.P256.Dsa.pub) : Yojson.Safe.t =
    let octets = Mirage_crypto_ec.P256.Dsa.pub_to_octets ~compress:false pub in
    if String.length octets <> 65 || octets.[0] <> '\x04' then
      failwith "Oauth: unexpected P-256 public key encoding";
    let x = Base64url.encode (String.sub octets 1 32) in
    let y = Base64url.encode (String.sub octets 33 32) in
    `Assoc
      [
        ("kty", `String "EC");
        ("crv", `String "P-256");
        ("x", `String x);
        ("y", `String y);
      ]

  let b64url_json json = Base64url.encode (Yojson.Safe.to_string json)

  let ath_of_access_token (access_token : string) : string =
    Base64url.encode (Hash.sha256 access_token)

  (* NIST P-256 group order n and floor(n/2) for low-S ECDSA (same as PLC). *)
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

  let sign_es256 ~(priv : Mirage_crypto_ec.P256.Dsa.priv) (input : string) :
      string =
    Lazy.force ensure_rng;
    let digest = Hash.sha256 input in
    let r, s = Mirage_crypto_ec.P256.Dsa.sign ~key:priv digest in
    r ^ low_s s

  (* RFC 9449: htu is the HTTP URI without query or fragment. *)
  let htu_of_url (url : string) : string =
    let cut c s =
      match String.index_opt s c with Some i -> String.sub s 0 i | None -> s
    in
    cut '#' (cut '?' url)

  let random_jti () : string =
    Lazy.force ensure_rng;
    Random.self_init ();
    let bytes = Bytes.create 32 in
    for i = 0 to 31 do
      Bytes.set bytes i (Char.chr (Random.int 256))
    done;
    Base64url.encode (Bytes.to_string bytes)

  let dpop_proof ~(priv : Mirage_crypto_ec.P256.Dsa.priv)
      ~(pub : Mirage_crypto_ec.P256.Dsa.pub) ~htm ~htu ?ath ?jti ?iat ?nonce ()
      : string =
    let jti = match jti with Some j -> j | None -> random_jti () in
    let iat =
      match iat with
      | Some n -> n
      | None -> Int64.of_float (Unix.gettimeofday ())
    in
    let header =
      `Assoc
        [
          ("typ", `String "dpop+jwt");
          ("alg", `String "ES256");
          ("jwk", p256_jwk pub);
        ]
    in
    let payload_fields =
      [
        ("jti", `String jti);
        ("htm", `String htm);
        ("htu", `String htu);
        ("iat", `Intlit (Int64.to_string iat));
      ]
      @ (match ath with Some a -> [ ("ath", `String a) ] | None -> [])
      @ match nonce with Some n -> [ ("nonce", `String n) ] | None -> []
    in
    let payload = `Assoc payload_fields in
    let input = b64url_json header ^ "." ^ b64url_json payload in
    let sig_ = Base64url.encode (sign_es256 ~priv input) in
    input ^ "." ^ sig_

  let split_jwt (jwt : string) : string * string * string =
    match String.split_on_char '.' jwt with
    | [ h; p; s ] -> (h, p, s)
    | _ -> failwith "Oauth: invalid JWT (expected three base64url parts)"

  let parse_dpop (jwt : string) : dpop_claims =
    let h, p, _ = split_jwt jwt in
    let header = Yojson.Safe.from_string (Base64url.decode h) in
    let payload = Yojson.Safe.from_string (Base64url.decode p) in
    let open Yojson.Safe.Util in
    {
      typ = (match header |> member "typ" with `String s -> s | _ -> "");
      alg = (match header |> member "alg" with `String s -> s | _ -> "");
      jwk = header |> member "jwk";
      jti = (match payload |> member "jti" with `String s -> s | _ -> "");
      htm = (match payload |> member "htm" with `String s -> s | _ -> "");
      htu = (match payload |> member "htu" with `String s -> s | _ -> "");
      iat =
        (match payload |> member "iat" with
        | `Int n -> Int64.of_int n
        | `Intlit s -> Int64.of_string s
        | _ -> 0L);
      ath =
        (match payload |> member "ath" with `String s -> Some s | _ -> None);
      nonce =
        (match payload |> member "nonce" with `String s -> Some s | _ -> None);
    }

  let verify_dpop ~(pub : Mirage_crypto_ec.P256.Dsa.pub) (jwt : string) : bool =
    try
      let h, p, s = split_jwt jwt in
      let input = h ^ "." ^ p in
      let sig_ = Base64url.decode s in
      if String.length sig_ <> 64 then false
      else
        let r = String.sub sig_ 0 32 in
        let t = String.sub sig_ 32 32 in
        if not (is_low_s t) then false
        else
          Mirage_crypto_ec.P256.Dsa.verify ~key:pub (r, t) (Hash.sha256 input)
    with _ -> false

  let par_url ?(host = "bsky.social") () =
    Printf.sprintf "https://%s/oauth/par" host

  let authorize_url ?(host = "bsky.social") () =
    Printf.sprintf "https://%s/oauth/authorize" host

  let token_url ?(host = "bsky.social") () =
    Printf.sprintf "https://%s/oauth/token" host

  let revocation_url ?(host = "bsky.social") () =
    Printf.sprintf "https://%s/oauth/revoke" host

  let jwt_bearer_assertion_type =
    "urn:ietf:params:oauth:client-assertion-type:jwt-bearer"

  let pushed_authorization_body ~client_id ~redirect_uri ~code_challenge ~state
      ?(scope = "atproto transition:generic") ?login_hint ?prompt ?dpop_jkt
      ?client_assertion () =
    [
      ("response_type", "code");
      ("client_id", client_id);
      ("redirect_uri", redirect_uri);
      ("code_challenge", code_challenge);
      ("code_challenge_method", "S256");
      ("state", state);
      ("scope", scope);
    ]
    @ (match login_hint with Some h -> [ ("login_hint", h) ] | None -> [])
    @ (match prompt with Some p -> [ ("prompt", p) ] | None -> [])
    @ (match dpop_jkt with Some j -> [ ("dpop_jkt", j) ] | None -> [])
    @
    match client_assertion with
    | Some assertion ->
        [
          ("client_assertion_type", jwt_bearer_assertion_type);
          ("client_assertion", assertion);
        ]
    | None -> []

  (* RFC 7638 JWK thumbprint for the DPoP EC key (PAR dpop_jkt). *)
  let jwk_thumbprint (jwk : Yojson.Safe.t) : string =
    let open Yojson.Safe.Util in
    let kty = jwk |> member "kty" |> to_string in
    let canonical =
      match kty with
      | "EC" ->
          let crv = jwk |> member "crv" |> to_string in
          let x = jwk |> member "x" |> to_string in
          let y = jwk |> member "y" |> to_string in
          Printf.sprintf {|{"crv":"%s","kty":"EC","x":"%s","y":"%s"}|} crv x y
      | _ -> failwith "Oauth: JWK thumbprint is only defined here for EC keys"
    in
    Base64url.encode (Hash.sha256 canonical)

  let dpop_jkt (pub : Mirage_crypto_ec.P256.Dsa.pub) : string =
    jwk_thumbprint (p256_jwk pub)

  let token_body ~client_id ~redirect_uri ~code ~code_verifier ?client_assertion
      () =
    [
      ("grant_type", "authorization_code");
      ("client_id", client_id);
      ("redirect_uri", redirect_uri);
      ("code", code);
      ("code_verifier", code_verifier);
    ]
    @
    match client_assertion with
    | Some assertion ->
        [
          ("client_assertion_type", jwt_bearer_assertion_type);
          ("client_assertion", assertion);
        ]
    | None -> []

  let refresh_body ~client_id ~refresh_token ?client_assertion () =
    [
      ("grant_type", "refresh_token");
      ("client_id", client_id);
      ("refresh_token", refresh_token);
    ]
    @
    match client_assertion with
    | Some assertion ->
        [
          ("client_assertion_type", jwt_bearer_assertion_type);
          ("client_assertion", assertion);
        ]
    | None -> []

  let revoke_body ~client_id ~token ?token_type_hint ?client_assertion () =
    [ ("client_id", client_id); ("token", token) ]
    @ (match token_type_hint with
      | Some h -> [ ("token_type_hint", h) ]
      | None -> [])
    @
    match client_assertion with
    | Some assertion ->
        [
          ("client_assertion_type", jwt_bearer_assertion_type);
          ("client_assertion", assertion);
        ]
    | None -> []

  let form_encode (pairs : (string * string) list) : string =
    let kv (k, v) = Uri.pct_encode k ^ "=" ^ Uri.pct_encode v in
    String.concat "&" (List.map kv pairs)

  let dpop_header proof = ("DPoP", proof)
  let authorization_dpop access_token = ("Authorization", "DPoP " ^ access_token)

  let query_append url pairs =
    let encoded = form_encode pairs in
    if encoded = "" then url
    else if String.contains url '?' then url ^ "&" ^ encoded
    else url ^ "?" ^ encoded

  let authorize_redirect_url ~authorization_endpoint ~client_id ~request_uri =
    query_append authorization_endpoint
      [ ("client_id", client_id); ("request_uri", request_uri) ]

  type callback =
    | Authorized of { code : string; state : string; iss : string option }
    | Denied of {
        error : string;
        description : string option;
        state : string option;
      }

  let parse_query_pairs (query : string) : (string * string) list =
    if query = "" then []
    else
      String.split_on_char '&' query
      |> List.filter_map (fun part ->
             match String.split_on_char '=' part with
             | [] | [ "" ] -> None
             | [ k ] -> Some (Uri.pct_decode k, "")
             | k :: rest ->
                 Some (Uri.pct_decode k, Uri.pct_decode (String.concat "=" rest)))

  let assoc_opt key pairs =
    match List.assoc_opt key pairs with Some "" -> None | other -> other

  let parse_redirect (uri : string) : callback =
    let q =
      match String.index_opt uri '?' with
      | None -> ""
      | Some i -> (
          let rest = String.sub uri (i + 1) (String.length uri - i - 1) in
          match String.index_opt rest '#' with
          | None -> rest
          | Some h -> String.sub rest 0 h)
    in
    let pairs = parse_query_pairs q in
    match assoc_opt "error" pairs with
    | Some err ->
        Denied
          {
            error = err;
            description = assoc_opt "error_description" pairs;
            state = assoc_opt "state" pairs;
          }
    | None -> (
        match (assoc_opt "code" pairs, assoc_opt "state" pairs) with
        | Some code, Some state ->
            Authorized { code; state; iss = assoc_opt "iss" pairs }
        | _ ->
            failwith
              "Oauth.parse_redirect: expected code+state or error query params")

  let expect_state ~expected = function
    | Authorized { state; _ } | Denied { state = Some state; _ } ->
        if state = expected then ()
        else failwith "Oauth: redirect state does not match the PAR request"
    | Denied { state = None; _ } ->
        failwith "Oauth: denied redirect is missing state"

  let expect_issuer ~expected = function
    | Authorized { iss = Some iss; _ } ->
        if iss = expected then ()
        else
          failwith
            (Printf.sprintf "Oauth: redirect iss %s != authorization server %s"
               iss expected)
    | Authorized { iss = None; _ } ->
        failwith "Oauth: redirect is missing iss (required by atproto OAuth)"
    | Denied _ -> ()

  type client_metadata = {
    client_id : string;
    application_type : string;
    grant_types : string list;
    response_types : string list;
    redirect_uris : string list;
    scope : string;
    token_endpoint_auth_method : string;
    token_endpoint_auth_signing_alg : string option;
    dpop_bound_access_tokens : bool;
    jwks : Yojson.Safe.t option;
    jwks_uri : string option;
    client_name : string option;
    client_uri : string option;
    logo_uri : string option;
    tos_uri : string option;
    policy_uri : string option;
  }

  let string_list json field =
    match Yojson.Safe.Util.member field json with
    | `List items ->
        List.filter_map (function `String s -> Some s | _ -> None) items
    | _ -> []

  let json_string_opt json field =
    match Yojson.Safe.Util.member field json with
    | `String s -> Some s
    | _ -> None

  let starts_with s prefix =
    let n = String.length prefix in
    String.length s >= n && String.sub s 0 n = prefix

  let contains_scope ~scope needle =
    List.mem needle (String.split_on_char ' ' scope)

  let public_metadata ~client_id ~redirect_uris
      ?(scope = "atproto transition:generic") ?(application_type = "web")
      ?client_name ?client_uri ?logo_uri ?tos_uri ?policy_uri () :
      client_metadata =
    {
      client_id;
      application_type;
      grant_types = [ "authorization_code"; "refresh_token" ];
      response_types = [ "code" ];
      redirect_uris;
      scope;
      token_endpoint_auth_method = "none";
      token_endpoint_auth_signing_alg = None;
      dpop_bound_access_tokens = true;
      jwks = None;
      jwks_uri = None;
      client_name;
      client_uri;
      logo_uri;
      tos_uri;
      policy_uri;
    }

  let confidential_metadata ~client_id ~redirect_uris ~jwks
      ?(scope = "atproto transition:generic") ?(application_type = "web")
      ?client_name ?client_uri ?logo_uri ?tos_uri ?policy_uri () :
      client_metadata =
    {
      (public_metadata ~client_id ~redirect_uris ~scope ~application_type
         ?client_name ?client_uri ?logo_uri ?tos_uri ?policy_uri ())
      with
      token_endpoint_auth_method = "private_key_jwt";
      token_endpoint_auth_signing_alg = Some "ES256";
      jwks = Some jwks;
    }

  let metadata_to_json (m : client_metadata) : Yojson.Safe.t =
    let str_list xs = `List (List.map (fun s -> `String s) xs) in
    let fields =
      [
        ("client_id", `String m.client_id);
        ("application_type", `String m.application_type);
        ("grant_types", str_list m.grant_types);
        ("response_types", str_list m.response_types);
        ("redirect_uris", str_list m.redirect_uris);
        ("scope", `String m.scope);
        ("token_endpoint_auth_method", `String m.token_endpoint_auth_method);
        ("dpop_bound_access_tokens", `Bool m.dpop_bound_access_tokens);
      ]
      @ (match m.token_endpoint_auth_signing_alg with
        | Some alg -> [ ("token_endpoint_auth_signing_alg", `String alg) ]
        | None -> [])
      @ (match m.jwks with Some j -> [ ("jwks", j) ] | None -> [])
      @ (match m.jwks_uri with
        | Some u -> [ ("jwks_uri", `String u) ]
        | None -> [])
      @ (match m.client_name with
        | Some n -> [ ("client_name", `String n) ]
        | None -> [])
      @ (match m.client_uri with
        | Some u -> [ ("client_uri", `String u) ]
        | None -> [])
      @ (match m.logo_uri with
        | Some u -> [ ("logo_uri", `String u) ]
        | None -> [])
      @ (match m.tos_uri with
        | Some u -> [ ("tos_uri", `String u) ]
        | None -> [])
      @
      match m.policy_uri with
      | Some u -> [ ("policy_uri", `String u) ]
      | None -> []
    in
    `Assoc fields

  let metadata_of_json json : client_metadata =
    let open Yojson.Safe.Util in
    {
      client_id = json |> member "client_id" |> to_string;
      application_type =
        (match json |> member "application_type" with
        | `String s -> s
        | _ -> "web");
      grant_types = string_list json "grant_types";
      response_types = string_list json "response_types";
      redirect_uris = string_list json "redirect_uris";
      scope = (match json |> member "scope" with `String s -> s | _ -> "");
      token_endpoint_auth_method =
        (match json |> member "token_endpoint_auth_method" with
        | `String s -> s
        | _ -> "none");
      token_endpoint_auth_signing_alg =
        json_string_opt json "token_endpoint_auth_signing_alg";
      dpop_bound_access_tokens =
        (match json |> member "dpop_bound_access_tokens" with
        | `Bool b -> b
        | _ -> false);
      jwks =
        (match json |> member "jwks" with
        | `Null | `Assoc [] -> None
        | j -> Some j);
      jwks_uri = json_string_opt json "jwks_uri";
      client_name = json_string_opt json "client_name";
      client_uri = json_string_opt json "client_uri";
      logo_uri = json_string_opt json "logo_uri";
      tos_uri = json_string_opt json "tos_uri";
      policy_uri = json_string_opt json "policy_uri";
    }

  let jwk_has_private_d json =
    match json with
    | `Assoc fields ->
        List.exists
          (function
            | "d", `String _ -> true
            | "keys", `List keys ->
                List.exists
                  (function
                    | `Assoc k ->
                        List.exists
                          (function "d", `String _ -> true | _ -> false)
                          k
                    | _ -> false)
                  keys
            | _ -> false)
          fields
    | _ -> false

  let is_localhost_client_id (client_id : string) : bool =
    client_id = "http://localhost" || starts_with client_id "http://localhost?"

  let validate_metadata (m : client_metadata) : unit =
    if m.client_id = "" then failwith "Oauth: client_id is required";
    if not m.dpop_bound_access_tokens then
      failwith "Oauth: dpop_bound_access_tokens must be true";
    if not (List.mem "authorization_code" m.grant_types) then
      failwith "Oauth: grant_types must include authorization_code";
    if not (List.mem "code" m.response_types) then
      failwith "Oauth: response_types must include code";
    if not (contains_scope ~scope:m.scope "atproto") then
      failwith "Oauth: scope must include atproto";
    ignore (Oauth_scope.Oauth_scope.parse_and_require m.scope);
    if m.redirect_uris = [] then
      failwith "Oauth: redirect_uris must contain at least one URI";
    if m.token_endpoint_auth_signing_alg = Some "none" then
      failwith "Oauth: token_endpoint_auth_signing_alg must not be none";
    (match (m.jwks, m.jwks_uri) with
    | Some _, Some _ ->
        failwith "Oauth: jwks and jwks_uri are mutually exclusive"
    | _ -> ());
    (match m.jwks with
    | Some j when jwk_has_private_d j ->
        failwith "Oauth: client JWKS must not contain private key material (d)"
    | _ -> ());
    match m.token_endpoint_auth_method with
    | "private_key_jwt" -> (
        match (m.jwks, m.jwks_uri) with
        | None, None ->
            failwith "Oauth: confidential clients must supply jwks or jwks_uri"
        | _ -> ())
    | "none" | "" -> ()
    | other ->
        failwith ("Oauth: unsupported token_endpoint_auth_method " ^ other)

  let localhost_metadata (client_id : string) : client_metadata =
    if not (is_localhost_client_id client_id) then
      failwith "Oauth: not a localhost development client_id";
    let query =
      match String.index_opt client_id '?' with
      | None -> ""
      | Some i -> String.sub client_id (i + 1) (String.length client_id - i - 1)
    in
    let pairs = parse_query_pairs query in
    let redirects =
      List.filter_map
        (fun (k, v) -> if k = "redirect_uri" && v <> "" then Some v else None)
        pairs
    in
    let redirects =
      if redirects = [] then [ "http://127.0.0.1/"; "http://[::1]/" ]
      else redirects
    in
    let scope =
      match assoc_opt "scope" pairs with Some s -> s | None -> "atproto"
    in
    public_metadata ~client_id ~redirect_uris:redirects ~scope
      ~application_type:"native" ~client_name:"Development client" ()

  type as_metadata = {
    issuer : string;
    authorization_endpoint : string;
    token_endpoint : string;
    pushed_authorization_request_endpoint : string;
    response_types_supported : string list;
    grant_types_supported : string list;
    code_challenge_methods_supported : string list;
    token_endpoint_auth_methods_supported : string list;
    token_endpoint_auth_signing_alg_values_supported : string list;
    scopes_supported : string list;
    dpop_signing_alg_values_supported : string list;
    require_pushed_authorization_requests : bool;
    authorization_response_iss_parameter_supported : bool;
    client_id_metadata_document_supported : bool;
    require_request_uri_registration : bool;
    revocation_endpoint : string option;
  }

  type resource_metadata = {
    resource : string option;
    authorization_servers : string list;
  }

  let protected_resource_url ?(host = "bsky.social") () =
    Printf.sprintf "https://%s/.well-known/oauth-protected-resource" host

  let authorization_server_url ?(issuer = "https://bsky.social") () =
    let issuer =
      if issuer <> "" && issuer.[String.length issuer - 1] = '/' then
        String.sub issuer 0 (String.length issuer - 1)
      else issuer
    in
    issuer ^ "/.well-known/oauth-authorization-server"

  let parse_resource_metadata json : resource_metadata =
    {
      resource = json_string_opt json "resource";
      authorization_servers = string_list json "authorization_servers";
    }

  let parse_as_metadata json : as_metadata =
    let open Yojson.Safe.Util in
    {
      issuer = json |> member "issuer" |> to_string;
      authorization_endpoint =
        json |> member "authorization_endpoint" |> to_string;
      token_endpoint = json |> member "token_endpoint" |> to_string;
      pushed_authorization_request_endpoint =
        json |> member "pushed_authorization_request_endpoint" |> to_string;
      response_types_supported = string_list json "response_types_supported";
      grant_types_supported = string_list json "grant_types_supported";
      code_challenge_methods_supported =
        string_list json "code_challenge_methods_supported";
      token_endpoint_auth_methods_supported =
        string_list json "token_endpoint_auth_methods_supported";
      token_endpoint_auth_signing_alg_values_supported =
        string_list json "token_endpoint_auth_signing_alg_values_supported";
      scopes_supported = string_list json "scopes_supported";
      dpop_signing_alg_values_supported =
        string_list json "dpop_signing_alg_values_supported";
      require_pushed_authorization_requests =
        (match json |> member "require_pushed_authorization_requests" with
        | `Bool b -> b
        | _ -> false);
      authorization_response_iss_parameter_supported =
        (match
           json |> member "authorization_response_iss_parameter_supported"
         with
        | `Bool b -> b
        | _ -> false);
      client_id_metadata_document_supported =
        (match json |> member "client_id_metadata_document_supported" with
        | `Bool b -> b
        | _ -> false);
      require_request_uri_registration =
        (match json |> member "require_request_uri_registration" with
        | `Bool b -> b
        | _ -> true);
      revocation_endpoint = json_string_opt json "revocation_endpoint";
    }

  let require_mem label xs value =
    if not (List.mem value xs) then
      failwith (Printf.sprintf "Oauth: %s must include %s" label value)

  let validate_as_metadata (m : as_metadata) : unit =
    if not (starts_with m.issuer "https://") then
      failwith "Oauth: issuer must be an https origin";
    if String.contains_from m.issuer (String.length "https://") '/' then
      failwith "Oauth: issuer must not include a path";
    require_mem "response_types_supported" m.response_types_supported "code";
    require_mem "grant_types_supported" m.grant_types_supported
      "authorization_code";
    require_mem "grant_types_supported" m.grant_types_supported "refresh_token";
    require_mem "code_challenge_methods_supported"
      m.code_challenge_methods_supported "S256";
    require_mem "token_endpoint_auth_methods_supported"
      m.token_endpoint_auth_methods_supported "none";
    require_mem "token_endpoint_auth_methods_supported"
      m.token_endpoint_auth_methods_supported "private_key_jwt";
    require_mem "token_endpoint_auth_signing_alg_values_supported"
      m.token_endpoint_auth_signing_alg_values_supported "ES256";
    if List.mem "none" m.token_endpoint_auth_signing_alg_values_supported then
      failwith
        "Oauth: token_endpoint_auth_signing_alg_values_supported has none";
    require_mem "scopes_supported" m.scopes_supported "atproto";
    require_mem "dpop_signing_alg_values_supported"
      m.dpop_signing_alg_values_supported "ES256";
    if not m.require_pushed_authorization_requests then
      failwith "Oauth: require_pushed_authorization_requests must be true";
    if not m.authorization_response_iss_parameter_supported then
      failwith
        "Oauth: authorization_response_iss_parameter_supported must be true";
    if not m.client_id_metadata_document_supported then
      failwith "Oauth: client_id_metadata_document_supported must be true";
    if not m.require_request_uri_registration then
      failwith "Oauth: require_request_uri_registration must not be false";
    if m.pushed_authorization_request_endpoint = "" then
      failwith "Oauth: pushed_authorization_request_endpoint is required"

  type par_response = { request_uri : string; expires_in : int option }

  type token = {
    access_token : string;
    token_type : string;
    expires_in : int option;
    refresh_token : string option;
    scope : string;
    sub : string;
  }

  let parse_par_response json : par_response =
    let open Yojson.Safe.Util in
    {
      request_uri = json |> member "request_uri" |> to_string;
      expires_in =
        (match json |> member "expires_in" with `Int n -> Some n | _ -> None);
    }

  let parse_token_response json : token =
    let open Yojson.Safe.Util in
    let token_type =
      match json |> member "token_type" with `String s -> s | _ -> ""
    in
    let scope = match json |> member "scope" with `String s -> s | _ -> "" in
    let sub = match json |> member "sub" with `String s -> s | _ -> "" in
    if String.lowercase_ascii token_type <> "dpop" then
      failwith "Oauth: token_type must be DPoP";
    if scope = "" then failwith "Oauth: token response is missing scope";
    if not (contains_scope ~scope "atproto") then
      failwith "Oauth: granted scope must include atproto";
    if not (starts_with sub "did:") then
      failwith "Oauth: token response sub must be an atproto DID";
    {
      access_token = json |> member "access_token" |> to_string;
      token_type;
      expires_in =
        (match json |> member "expires_in" with `Int n -> Some n | _ -> None);
      refresh_token = json_string_opt json "refresh_token";
      scope;
      sub;
    }

  let expect_sub ~expected (t : token) : unit =
    if t.sub <> expected then
      failwith
        (Printf.sprintf "Oauth: token sub %s != expected DID %s" t.sub expected)

  let expires_at ~issued_at (t : token) : float option =
    match t.expires_in with
    | Some n -> Some (issued_at +. float_of_int n)
    | None -> None

  let client_assertion ~(priv : Mirage_crypto_ec.P256.Dsa.priv)
      ~pub:(_pub : Mirage_crypto_ec.P256.Dsa.pub) ~client_id ~issuer ?kid ?jti
      ?iat ?exp () : string =
    let jti = match jti with Some j -> j | None -> random_jti () in
    let iat =
      match iat with
      | Some n -> n
      | None -> Int64.of_float (Unix.gettimeofday ())
    in
    let exp = match exp with Some n -> n | None -> Int64.add iat 300L in
    let header_fields =
      [ ("typ", `String "JWT"); ("alg", `String "ES256") ]
      @ match kid with Some k -> [ ("kid", `String k) ] | None -> []
    in
    let header = `Assoc header_fields in
    let payload =
      `Assoc
        [
          ("iss", `String client_id);
          ("sub", `String client_id);
          ("aud", `String issuer);
          ("jti", `String jti);
          ("iat", `Intlit (Int64.to_string iat));
          ("exp", `Intlit (Int64.to_string exp));
        ]
    in
    let input = b64url_json header ^ "." ^ b64url_json payload in
    let sig_ = Base64url.encode (sign_es256 ~priv input) in
    input ^ "." ^ sig_

  let jwks_of_pub ?(kid = "oauth-client") (pub : Mirage_crypto_ec.P256.Dsa.pub)
      : Yojson.Safe.t =
    match p256_jwk pub with
    | `Assoc fields ->
        `Assoc
          [
            ( "keys",
              `List
                [
                  `Assoc
                    (fields
                    @ [
                        ("kid", `String kid);
                        ("use", `String "sig");
                        ("alg", `String "ES256");
                      ]);
                ] );
          ]
    | other -> other

  type http_response = {
    status : int;
    headers : (string * string) list;
    body : string;
  }

  type http_post =
    url:string -> headers:(string * string) list -> body:string -> http_response

  let header_value headers name =
    let lower = String.lowercase_ascii name in
    List.find_map
      (fun (k, v) -> if String.lowercase_ascii k = lower then Some v else None)
      headers

  let use_dpop_nonce status body =
    status = 400
    &&
    match Error.Error.of_body body with
    | Some e -> e.Error.Error.error = "use_dpop_nonce"
    | None -> false

  let missing_dpop_nonce () =
    failwith "Oauth: use_dpop_nonce response is missing a DPoP-Nonce header"

  let post_with_dpop ~(http : http_post) ~priv ~pub ~url ~htm ~body ?ath ?nonce
      () : http_response * string option =
    let htu = htu_of_url url in
    let rec attempt nonce tries =
      let proof = dpop_proof ~priv ~pub ~htm ~htu ?ath ?nonce () in
      let headers =
        [
          ("Content-Type", "application/x-www-form-urlencoded");
          dpop_header proof;
        ]
      in
      let resp = http ~url ~headers ~body in
      let next_nonce = header_value resp.headers "DPoP-Nonce" in
      if use_dpop_nonce resp.status resp.body && tries > 0 then
        match next_nonce with
        | Some n -> attempt (Some n) (tries - 1)
        | None -> missing_dpop_nonce ()
      else (resp, match next_nonce with Some n -> Some n | None -> nonce)
    in
    attempt nonce 1

  let decode_json_body label body =
    try Yojson.Safe.from_string body
    with _ -> failwith (Printf.sprintf "Oauth: %s returned non-JSON" label)

  let ensure_ok label resp =
    if resp.status < 200 || resp.status >= 300 then
      match Error.Error.of_body resp.body with
      | Some e ->
          failwith
            (Printf.sprintf "Oauth %s: %s" label (Error.Error.to_string e))
      | None -> failwith (Printf.sprintf "Oauth %s: HTTP %d" label resp.status)
    else decode_json_body label resp.body

  let push_authorization ~(http : http_post) ~priv ~pub ~par_url ~form ?nonce ()
      : par_response * string option =
    let body = form_encode form in
    let resp, nonce =
      post_with_dpop ~http ~priv ~pub ~url:par_url ~htm:"POST" ~body ?nonce ()
    in
    (parse_par_response (ensure_ok "PAR" resp), nonce)

  let exchange_code ~(http : http_post) ~priv ~pub ~token_url ~form ?nonce () :
      token * string option =
    let body = form_encode form in
    let resp, nonce =
      post_with_dpop ~http ~priv ~pub ~url:token_url ~htm:"POST" ~body ?nonce ()
    in
    (parse_token_response (ensure_ok "token" resp), nonce)

  let refresh ~(http : http_post) ~priv ~pub ~token_url ~form ?nonce () :
      token * string option =
    exchange_code ~http ~priv ~pub ~token_url ~form ?nonce ()

  (* RFC 7009 token revocation. 200 with an empty body is success. *)
  let revoke ~(http : http_post) ~priv ~pub ~revoke_url ~form ?nonce () :
      unit * string option =
    let body = form_encode form in
    let resp, nonce =
      post_with_dpop ~http ~priv ~pub ~url:revoke_url ~htm:"POST" ~body ?nonce
        ()
    in
    if resp.status < 200 || resp.status >= 300 then
      ignore (ensure_ok "revoke" resp)
    else ();
    ((), nonce)

  let resource_request_headers ~priv ~pub ~htm ~htu ~access_token ?ath ?nonce ()
      =
    let ath =
      match ath with Some a -> a | None -> ath_of_access_token access_token
    in
    let proof =
      dpop_proof ~priv ~pub ~htm ~htu:(htu_of_url htu) ~ath ?nonce ()
    in
    [ authorization_dpop access_token; dpop_header proof ]

  type http_request =
    url:string ->
    method_:string ->
    headers:(string * string) list ->
    body:string option ->
    http_response

  (* DPoP-bound resource request with one use_dpop_nonce retry. *)
  let request_with_dpop ~(http : http_request) ~priv ~pub ~url ~htm
      ~access_token ?body ?ath ?nonce () : http_response * string option =
    let ath =
      match ath with Some a -> a | None -> ath_of_access_token access_token
    in
    let htu = htu_of_url url in
    let rec attempt nonce tries =
      let proof = dpop_proof ~priv ~pub ~htm ~htu ~ath ?nonce () in
      let headers = [ authorization_dpop access_token; dpop_header proof ] in
      let resp = http ~url ~method_:htm ~headers ~body in
      let next_nonce = header_value resp.headers "DPoP-Nonce" in
      if use_dpop_nonce resp.status resp.body && tries > 0 then
        match next_nonce with
        | Some n -> attempt (Some n) (tries - 1)
        | None -> missing_dpop_nonce ()
      else (resp, match next_nonce with Some n -> Some n | None -> nonce)
    in
    attempt nonce 1
end
