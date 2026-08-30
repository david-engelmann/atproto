open Hash
open Base64url

(** AT Protocol OAuth core: PKCE (S256), DPoP (ES256), PAR/token request shapes.
    Browser redirects, client-metadata hosting, and a live token loop are
    product-level and left to the application. *)
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
    let digest = Hash.sha256 input in
    let r, s = Mirage_crypto_ec.P256.Dsa.sign ~key:priv digest in
    r ^ low_s s

  let dpop_proof ~(priv : Mirage_crypto_ec.P256.Dsa.priv)
      ~(pub : Mirage_crypto_ec.P256.Dsa.pub) ~htm ~htu ?ath ?jti ?iat () :
      string =
    let jti =
      match jti with
      | Some j -> j
      | None ->
          Base64url.encode
            (Hash.sha256 (string_of_float (Unix.gettimeofday ())))
    in
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
      @ match ath with Some a -> [ ("ath", `String a) ] | None -> []
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

  let pushed_authorization_body ~client_id ~redirect_uri ~code_challenge ~state
      ?(scope = "atproto transition:generic") () =
    [
      ("response_type", "code");
      ("client_id", client_id);
      ("redirect_uri", redirect_uri);
      ("code_challenge", code_challenge);
      ("code_challenge_method", "S256");
      ("state", state);
      ("scope", scope);
    ]

  let token_body ~client_id ~redirect_uri ~code ~code_verifier =
    [
      ("grant_type", "authorization_code");
      ("client_id", client_id);
      ("redirect_uri", redirect_uri);
      ("code", code);
      ("code_verifier", code_verifier);
    ]

  let refresh_body ~client_id ~refresh_token =
    [
      ("grant_type", "refresh_token");
      ("client_id", client_id);
      ("refresh_token", refresh_token);
    ]

  let form_encode (pairs : (string * string) list) : string =
    let kv (k, v) = Uri.pct_encode k ^ "=" ^ Uri.pct_encode v in
    String.concat "&" (List.map kv pairs)

  let dpop_header proof = ("DPoP", proof)
end
