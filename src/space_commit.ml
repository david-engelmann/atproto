open Hash
open Lt_hash
open Did_key
open Did_plc
open K256
open Dag_cbor
open Syntax
open At_uri

let ensure_rng = lazy (Mirage_crypto_rng_unix.use_default ())

(** Experimental permissioned-repo commit from AT Protocol proposal
    0016 (permissioned data / spaces).

    Tracks
    {{:https://github.com/bluesky-social/proposals/blob/main/0016-permissioned-data/README.md}0016
    § Commit signature}. The user signs a domain-separated [context]
    (space URI, author DID, rev, [ikm]) — never the repo digest. The
    digest is bound by a symmetric MAC keyed from public [ikm], so a
    leaked commit is deniable.

    [hash] is [Lt_hash.hash] ([sha256] of the 2048-byte LtHash state).
    [mac] is [HMAC-SHA256(HKDF-Expand(ikm, context, 32), hash)] with
    expand-only HKDF-SHA256 (RFC 5869 §2.3; no extract). [sig] is
    ES256 / ES256K over [sha256(context)] (IEEE P1363, low-S), the
    same construction as public repo commits.

    The proposal is not final; this module may change and is {e not} a
    stable spaces API (no credentials, XRPC, sync, or space host). *)
module Space_commit : sig
  val version : int
  (** Commit format version (currently [1]). *)

  val domain_tag : string
  (** Fixed context prefix ([atproto-space-v1]). *)

  type ctx = { space : string; author : string; rev : string }
  (** Unsigned context fields. [space] is a space URI (through [skey]). *)

  type t = {
    ver : int;
    hash : string;
    ikm : string;
    sig : string;
    mac : string;
    rev : string;
  }

  type sig_status =
    [ `Valid | `Invalid | `Unsupported_curve of string | `Missing ]

  type signer =
    [ `P256 of Mirage_crypto_ec.P256.Dsa.priv | `K256 of K256.priv ]

  exception Invalid of string

  val context : ctx:ctx -> ikm:string -> string
  (** TLS 1.3 variable-length-vector encoding of the commit context
      (big-endian [uint16] length prefixes). Does not invent fields. *)

  val hkdf_expand : ikm:string -> info:string -> string
  (** HKDF-SHA256 expand-only (RFC 5869 §2.3): [ikm] is the PRK,
      [info] is the context, output 32 bytes. *)

  val mac : ikm:string -> ctx_bytes:string -> hash:string -> string
  (** [HMAC-SHA256(HKDF-Expand(ikm, ctx_bytes, 32), hash)]. *)

  val random_ikm : unit -> string
  (** 32 fresh random bytes. *)

  val of_lt_hash : ?ikm:string -> ctx:ctx -> sign:signer -> Lt_hash.t -> t
  (** Sign a commit whose [hash] is [Lt_hash.hash]. Generates [ikm]
      when omitted. *)

  val create : ?ikm:string -> ctx:ctx -> sign:signer -> hash:string -> t
  (** Sign a commit over an already-computed 32-byte [hash]. *)

  val matches : Lt_hash.t -> t -> bool
  (** True when [Lt_hash.hash] equals [t.hash]. Does not verify
      signature or MAC. *)

  val verify_mac : ctx:ctx -> t -> bool
  (** Recompute the MAC and compare. *)

  val verify_sig : keys:string list -> ctx:ctx -> t -> sig_status
  (** Verify [sig] over [context] against [did:key] public keys. *)

  val verify : keys:string list -> ctx:ctx -> t -> bool
  (** [ver] is 1, [rev] matches [ctx], MAC and signature both hold. *)

  val encode : t -> string
  (** DAG-CBOR map ([ver], [hash], [ikm], [sig], [mac], [rev]). *)

  val decode : string -> t
  (** Parse a DAG-CBOR signed commit. Raises [Invalid] on a bad map. *)
end = struct
  let version = 1
  let domain_tag = "atproto-space-v1"
  let digest_len = 32

  type ctx = { space : string; author : string; rev : string }

  type t = {
    ver : int;
    hash : string;
    ikm : string;
    sig : string;
    mac : string;
    rev : string;
  }

  type sig_status =
    [ `Valid | `Invalid | `Unsupported_curve of string | `Missing ]

  type signer =
    [ `P256 of Mirage_crypto_ec.P256.Dsa.priv | `K256 of K256.priv ]

  exception Invalid of string

  let fail msg = raise (Invalid msg)

  let hmac_sha256 ~key data =
    Digestif.SHA256.(hmac_string ~key data |> to_raw_string)

  (** HKDF-SHA256 expand-only (RFC 5869 §2.3). [L] is 32, so [N = 1]:
      [T(1) = HMAC-SHA256(ikm, info || 0x01)]. *)
  let hkdf_expand ~ikm ~info : string =
    if String.length ikm <> digest_len then
      fail
        (Printf.sprintf "ikm must be %d bytes, got %d" digest_len
           (String.length ikm));
    hmac_sha256 ~key:ikm (info ^ "\x01")

  let uint16be n =
    if n < 0 || n > 0xFFFF then fail "commit ctx field exceeds uint16 length";
    let b = Bytes.create 2 in
    Bytes.set b 0 (Char.chr ((n lsr 8) land 0xff));
    Bytes.set b 1 (Char.chr (n land 0xff));
    Bytes.to_string b

  let add_field buf s =
    let n = String.length s in
    Buffer.add_string buf (uint16be n);
    Buffer.add_string buf s

  (** TLS 1.3 length-prefixed context from proposal 0016. *)
  let context ~(ctx : ctx) ~ikm : string =
    if String.length ikm > 0xFFFF then
      fail "commit ctx field exceeds uint16 length";
    let buf = Buffer.create 128 in
    Buffer.add_string buf domain_tag;
    add_field buf ctx.space;
    add_field buf ctx.author;
    add_field buf ctx.rev;
    add_field buf ikm;
    Buffer.contents buf

  let mac ~ikm ~ctx_bytes ~hash : string =
    hmac_sha256 ~key:(hkdf_expand ~ikm ~info:ctx_bytes) hash

  let random_bytes n =
    Lazy.force ensure_rng;
    Random.self_init ();
    String.init n (fun _ -> Char.chr (Random.int 256))

  let random_ikm () : string = random_bytes digest_len

  let sign_ctx ~(sign : signer) (ctx_bytes : string) : string =
    let digest = Hash.sha256 ctx_bytes in
    match sign with
    | `P256 priv ->
        Lazy.force ensure_rng;
        let r, s = Mirage_crypto_ec.P256.Dsa.sign ~key:priv digest in
        let s = Did_plc.low_s s in
        r ^ s
    | `K256 priv ->
        let r, s = K256.sign ~key:priv digest in
        r ^ s

  let ensure_space_uri space =
    match Space.of_string space with
    | Space.Space _ -> ()
    | Space.Record _ ->
        fail "commit space must be a space URI (through skey), not a record URI"
    | exception Space.Invalid msg -> fail ("commit space: " ^ msg)

  let ensure_ctx (ctx : ctx) =
    ensure_space_uri ctx.space;
    if not (Syntax.is_valid_did ctx.author) then
      fail ("invalid author DID " ^ ctx.author);
    if ctx.rev = "" then fail "commit rev must be non-empty"

  let ensure_hash hash =
    if String.length hash <> digest_len then
      fail
        (Printf.sprintf "hash must be %d bytes, got %d" digest_len
           (String.length hash))

  let ensure_ikm ikm =
    if String.length ikm <> digest_len then
      fail
        (Printf.sprintf "ikm must be %d bytes, got %d" digest_len
           (String.length ikm))

  let create ?ikm ~(ctx : ctx) ~sign ~hash : t =
    ensure_ctx ctx;
    ensure_hash hash;
    let ikm = match ikm with Some i -> i | None -> random_ikm () in
    ensure_ikm ikm;
    let ctx_bytes = context ~ctx ~ikm in
    {
      ver = version;
      hash;
      ikm;
      sig = sign_ctx ~sign ctx_bytes;
      mac = mac ~ikm ~ctx_bytes ~hash;
      rev = ctx.rev;
    }

  let of_lt_hash ?ikm ~ctx ~sign (h : Lt_hash.t) : t =
    create ?ikm ~ctx ~sign ~hash:(Lt_hash.hash h)

  let matches (h : Lt_hash.t) (c : t) : bool = Lt_hash.hash h = c.hash

  let ctx_bytes_of ~(ctx : ctx) (c : t) = context ~ctx ~ikm:c.ikm

  let verify_mac ~(ctx : ctx) (c : t) : bool =
    String.length c.hash = digest_len
    && String.length c.ikm = digest_len
    && String.length c.mac = digest_len
    && mac ~ikm:c.ikm ~ctx_bytes:(ctx_bytes_of ~ctx c) ~hash:c.hash = c.mac

  let verify_sig ~keys ~(ctx : ctx) (c : t) : sig_status =
    if String.length c.sig <> 64 then `Invalid
    else
      let r = String.sub c.sig 0 32 in
      let s = String.sub c.sig 32 32 in
      let digest = Hash.sha256 (ctx_bytes_of ~ctx c) in
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
            | Some curve -> `Unsupported_curve curve
            | None -> `Invalid)
        | k :: rest -> (
            match k.Did_key.curve with
            | Did_key.P256 -> (
                match Did_key.p256_pub k with
                | Some pub ->
                    if
                      Did_plc.is_low_s s
                      && Mirage_crypto_ec.P256.Dsa.verify ~key:pub (r, s) digest
                    then `Valid
                    else try_keys rest
                | None -> try_keys rest)
            | Did_key.K256 -> (
                match Did_key.k256_pub k with
                | Some pub ->
                    if
                      K256.is_low_s s && K256.verify ~key:pub (r, s) digest
                    then `Valid
                    else try_keys rest
                | None -> try_keys rest)
            | Did_key.Other _ -> try_keys rest)
      in
      try_keys parsed

  let verify ~keys ~(ctx : ctx) (c : t) : bool =
    c.ver = version && c.rev = ctx.rev && verify_mac ~ctx c
    &&
    match verify_sig ~keys ~ctx c with `Valid -> true | _ -> false

  let encode (c : t) : string =
    Dag_cbor.encode
      (Dag_cbor.Map
         [
           ("ver", Dag_cbor.Int c.ver);
           ("hash", Dag_cbor.Bytes c.hash);
           ("ikm", Dag_cbor.Bytes c.ikm);
           ("sig", Dag_cbor.Bytes c.sig);
           ("mac", Dag_cbor.Bytes c.mac);
           ("rev", Dag_cbor.Text c.rev);
         ])

  let decode (raw : string) : t =
    try
      let fields = Dag_cbor.get_map (Dag_cbor.decode raw) in
      let hash = Dag_cbor.as_bytes (Dag_cbor.require "hash" fields) in
      let ikm = Dag_cbor.as_bytes (Dag_cbor.require "ikm" fields) in
      let sig_ = Dag_cbor.as_bytes (Dag_cbor.require "sig" fields) in
      let mac = Dag_cbor.as_bytes (Dag_cbor.require "mac" fields) in
      let rev = Dag_cbor.as_text (Dag_cbor.require "rev" fields) in
      let ver = Dag_cbor.as_int (Dag_cbor.require "ver" fields) in
      if String.length hash <> digest_len then fail "hash must be 32 bytes";
      if String.length ikm <> digest_len then fail "ikm must be 32 bytes";
      if String.length mac <> digest_len then fail "mac must be 32 bytes";
      { ver; hash; ikm; sig = sig_; mac; rev }
    with
    | Invalid _ as e -> raise e
    | Dag_cbor.Decode_error msg -> fail msg
    | _ -> fail "invalid signed commit"
end
