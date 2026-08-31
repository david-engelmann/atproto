open Varint
open Base32

(** CIDv1 as used by AT Protocol (dag-cbor / raw + sha2-256, multibase base32). *)
module Cid = struct
  type codec = Dag_cbor | Raw | Other of int
  type t = { version : int; codec : codec; hash_code : int; digest : string }

  let codec_to_int = function Dag_cbor -> 0x71 | Raw -> 0x55 | Other n -> n
  let codec_of_int = function 0x71 -> Dag_cbor | 0x55 -> Raw | n -> Other n

  let codec_name = function
    | Dag_cbor -> "dag-cbor"
    | Raw -> "raw"
    | Other n -> Printf.sprintf "0x%x" n

  let sha2_256 = 0x12

  let to_bytes (c : t) : string =
    let buf = Buffer.create (4 + String.length c.digest) in
    Buffer.add_string buf (Varint.encode c.version);
    Buffer.add_string buf (Varint.encode (codec_to_int c.codec));
    Buffer.add_string buf (Varint.encode c.hash_code);
    Buffer.add_string buf (Varint.encode (String.length c.digest));
    Buffer.add_string buf c.digest;
    Buffer.contents buf

  let of_bytes_from (s : string) (off : int) : t * int =
    let version, i = Varint.decode_from s off in
    let codec_i, i = Varint.decode_from s i in
    let hash_code, i = Varint.decode_from s i in
    let digest_len, i = Varint.decode_from s i in
    if i + digest_len > String.length s then
      failwith "Cid.of_bytes: truncated digest";
    let digest = String.sub s i digest_len in
    ( { version; codec = codec_of_int codec_i; hash_code; digest },
      i + digest_len )

  let of_bytes (s : string) : t =
    let cid, consumed = of_bytes_from s 0 in
    if consumed <> String.length s then failwith "Cid.of_bytes: trailing bytes";
    cid

  let to_string (c : t) : string = "b" ^ Base32.encode (to_bytes c)

  let of_string (s : string) : t =
    if String.length s < 2 then failwith "Cid.of_string: empty";
    match s.[0] with
    | 'b' | 'B' ->
        of_bytes (Base32.decode (String.sub s 1 (String.length s - 1)))
    | _ -> failwith "Cid.of_string: only multibase base32 (b...) is supported"

  let is_cid (s : string) : bool =
    try
      ignore (of_string s);
      true
    with _ -> false

  let equal a b =
    a.version = b.version
    && codec_to_int a.codec = codec_to_int b.codec
    && a.hash_code = b.hash_code
    && String.equal a.digest b.digest

  let of_digest ?(version = 1) ?(codec = Dag_cbor) ?(hash_code = sha2_256)
      (digest : string) : t =
    { version; codec; hash_code; digest }

  let sha256 (data : string) : string =
    Digestif.SHA256.(digest_string data |> to_raw_string)

  let create ?(codec = Dag_cbor) (data : string) : t =
    of_digest ~codec (sha256 data)

  (* Blobs are raw + SHA-256; repo records / MST nodes are dag-cbor. *)
  let of_blob (bytes : string) : t = create ~codec:Raw bytes

  let verify_blob ?(expected : t option) (bytes : string) : t =
    let got = of_blob bytes in
    match expected with
    | None -> got
    | Some cid ->
        if equal cid got then got
        else
          failwith
            (Printf.sprintf "Cid.verify_blob: expected %s got %s"
               (to_string cid) (to_string got))

  let verify_block ?(expected : t option) ?(codec = Dag_cbor) (bytes : string) :
      t =
    let got = create ~codec bytes in
    match expected with
    | None -> got
    | Some cid ->
        if equal cid got then got
        else
          failwith
            (Printf.sprintf "Cid.verify_block: expected %s got %s"
               (to_string cid) (to_string got))

  (* Data-model "blessed" CID: CIDv1 + sha2-256 (32 bytes) + dag-cbor or raw.
     MST/commit links must be dag-cbor; record leaves may be either. *)
  let is_blessed ?(codec : codec option) (c : t) : bool =
    c.version = 1 && c.hash_code = sha2_256
    && String.length c.digest = 32
    &&
    match codec with
    | Some want -> c.codec = want
    | None -> ( match c.codec with Dag_cbor | Raw -> true | Other _ -> false)

  let ensure_blessed ?(codec : codec option) (c : t) : unit =
    if not (is_blessed ?codec c) then
      failwith
        (Printf.sprintf "Cid.ensure_blessed: %s is not a blessed atproto CID"
           (to_string c))
end
