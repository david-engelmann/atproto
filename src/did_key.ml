open Base58
open Varint
open K256

(** did:key encoding for AT Protocol rotation and signing keys (p256 / k256). *)
module Did_key = struct
  type curve = P256 | K256 | Other of int
  type t = { curve : curve; public_key : string }

  let p256_code = 0x1200
  let k256_code = 0xe7

  let curve_name = function
    | P256 -> "p256"
    | K256 -> "k256"
    | Other n -> Printf.sprintf "0x%x" n

  let is_did_key (s : string) : bool =
    String.length s > 8 && String.sub s 0 8 = "did:key:"

  let of_string (s : string) : t =
    if not (is_did_key s) then failwith "Did_key.of_string: not a did:key";
    let rest = String.sub s 8 (String.length s - 8) in
    if String.length rest < 2 || (rest.[0] <> 'z' && rest.[0] <> 'Z') then
      failwith "Did_key.of_string: expected multibase base58btc (z...)";
    let raw = Base58.decode (String.sub rest 1 (String.length rest - 1)) in
    let code, i = Varint.decode raw in
    let public_key = String.sub raw i (String.length raw - i) in
    let curve =
      if code = p256_code then P256
      else if code = k256_code then K256
      else Other code
    in
    { curve; public_key }

  let to_string (k : t) : string =
    let code =
      match k.curve with P256 -> p256_code | K256 -> k256_code | Other n -> n
    in
    "did:key:z" ^ Base58.encode (Varint.encode code ^ k.public_key)

  let of_p256_octets (public_key : string) : t = { curve = P256; public_key }
  let of_k256_octets (public_key : string) : t = { curve = K256; public_key }

  let p256_pub (k : t) : Mirage_crypto_ec.P256.Dsa.pub option =
    if k.curve <> P256 then None
    else
      match Mirage_crypto_ec.P256.Dsa.pub_of_octets k.public_key with
      | Ok pub -> Some pub
      | Error _ -> None

  let k256_pub (k : t) : K256.pub option =
    if k.curve <> K256 then None
    else
      match K256.pub_of_octets k.public_key with
      | Ok pub -> Some pub
      | Error _ -> None
end
