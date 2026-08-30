(** Unsigned LEB128 / multiformat varints used by CID, CAR, and DAG-CBOR. *)
module Varint = struct
  let encode (n : int) : string =
    if n < 0 then invalid_arg "Varint.encode: negative";
    let buf = Buffer.create 4 in
    let rec loop x =
      let byte = x land 0x7f in
      let next = x lsr 7 in
      if next = 0 then Buffer.add_char buf (Char.chr byte)
      else (
        Buffer.add_char buf (Char.chr (byte lor 0x80));
        loop next)
    in
    loop n;
    Buffer.contents buf

  let decode_from (s : string) (off : int) : int * int =
    let rec loop acc shift i =
      if i >= String.length s then failwith "Varint.decode: truncated";
      let byte = Char.code s.[i] in
      let acc = acc lor ((byte land 0x7f) lsl shift) in
      if byte land 0x80 = 0 then (acc, i + 1)
      else if shift >= 28 then failwith "Varint.decode: overflow"
      else loop acc (shift + 7) (i + 1)
    in
    loop 0 0 off

  let decode (s : string) : int * int = decode_from s 0
end
