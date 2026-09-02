(** Hash helpers used by CID, MST, PLC, DPoP, and WebSocket. *)
module Hash = struct
  (** SHA-256 digest of [data] (raw 32 bytes). *)
  let sha256 (data : string) : string =
    Digestif.SHA256.(digest_string data |> to_raw_string)

  (** SHA-1 digest of [data] (raw 20 bytes). *)
  let sha1 (data : string) : string =
    Digestif.SHA1.(digest_string data |> to_raw_string)

  (** SHA-256 digest of [data] as lowercase hex. *)
  let sha256_hex (data : string) : string =
    Digestif.SHA256.(digest_string data |> to_hex)

  (** Decode lowercase/uppercase hex to raw bytes. *)
  let hex_decode (hex : string) : string =
    let len = String.length hex in
    if len mod 2 <> 0 then failwith "Hash.hex_decode: odd length";
    String.init (len / 2) (fun i ->
        Char.chr (int_of_string ("0x" ^ String.sub hex (i * 2) 2)))

  (** Encode raw bytes as lowercase hex. *)
  let hex_encode (data : string) : string =
    let buf = Buffer.create (String.length data * 2) in
    String.iter
      (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c)))
      data;
    Buffer.contents buf
end
