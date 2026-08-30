(** Hash helpers used by CID, MST, PLC, DPoP, and WebSocket. *)
module Hash = struct
  let sha256 (data : string) : string =
    Digestif.SHA256.(digest_string data |> to_raw_string)

  let sha1 (data : string) : string =
    Digestif.SHA1.(digest_string data |> to_raw_string)

  let sha256_hex (data : string) : string =
    Digestif.SHA256.(digest_string data |> to_hex)

  let hex_decode (hex : string) : string =
    let len = String.length hex in
    if len mod 2 <> 0 then failwith "Hash.hex_decode: odd length";
    String.init (len / 2) (fun i ->
        Char.chr (int_of_string ("0x" ^ String.sub hex (i * 2) 2)))

  let hex_encode (data : string) : string =
    let buf = Buffer.create (String.length data * 2) in
    String.iter
      (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c)))
      data;
    Buffer.contents buf
end
