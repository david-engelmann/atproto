(** RFC 4648 base64 and base64url (no padding) used by PLC, DPoP, and WebSocket. *)
module Base64url = struct
  let std_alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

  let url_alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

  let encode_with alphabet ?(pad = true) (data : string) : string =
    let len = String.length data in
    if len = 0 then ""
    else
      let rec loop i bitbuf bits acc =
        if i < len then
          let bitbuf = (bitbuf lsl 8) lor Char.code data.[i] in
          let bits = bits + 8 in
          let rec flush bitbuf bits acc =
            if bits >= 6 then
              let bits = bits - 6 in
              let idx = (bitbuf lsr bits) land 0x3f in
              flush bitbuf bits (acc ^ String.make 1 alphabet.[idx])
            else (bitbuf, bits, acc)
          in
          let bitbuf, bits, acc = flush bitbuf bits acc in
          loop (i + 1) bitbuf bits acc
        else if bits > 0 then
          let acc =
            acc ^ String.make 1 alphabet.[(bitbuf lsl (6 - bits)) land 0x3f]
          in
          if not pad then acc
          else
            let padding = (3 - (len mod 3)) mod 3 in
            acc ^ String.make padding '='
        else if pad then
          let padding = (3 - (len mod 3)) mod 3 in
          acc ^ String.make padding '='
        else acc
      in
      loop 0 0 0 ""

  (** Encode [data] as unpadded base64url. [~pad:true] adds [=]. *)
  let encode ?(pad = false) data = encode_with url_alphabet ~pad data

  (** Encode [data] as standard base64. Pads by default. *)
  let encode_std ?(pad = true) data = encode_with std_alphabet ~pad data

  let value_of alphabet c =
    match c with
    | 'A' .. 'Z' -> Char.code c - Char.code 'A'
    | 'a' .. 'z' -> 26 + (Char.code c - Char.code 'a')
    | '0' .. '9' -> 52 + (Char.code c - Char.code '0')
    | '+' | '-' ->
        if alphabet.[62] = c then 62
        else if alphabet.[63] = c then 63
        else failwith "Base64url.decode: invalid character"
    | '/' | '_' ->
        if alphabet.[62] = c then 62
        else if alphabet.[63] = c then 63
        else failwith "Base64url.decode: invalid character"
    | '=' -> -1
    | _ -> failwith "Base64url.decode: invalid character"

  let decode_with alphabet (s : string) : string =
    let buf = Buffer.create (String.length s * 3 / 4) in
    let rec loop i bitbuf bits =
      if i >= String.length s then ()
      else
        let v = value_of alphabet s.[i] in
        if v < 0 then loop (i + 1) bitbuf bits
        else
          let bitbuf = (bitbuf lsl 6) lor v in
          let bits = bits + 6 in
          if bits >= 8 then (
            let bits = bits - 8 in
            Buffer.add_char buf (Char.chr ((bitbuf lsr bits) land 0xff));
            loop (i + 1) bitbuf bits)
          else loop (i + 1) bitbuf bits
    in
    loop 0 0 0;
    Buffer.contents buf

  (** Decode unpadded or padded base64url; falls back to standard base64. *)
  let decode s =
    try decode_with url_alphabet s
    with Failure _ -> decode_with std_alphabet s

  (** Decode standard base64, ignoring padding. *)
  let decode_std s = decode_with std_alphabet s
end
