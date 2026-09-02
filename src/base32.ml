(** RFC 4648 base32 (lowercase, no padding) as used by multibase CIDs. *)
module Base32 = struct
  let alphabet = "abcdefghijklmnopqrstuvwxyz234567"

  (** RFC 4648 base32 encode (lowercase, no padding). *)
  let encode (data : string) : string =
    let len = String.length data in
    if len = 0 then ""
    else
      let out_len = ((len * 8) + 4) / 5 in
      let buf = Bytes.create out_len in
      let rec loop i bitbuf bits written =
        if i < len then
          let bitbuf = (bitbuf lsl 8) lor Char.code data.[i] in
          let bits = bits + 8 in
          let rec flush bitbuf bits written =
            if bits >= 5 then (
              let bits = bits - 5 in
              let idx = (bitbuf lsr bits) land 0x1f in
              Bytes.set buf written alphabet.[idx];
              flush bitbuf bits (written + 1))
            else (bitbuf, bits, written)
          in
          let bitbuf, bits, written = flush bitbuf bits written in
          loop (i + 1) bitbuf bits written
        else if bits > 0 then (
          let idx = (bitbuf lsl (5 - bits)) land 0x1f in
          Bytes.set buf written alphabet.[idx];
          written + 1)
        else written
      in
      let written = loop 0 0 0 0 in
      Bytes.sub_string buf 0 written

  let value_of_char c =
    match c with
    | 'a' .. 'z' -> Char.code c - Char.code 'a'
    | 'A' .. 'Z' -> Char.code c - Char.code 'A'
    | '2' .. '7' -> 26 + (Char.code c - Char.code '2')
    | '=' -> -1
    | _ -> failwith ("Base32.decode: invalid character " ^ String.make 1 c)

  (** RFC 4648 base32 decode (case-insensitive; skips [=] padding). *)
  let decode (s : string) : string =
    let len = String.length s in
    let buf = Buffer.create (len * 5 / 8) in
    let rec loop i bitbuf bits =
      if i >= len then ()
      else
        let v = value_of_char s.[i] in
        if v < 0 then loop (i + 1) bitbuf bits
        else
          let bitbuf = (bitbuf lsl 5) lor v in
          let bits = bits + 5 in
          if bits >= 8 then (
            let bits = bits - 8 in
            Buffer.add_char buf (Char.chr ((bitbuf lsr bits) land 0xff));
            loop (i + 1) bitbuf bits)
          else loop (i + 1) bitbuf bits
    in
    loop 0 0 0;
    Buffer.contents buf
end
