(** Base58Check-style base58btc (Bitcoin alphabet, no checksum) for did:key. *)
module Base58 = struct
  let alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

  let encode (data : string) : string =
    let len = String.length data in
    if len = 0 then ""
    else
      let rec count_zeros i =
        if i < len && data.[i] = '\x00' then count_zeros (i + 1) else i
      in
      let leading = count_zeros 0 in
      let size = (len * 138 / 100) + 1 in
      let buf = Array.make size 0 in
      let length = ref 0 in
      for i = leading to len - 1 do
        let carry = ref (Char.code data.[i]) in
        let j = ref 0 in
        let k = ref (size - 1) in
        while !k >= 0 && (!carry <> 0 || !j < !length) do
          carry := !carry + (256 * buf.(!k));
          buf.(!k) <- !carry mod 58;
          carry := !carry / 58;
          incr j;
          decr k
        done;
        length := !j
      done;
      let rec skip i = if i < size && buf.(i) = 0 then skip (i + 1) else i in
      let start = skip 0 in
      let prefix = String.make leading '1' in
      let body =
        String.init (size - start) (fun i -> alphabet.[buf.(start + i)])
      in
      prefix ^ body

  let value_of c =
    match String.index_opt alphabet c with
    | Some i -> i
    | None -> failwith ("Base58.decode: invalid character " ^ String.make 1 c)

  let decode (s : string) : string =
    let len = String.length s in
    if len = 0 then ""
    else
      let rec count_ones i =
        if i < len && s.[i] = '1' then count_ones (i + 1) else i
      in
      let leading = count_ones 0 in
      let size = (len * 733 / 1000) + 1 in
      let buf = Array.make size 0 in
      let length = ref 0 in
      for i = leading to len - 1 do
        let carry = ref (value_of s.[i]) in
        let j = ref 0 in
        let k = ref (size - 1) in
        while !k >= 0 && (!carry <> 0 || !j < !length) do
          carry := !carry + (58 * buf.(!k));
          buf.(!k) <- !carry mod 256;
          carry := !carry / 256;
          incr j;
          decr k
        done;
        length := !j
      done;
      let rec skip i = if i < size && buf.(i) = 0 then skip (i + 1) else i in
      let start = skip 0 in
      let prefix = String.make leading '\x00' in
      let body =
        String.init (size - start) (fun i -> Char.chr buf.(start + i))
      in
      prefix ^ body
end
