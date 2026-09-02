open Cid

(** Minimal DAG-CBOR (DRISL) codec for CAR headers and firehose frames. *)
module Dag_cbor = struct
  type value =
    | Null
    | Bool of bool
    | Int of int
    | Int64 of int64
    | Bytes of string
    | Text of string
    | Array of value list
    | Map of (string * value) list
    | Tag of int * value
    | Cid of Cid.t

  exception Decode_error of string

  let fail msg = raise (Decode_error msg)

  (** Encode [v] as DAG-CBOR (map keys sorted by encoded CBOR bytes). *)
  let rec encode (v : value) : string =
    match v with
    | Null -> "\xF6"
    | Bool false -> "\xF4"
    | Bool true -> "\xF5"
    | Int n -> encode_int (Int64.of_int n)
    | Int64 n -> encode_int n
    | Bytes b -> encode_bytes 2 b
    | Text t -> encode_bytes 3 t
    | Array items ->
        let buf = Buffer.create 16 in
        Buffer.add_string buf (encode_head 4 (List.length items));
        List.iter (fun x -> Buffer.add_string buf (encode x)) items;
        Buffer.contents buf
    | Map fields ->
        (* IPLD DAG-CBOR: keys sort by their encoded CBOR bytes, not
           lexicographic string order. Shorter keys therefore come first. *)
        let key_bytes k = encode_bytes 3 k in
        let sorted =
          List.sort
            (fun (a, _) (b, _) -> String.compare (key_bytes a) (key_bytes b))
            fields
        in
        let buf = Buffer.create 16 in
        Buffer.add_string buf (encode_head 5 (List.length sorted));
        List.iter
          (fun (k, x) ->
            Buffer.add_string buf (encode (Text k));
            Buffer.add_string buf (encode x))
          sorted;
        Buffer.contents buf
    | Tag (n, inner) -> encode_head 6 n ^ encode inner
    | Cid c ->
        let binary = "\x00" ^ Cid.to_bytes c in
        encode_head 6 42 ^ encode (Bytes binary)

  and encode_head major n =
    let major = (major land 0x7) lsl 5 in
    if n < 24 then String.make 1 (Char.chr (major lor n))
    else if n < 256 then (
      let b = Bytes.create 2 in
      Bytes.set b 0 (Char.chr (major lor 24));
      Bytes.set b 1 (Char.chr n);
      Bytes.to_string b)
    else if n < 65536 then (
      let b = Bytes.create 3 in
      Bytes.set b 0 (Char.chr (major lor 25));
      Bytes.set b 1 (Char.chr ((n lsr 8) land 0xff));
      Bytes.set b 2 (Char.chr (n land 0xff));
      Bytes.to_string b)
    else
      let b = Bytes.create 5 in
      Bytes.set b 0 (Char.chr (major lor 26));
      Bytes.set b 1 (Char.chr ((n lsr 24) land 0xff));
      Bytes.set b 2 (Char.chr ((n lsr 16) land 0xff));
      Bytes.set b 3 (Char.chr ((n lsr 8) land 0xff));
      Bytes.set b 4 (Char.chr (n land 0xff));
      Bytes.to_string b

  and encode_bytes major s = encode_head major (String.length s) ^ s

  and encode_int (n : int64) : string =
    if Int64.compare n 0L >= 0 then encode_uint 0 n
    else encode_uint 1 (Int64.sub (Int64.neg n) 1L)

  and encode_uint major (n : int64) : string =
    if Int64.compare n 24L < 0 then
      String.make 1 (Char.chr ((major lsl 5) lor Int64.to_int n))
    else if Int64.compare n 256L < 0 then (
      let b = Bytes.create 2 in
      Bytes.set b 0 (Char.chr ((major lsl 5) lor 24));
      Bytes.set b 1 (Char.chr (Int64.to_int n));
      Bytes.to_string b)
    else if Int64.compare n 65536L < 0 then (
      let v = Int64.to_int n in
      let b = Bytes.create 3 in
      Bytes.set b 0 (Char.chr ((major lsl 5) lor 25));
      Bytes.set b 1 (Char.chr ((v lsr 8) land 0xff));
      Bytes.set b 2 (Char.chr (v land 0xff));
      Bytes.to_string b)
    else if Int64.compare n 0x1_0000_0000L < 0 then (
      let v = Int64.to_int n in
      let b = Bytes.create 5 in
      Bytes.set b 0 (Char.chr ((major lsl 5) lor 26));
      Bytes.set b 1 (Char.chr ((v lsr 24) land 0xff));
      Bytes.set b 2 (Char.chr ((v lsr 16) land 0xff));
      Bytes.set b 3 (Char.chr ((v lsr 8) land 0xff));
      Bytes.set b 4 (Char.chr (v land 0xff));
      Bytes.to_string b)
    else
      let b = Bytes.create 9 in
      Bytes.set b 0 (Char.chr ((major lsl 5) lor 27));
      let rec put i shift =
        if i > 8 then ()
        else (
          Bytes.set b i
            (Char.chr
               (Int64.to_int
                  (Int64.logand (Int64.shift_right_logical n shift) 0xffL)));
          put (i + 1) (shift - 8))
      in
      put 1 56;
      Bytes.to_string b

  let rec decode_from (s : string) (off : int) : value * int =
    if off >= String.length s then fail "truncated";
    let b = Char.code s.[off] in
    let major = b lsr 5 in
    let addl = b land 0x1f in
    let read_len addl i =
      if addl < 24 then (addl, i)
      else if addl = 24 then (
        if i >= String.length s then fail "truncated arg";
        (Char.code s.[i], i + 1))
      else if addl = 25 then (
        if i + 1 >= String.length s then fail "truncated arg";
        ((Char.code s.[i] lsl 8) lor Char.code s.[i + 1], i + 2))
      else if addl = 26 then (
        if i + 3 >= String.length s then fail "truncated arg";
        let n =
          (Char.code s.[i] lsl 24)
          lor (Char.code s.[i + 1] lsl 16)
          lor (Char.code s.[i + 2] lsl 8)
          lor Char.code s.[i + 3]
        in
        (n, i + 4))
      else if addl = 27 then (
        if i + 7 >= String.length s then fail "truncated arg";
        let rec acc k n =
          if k = 8 then n else acc (k + 1) ((n lsl 8) lor Char.code s.[i + k])
        in
        (acc 0 0, i + 8))
      else fail "indefinite lengths are not allowed in DAG-CBOR"
    in
    match major with
    | 0 ->
        if addl = 27 then
          let n, i = read_int64 s (off + 1) in
          (Int64 n, i)
        else
          let n, i = read_len addl (off + 1) in
          (Int n, i)
    | 1 ->
        if addl = 27 then
          let n, i = read_int64 s (off + 1) in
          (Int64 (Int64.sub (Int64.neg n) 1L), i)
        else
          let n, i = read_len addl (off + 1) in
          (Int (-1 - n), i)
    | 2 ->
        let len, i = read_len addl (off + 1) in
        if i + len > String.length s then fail "truncated bytes";
        (Bytes (String.sub s i len), i + len)
    | 3 ->
        let len, i = read_len addl (off + 1) in
        if i + len > String.length s then fail "truncated text";
        (Text (String.sub s i len), i + len)
    | 4 ->
        let count, i = read_len addl (off + 1) in
        let rec take n i acc =
          if n = 0 then (List.rev acc, i)
          else
            let v, i = decode_from s i in
            take (n - 1) i (v :: acc)
        in
        let items, i = take count i [] in
        (Array items, i)
    | 5 ->
        let count, i = read_len addl (off + 1) in
        let rec take n i acc =
          if n = 0 then (List.rev acc, i)
          else
            let k, i = decode_from s i in
            let v, i = decode_from s i in
            let key =
              match k with
              | Text t -> t
              | _ -> fail "DAG-CBOR map keys must be strings"
            in
            take (n - 1) i ((key, v) :: acc)
        in
        let fields, i = take count i [] in
        (Map fields, i)
    | 6 ->
        let tag, i = read_len addl (off + 1) in
        let inner, i = decode_from s i in
        if tag = 42 then
          match inner with
          | Bytes raw ->
              if String.length raw < 2 || raw.[0] <> '\x00' then
                fail "CID tag 42 must be identity-multibase bytes";
              let cid =
                Cid.of_bytes (String.sub raw 1 (String.length raw - 1))
              in
              (Cid cid, i)
          | _ -> fail "CID tag 42 payload must be bytes"
        else (Tag (tag, inner), i)
    | 7 -> (
        match addl with
        | 20 -> (Bool false, off + 1)
        | 21 -> (Bool true, off + 1)
        | 22 -> (Null, off + 1)
        | _ ->
            fail
              (Printf.sprintf "unsupported simple/float CBOR additional %d" addl)
        )
    | _ -> fail "unknown CBOR major type"

  and read_int64 s i =
    if i + 7 >= String.length s then fail "truncated int64";
    let rec acc k n =
      if k = 8 then n
      else
        acc (k + 1)
          (Int64.logor (Int64.shift_left n 8)
             (Int64.of_int (Char.code s.[i + k])))
    in
    (acc 0 0L, i + 8)

  (** Decode a single DAG-CBOR value. Fails on trailing bytes. *)
  let decode (s : string) : value =
    let v, i = decode_from s 0 in
    if i <> String.length s then fail "trailing bytes";
    v

  (** Decode concatenated DAG-CBOR values (firehose header + body). *)
  let decode_sequence (s : string) : value list =
    let rec loop i acc =
      if i >= String.length s then List.rev acc
      else
        let v, i = decode_from s i in
        loop i (v :: acc)
    in
    loop 0 []

  (** Unwrap a [Map]; raise [Decode_error] otherwise. *)
  let get_map (v : value) : (string * value) list =
    match v with Map m -> m | _ -> fail "expected map"

  (** Optional map field [key]. *)
  let find key fields =
    try Some (List.assoc key fields) with Not_found -> None

  (** Required map field [key]; raise [Decode_error] if missing. *)
  let require key fields =
    match find key fields with
    | Some v -> v
    | None -> fail ("missing field " ^ key)

  let as_text = function Text t -> t | _ -> fail "expected text"

  let as_int = function
    | Int n -> n
    | Int64 n -> Int64.to_int n
    | _ -> fail "expected int"

  let as_int64 = function
    | Int n -> Int64.of_int n
    | Int64 n -> n
    | _ -> fail "expected int64"

  let as_bool = function Bool b -> b | _ -> fail "expected bool"
  let as_bytes = function Bytes b -> b | _ -> fail "expected bytes"
  let as_array = function Array a -> a | _ -> fail "expected array"

  (** Unwrap a [Cid] (or a text CID string). *)
  let as_cid = function
    | Cid c -> c
    | Text t -> Cid.of_string t
    | _ -> fail "expected CID"

  let as_text_opt = function
    | Null -> None
    | Text t -> Some t
    | _ -> fail "expected text or null"

  let as_cid_opt = function Null -> None | v -> Some (as_cid v)

  (** IPLD JSON to DAG-CBOR. Official [$link] / [$bytes] objects become
      [Cid] and [Bytes]. *)
  let rec of_yojson (json : Yojson.Safe.t) : value =
    match json with
    | `Null -> Null
    | `Bool b -> Bool b
    | `Int n -> Int n
    | `Intlit s -> (
        try
          let n = Int64.of_string s in
          if n >= Int64.of_int min_int && n <= Int64.of_int max_int then
            Int (Int64.to_int n)
          else Int64 n
        with _ -> Text s)
    | `Float _ -> fail "DAG-CBOR does not encode IEEE floats"
    | `String s -> Text s
    | `List xs -> Array (List.map of_yojson xs)
    | `Assoc fields -> (
        match fields with
        | [ ("$link", `String link) ] -> Cid (Cid.of_string link)
        | [ ("$bytes", `String b64) ] -> Bytes (Base64url.Base64url.decode b64)
        | _ -> Map (List.map (fun (k, v) -> (k, of_yojson v)) fields))
end
