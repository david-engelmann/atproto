(** Timestamp Identifiers (TIDs) — https://atproto.com/specs/tid *)
module Tid = struct
  let alphabet = "234567abcdefghijklmnopqrstuvwxyz"
  let first_ok = "234567abcdefghij"
  let zero = "2222222222222"

  exception Invalid of string

  let fail msg = raise (Invalid msg)

  let index_of alphabet c =
    let rec loop i =
      if i >= String.length alphabet then None
      else if alphabet.[i] = c then Some i
      else loop (i + 1)
    in
    loop 0

  let is_valid (s : string) : bool =
    String.length s = 13
    &&
    match index_of first_ok s.[0] with
    | None -> false
    | Some _ ->
        let rec loop i =
          if i >= 13 then true
          else
            match index_of alphabet s.[i] with
            | None -> false
            | Some _ -> loop (i + 1)
        in
        loop 1

  let of_int64 (v : int64) : string =
    let v = Int64.logand v 0x7FFF_FFFF_FFFF_FFFFL in
    let buf = Bytes.create 13 in
    let rec loop i n =
      if i < 0 then ()
      else (
        Bytes.set buf i alphabet.[Int64.to_int (Int64.logand n 0x1FL)];
        loop (i - 1) (Int64.shift_right_logical n 5))
    in
    loop 12 v;
    Bytes.to_string buf

  let to_int64 (s : string) : int64 =
    if String.length s <> 13 then fail "TID must be 13 characters";
    let rec loop i acc =
      if i >= 13 then acc
      else
        match index_of alphabet s.[i] with
        | None -> fail ("TID has invalid character " ^ String.make 1 s.[i])
        | Some n ->
            loop (i + 1)
              (Int64.logor (Int64.shift_left acc 5) (Int64.of_int n))
    in
    let v = loop 0 0L in
    if Int64.logand v 0x8000_0000_0000_0000L <> 0L then
      fail "TID top bit must be 0";
    v

  let of_string (s : string) : string =
    if not (is_valid s) then fail ("invalid TID " ^ s);
    s

  let timestamp_us (s : string) : int64 =
    Int64.shift_right_logical (to_int64 s) 10

  let clock_id (s : string) : int = Int64.to_int (Int64.logand (to_int64 s) 0x3FFL)

  let create ?(clock_id = 0) (timestamp_us : int64) : string =
    let ts = Int64.logand timestamp_us 0x1F_FFFF_FFFF_FFFFL in
    let clk = Int64.logand (Int64.of_int clock_id) 0x3FFL in
    of_int64 (Int64.logor (Int64.shift_left ts 10) clk)

  let now ?(clock_id = Random.int 1024) () : string =
    let us = Int64.of_float (Unix.gettimeofday () *. 1_000_000.) in
    create ~clock_id us
end
