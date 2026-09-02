(** Timestamp Identifiers (TIDs) — https://atproto.com/specs/tid *)
module Tid = struct
  (** Crockford-style base32 alphabet used by TIDs (no [0], [1], or [l]). *)
  let alphabet = "234567abcdefghijklmnopqrstuvwxyz"

  (** First-character alphabet: keeps the TID's high bit zero. *)
  let first_ok = "234567abcdefghij"

  (** The all-zero TID ([2222222222222]). *)
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

  (** True when [s] is a 13-character TID (base32, top bit zero). *)
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

  (** Encode the low 63 bits of [v] as a 13-character TID. *)
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

  (** Decode a 13-character TID to its 64-bit value. Fails if [s] is not
      a valid TID or the high bit is set. *)
  let to_int64 (s : string) : int64 =
    if String.length s <> 13 then fail "TID must be 13 characters";
    let rec loop i acc =
      if i >= 13 then acc
      else
        match index_of alphabet s.[i] with
        | None -> fail ("TID has invalid character " ^ String.make 1 s.[i])
        | Some n ->
            loop (i + 1) (Int64.logor (Int64.shift_left acc 5) (Int64.of_int n))
    in
    let v = loop 0 0L in
    if Int64.logand v 0x8000_0000_0000_0000L <> 0L then
      fail "TID top bit must be 0";
    v

  (** Validate [s] as a TID and return it. Raises [Invalid] otherwise. *)
  let of_string (s : string) : string =
    if not (is_valid s) then fail ("invalid TID " ^ s);
    s

  (** Microseconds since the Unix epoch encoded in [s] (high 53 bits). *)
  let timestamp_us (s : string) : int64 =
    Int64.shift_right_logical (to_int64 s) 10

  (** 10-bit clock identifier encoded in [s] (low 10 bits). *)
  let clock_id (s : string) : int =
    Int64.to_int (Int64.logand (to_int64 s) 0x3FFL)

  (** Build a TID from [timestamp_us] and optional [clock_id] (0-1023).
      Used as record keys ([rkey]) and repo commit [rev]s. *)
  let create ?(clock_id = 0) (timestamp_us : int64) : string =
    let ts = Int64.logand timestamp_us 0x1F_FFFF_FFFF_FFFFL in
    let clk = Int64.logand (Int64.of_int clock_id) 0x3FFL in
    of_int64 (Int64.logor (Int64.shift_left ts 10) clk)

  (** TID for the current wall-clock time. [clock_id] defaults to a
      random 10-bit value. *)
  let now ?(clock_id = Random.int 1024) () : string =
    let us = Int64.of_float (Unix.gettimeofday () *. 1_000_000.) in
    create ~clock_id us

  (* Sync spec: reject commit revs corresponding to a future timestamp
     beyond a short clock-drift window (default 5 minutes). *)
  (** Default clock-skew window for {!is_future} (5 minutes, in microseconds). *)
  let default_clock_skew_us = 300_000_000L

  (** True when [s] is a valid TID whose timestamp is more than [skew_us]
      ahead of [now_us] (or wall-clock now). Sync rejects future commit
      [rev]s beyond this drift window. *)
  let is_future ?(now_us : int64 option) ?(skew_us = default_clock_skew_us)
      (s : string) : bool =
    if not (is_valid s) then false
    else
      let now =
        match now_us with
        | Some n -> n
        | None -> Int64.of_float (Unix.gettimeofday () *. 1_000_000.)
      in
      Int64.compare (timestamp_us s) (Int64.add now skew_us) > 0
end
