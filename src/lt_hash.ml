(** Experimental LtHash set commitment from AT Protocol proposal 0016
    (permissioned data / spaces).

    Tracks
    {{:https://github.com/bluesky-social/proposals/blob/main/0016-permissioned-data/README.md}0016
    § Commit digest}. The proposal is not final; this module may change
    and is {e not} a stable spaces API (no space URI, credentials, XRPC,
    or signed commits).

    State is a 2048-byte buffer of 1024 little-endian [uint16] lanes.
    Each record element is the UTF-8 bytes of
    [{collection}/{rkey}/{record_cid}], expanded with unkeyed BLAKE3 in
    XOF mode to 2048 bytes, then added or subtracted lane-wise modulo
    [2^16]. The commit [hash] is [sha256(state)] (32 bytes). The empty
    repo is all zeroes. Addition and subtraction commute, so the digest
    depends only on the current set. *)
module Lt_hash : sig
  val lanes : int
  (** Number of little-endian unsigned 16-bit lanes. *)

  val state_bytes : int
  (** State size in bytes ([lanes] * 2). *)

  type t

  exception Invalid of string

  val empty : unit -> t
  (** Empty repo: 2048 zero bytes. *)

  val of_state : string -> t
  (** Copy of a persisted 2048-byte state. Raises [Invalid] if [s] is
      the wrong length. *)

  val copy : t -> t
  (** Independent copy of [t]. *)

  val state : t -> string
  (** Current 2048-byte buffer (copy). *)

  val equal : t -> t -> bool
  val is_empty : t -> bool

  val element : collection:string -> rkey:string -> record_cid:string -> string
  (** UTF-8 record identifier [{collection}/{rkey}/{record_cid}] from
      proposal 0016. Components are currently ASCII. *)

  val blake3_xof : string -> int -> string
  (** Unkeyed BLAKE3 XOF (proposal 0016 step 1). Record expansion uses
      [out_len = state_bytes]. *)

  val add : t -> string -> t
  (** New state with [element] added lane-wise modulo [2^16]. [h] is
      unchanged. *)

  val remove : t -> string -> t
  (** New state with [element] subtracted lane-wise modulo [2^16]. [h]
      is unchanged. *)

  val hash : t -> string
  (** Commit digest: [sha256] of the 2048-byte state (32 raw bytes). *)
end = struct
  (** Number of little-endian unsigned 16-bit lanes. *)
  let lanes = 1024

  (** State size in bytes ([lanes] * 2). *)
  let state_bytes = 2048

  type t = { buf : bytes }

  exception Invalid of string

  (** Unkeyed BLAKE3 ({{:https://github.com/BLAKE3-team/BLAKE3}reference
      impl}) used only to expand record elements. digestif has no BLAKE3;
      the [blake3] opam package needs OCaml [< 5.0] and is unusable here. *)
  module Blake3 = struct
    let block_len = 64
    let chunk_len = 1024
    let chunk_start = 1
    let chunk_end = 2
    let parent = 4
    let root = 8

    let iv =
      [|
        0x6A09E667;
        0xBB67AE85;
        0x3C6EF372;
        0xA54FF53A;
        0x510E527F;
        0x9B05688C;
        0x1F83D9AB;
        0x5BE0CD19;
      |]

    let msg_perm = [| 2; 6; 3; 10; 7; 0; 4; 13; 1; 11; 12; 5; 9; 14; 15; 8 |]
    let u32 x = x land 0xFFFF_FFFF
    let add32 a b = u32 (a + b)
    let xor32 a b = u32 (a lxor b)

    let rotr32 x n =
      let x = u32 x in
      u32 ((x lsr n) lor (x lsl (32 - n)))

    let g state a b c d mx my =
      state.(a) <- add32 (add32 state.(a) state.(b)) mx;
      state.(d) <- rotr32 (xor32 state.(d) state.(a)) 16;
      state.(c) <- add32 state.(c) state.(d);
      state.(b) <- rotr32 (xor32 state.(b) state.(c)) 12;
      state.(a) <- add32 (add32 state.(a) state.(b)) my;
      state.(d) <- rotr32 (xor32 state.(d) state.(a)) 8;
      state.(c) <- add32 state.(c) state.(d);
      state.(b) <- rotr32 (xor32 state.(b) state.(c)) 7

    let round state m =
      g state 0 4 8 12 m.(0) m.(1);
      g state 1 5 9 13 m.(2) m.(3);
      g state 2 6 10 14 m.(4) m.(5);
      g state 3 7 11 15 m.(6) m.(7);
      g state 0 5 10 15 m.(8) m.(9);
      g state 1 6 11 12 m.(10) m.(11);
      g state 2 7 8 13 m.(12) m.(13);
      g state 3 4 9 14 m.(14) m.(15)

    let permute m =
      let next = Array.init 16 (fun i -> m.(msg_perm.(i))) in
      Array.blit next 0 m 0 16

    let get_u32_le_bytes b off =
      Char.code (Bytes.get b off)
      lor (Char.code (Bytes.get b (off + 1)) lsl 8)
      lor (Char.code (Bytes.get b (off + 2)) lsl 16)
      lor (Char.code (Bytes.get b (off + 3)) lsl 24)

    let set_u32_le buf off w =
      Bytes.set buf off (Char.chr (w land 0xff));
      Bytes.set buf (off + 1) (Char.chr ((w lsr 8) land 0xff));
      Bytes.set buf (off + 2) (Char.chr ((w lsr 16) land 0xff));
      Bytes.set buf (off + 3) (Char.chr ((w lsr 24) land 0xff))

    let words_of_block s off len =
      let block = Bytes.make block_len '\000' in
      if len > 0 then Bytes.blit_string s off block 0 len;
      Array.init 16 (fun i -> get_u32_le_bytes block (i * 4))

    let compress chaining_value block_words counter block_len flags =
      let counter_low = Int64.to_int (Int64.logand counter 0xFFFF_FFFFL) in
      let counter_high =
        Int64.to_int
          (Int64.logand (Int64.shift_right_logical counter 32) 0xFFFF_FFFFL)
      in
      let state =
        [|
          chaining_value.(0);
          chaining_value.(1);
          chaining_value.(2);
          chaining_value.(3);
          chaining_value.(4);
          chaining_value.(5);
          chaining_value.(6);
          chaining_value.(7);
          iv.(0);
          iv.(1);
          iv.(2);
          iv.(3);
          counter_low;
          counter_high;
          block_len;
          flags;
        |]
      in
      let m = Array.copy block_words in
      for r = 1 to 7 do
        round state m;
        if r < 7 then permute m
      done;
      for i = 0 to 7 do
        state.(i) <- xor32 state.(i) state.(i + 8);
        state.(i + 8) <- xor32 state.(i + 8) chaining_value.(i)
      done;
      state

    type output = {
      cv : int array;
      block : int array;
      counter : int64;
      block_len : int;
      flags : int;
    }

    let first8 words = Array.sub words 0 8

    let chaining_value (out : output) =
      first8 (compress out.cv out.block out.counter out.block_len out.flags)

    let root_output_bytes (out : output) out_len =
      let buf = Bytes.create out_len in
      let rec loop i pos =
        if pos >= out_len then ()
        else
          let words =
            compress out.cv out.block (Int64.of_int i) out.block_len
              (out.flags lor root)
          in
          let rec write w pos =
            if w >= 16 || pos >= out_len then pos
            else
              let remaining = out_len - pos in
              if remaining >= 4 then (
                set_u32_le buf pos words.(w);
                write (w + 1) (pos + 4))
              else
                let tmp = Bytes.create 4 in
                set_u32_le tmp 0 words.(w);
                Bytes.blit tmp 0 buf pos remaining;
                pos + remaining
          in
          loop (i + 1) (write 0 pos)
      in
      loop 0 0;
      Bytes.to_string buf

    let chunk_output ~key_words ~chunk_counter ~flags chunk =
      let len = String.length chunk in
      let rec go cv blocks_compressed off =
        let remaining = len - off in
        if remaining > block_len then
          let block = words_of_block chunk off block_len in
          let start = if blocks_compressed = 0 then chunk_start else 0 in
          let cv =
            first8 (compress cv block chunk_counter block_len (flags lor start))
          in
          go cv (blocks_compressed + 1) (off + block_len)
        else
          let block = words_of_block chunk off remaining in
          let start = if blocks_compressed = 0 then chunk_start else 0 in
          {
            cv;
            block;
            counter = chunk_counter;
            block_len = remaining;
            flags = flags lor start lor chunk_end;
          }
      in
      go (Array.copy key_words) 0 0

    let parent_output left_cv right_cv =
      {
        cv = Array.copy iv;
        block = Array.append left_cv right_cv;
        counter = 0L;
        block_len;
        flags = parent;
      }

    let add_chunk_cv stack new_cv total_chunks =
      let rec loop stack new_cv total =
        if Int64.logand total 1L = 0L then
          match stack with
          | left :: rest ->
              loop rest
                (chaining_value (parent_output left new_cv))
                (Int64.shift_right_logical total 1)
          | [] -> failwith "Lt_hash.Blake3: empty CV stack"
        else new_cv :: stack
      in
      loop stack new_cv total_chunks

    let rec fold_parents stack output =
      match stack with
      | [] -> output
      | left :: rest ->
          fold_parents rest (parent_output left (chaining_value output))

    let xof (input : string) (out_len : int) : string =
      if out_len < 0 then invalid_arg "Lt_hash.blake3_xof: negative length";
      let rec go stack chunk_counter off =
        let remaining = String.length input - off in
        if remaining > chunk_len then
          let chunk = String.sub input off chunk_len in
          let cv =
            chaining_value
              (chunk_output ~key_words:iv ~chunk_counter ~flags:0 chunk)
          in
          let total = Int64.succ chunk_counter in
          let stack = add_chunk_cv stack cv total in
          go stack total (off + chunk_len)
        else
          let chunk = String.sub input off remaining in
          let output =
            chunk_output ~key_words:iv ~chunk_counter ~flags:0 chunk
          in
          root_output_bytes (fold_parents stack output) out_len
      in
      go [] 0L 0
  end

  let get_u16_le s off = Char.code s.[off] lor (Char.code s.[off + 1] lsl 8)

  let get_u16_le_bytes b off =
    Char.code (Bytes.get b off) lor (Char.code (Bytes.get b (off + 1)) lsl 8)

  let set_u16_le buf off n =
    Bytes.set buf off (Char.chr (n land 0xff));
    Bytes.set buf (off + 1) (Char.chr ((n lsr 8) land 0xff))

  (** Empty repo: 2048 zero bytes. *)
  let empty () : t = { buf = Bytes.make state_bytes '\000' }

  (** Copy of the 2048-byte state. Fails if [s] is the wrong length. *)
  let of_state (s : string) : t =
    if String.length s <> state_bytes then
      raise
        (Invalid
           (Printf.sprintf "Lt_hash state must be %d bytes, got %d" state_bytes
              (String.length s)));
    { buf = Bytes.of_string s }

  (** Independent copy of [t]. *)
  let copy (h : t) : t = { buf = Bytes.copy h.buf }

  (** Current 2048-byte buffer (copy). *)
  let state (h : t) : string = Bytes.to_string h.buf

  let equal (a : t) (b : t) : bool = Bytes.equal a.buf b.buf

  let is_empty (h : t) : bool =
    let rec loop i =
      if i >= state_bytes then true
      else if Bytes.get h.buf i <> '\000' then false
      else loop (i + 1)
    in
    loop 0

  (** UTF-8 record identifier [{collection}/{rkey}/{record_cid}] from
      proposal 0016. Components are currently ASCII, so this is a
      concatenation. *)
  let element ~collection ~rkey ~record_cid : string =
    collection ^ "/" ^ rkey ^ "/" ^ record_cid

  (** Unkeyed BLAKE3 XOF (proposal 0016 step 1). Record expansion uses
      [out_len = state_bytes]. *)
  let blake3_xof (input : string) (out_len : int) : string =
    Blake3.xof input out_len

  let expand (element : string) : string = blake3_xof element state_bytes

  let apply (h : t) (element : string) op : t =
    let expanded = expand element in
    let buf = Bytes.copy h.buf in
    for i = 0 to lanes - 1 do
      let off = i * 2 in
      let lane = get_u16_le_bytes buf off in
      let contrib = get_u16_le expanded off in
      set_u16_le buf off (op lane contrib land 0xFFFF)
    done;
    { buf }

  (** New state with [element] added lane-wise modulo [2^16]. [h] is
      unchanged. *)
  let add (h : t) (element : string) : t = apply h element ( + )

  (** New state with [element] subtracted lane-wise modulo [2^16]. [h]
      is unchanged. *)
  let remove (h : t) (element : string) : t = apply h element ( - )

  (** Commit digest: [sha256] of the 2048-byte state (32 raw bytes). *)
  let hash (h : t) : string =
    Digestif.SHA256.(digest_string (Bytes.to_string h.buf) |> to_raw_string)
end
