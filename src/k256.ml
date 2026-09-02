(** secp256k1 ECDSA (IEEE P1363 r||s) used by PLC k256 / did:key. *)
module K256 = struct
  type priv = { d : Z.t }
  type pub = { x : Z.t; y : Z.t }
  type error = [ `Invalid_key | `Invalid_signature ]

  let p =
    Z.of_string_base 16
      "fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f"

  let n =
    Z.of_string_base 16
      "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141"

  let n_half = Z.shift_right n 1

  let gx =
    Z.of_string_base 16
      "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"

  let gy =
    Z.of_string_base 16
      "483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8"

  let g = { x = gx; y = gy }
  let z0 = Z.zero
  let z1 = Z.one
  let z2 = Z.of_int 2
  let z3 = Z.of_int 3

  let be_of_z ?(len = 32) z =
    let raw = Z.to_bits z in
    let out = Bytes.make len '\x00' in
    let n = min (String.length raw) len in
    for i = 0 to n - 1 do
      Bytes.set out (len - 1 - i) raw.[i]
    done;
    Bytes.to_string out

  let z_of_be s =
    let buf = Bytes.make (String.length s) '\x00' in
    for i = 0 to String.length s - 1 do
      Bytes.set buf i s.[String.length s - 1 - i]
    done;
    Z.of_bits (Bytes.to_string buf)

  (** 32-byte big-endian secp256k1 curve order [n]. *)
  let n_octets = be_of_z n

  (** 32-byte big-endian [n/2], the low-S threshold. *)
  let n_half_octets = be_of_z n_half

  let sub_be a b =
    let out = Bytes.create (String.length a) in
    let borrow = ref 0 in
    for i = String.length a - 1 downto 0 do
      let d = Char.code a.[i] - Char.code b.[i] - !borrow in
      if d < 0 then (
        Bytes.set out i (Char.chr (d + 256));
        borrow := 1)
      else (
        Bytes.set out i (Char.chr d);
        borrow := 0)
    done;
    Bytes.to_string out

  (** Force IEEE P1363 [s] into the low-S range ([s] or [n-s]). *)
  let low_s (s : string) : string =
    if String.compare s n_half_octets > 0 then sub_be n_octets s else s

  (** True when IEEE P1363 [s] is already low-S ([s <= n/2]). *)
  let is_low_s (s : string) : bool = String.compare s n_half_octets <= 0

  let in_scalar z = Z.gt z z0 && Z.lt z n
  let ( %: ) a m = Z.(erem a m)
  let ( +: ) a b = Z.(a + b) %: p
  let ( -: ) a b = Z.(a - b) %: p
  let ( *: ) a b = Z.(a * b) %: p
  let inv a = Z.invert a p

  type point = Inf | Pt of pub

  let on_curve (q : pub) =
    (* y^2 = x^3 + 7 (mod p) *)
    let y2 = q.y *: q.y in
    let x3 = q.x *: q.x *: q.x in
    Z.equal y2 (x3 +: Z.of_int 7)

  let double = function
    | Inf -> Inf
    | Pt q ->
        if Z.equal q.y z0 then Inf
        else
          let lam = z3 *: q.x *: q.x *: inv (z2 *: q.y) in
          let xr = (lam *: lam) -: (z2 *: q.x) in
          let yr = (lam *: (q.x -: xr)) -: q.y in
          Pt { x = xr; y = yr }

  let add a b =
    match (a, b) with
    | Inf, p | p, Inf -> p
    | Pt a, Pt b ->
        if Z.equal a.x b.x then if Z.equal a.y b.y then double (Pt a) else Inf
        else
          let lam = (b.y -: a.y) *: inv (b.x -: a.x) in
          let xr = (lam *: lam) -: a.x -: b.x in
          let yr = (lam *: (a.x -: xr)) -: a.y in
          Pt { x = xr; y = yr }

  let rec mul k = function
    | Inf -> Inf
    | p ->
        if Z.equal k z0 then Inf
        else if Z.equal k z1 then p
        else
          let q = mul (Z.shift_right k 1) (double p) in
          if Z.testbit k 0 then add q p else q

  (** Parse a 32-byte secp256k1 private scalar. *)
  let priv_of_octets (s : string) : (priv, error) result =
    if String.length s <> 32 then Error `Invalid_key
    else
      let d = z_of_be s in
      if in_scalar d then Ok { d } else Error `Invalid_key

  (** Public point [d·G] for private key [k]. *)
  let pub_of_priv (k : priv) : pub =
    match mul k.d (Pt g) with
    | Pt q -> q
    | Inf -> failwith "K256.pub_of_priv: identity"

  let decompress x y_odd =
    (* y^2 = x^3 + 7; p % 4 = 3 so y = (y2)^((p+1)/4) *)
    let y2 = (x *: x *: x) +: Z.of_int 7 in
    let exp = Z.shift_right (Z.succ p) 2 in
    let y = Z.powm y2 exp p in
    let y = if Z.testbit y 0 = y_odd then y else Z.(p - y) in
    let q = { x; y } in
    if on_curve q then Ok q else Error `Invalid_key

  (** Parse a compressed (33-byte) or uncompressed (65-byte) public key. *)
  let pub_of_octets (s : string) : (pub, error) result =
    if String.length s = 33 && (s.[0] = '\x02' || s.[0] = '\x03') then
      decompress (z_of_be (String.sub s 1 32)) (s.[0] = '\x03')
    else if String.length s = 65 && s.[0] = '\x04' then
      let q =
        { x = z_of_be (String.sub s 1 32); y = z_of_be (String.sub s 33 32) }
      in
      if on_curve q then Ok q else Error `Invalid_key
    else Error `Invalid_key

  (** Encode [q] as compressed ([02]/[03]+x) or uncompressed ([04]+x+y). *)
  let pub_to_octets ?(compress = true) (q : pub) : string =
    if compress then
      let prefix = if Z.testbit q.y 0 then "\x03" else "\x02" in
      prefix ^ be_of_z q.x
    else "\x04" ^ be_of_z q.x ^ be_of_z q.y

  let random_scalar () =
    let rec loop () =
      let buf = Bytes.create 32 in
      for i = 0 to 31 do
        Bytes.set buf i (Char.chr (Random.int 256))
      done;
      let k = z_of_be (Bytes.to_string buf) in
      if in_scalar k then k else loop ()
    in
    loop ()

  (** Fresh secp256k1 key pair. *)
  let generate () : priv * pub =
    Random.self_init ();
    let priv = { d = random_scalar () } in
    (priv, pub_of_priv priv)

  (** ECDSA sign [digest] (32 bytes) as IEEE P1363 [r, s] (low-S). *)
  let sign ~(key : priv) (digest : string) : string * string =
    if String.length digest <> 32 then
      failwith "K256.sign: digest must be 32 bytes";
    let z = z_of_be digest %: n in
    let rec attempt () =
      let k = random_scalar () in
      match mul k (Pt g) with
      | Inf -> attempt ()
      | Pt r_pt ->
          let r = r_pt.x %: n in
          if Z.equal r z0 then attempt ()
          else
            let s = Z.(invert k n * (z + (r * key.d)) %: n) in
            if Z.equal s z0 then attempt ()
            else
              let s = if Z.gt s n_half then Z.(n - s) else s in
              (be_of_z r, be_of_z s)
    in
    Random.self_init ();
    attempt ()

  (** Verify IEEE P1363 [r, s] over 32-byte [digest]. *)
  let verify ~(key : pub) (r, s) (digest : string) : bool =
    try
      if
        String.length digest <> 32
        || String.length r <> 32
        || String.length s <> 32
      then false
      else
        let r_z = z_of_be r and s_z = z_of_be s in
        if (not (in_scalar r_z)) || not (in_scalar s_z) then false
        else if not (on_curve key) then false
        else
          let z = z_of_be digest %: n in
          let w = Z.invert s_z n in
          let u1 = Z.(z * w) %: n in
          let u2 = Z.(r_z * w) %: n in
          match add (mul u1 (Pt g)) (mul u2 (Pt key)) with
          | Inf -> false
          | Pt q -> Z.equal (q.x %: n) r_z
    with _ -> false
end
