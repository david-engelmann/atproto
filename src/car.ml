(*open Session
open Cohttp_client
open App
open CBOR*)

module Car = struct
  type block =
    {
      data: bytes;
      cid: string;
    }

  type car =
    {
      root : string; (* cid *)
      blocks : (string * block) list;
    }
  let decode_unsigned_leb128_from_channel ch =
    let rec loop acc shift bytes_read =
      try
        let byte = input_byte ch in
        let value = byte land 0x7F in
        let acc = acc lor (value lsl shift) in
        if byte land 0x80 = 0 then (acc, bytes_read + 1)
        else loop acc (shift + 7) (bytes_read + 1)
      with End_of_file ->
        failwith "Bad LEB128 encoding"
    in
    loop 0 0 0

  let extract_roots_from_car ch =
    (* Read CAR magic bytes and verify *)
    (*
    if magic_bytes <> "\227\020\015\019" then
      failwith "Not a valid CAR file";
    *)
    (* Decode the length of the CAR header using LEB128 *)
    let header_len, _ = decode_unsigned_leb128_from_channel ch in

    (* Read the CBOR-encoded CAR header *)
    let cbor_header = really_input_string ch header_len in
    (* Decode the CBOR header *)
    let header = CBOR.Simple.decode cbor_header in
    (* Extract the "roots" key from the header *)
    print_endline (CBOR.Simple.to_diagnostic header);
    match header with
    | `Map lst ->
        let roots_entry = List.assoc (`Text "roots") lst in
        Some roots_entry
    | _ -> None

  let show_car_contents ch =
    let roots = extract_roots_from_car ch in
    match roots with
    | Some r -> print_endline (CBOR.Simple.encode r)
    | None -> print_endline "No roots found"

end
