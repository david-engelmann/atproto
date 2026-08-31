open Varint
open Cid
open Dag_cbor

(** CARv1 reader/writer used by com.atproto.sync.getRepo and firehose diffs. *)
module Car = struct
  type block = { cid : Cid.t; data : string }
  type t = { roots : Cid.t list; blocks : block list }

  let parse (bytes : string) : t =
    if String.length bytes = 0 then failwith "Car.parse: empty";
    let header_len, i = Varint.decode_from bytes 0 in
    if i + header_len > String.length bytes then
      failwith "Car.parse: truncated header";
    let header = Dag_cbor.decode (String.sub bytes i header_len) in
    let i = i + header_len in
    let fields = Dag_cbor.get_map header in
    let version =
      match Dag_cbor.find "version" fields with
      | Some v -> Dag_cbor.as_int v
      | None -> 1
    in
    if version <> 1 then
      failwith (Printf.sprintf "Car.parse: unsupported version %d" version);
    let roots =
      match Dag_cbor.find "roots" fields with
      | Some (Dag_cbor.Array items) -> List.map Dag_cbor.as_cid items
      | _ -> []
    in
    let rec read_blocks i acc =
      if i >= String.length bytes then List.rev acc
      else
        let block_len, i = Varint.decode_from bytes i in
        if i + block_len > String.length bytes then
          failwith "Car.parse: truncated block";
        let cid, after_cid = Cid.of_bytes_from bytes i in
        let data_len = block_len - (after_cid - i) in
        if data_len < 0 then failwith "Car.parse: CID longer than block";
        let data = String.sub bytes after_cid data_len in
        read_blocks (after_cid + data_len) ({ cid; data } :: acc)
    in
    { roots; blocks = read_blocks i [] }

  let encode (car : t) : string =
    let header =
      Dag_cbor.encode
        (Dag_cbor.Map
           [
             ("version", Dag_cbor.Int 1);
             ( "roots",
               Dag_cbor.Array (List.map (fun c -> Dag_cbor.Cid c) car.roots) );
           ])
    in
    let buf = Buffer.create (String.length header + 64) in
    Buffer.add_string buf (Varint.encode (String.length header));
    Buffer.add_string buf header;
    List.iter
      (fun (b : block) ->
        let cid_bytes = Cid.to_bytes b.cid in
        let block_len = String.length cid_bytes + String.length b.data in
        Buffer.add_string buf (Varint.encode block_len);
        Buffer.add_string buf cid_bytes;
        Buffer.add_string buf b.data)
      car.blocks;
    Buffer.contents buf

  let find_block (car : t) (cid : Cid.t) : block option =
    List.find_opt (fun (b : block) -> Cid.equal b.cid cid) car.blocks

  let root (car : t) : Cid.t option =
    match car.roots with hd :: _ -> Some hd | [] -> None

  let block_cids (car : t) : Cid.t list =
    List.map (fun (b : block) -> b.cid) car.blocks

  let first_occurrences (cids : Cid.t list) : Cid.t list =
    let seen = Hashtbl.create 16 in
    List.filter
      (fun c ->
        let k = Cid.to_string c in
        if Hashtbl.mem seen k then false
        else (
          Hashtbl.add seen k ();
          true))
      cids

  (* True when [actual] visits every CID in [expected] in that order. Extra
     unlinked blocks and later duplicates are tolerated (CARv1 / repo spec). *)
  let follows_order ~(expected : Cid.t list) (actual : Cid.t list) : bool =
    let rec loop exp act =
      match (exp, act) with
      | [], _ -> true
      | _ :: _, [] -> false
      | e :: er, a :: ar -> if Cid.equal e a then loop er ar else loop exp ar
    in
    loop expected (first_occurrences actual)

  let reorder ~(expected : Cid.t list) (car : t) : t =
    let by_cid = Hashtbl.create (List.length car.blocks) in
    List.iter
      (fun (b : block) ->
        let k = Cid.to_string b.cid in
        if not (Hashtbl.mem by_cid k) then Hashtbl.add by_cid k b)
      car.blocks;
    let ordered =
      List.filter_map
        (fun c ->
          match Hashtbl.find_opt by_cid (Cid.to_string c) with
          | None -> None
          | Some b ->
              Hashtbl.remove by_cid (Cid.to_string c);
              Some b)
        expected
    in
    let extras =
      List.filter
        (fun (b : block) -> Hashtbl.mem by_cid (Cid.to_string b.cid))
        car.blocks
    in
    { car with blocks = ordered @ extras }
end
