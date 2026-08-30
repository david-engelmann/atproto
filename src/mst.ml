open Cid
open Dag_cbor
open Hash

(** Merkle Search Tree used by AT Protocol repositories (fanout 4). *)
module Mst = struct
  type entry = {
    prefix_len : int;
    key_suffix : string;
    value : Cid.t;
    right : Cid.t option;
  }

  type node = { left : Cid.t option; entries : entry list }
  type reconstructed = { key : string; value : Cid.t; right : Cid.t option }

  exception Verify_error of string

  let fail msg = raise (Verify_error msg)

  let leading_zeros_on_hash (key : string) : int =
    let digest = Hash.sha256 key in
    let rec loop i acc =
      if i >= String.length digest then acc
      else
        let b = Char.code digest.[i] in
        if b = 0 then loop (i + 1) (acc + 8)
        else
          let rec bit n c =
            if n land 0x80 <> 0 then c else bit (n lsl 1) (c + 1)
          in
          acc + bit b 0
    in
    loop 0 0

  let layer_for_key (key : string) : int = leading_zeros_on_hash key / 2

  let common_prefix_len (a : string) (b : string) : int =
    let n = min (String.length a) (String.length b) in
    let rec loop i = if i < n && a.[i] = b.[i] then loop (i + 1) else i in
    loop 0

  let parse_cid_opt = function
    | None | Some Dag_cbor.Null -> None
    | Some v -> Some (Dag_cbor.as_cid v)

  let parse_entry (v : Dag_cbor.value) : entry =
    let fields = Dag_cbor.get_map v in
    {
      prefix_len = Dag_cbor.as_int (Dag_cbor.require "p" fields);
      key_suffix = Dag_cbor.as_bytes (Dag_cbor.require "k" fields);
      value = Dag_cbor.as_cid (Dag_cbor.require "v" fields);
      right = parse_cid_opt (Dag_cbor.find "t" fields);
    }

  let parse_node (v : Dag_cbor.value) : node =
    let fields = Dag_cbor.get_map v in
    let entries =
      match Dag_cbor.find "e" fields with
      | Some a -> List.map parse_entry (Dag_cbor.as_array a)
      | None -> fail "MST node missing entries"
    in
    { left = parse_cid_opt (Dag_cbor.find "l" fields); entries }

  let node_of_bytes (data : string) : node = parse_node (Dag_cbor.decode data)

  let encode_entry (e : entry) : Dag_cbor.value =
    let fields =
      [ ("k", Dag_cbor.Bytes e.key_suffix); ("p", Dag_cbor.Int e.prefix_len) ]
      @ (match e.right with Some c -> [ ("t", Dag_cbor.Cid c) ] | None -> [])
      @ [ ("v", Dag_cbor.Cid e.value) ]
    in
    Dag_cbor.Map fields

  let encode_node (n : node) : Dag_cbor.value =
    let fields =
      (match n.left with Some c -> [ ("l", Dag_cbor.Cid c) ] | None -> [])
      @ [ ("e", Dag_cbor.Array (List.map encode_entry n.entries)) ]
    in
    Dag_cbor.Map fields

  let to_bytes (n : node) : string = Dag_cbor.encode (encode_node n)
  let cid_of_node (n : node) : Cid.t = Cid.create (to_bytes n)

  let reconstruct (n : node) : reconstructed list =
    let rec loop prev acc = function
      | [] -> List.rev acc
      | (e : entry) :: rest ->
          if e.prefix_len < 0 || e.prefix_len > String.length prev then
            fail "MST prefix length out of range";
          let key = String.sub prev 0 e.prefix_len ^ e.key_suffix in
          if e.prefix_len <> common_prefix_len prev key && prev <> "" then
            fail "MST prefix length does not match previous key";
          loop key ({ key; value = e.value; right = e.right } :: acc) rest
    in
    loop "" [] n.entries

  let keys_strictly_increasing keys =
    let rec loop = function
      | a :: (b :: _ as rest) ->
          if String.compare a b >= 0 then
            fail "MST keys are not strictly increasing";
          loop rest
      | _ -> ()
    in
    loop keys

  let verify_node ?(expected_layer : int option) (n : node) : reconstructed list
      =
    let items = reconstruct n in
    let keys = List.map (fun r -> r.key) items in
    keys_strictly_increasing keys;
    (match (expected_layer, items) with
    | Some layer, _ ->
        List.iter
          (fun r ->
            if layer_for_key r.key <> layer then
              fail
                (Printf.sprintf "MST key %s is layer %d, expected %d" r.key
                   (layer_for_key r.key) layer))
          items
    | None, hd :: tl ->
        let layer = layer_for_key hd.key in
        List.iter
          (fun r ->
            if layer_for_key r.key <> layer then
              fail "MST node mixes keys from different layers")
          tl
    | None, [] -> ());
    items

  let rec verify_tree ~(get_block : Cid.t -> string option)
      ?(layer : int option) (root : Cid.t) : unit =
    match get_block root with
    | None -> fail ("MST missing block " ^ Cid.to_string root)
    | Some data ->
        let expected = Cid.create data in
        if not (Cid.equal expected root) then
          fail
            (Printf.sprintf "MST CID mismatch: got %s expected %s"
               (Cid.to_string expected) (Cid.to_string root));
        let node = node_of_bytes data in
        let items = verify_node ?expected_layer:layer node in
        let this_layer =
          match (layer, items) with
          | Some l, _ -> l
          | None, hd :: _ -> layer_for_key hd.key
          | None, [] -> 0
        in
        let child_layer = this_layer - 1 in
        let check_child = function
          | None -> ()
          | Some cid ->
              if child_layer < 0 then fail "MST child below layer 0";
              verify_tree ~get_block ~layer:child_layer cid
        in
        check_child node.left;
        List.iter (fun r -> check_child r.right) items

  let rec lookup ~(get_block : Cid.t -> string option) (root : Cid.t)
      (key : string) : Cid.t option =
    match get_block root with
    | None -> None
    | Some data ->
        let node = node_of_bytes data in
        let items = reconstruct node in
        let rec find prev_right = function
          | [] -> (
              match prev_right with
              | Some cid -> lookup ~get_block cid key
              | None -> (
                  match node.left with
                  | Some cid -> lookup ~get_block cid key
                  | None -> None))
          | hd :: rest ->
              if String.equal hd.key key then Some hd.value
              else if String.compare key hd.key < 0 then
                match prev_right with
                | Some cid -> lookup ~get_block cid key
                | None -> (
                    match node.left with
                    | Some cid -> lookup ~get_block cid key
                    | None -> None)
              else find hd.right rest
        in
        find None items

  let get_block_of_car (car : Car.Car.t) (cid : Cid.t) : string option =
    match Car.Car.find_block car cid with Some b -> Some b.data | None -> None
end
