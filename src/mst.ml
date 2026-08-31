open Cid
open Dag_cbor
open Hash

let ensure_rng = lazy (Mirage_crypto_rng_unix.use_default ())

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

  (* Spec: limit TreeEntries per node against key-mining DoS. Fanout 4 makes
     64 statistically extreme (expected ~4). *)
  let max_entries_per_node = 64

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

  let ensure_mst_link (cid : Cid.t) : unit =
    if not (Cid.is_blessed ~codec:Cid.Dag_cbor cid) then
      fail
        (Printf.sprintf
           "MST child link %s is not a blessed dag-cbor SHA-256 CID"
           (Cid.to_string cid))

  let verify_node ?(expected_layer : int option) (n : node) : reconstructed list
      =
    if List.length n.entries > max_entries_per_node then
      fail
        (Printf.sprintf "MST node has %d entries (max %d)"
           (List.length n.entries) max_entries_per_node);
    (match n.left with Some c -> ensure_mst_link c | None -> ());
    List.iter
      (fun (e : entry) ->
        match e.right with Some c -> ensure_mst_link c | None -> ())
      n.entries;
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

  (* Mutable overlay so inversion can write new MST nodes without losing CAR
     blocks from the firehose diff. *)
  type store = {
    get : Cid.t -> string option;
    created : (string, string) Hashtbl.t;
  }

  let store_of_get get = { get; created = Hashtbl.create 32 }
  let store_of_car car = store_of_get (get_block_of_car car)

  let store_get (s : store) (cid : Cid.t) : string option =
    try Some (Hashtbl.find s.created (Cid.to_string cid))
    with Not_found -> s.get cid

  let store_put (s : store) (cid : Cid.t) (data : string) =
    Hashtbl.replace s.created (Cid.to_string cid) data

  type repo_commit = {
    did : string;
    version : int;
    data : Cid.t;
    rev : string;
    prev : Cid.t option;
    sig_ : string option;
  }

  let parse_repo_commit (v : Dag_cbor.value) : repo_commit =
    let fields = Dag_cbor.get_map v in
    {
      did = Dag_cbor.as_text (Dag_cbor.require "did" fields);
      version =
        (match Dag_cbor.find "version" fields with
        | Some v -> Dag_cbor.as_int v
        | None -> 3);
      data = Dag_cbor.as_cid (Dag_cbor.require "data" fields);
      rev = Dag_cbor.as_text (Dag_cbor.require "rev" fields);
      prev =
        (match Dag_cbor.find "prev" fields with
        | None | Some Dag_cbor.Null -> None
        | Some c -> Some (Dag_cbor.as_cid c));
      sig_ =
        (match Dag_cbor.find "sig" fields with
        | Some (Dag_cbor.Bytes b) -> Some b
        | _ -> None);
    }

  let encode_repo_commit ?(version = 3) ~did ~data ~rev ?prev ?sig_ () : string
      =
    let fields =
      [
        ("data", Dag_cbor.Cid data);
        ("did", Dag_cbor.Text did);
        ("rev", Dag_cbor.Text rev);
        ("version", Dag_cbor.Int version);
      ]
      @ [
          ( "prev",
            match prev with Some c -> Dag_cbor.Cid c | None -> Dag_cbor.Null );
        ]
      @ match sig_ with Some b -> [ ("sig", Dag_cbor.Bytes b) ] | None -> []
    in
    Dag_cbor.encode (Dag_cbor.Map fields)

  let unsigned_repo_commit ?(version = 3) ~did ~data ~rev ?prev () : string =
    encode_repo_commit ~version ~did ~data ~rev ?prev ()

  type sig_status =
    [ `Valid | `Invalid | `Unsupported_curve of string | `Missing ]

  let sign_p256 ~(priv : Mirage_crypto_ec.P256.Dsa.priv) ?(version = 3) ~did
      ~data ~rev ?prev () : string =
    Lazy.force ensure_rng;
    let unsigned = unsigned_repo_commit ~version ~did ~data ~rev ?prev () in
    let digest = Hash.sha256 unsigned in
    let r, s = Mirage_crypto_ec.P256.Dsa.sign ~key:priv digest in
    let s =
      if String.compare s Did_plc.Did_plc.p256_n_half > 0 then
        Did_plc.Did_plc.sub_be Did_plc.Did_plc.p256_n s
      else s
    in
    encode_repo_commit ~version ~did ~data ~rev ?prev ~sig_:(r ^ s) ()

  let sign_k256 ~(priv : K256.K256.priv) ?(version = 3) ~did ~data ~rev ?prev ()
      : string =
    let unsigned = unsigned_repo_commit ~version ~did ~data ~rev ?prev () in
    let digest = Hash.sha256 unsigned in
    let r, s = K256.K256.sign ~key:priv digest in
    encode_repo_commit ~version ~did ~data ~rev ?prev ~sig_:(r ^ s) ()

  let verify_commit_sig ~(keys : string list) (c : repo_commit) : sig_status =
    match c.sig_ with
    | None -> `Missing
    | Some raw ->
        if String.length raw <> 64 then `Invalid
        else
          let r = String.sub raw 0 32 in
          let s = String.sub raw 32 32 in
          let digest =
            Hash.sha256
              (unsigned_repo_commit ~version:c.version ~did:c.did ~data:c.data
                 ~rev:c.rev ?prev:c.prev ())
          in
          let parsed =
            List.filter_map
              (fun k -> try Some (Did_key.Did_key.of_string k) with _ -> None)
              keys
          in
          let rec try_keys = function
            | [] -> (
                let other =
                  List.find_map
                    (fun k ->
                      match k.Did_key.Did_key.curve with
                      | Did_key.Did_key.Other n ->
                          Some (Printf.sprintf "0x%x" n)
                      | _ -> None)
                    parsed
                in
                match other with
                | Some curve -> `Unsupported_curve curve
                | None -> `Invalid)
            | k :: rest -> (
                match k.Did_key.Did_key.curve with
                | Did_key.Did_key.P256 -> (
                    match Did_key.Did_key.p256_pub k with
                    | Some pub ->
                        if
                          Did_plc.Did_plc.is_low_s s
                          && Mirage_crypto_ec.P256.Dsa.verify ~key:pub (r, s)
                               digest
                        then `Valid
                        else try_keys rest
                    | None -> try_keys rest)
                | Did_key.Did_key.K256 -> (
                    match Did_key.Did_key.k256_pub k with
                    | Some pub ->
                        if
                          K256.K256.is_low_s s
                          && K256.K256.verify ~key:pub (r, s) digest
                        then `Valid
                        else try_keys rest
                    | None -> try_keys rest)
                | Did_key.Did_key.Other _ -> try_keys rest)
          in
          try_keys parsed

  type record_op = {
    action : string;
    path : string;
    cid : Cid.t option;
    prev : Cid.t option;
  }

  type node_entry = Value of string * Cid.t | Child of tree

  and tree = {
    store : store;
    mutable pointer : Cid.t;
    mutable entries : node_entry list option;
    mutable layer : int option;
    mutable outdated : bool;
  }

  let empty_node_bytes = to_bytes { left = None; entries = [] }
  let empty_node_cid = Cid.create empty_node_bytes

  let entries_to_node (entries : node_entry list) : node =
    let left, rest =
      match entries with
      | Child t :: rest -> (Some t.pointer, rest)
      | rest -> (None, rest)
    in
    let rec loop prev acc = function
      | [] -> List.rev acc
      | Value (key, value) :: rest ->
          let prefix = common_prefix_len prev key in
          let suffix = String.sub key prefix (String.length key - prefix) in
          let right, rest =
            match rest with
            | Child t :: rest -> (Some t.pointer, rest)
            | rest -> (None, rest)
          in
          loop key
            ({ prefix_len = prefix; key_suffix = suffix; value; right } :: acc)
            rest
      | Child _ :: _ -> fail "MST: two child pointers next to each other"
    in
    { left; entries = loop "" [] rest }

  let rec load_tree ?(layer : int option) (store : store) (cid : Cid.t) : tree =
    { store; pointer = cid; entries = None; layer; outdated = false }

  and child_of_cid store layer cid = load_tree ?layer store cid

  let node_to_entries store layer (n : node) : node_entry list =
    let child_layer =
      match layer with Some l -> Some (l - 1) | None -> None
    in
    let left =
      match n.left with
      | Some cid -> [ Child (child_of_cid store child_layer cid) ]
      | None -> []
    in
    let items = reconstruct n in
    left
    @ List.concat
        (List.map
           (fun (r : reconstructed) ->
             Value (r.key, r.value)
             ::
             (match r.right with
             | Some cid -> [ Child (child_of_cid store child_layer cid) ]
             | None -> []))
           items)

  let get_entries (t : tree) : node_entry list =
    match t.entries with
    | Some e -> e
    | None -> (
        match store_get t.store t.pointer with
        | None -> fail ("MST missing block " ^ Cid.to_string t.pointer)
        | Some data ->
            let node = node_of_bytes data in
            let layer =
              match t.layer with
              | Some l -> Some l
              | None -> (
                  match reconstruct node with
                  | hd :: _ -> Some (layer_for_key hd.key)
                  | [] -> Some 0)
            in
            t.layer <- layer;
            let entries = node_to_entries t.store layer node in
            t.entries <- Some entries;
            entries)

  let new_tree (t : tree) (entries : node_entry list) : tree =
    { t with entries = Some entries; outdated = true }

  let rec serialize (t : tree) : Cid.t * string =
    let entries = get_entries t in
    List.iter
      (function Child c -> ignore (root_cid c) | Value _ -> ())
      entries;
    let entries = get_entries t in
    let node = entries_to_node entries in
    let data = to_bytes node in
    let cid = Cid.create data in
    store_put t.store cid data;
    t.pointer <- cid;
    t.outdated <- false;
    (cid, data)

  and root_cid (t : tree) : Cid.t =
    if t.outdated then fst (serialize t) else t.pointer

  let create_tree store ?(layer = 0) entries =
    let t =
      {
        store;
        pointer = empty_node_cid;
        entries = Some entries;
        layer = Some layer;
        outdated = true;
      }
    in
    ignore (root_cid t);
    t

  let empty_tree store =
    store_put store empty_node_cid empty_node_bytes;
    {
      store;
      pointer = empty_node_cid;
      entries = Some [];
      layer = Some 0;
      outdated = false;
    }

  let at_index t i =
    let entries = get_entries t in
    if i < 0 || i >= List.length entries then None
    else Some (List.nth entries i)

  let slice t start_i end_i =
    let entries = get_entries t in
    let rec take i acc = function
      | [] -> List.rev acc
      | _ when i >= end_i -> List.rev acc
      | hd :: rest ->
          if i >= start_i then take (i + 1) (hd :: acc) rest
          else take (i + 1) acc rest
    in
    take 0 [] entries

  let find_gt_or_equal_leaf_index t key =
    let entries = get_entries t in
    let rec loop i = function
      | [] -> List.length entries
      | Value (k, _) :: _ when String.compare k key >= 0 -> i
      | _ :: rest -> loop (i + 1) rest
    in
    loop 0 entries

  let rec attempt_get_layer t =
    match t.layer with
    | Some l -> Some l
    | None ->
        let entries = get_entries t in
        let rec first_leaf = function
          | [] -> None
          | Value (k, _) :: _ -> Some (layer_for_key k)
          | Child c :: rest -> (
              match attempt_get_layer c with
              | Some l -> Some (l + 1)
              | None -> first_leaf rest)
        in
        let layer = first_leaf entries in
        t.layer <- layer;
        layer

  let get_layer t = match attempt_get_layer t with Some l -> l | None -> 0

  let update_entry t index entry =
    new_tree t (slice t 0 index @ [ entry ] @ slice t (index + 1) max_int)

  let remove_entry t index =
    new_tree t (slice t 0 index @ slice t (index + 1) max_int)

  let splice_in t entry index =
    new_tree t (slice t 0 index @ [ entry ] @ slice t index max_int)

  let replace_with_split t index left leaf right =
    let left_e = match left with Some c -> [ Child c ] | None -> [] in
    let right_e = match right with Some c -> [ Child c ] | None -> [] in
    new_tree t
      (slice t 0 index @ left_e @ [ leaf ] @ right_e
      @ slice t (index + 1) max_int)

  let rec split_around t key : tree option * tree option =
    let index = find_gt_or_equal_leaf_index t key in
    let left = new_tree t (slice t 0 index) in
    let right = new_tree t (slice t index max_int) in
    let left_entries = get_entries left in
    let left, right =
      match
        if left_entries = [] then None
        else Some (List.nth left_entries (List.length left_entries - 1))
      with
      | Some (Child last) ->
          let left = remove_entry left (List.length left_entries - 1) in
          let sl, sr = split_around last key in
          let left =
            match sl with
            | Some c -> new_tree left (get_entries left @ [ Child c ])
            | None -> left
          in
          let right =
            match sr with
            | Some c -> new_tree right (Child c :: get_entries right)
            | None -> right
          in
          (left, right)
      | _ -> (left, right)
    in
    ( (if get_entries left = [] then None else Some left),
      if get_entries right = [] then None else Some right )

  let rec append_merge left right =
    if get_layer left <> get_layer right then
      fail "MST: merge of nodes from different layers";
    let le = get_entries left and re = get_entries right in
    match (le, re) with
    | _ :: _, Child first_r :: rest_r -> (
        match List.nth le (List.length le - 1) with
        | Child last_l ->
            let merged = append_merge last_l first_r in
            new_tree left
              (slice left 0 (List.length le - 1) @ [ Child merged ] @ rest_r)
        | Value _ -> new_tree left (le @ re))
    | _ -> new_tree left (le @ re)

  let rec trim_top t =
    match get_entries t with [ Child child ] -> trim_top child | _ -> t

  let create_child t = create_tree t.store ~layer:(get_layer t - 1) []
  let create_parent t = create_tree t.store ~layer:(get_layer t + 1) [ Child t ]

  let rec insert_rec t key value key_layer : tree * Cid.t option =
    let layer = get_layer t in
    if key_layer = layer then
      let index = find_gt_or_equal_leaf_index t key in
      match at_index t index with
      | Some (Value (k, old)) when String.equal k key ->
          (update_entry t index (Value (key, value)), Some old)
      | _ -> (
          let prev = at_index t (index - 1) in
          match prev with
          | Some (Child child) ->
              let left, right = split_around child key in
              ( replace_with_split t (index - 1) left (Value (key, value)) right,
                None )
          | _ -> (splice_in t (Value (key, value)) index, None))
    else if key_layer < layer then
      let index = find_gt_or_equal_leaf_index t key in
      match at_index t (index - 1) with
      | Some (Child child) ->
          let child, prev = insert_rec child key value key_layer in
          (update_entry t (index - 1) (Child child), prev)
      | _ ->
          let child = create_child t in
          let child, prev = insert_rec child key value key_layer in
          (splice_in t (Child child) index, prev)
    else
      let left, right = split_around t key in
      let extra = key_layer - layer in
      let rec wrap n side =
        if n <= 0 || side = None then side
        else
          match side with
          | None -> None
          | Some c -> wrap (n - 1) (Some (create_parent c))
      in
      (* first split already accounts for one layer; wrap extras-1 *)
      let left = wrap (extra - 1) left in
      let right = wrap (extra - 1) right in
      let entries =
        (match left with Some c -> [ Child c ] | None -> [])
        @ [ Value (key, value) ]
        @ match right with Some c -> [ Child c ] | None -> []
      in
      (create_tree t.store ~layer:key_layer entries, None)

  let insert (t : tree) (key : string) (value : Cid.t) : tree * Cid.t option =
    if key = "" then fail "MST insert: empty key";
    insert_rec t key value (layer_for_key key)

  let rec get_rec t key =
    let index = find_gt_or_equal_leaf_index t key in
    match at_index t index with
    | Some (Value (k, v)) when String.equal k key -> Some v
    | _ -> (
        match at_index t (index - 1) with
        | Some (Child child) -> get_rec child key
        | _ -> None)

  let get t key = get_rec t key

  let rec delete_recurse t key : tree * Cid.t option =
    let index = find_gt_or_equal_leaf_index t key in
    match at_index t index with
    | Some (Value (k, old)) when String.equal k key -> (
        match (at_index t (index - 1), at_index t (index + 1)) with
        | Some (Child prev), Some (Child next) ->
            let merged = append_merge prev next in
            ( new_tree t
                (slice t 0 (index - 1)
                @ [ Child merged ]
                @ slice t (index + 2) max_int),
              Some old )
        | _ -> (remove_entry t index, Some old))
    | _ -> (
        match at_index t (index - 1) with
        | Some (Child child) ->
            let child, prev = delete_recurse child key in
            if get_entries child = [] then (remove_entry t (index - 1), prev)
            else (update_entry t (index - 1) (Child child), prev)
        | _ -> (t, None))

  let remove t key =
    let t, prev = delete_recurse t key in
    (trim_top t, prev)

  let normalize_ops (ops : record_op list) : record_op list =
    let last = Hashtbl.create 8 in
    List.iter (fun (op : record_op) -> Hashtbl.replace last op.path op) ops;
    let seen = Hashtbl.create 8 in
    List.rev
      (List.fold_left
         (fun acc (op : record_op) ->
           if Hashtbl.mem seen op.path then acc
           else (
             Hashtbl.add seen op.path ();
             Hashtbl.find last op.path :: acc))
         [] (List.rev ops))

  let invert_op (t : tree) (op : record_op) : tree =
    match op.action with
    | "create" -> (
        match op.cid with
        | None -> fail "MST invert: create is missing cid"
        | Some expected -> (
            let t, prev = remove t op.path in
            match prev with
            | Some c when Cid.equal c expected -> t
            | Some c ->
                fail
                  (Printf.sprintf "MST invert create: tree had %s, op.cid is %s"
                     (Cid.to_string c) (Cid.to_string expected))
            | None -> fail ("MST invert create: missing " ^ op.path)))
    | "update" -> (
        match (op.cid, op.prev) with
        | Some expected, Some old -> (
            let t, prev = insert t op.path old in
            match prev with
            | Some c when Cid.equal c expected -> t
            | Some c ->
                fail
                  (Printf.sprintf "MST invert update: tree had %s, op.cid is %s"
                     (Cid.to_string c) (Cid.to_string expected))
            | None -> fail ("MST invert update: missing " ^ op.path))
        | _ -> fail "MST invert: update requires cid and prev")
    | "delete" -> (
        match op.prev with
        | None -> fail "MST invert: delete is missing prev"
        | Some old -> (
            let t, prev = insert t op.path old in
            match prev with
            | None -> t
            | Some c ->
                fail
                  (Printf.sprintf "MST invert delete: %s was present as %s"
                     op.path (Cid.to_string c))))
    | other -> fail ("MST invert: unknown action " ^ other)

  let invert_ops (t : tree) (ops : record_op list) : tree =
    List.fold_left invert_op t (normalize_ops ops)

  let apply_op (t : tree) (op : record_op) : tree =
    match op.action with
    | "create" -> (
        match op.cid with
        | None -> fail "MST apply: create is missing cid"
        | Some cid -> (
            let t, prev = insert t op.path cid in
            match prev with
            | None -> t
            | Some c ->
                fail
                  (Printf.sprintf "MST apply create: %s already present as %s"
                     op.path (Cid.to_string c))))
    | "update" -> (
        match op.cid with
        | None -> fail "MST apply: update is missing cid"
        | Some cid -> (
            let t, prev = insert t op.path cid in
            match (prev, op.prev) with
            | None, _ -> fail ("MST apply update: missing " ^ op.path)
            | Some got, Some expected when not (Cid.equal got expected) ->
                fail
                  (Printf.sprintf "MST apply update: tree had %s, op.prev is %s"
                     (Cid.to_string got) (Cid.to_string expected))
            | Some _, _ -> t))
    | "delete" -> (
        let t, prev = remove t op.path in
        match (prev, op.prev) with
        | None, _ -> fail ("MST apply delete: missing " ^ op.path)
        | Some got, Some expected when not (Cid.equal got expected) ->
            fail
              (Printf.sprintf "MST apply delete: tree had %s, op.prev is %s"
                 (Cid.to_string got) (Cid.to_string expected))
        | Some _, _ -> t)
    | other -> fail ("MST apply: unknown action " ^ other)

  let apply_ops (t : tree) (ops : record_op list) : tree =
    List.fold_left apply_op t ops

  let rec collect_entries (t : tree) acc =
    List.fold_left
      (fun acc e ->
        match e with
        | Value (path, cid) -> (path, cid) :: acc
        | Child c -> collect_entries c acc)
      acc (get_entries t)

  let walk (t : tree) : (string * Cid.t) list = List.rev (collect_entries t [])

  let rec collect_available (t : tree) acc =
    match store_get t.store t.pointer with
    | None when t.entries = None -> acc
    | _ -> (
        try
          List.fold_left
            (fun acc e ->
              match e with
              | Value (path, cid) -> (path, cid) :: acc
              | Child c -> collect_available c acc)
            acc (get_entries t)
        with Verify_error _ -> acc)

  let walk_available (t : tree) : (string * Cid.t) list =
    List.rev (collect_available t [])

  let block_of_cid store cid =
    match store_get store cid with
    | Some data -> { Car.Car.cid; data }
    | None -> fail ("MST missing block " ^ Cid.to_string cid)

  (* Streamable CAR pre-order: this MST node, then each entry in node order
     (left child, then for every leaf: record then right child).
     https://atproto.com/specs/repository#streamable-car-block-ordering *)
  let rec preorder_blocks ?(records = true) (t : tree) : Car.Car.block list =
    let cid = root_cid t in
    let node_block = block_of_cid t.store cid in
    node_block
    :: List.concat
         (List.map
            (function
              | Child c -> preorder_blocks ~records c
              | Value (_, rec_cid) -> (
                  if not records then []
                  else
                    match store_get t.store rec_cid with
                    | Some data -> [ { Car.Car.cid = rec_cid; data } ]
                    | None -> []))
            (get_entries t))

  let collection_start (collection : string) = collection ^ "/"

  (* Exclusive end of `collection/` — '/' (0x2F) + 1 = '0'. *)
  let collection_end (collection : string) = collection ^ "0"

  let collection_range (collection : string) : string * string =
    (collection_start collection, collection_end collection)

  let key_in_range ~start ~end_exclusive key =
    String.compare key start >= 0 && String.compare key end_exclusive < 0

  let rec subtree_key_bounds (t : tree) : (string * string) option =
    let rec loop lo hi = function
      | [] -> ( match (lo, hi) with Some a, Some b -> Some (a, b) | _ -> None)
      | Value (k, _) :: rest ->
          let lo = match lo with None -> Some k | Some a -> Some a in
          loop lo (Some k) rest
      | Child c :: rest -> (
          match subtree_key_bounds c with
          | None -> loop lo hi rest
          | Some (a, b) ->
              let lo = match lo with None -> Some a | Some x -> Some x in
              loop lo (Some b) rest)
    in
    loop None None (get_entries t)

  let ranges_overlap (a0, a1) (b0, b1) =
    String.compare a0 b1 < 0 && String.compare b0 a1 < 0

  let last_opt xs = match List.rev xs with hd :: _ -> Some hd | [] -> None

  (* MST nodes proving every key in [start, end) plus the immediately
     adjacent keys, plus in-range record blocks. *)
  let range_blocks ~start ~end_exclusive ?(records = true) (t : tree) :
      Car.Car.block list =
    let walked = walk t in
    let in_range =
      List.filter (fun (k, _) -> key_in_range ~start ~end_exclusive k) walked
    in
    let left_adj =
      walked
      |> List.filter (fun (k, _) -> String.compare k start < 0)
      |> last_opt
    in
    let right_adj =
      List.find_opt (fun (k, _) -> String.compare k end_exclusive >= 0) walked
    in
    let needed =
      List.map fst in_range
      @ (match left_adj with Some (k, _) -> [ k ] | None -> [])
      @ match right_adj with Some (k, _) -> [ k ] | None -> []
    in
    let rec collect (node : tree) : Car.Car.block list =
      let this = block_of_cid node.store (root_cid node) in
      let rec loop acc = function
        | [] -> List.rev acc
        | Child c :: rest ->
            let keep =
              match subtree_key_bounds c with
              | None -> false
              | Some (lo, hi) ->
                  List.exists
                    (fun k ->
                      String.compare lo k <= 0 && String.compare k hi <= 0)
                    needed
                  || ranges_overlap (lo, hi ^ "\x00") (start, end_exclusive)
            in
            let acc = if keep then List.rev_append (collect c) acc else acc in
            loop acc rest
        | Value (k, rec_cid) :: rest ->
            let acc =
              if records && key_in_range ~start ~end_exclusive k then
                match store_get node.store rec_cid with
                | Some data -> { Car.Car.cid = rec_cid; data } :: acc
                | None -> acc
              else acc
            in
            loop acc rest
      in
      this :: loop [] (get_entries node)
    in
    collect t

  let covering_proof (t : tree) (key : string) : Car.Car.block list =
    range_blocks ~start:key ~end_exclusive:(key ^ "\x00") ~records:true t

  (* Verify included MST nodes; missing children are allowed (partial proof). *)
  let rec verify_tree_available ~(get_block : Cid.t -> string option)
      ?(layer : int option) (root : Cid.t) : unit =
    match get_block root with
    | None -> ()
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
              verify_tree_available ~get_block ~layer:child_layer cid
        in
        check_child node.left;
        List.iter (fun r -> check_child r.right) items

  type range_status = Complete | Incomplete of string

  let rec range_completeness ~start ~end_exclusive
      ~(get_block : Cid.t -> string option) ?(layer : int option) (root : Cid.t)
      : range_status =
    match get_block root with
    | None ->
        Incomplete
          (Printf.sprintf "missing MST node %s in collection range"
             (Cid.to_string root))
    | Some data -> (
        let node = node_of_bytes data in
        let items = verify_node ?expected_layer:layer node in
        let this_layer =
          match (layer, items) with
          | Some l, _ -> l
          | None, hd :: _ -> layer_for_key hd.key
          | None, [] -> 0
        in
        let child_layer = this_layer - 1 in
        let check_child cid lo hi =
          if ranges_overlap (lo, hi) (start, end_exclusive) then
            if child_layer < 0 then Incomplete "MST child below layer 0"
            else
              range_completeness ~start ~end_exclusive ~get_block
                ~layer:child_layer cid
          else Complete
        in
        let left_status =
          match node.left with
          | None -> Complete
          | Some cid ->
              let first =
                match items with hd :: _ -> hd.key | [] -> end_exclusive
              in
              check_child cid start first
        in
        let rec walk = function
          | [] -> Complete
          | hd :: rest -> (
              let next_lo =
                match rest with r :: _ -> r.key | [] -> end_exclusive
              in
              match hd.right with
              | None -> walk rest
              | Some cid -> (
                  match check_child cid hd.key next_lo with
                  | Incomplete _ as i -> i
                  | Complete -> walk rest))
        in
        match left_status with Incomplete _ as i -> i | Complete -> walk items)

  let check_op (t : tree) (op : record_op) : unit =
    match op.action with
    | "create" | "update" -> (
        match (op.cid, get t op.path) with
        | Some expected, Some got when Cid.equal expected got -> ()
        | Some expected, Some got ->
            fail
              (Printf.sprintf "MST op %s %s: tree %s != op.cid %s" op.action
                 op.path (Cid.to_string got) (Cid.to_string expected))
        | Some _, None ->
            fail
              (Printf.sprintf "MST op %s %s: path missing in tree" op.action
                 op.path)
        | None, _ -> fail (Printf.sprintf "MST op %s missing cid" op.action))
    | "delete" -> (
        match get t op.path with
        | None -> ()
        | Some c ->
            fail
              (Printf.sprintf "MST op delete %s: still present as %s" op.path
                 (Cid.to_string c)))
    | other -> fail ("MST check_op: unknown action " ^ other)

  let tree_of_root store (root : Cid.t) = load_tree store root

  let invert_firehose_ops ~get_block ~mst_root (ops : record_op list) : Cid.t =
    let store = store_of_get get_block in
    let tree = invert_ops (tree_of_root store mst_root) ops in
    root_cid tree
end
