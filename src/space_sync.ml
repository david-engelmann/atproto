open Cid
open Car
open Dag_cbor
open Lt_hash
open Space_commit
open Space_xrpc
open Syntax

(** Experimental local oplog / two-root CAR apply from AT Protocol
    proposal 0016 (permissioned data / spaces).

    Tracks
    {{:https://github.com/bluesky-social/proposals/blob/main/0016-permissioned-data/README.md}0016
    § Incremental sync} and {e Repo serialization} / {e Full-state
    recovery}, plus the \#5187 [packages/space/src/sync] consumer
    ([verifyRepoCar] / [applyOps]). [listRepoOps] entries are
    [{rev, collection, rkey, cid, prev}] ([cid] null on delete,
    [prev] null on create). The CAR declares two roots in order —
    signed commit, then a DRISL map ["{collection}/{rkey}"] → CID
    (canonical DAG-CBOR key order: shortest first, then bytewise) —
    followed by record blocks in that index order.

    Apply is offline: fold ops or the index into a running
    [Lt_hash], then compare [Space_commit.hash]. A mismatch means
    the copy diverged and must recover from a full CAR. This module
    does not start or stub a space host. Live [getRepo] /
    [listRepoOps] hops stay on [Space_xrpc] and skip unless
    [ATP_SPACE] is set and [ATP_SPACE_HOST] names a real host.

    Named [Space_sync] so the compilation unit does not clash with
    {!At_uri.module-Space}. The proposal is not final; this module
    may change and is {e not} a stable spaces product API.
    Deferred: [com.atproto.simplespace.*]; [space:] OAuth scopes;
    [registerNotify] [repo] (proposal prose; not in the \#5187
    lexicon). *)
module Space_sync : sig
  exception Invalid of string

  type op = {
    collection : string;
    rkey : string;
    cid : string option;
    prev : string option;
  }
  (** One [listRepoOps] mutation. [cid] is [None] on delete; [prev]
      is [None] on create. *)

  type index_entry = { collection : string; rkey : string; cid : Cid.t }

  type record_block = {
    collection : string;
    rkey : string;
    cid : Cid.t;
    data : string;
    value : Yojson.Safe.t;
  }

  type applied = {
    commit : Space_commit.t;
    index : index_entry list;
    state : Lt_hash.t;
    records : record_block list;
  }

  type catch_up =
    | Partial of Lt_hash.t
    | Caught_up of Lt_hash.t * Space_commit.t
    | Diverged of Lt_hash.t * Space_commit.t
        (** Incremental result. [Caught_up] when the response carried a
      signed commit whose [hash] matches the running set hash.
      [Diverged] means fall back to CAR recovery. *)

  val path : collection:string -> rkey:string -> string
  (** ["{collection}/{rkey}"]. *)

  val parse_path : string -> string * string
  (** Inverse of [path]. Raises [Invalid] unless the path is exactly
      one slash between an NSID and a record key. *)

  val compare_index_key : string -> string -> int
  (** Canonical DAG-CBOR map key order: shortest first, then
      bytewise. *)

  val op :
    collection:string ->
    rkey:string ->
    ?cid:string ->
    ?prev:string ->
    unit ->
    op

  val op_of_xrpc : Space_xrpc.op_entry -> op
  (** Drop [rev] / inlined [value]; keep the set-hash mutation. *)

  val apply_op : Lt_hash.t -> op -> Lt_hash.t
  (** Subtract [prev] (if any), then add [cid] (if any). [h] is
      unchanged. *)

  val apply_ops : Lt_hash.t -> op list -> Lt_hash.t

  val apply_listed : Lt_hash.t -> Space_xrpc.listed_ops -> catch_up
  (** Replay [ops], then classify against the optional trailing
      commit. Does not verify the commit signature — call
      [Space_commit.verify] when keys are available. *)

  val fold_index : Lt_hash.t -> index_entry list -> Lt_hash.t
  (** Add each ["{collection}/{rkey}/{cid}"] element. *)

  val encode_index : index_entry list -> string
  (** DAG-CBOR map from [path] to CID. *)

  val decode_index : string -> index_entry list
  (** Parse a DRISL repo index. Raises [Invalid] on a bad map. *)

  val record_of_json :
    collection:string -> rkey:string -> value:Yojson.Safe.t -> record_block
  (** DAG-CBOR-encode [value] and CID it (dag-cbor / sha2-256). *)

  val encode :
    commit:Space_commit.t ->
    records:record_block list ->
    ?exclude_values:bool ->
    unit ->
    string
  (** CARv1 bytes: roots [commit; index], then those two blocks, then
      records in index order. [exclude_values] writes only the two
      roots (index-only CAR). *)

  val apply :
    ?expect_values:bool ->
    keys:string list ->
    space:string ->
    author:string ->
    string ->
    applied
  (** Verify a two-root permissioned CAR (commit + index + records).
      Checks the commit MAC/signature, folds the index into LtHash,
      and validates each record CID. [expect_values=false] allows an
      index-only CAR. *)
end = struct
  exception Invalid of string

  type op = {
    collection : string;
    rkey : string;
    cid : string option;
    prev : string option;
  }

  type index_entry = { collection : string; rkey : string; cid : Cid.t }

  type record_block = {
    collection : string;
    rkey : string;
    cid : Cid.t;
    data : string;
    value : Yojson.Safe.t;
  }

  type applied = {
    commit : Space_commit.t;
    index : index_entry list;
    state : Lt_hash.t;
    records : record_block list;
  }

  type catch_up =
    | Partial of Lt_hash.t
    | Caught_up of Lt_hash.t * Space_commit.t
    | Diverged of Lt_hash.t * Space_commit.t

  let fail msg = raise (Invalid msg)
  let path ~collection ~rkey = collection ^ "/" ^ rkey

  let parse_path (p : string) : string * string =
    match String.split_on_char '/' p with
    | [ collection; rkey ]
      when Syntax.is_valid_nsid collection && Syntax.is_valid_record_key rkey ->
        (collection, rkey)
    | _ -> fail ("invalid record path: " ^ p)

  (** Canonical dag-cbor map key order: shortest first, then bytewise. *)
  let compare_index_key a b =
    let d = String.length a - String.length b in
    if d <> 0 then d else String.compare a b

  let sort_entries (entries : index_entry list) =
    List.sort
      (fun (a : index_entry) (b : index_entry) ->
        compare_index_key
          (path ~collection:a.collection ~rkey:a.rkey)
          (path ~collection:b.collection ~rkey:b.rkey))
      entries

  let ensure_path_parts ~collection ~rkey =
    if not (Syntax.is_valid_nsid collection) then
      fail ("invalid NSID in record path: " ^ collection ^ "/" ^ rkey);
    if not (Syntax.is_valid_record_key rkey) then
      fail ("invalid record key in record path: " ^ collection ^ "/" ^ rkey)

  let op ~collection ~rkey ?cid ?prev () : op =
    ensure_path_parts ~collection ~rkey;
    { collection; rkey; cid; prev }

  let op_of_xrpc (e : Space_xrpc.op_entry) : op =
    op ~collection:e.Space_xrpc.collection ~rkey:e.Space_xrpc.rkey
      ?cid:e.Space_xrpc.cid ?prev:e.Space_xrpc.prev ()

  let element ~collection ~rkey ~cid =
    Lt_hash.element ~collection ~rkey ~record_cid:cid

  let apply_op (h : Lt_hash.t) (o : op) : Lt_hash.t =
    let h =
      match o.prev with
      | Some cid ->
          Lt_hash.remove h (element ~collection:o.collection ~rkey:o.rkey ~cid)
      | None -> h
    in
    match o.cid with
    | Some cid ->
        Lt_hash.add h (element ~collection:o.collection ~rkey:o.rkey ~cid)
    | None -> h

  let apply_ops (h : Lt_hash.t) (ops : op list) : Lt_hash.t =
    List.fold_left apply_op h ops

  let apply_listed (h : Lt_hash.t) (listed : Space_xrpc.listed_ops) : catch_up =
    let h = apply_ops h (List.map op_of_xrpc listed.Space_xrpc.ops) in
    match listed.Space_xrpc.commit with
    | None -> Partial h
    | Some commit ->
        if Space_commit.matches h commit then Caught_up (h, commit)
        else Diverged (h, commit)

  let fold_index (h : Lt_hash.t) (entries : index_entry list) : Lt_hash.t =
    List.fold_left
      (fun acc (e : index_entry) ->
        Lt_hash.add acc
          (element ~collection:e.collection ~rkey:e.rkey
             ~cid:(Cid.to_string e.cid)))
      h entries

  let encode_index (entries : index_entry list) : string =
    let fields =
      List.map
        (fun (e : index_entry) ->
          (path ~collection:e.collection ~rkey:e.rkey, Dag_cbor.Cid e.cid))
        (sort_entries entries)
    in
    Dag_cbor.encode (Dag_cbor.Map fields)

  let decode_index (raw : string) : index_entry list =
    try
      let fields = Dag_cbor.get_map (Dag_cbor.decode raw) in
      List.map
        (fun (p, v) ->
          let collection, rkey = parse_path p in
          let cid =
            try Dag_cbor.as_cid v
            with Dag_cbor.Decode_error _ ->
              fail ("invalid repo index: " ^ p ^ " is not a CID")
          in
          { collection; rkey; cid })
        fields
    with
    | Invalid _ as e -> raise e
    | Dag_cbor.Decode_error msg -> fail ("invalid repo index: " ^ msg)
    | _ -> fail "invalid repo index"

  let record_of_json ~collection ~rkey ~value : record_block =
    ensure_path_parts ~collection ~rkey;
    let data = Dag_cbor.encode (Dag_cbor.of_yojson value) in
    let cid = Cid.create ~codec:Cid.Dag_cbor data in
    { collection; rkey; cid; data; value }

  let verify_block (b : Car.block) =
    let expected = Cid.create ~codec:b.Car.cid.Cid.codec b.Car.data in
    if not (Cid.equal expected b.Car.cid) then fail "not a valid cid for bytes"

  let encode ~commit ~records ?(exclude_values = false) () : string =
    let by_path = Hashtbl.create 16 in
    List.iter
      (fun (r : record_block) ->
        Hashtbl.replace by_path (path ~collection:r.collection ~rkey:r.rkey) r)
      records;
    let paths =
      Hashtbl.fold (fun p _ acc -> p :: acc) by_path []
      |> List.sort compare_index_key
    in
    let entries =
      List.map
        (fun p ->
          let r = Hashtbl.find by_path p in
          { collection = r.collection; rkey = r.rkey; cid = r.cid })
        paths
    in
    let commit_data = Space_commit.encode commit in
    let commit_cid = Cid.create ~codec:Cid.Dag_cbor commit_data in
    let index_data = encode_index entries in
    let index_cid = Cid.create ~codec:Cid.Dag_cbor index_data in
    let record_blocks =
      if exclude_values then []
      else
        List.map
          (fun p ->
            let r = Hashtbl.find by_path p in
            { Car.cid = r.cid; data = r.data })
          paths
    in
    Car.encode
      {
        Car.roots = [ commit_cid; index_cid ];
        blocks =
          { Car.cid = commit_cid; data = commit_data }
          :: { Car.cid = index_cid; data = index_data }
          :: record_blocks;
      }

  let apply ?(expect_values = true) ~keys ~space ~author (bytes : string) :
      applied =
    let car =
      try Car.parse bytes with
      | Failure msg -> fail msg
      | Invalid_argument msg -> fail msg
    in
    (match car.Car.roots with
    | [ _; _ ] -> ()
    | roots ->
        fail
          (Printf.sprintf "expected 2 car roots (commit, index), got %d"
             (List.length roots)));
    let commit_root = List.nth car.Car.roots 0 in
    let index_root = List.nth car.Car.roots 1 in
    let blocks = car.Car.blocks in
    let commit_block, index_block, rest =
      match blocks with
      | c :: i :: rest -> (c, i, rest)
      | _ -> fail "expected the commit block to lead the car"
    in
    if not (Cid.equal commit_block.Car.cid commit_root) then
      fail "expected the commit block to lead the car";
    verify_block commit_block;
    let commit =
      try Space_commit.decode commit_block.Car.data
      with Space_commit.Invalid msg -> fail ("invalid signed commit: " ^ msg)
    in
    let ctx : Space_commit.ctx =
      { space; author; rev = commit.Space_commit.rev }
    in
    if not (Space_commit.verify ~keys ~ctx commit) then
      fail "commit failed verification";
    if not (Cid.equal index_block.Car.cid index_root) then
      fail "expected the index block to follow the commit";
    verify_block index_block;
    let index = decode_index index_block.Car.data in
    let state = fold_index (Lt_hash.empty ()) index in
    if not (Space_commit.matches state commit) then
      fail "index does not match the commit hash";
    let rec take (entries : index_entry list) (blocks : Car.block list)
        (acc : record_block list) : record_block list =
      match (entries, blocks) with
      | [], [] -> List.rev acc
      | [], _ :: _ -> fail "car has more blocks than index entries"
      | _ :: _, [] ->
          let missing = List.length entries in
          if (not expect_values) && acc = [] then []
          else
            fail
              (Printf.sprintf "car is missing %d record(s) named in the index"
                 missing)
      | (e : index_entry) :: er, b :: br ->
          verify_block b;
          if not (Cid.equal b.Car.cid e.cid) then
            fail
              (Printf.sprintf "expected block %s at %s, got %s"
                 (Cid.to_string e.cid)
                 (path ~collection:e.collection ~rkey:e.rkey)
                 (Cid.to_string b.Car.cid));
          let value =
            try Dag_cbor.to_yojson (Dag_cbor.decode b.Car.data)
            with Dag_cbor.Decode_error msg ->
              fail
                (Printf.sprintf "invalid record cbor at %s: %s"
                   (path ~collection:e.collection ~rkey:e.rkey)
                   msg)
          in
          (match value with
          | `Assoc _ -> ()
          | _ ->
              fail
                ("invalid record at "
                ^ path ~collection:e.collection ~rkey:e.rkey));
          take er br
            ({
               collection = e.collection;
               rkey = e.rkey;
               cid = e.cid;
               data = b.Car.data;
               value;
             }
            :: acc)
    in
    let records = take index rest [] in
    { commit; index; state; records }
end
