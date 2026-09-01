open OUnit2
open Atproto.Cid
open Atproto.Car
open Atproto.Dag_cbor

let test_empty_car_roundtrip _ =
  let cid = Cid.of_digest ~codec:Cid.Dag_cbor (String.make 32 '\x00') in
  let car = { Car.roots = [ cid ]; blocks = [] } in
  let parsed = Car.parse (Car.encode car) in
  OUnit2.assert_equal 1 (List.length parsed.roots);
  OUnit2.assert_bool "root CID mismatch" (Cid.equal cid (List.hd parsed.roots));
  OUnit2.assert_equal 0 (List.length parsed.blocks)

let test_car_with_block _ =
  let cid = Cid.of_digest ~codec:Cid.Raw (String.make 32 '\x01') in
  let data = "hello-car" in
  let car = { Car.roots = [ cid ]; blocks = [ { Car.cid; data } ] } in
  let parsed = Car.parse (Car.encode car) in
  match Car.find_block parsed cid with
  | None -> OUnit2.assert_failure "expected block missing after CAR roundtrip"
  | Some block -> OUnit2.assert_equal ~printer:(fun x -> x) data block.data

let test_dag_cbor_map _ =
  let encoded =
    Dag_cbor.encode
      (Dag_cbor.Map
         [ ("version", Dag_cbor.Int 1); ("name", Dag_cbor.Text "atproto") ])
  in
  match Dag_cbor.decode encoded with
  | Dag_cbor.Map fields ->
      OUnit2.assert_equal ~printer:string_of_int 1
        (Dag_cbor.as_int (Dag_cbor.require "version" fields));
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "atproto"
        (Dag_cbor.as_text (Dag_cbor.require "name" fields))
  | _ -> OUnit2.assert_failure "expected DAG-CBOR map"

let test_follows_order_and_reorder _ =
  let a = Cid.of_digest ~codec:Cid.Raw (String.make 32 '\x0a') in
  let b = Cid.of_digest ~codec:Cid.Raw (String.make 32 '\x0b') in
  let c = Cid.of_digest ~codec:Cid.Raw (String.make 32 '\x0c') in
  let car =
    {
      Car.roots = [ a ];
      blocks =
        [
          { Car.cid = c; data = "c" };
          { Car.cid = a; data = "a" };
          { Car.cid = b; data = "b" };
        ];
    }
  in
  OUnit2.assert_bool "shuffled is not pre-order"
    (not (Car.follows_order ~expected:[ a; b; c ] (Car.block_cids car)));
  let ordered = Car.reorder ~expected:[ a; b; c ] car in
  OUnit2.assert_bool "reorder matches"
    (Car.follows_order ~expected:[ a; b; c ] (Car.block_cids ordered));
  OUnit2.assert_equal [ "a"; "b"; "c" ]
    (List.map (fun (b : Car.block) -> b.data) ordered.blocks)

let index_of_sub hay needle =
  let n = String.length needle in
  let rec find i =
    if i + n > String.length hay then failwith ("missing " ^ needle)
    else if String.sub hay i n = needle then i
    else find (i + 1)
  in
  find 0

let test_dag_cbor_ipld_key_sort _ =
  (* Encoded-key order: prev (4) and type (4) before alsoKnownAs (11). *)
  let encoded =
    Dag_cbor.encode
      (Dag_cbor.Map
         [
           ("alsoKnownAs", Dag_cbor.Array []);
           ("prev", Dag_cbor.Null);
           ("type", Dag_cbor.Text "plc_operation");
         ])
  in
  let prev_at = index_of_sub encoded "prev" in
  let aka_at = index_of_sub encoded "alsoKnownAs" in
  OUnit2.assert_bool "IPLD sorts shorter encoded keys first" (prev_at < aka_at)

let test_dag_cbor_cid_tag _ =
  let cid = Cid.of_digest (String.make 32 '\x02') in
  let encoded = Dag_cbor.encode (Dag_cbor.Cid cid) in
  match Dag_cbor.decode encoded with
  | Dag_cbor.Cid again ->
      OUnit2.assert_bool "CID tag 42 roundtrip failed" (Cid.equal cid again)
  | _ -> OUnit2.assert_failure "expected CID value"

let test_dag_cbor_hard_types _ =
  let encoded =
    Dag_cbor.encode
      (Dag_cbor.Map
         [
           ("flag", Dag_cbor.Bool true);
           ("empty", Dag_cbor.Null);
           ("blob", Dag_cbor.Bytes "car");
           ("items", Dag_cbor.Array [ Dag_cbor.Int 1; Dag_cbor.Text "two" ]);
           ("big", Dag_cbor.Int64 1_000_000_000_000L);
         ])
  in
  (match Dag_cbor.decode encoded with
  | Dag_cbor.Map fields ->
      OUnit2.assert_equal true
        (Dag_cbor.as_bool (Dag_cbor.require "flag" fields));
      (match Dag_cbor.require "empty" fields with
      | Dag_cbor.Null -> ()
      | _ -> OUnit2.assert_failure "expected null");
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "car"
        (Dag_cbor.as_bytes (Dag_cbor.require "blob" fields));
      OUnit2.assert_equal 2
        (List.length (Dag_cbor.as_array (Dag_cbor.require "items" fields)));
      OUnit2.assert_equal 1_000_000_000_000L
        (Dag_cbor.as_int64 (Dag_cbor.require "big" fields));
      (* IPLD DAG-CBOR: shorter encoded keys first, then lexicographic *)
      OUnit2.assert_equal
        [ "big"; "blob"; "flag"; "empty"; "items" ]
        (List.map fst fields)
  | _ -> OUnit2.assert_failure "expected map");
  (try
     ignore (Dag_cbor.decode "\x18");
     OUnit2.assert_failure "truncated CBOR accepted"
   with Dag_cbor.Decode_error _ -> ());
  let seq =
    Dag_cbor.decode_sequence
      (Dag_cbor.encode (Dag_cbor.Int 1) ^ Dag_cbor.encode (Dag_cbor.Text "x"))
  in
  OUnit2.assert_equal 2 (List.length seq)

let test_dag_cbor_of_yojson _ =
  let json =
    `Assoc
      [
        ("$type", `String "app.bsky.feed.post");
        ("text", `String "hello");
        ("n", `Int 3);
        ("flag", `Bool true);
        ( "nested",
          `Assoc [ ("$link", `String (Cid.to_string (Cid.create "x"))) ] );
      ]
  in
  (match Dag_cbor.of_yojson json with
  | Dag_cbor.Map fields -> (
      OUnit2.assert_equal "hello"
        (Dag_cbor.as_text (Dag_cbor.require "text" fields));
      OUnit2.assert_equal 3 (Dag_cbor.as_int (Dag_cbor.require "n" fields));
      match Dag_cbor.require "nested" fields with
      | Dag_cbor.Cid _ -> ()
      | _ -> OUnit2.assert_failure "expected $link CID")
  | _ -> OUnit2.assert_failure "expected map");
  let bytes_json = `Assoc [ ("$bytes", `String "Y2Fy") ] in
  match Dag_cbor.of_yojson bytes_json with
  | Dag_cbor.Bytes b -> OUnit2.assert_equal ~printer:(fun x -> x) "car" b
  | _ -> OUnit2.assert_failure "expected $bytes"

let suite =
  "car"
  >::: [
         "test_empty_car_roundtrip" >:: test_empty_car_roundtrip;
         "test_car_with_block" >:: test_car_with_block;
         "test_follows_order_and_reorder" >:: test_follows_order_and_reorder;
         "test_dag_cbor_map" >:: test_dag_cbor_map;
         "test_dag_cbor_ipld_key_sort" >:: test_dag_cbor_ipld_key_sort;
         "test_dag_cbor_cid_tag" >:: test_dag_cbor_cid_tag;
         "test_dag_cbor_hard_types" >:: test_dag_cbor_hard_types;
         "test_dag_cbor_of_yojson" >:: test_dag_cbor_of_yojson;
       ]

let () = run_test_tt_main suite
