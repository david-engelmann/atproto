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

let test_dag_cbor_cid_tag _ =
  let cid = Cid.of_digest (String.make 32 '\x02') in
  let encoded = Dag_cbor.encode (Dag_cbor.Cid cid) in
  match Dag_cbor.decode encoded with
  | Dag_cbor.Cid again ->
      OUnit2.assert_bool "CID tag 42 roundtrip failed" (Cid.equal cid again)
  | _ -> OUnit2.assert_failure "expected CID value"

let suite =
  "car"
  >::: [
         "test_empty_car_roundtrip" >:: test_empty_car_roundtrip;
         "test_car_with_block" >:: test_car_with_block;
         "test_follows_order_and_reorder" >:: test_follows_order_and_reorder;
         "test_dag_cbor_map" >:: test_dag_cbor_map;
         "test_dag_cbor_cid_tag" >:: test_dag_cbor_cid_tag;
       ]

let () = run_test_tt_main suite
