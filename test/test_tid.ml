open OUnit2
open Atproto.Tid

let test_zero _ =
  OUnit2.assert_equal ~printer:(fun x -> x) Tid.zero (Tid.of_int64 0L);
  OUnit2.assert_equal ~printer:Int64.to_string 0L (Tid.to_int64 Tid.zero)

let test_official_example _ =
  OUnit2.assert_bool "3jzfcijpj2z2a" (Tid.is_valid "3jzfcijpj2z2a");
  OUnit2.assert_equal ~printer:(fun x -> x) "3jzfcijpj2z2a"
    (Tid.of_string "3jzfcijpj2z2a")

let test_roundtrip _ =
  let samples = [ 0L; 1L; 1024L; 1_700_000_000_000_000L; Int64.max_int ] in
  List.iter
    (fun n ->
      let encoded = Tid.of_int64 n in
      OUnit2.assert_equal 13 (String.length encoded);
      OUnit2.assert_bool encoded (Tid.is_valid encoded);
      let decoded = Tid.to_int64 encoded in
      let expected = Int64.logand n 0x7FFF_FFFF_FFFF_FFFFL in
      OUnit2.assert_equal ~printer:Int64.to_string expected decoded)
    samples

let test_timestamp_and_clock _ =
  let tid = Tid.create ~clock_id:42 1_700_000_000_000_123L in
  OUnit2.assert_equal ~printer:Int64.to_string 1_700_000_000_000_123L
    (Tid.timestamp_us tid);
  OUnit2.assert_equal ~printer:string_of_int 42 (Tid.clock_id tid)

let test_sorts_with_time _ =
  let older = Tid.create ~clock_id:0 1000L in
  let newer = Tid.create ~clock_id:0 2000L in
  OUnit2.assert_bool "string order matches time" (older < newer)

let test_rejects_bad_syntax _ =
  let bad =
    [
      "";
      "short";
      "22222222222221";
      "l234567abcdef";
      "k234567abcdef";
      "3jzfcijpj2z2A";
    ]
  in
  List.iter
    (fun s ->
      OUnit2.assert_bool ("accepted " ^ s) (not (Tid.is_valid s));
      OUnit2.assert_bool ("of_string " ^ s)
        (try
           ignore (Tid.of_string s);
           false
         with Tid.Invalid _ -> true))
    bad

let test_now_is_valid _ =
  Random.init 7;
  let tid = Tid.now ~clock_id:1 () in
  OUnit2.assert_bool tid (Tid.is_valid tid)

let suite =
  "tid"
  >::: [
         "test_zero" >:: test_zero;
         "test_official_example" >:: test_official_example;
         "test_roundtrip" >:: test_roundtrip;
         "test_timestamp_and_clock" >:: test_timestamp_and_clock;
         "test_sorts_with_time" >:: test_sorts_with_time;
         "test_rejects_bad_syntax" >:: test_rejects_bad_syntax;
         "test_now_is_valid" >:: test_now_is_valid;
       ]

let () = run_test_tt_main suite
