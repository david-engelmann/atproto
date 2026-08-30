open OUnit2
open Atproto.At_uri

let sample_uri_without_search_params : Uri.uri =
  {
    host = "david.host.com";
    path_name = "io.example.song";
    hash = "3yI5-c1z-cc2p-1a";
    search_params = None;
  }

let sample_uri_with_search_params : Uri.uri =
  {
    host = "david.host.com";
    path_name = "io.example.song";
    hash = "3yI5-c1z-cc2p-1a";
    search_params = Some [ ("july", "jackson") ];
  }

let test_sample_uri_with_search_params_host _ =
  match sample_uri_with_search_params with
  | { host; _ } -> OUnit2.assert_equal "david.host.com" host

let test_sample_uri_with_search_params_path_name _ =
  match sample_uri_with_search_params with
  | { path_name; _ } -> OUnit2.assert_equal "io.example.song" path_name

let test_sample_uri_with_search_params_hash _ =
  match sample_uri_with_search_params with
  | { hash; _ } -> OUnit2.assert_equal "3yI5-c1z-cc2p-1a" hash

let test_sample_uri_with_search_params_search_params _ =
  match sample_uri_with_search_params with
  | { search_params; _ } -> (
      match search_params with
      | Some ((param_name, _) :: _) -> OUnit2.assert_equal "july" param_name
      | _ -> OUnit2.assert_failure "expected search params")

let test_sample_uri_without_search_params_host _ =
  match sample_uri_without_search_params with
  | { host; _ } -> OUnit2.assert_equal "david.host.com" host

let test_sample_uri_without_search_params_path_name _ =
  match sample_uri_without_search_params with
  | { path_name; _ } -> OUnit2.assert_equal "io.example.song" path_name

let test_sample_uri_without_search_params_hash _ =
  match sample_uri_without_search_params with
  | { hash; _ } -> OUnit2.assert_equal "3yI5-c1z-cc2p-1a" hash

let test_sample_uri_without_search_params_search_params _ =
  match sample_uri_without_search_params with
  | { search_params; _ } -> (
      match search_params with
      | None -> OUnit2.assert_equal 1 1
      | Some _ -> OUnit2.assert_failure "did not expect search params")

let test_parse_did_record _ =
  let raw =
    "at://did:plc:xov3uvxfd4to6ev3ak5g5uxk/app.bsky.feed.post/3jyf6gx25eb27"
  in
  let u = Uri.of_string raw in
  OUnit2.assert_equal ~printer:(fun x -> x) "did:plc:xov3uvxfd4to6ev3ak5g5uxk"
    u.authority;
  OUnit2.assert_equal (Some "app.bsky.feed.post") u.collection;
  OUnit2.assert_equal (Some "3jyf6gx25eb27") u.rkey;
  OUnit2.assert_equal ~printer:(fun x -> x) raw (Uri.to_string u)

let test_parse_handle_authority _ =
  let u = Uri.of_string "at://jay.bsky.team/app.bsky.actor.profile/self" in
  OUnit2.assert_equal ~printer:(fun x -> x) "jay.bsky.team" u.authority;
  OUnit2.assert_equal (Some "self") u.rkey

let test_legacy_roundtrip _ =
  let u = Uri.of_legacy sample_uri_with_search_params in
  let back = Uri.to_legacy u in
  OUnit2.assert_equal ~printer:(fun x -> x) sample_uri_with_search_params.host
    back.host;
  OUnit2.assert_equal ~printer:(fun x -> x)
    sample_uri_with_search_params.path_name back.path_name

let suite =
  "at_uri"
  >::: [
         "test_sample_uri_with_search_params_host"
         >:: test_sample_uri_with_search_params_host;
         "test_sample_uri_with_search_params_path_name"
         >:: test_sample_uri_with_search_params_path_name;
         "test_sample_uri_with_search_params_hash"
         >:: test_sample_uri_with_search_params_hash;
         "test_sample_uri_with_search_params_search_params"
         >:: test_sample_uri_with_search_params_search_params;
         "test_sample_uri_without_search_params_host"
         >:: test_sample_uri_without_search_params_host;
         "test_sample_uri_without_search_params_path_name"
         >:: test_sample_uri_without_search_params_path_name;
         "test_sample_uri_without_search_params_hash"
         >:: test_sample_uri_without_search_params_hash;
         "test_sample_uri_without_search_params_search_params"
         >:: test_sample_uri_without_search_params_search_params;
         "test_parse_did_record" >:: test_parse_did_record;
         "test_parse_handle_authority" >:: test_parse_handle_authority;
         "test_legacy_roundtrip" >:: test_legacy_roundtrip;
       ]

let () = run_test_tt_main suite
