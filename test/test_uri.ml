open OUnit2
open Bluesky.Uri

let sample_uri_without_search_params : Uri.uri = {
      host = "david.host.com";
      path_name = "io.example.song";
      hash = "3yI5-c1z-cc2p-1a";
      search_params = None;
    }

let sample_uri_with_search_params : Uri.uri = {
      host = "david.host.com";
      path_name = "io.example.song";
      hash = "3yI5-c1z-cc2p-1a";
      search_params = Some [("july", "jackson")];
    }

let test_sample_uri_with_search_params_host _ =
    match sample_uri_with_search_params with
     | { host; _ } ->
        OUnit2.assert_equal "david.host.com" host

let test_sample_uri_with_search_params_path_name _ =
    match sample_uri_with_search_params with
     | { path_name; _ } ->
        OUnit2.assert_equal "io.example.song" path_name

let test_sample_uri_with_search_params_hash _ =
    match sample_uri_with_search_params with
     | { hash; _ } ->
        OUnit2.assert_equal "3yI5-c1z-cc2p-1a" hash

let test_sample_uri_with_search_params_search_params _ =
    match sample_uri_with_search_params with
     | { search_params; _ } ->
      match search_params with
       | Some p ->
         match p with
          | (param_name, _) :: _ ->
            OUnit2.assert_equal "july" param_name
          | _ -> OUnit2.assert_equal 0 1
       | None -> OUnit2.assert_equal 0 1

let test_sample_uri_without_search_params_host _ =
    match sample_uri_without_search_params with
     | { host; _ } ->
        OUnit2.assert_equal "david.host.com" host

let test_sample_uri_without_search_params_path_name _ =
    match sample_uri_without_search_params with
     | { path_name; _ } ->
        OUnit2.assert_equal "io.example.song" path_name

let test_sample_uri_without_search_params_hash _ =
    match sample_uri_without_search_params with
     | { hash; _ } ->
        OUnit2.assert_equal "3yI5-c1z-cc2p-1a" hash

let test_sample_uri_without_search_params_search_params _ =
    match sample_uri_without_search_params with
     | { search_params; _ } ->
      match search_params with
       | None -> OUnit2.assert_equal 1 1
       | Some _ -> OUnit2.assert_equal 0 1

let suite =
  "suite"
  >::: [
         "test_sample_uri_with_search_params_host" >:: test_sample_uri_with_search_params_host;
         "test_sample_uri_with_search_params_path_name" >:: test_sample_uri_with_search_params_path_name;
         "test_sample_uri_with_search_params_hash" >:: test_sample_uri_with_search_params_hash;
         "test_sample_uri_with_search_params_search_params" >:: test_sample_uri_with_search_params_search_params;
         "test_sample_uri_without_search_params_host" >:: test_sample_uri_without_search_params_host;
         "test_sample_uri_without_search_params_path_name" >:: test_sample_uri_without_search_params_path_name;
         "test_sample_uri_without_search_params_hash" >:: test_sample_uri_without_search_params_hash;
         "test_sample_uri_without_search_params_search_params" >:: test_sample_uri_without_search_params_search_params;
       ]

let () = run_test_tt_main suite
