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
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:plc:xov3uvxfd4to6ev3ak5g5uxk" u.authority;
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
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    sample_uri_with_search_params.host back.host;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    sample_uri_with_search_params.path_name back.path_name

(* Official space URI from bluesky-social/atproto#5187 repo-commit tests. *)
let official_space = "at://did:example:space/space/app.bsky.group/test"

let official_record =
  "at://did:example:space/space/app.bsky.group/test/did:example:alice/app.bsky.feed.post/3jzfcijpj2z2a"

let test_space_uri_roundtrip _ =
  let u = Space.of_string official_space in
  OUnit2.assert_bool "not a record" (not (Space.is_record u));
  let s = Space.space_of u in
  OUnit2.assert_equal ~printer:(fun x -> x) "did:example:space" s.space_did;
  OUnit2.assert_equal ~printer:(fun x -> x) "app.bsky.group" s.space_type;
  OUnit2.assert_equal ~printer:(fun x -> x) "test" s.skey;
  OUnit2.assert_equal ~printer:(fun x -> x) official_space (Space.to_string u)

let test_space_record_uri_roundtrip _ =
  let u = Space.of_string official_record in
  OUnit2.assert_bool "is a record" (Space.is_record u);
  match u with
  | Space.Space _ -> OUnit2.assert_failure "expected record"
  | Space.Record r ->
      OUnit2.assert_equal ~printer:(fun x -> x) "did:example:space" r.space_did;
      OUnit2.assert_equal ~printer:(fun x -> x) "did:example:alice" r.author_did;
      OUnit2.assert_equal ~printer:(fun x -> x) "app.bsky.feed.post"
        r.collection;
      OUnit2.assert_equal ~printer:(fun x -> x) "3jzfcijpj2z2a" r.rkey;
      OUnit2.assert_equal ~printer:(fun x -> x) official_record
        (Space.to_string u);
      OUnit2.assert_equal ~printer:(fun x -> x) official_space
        (Space.to_string (Space.Space (Space.space_of u)))

let test_space_builders _ =
  let s =
    Space.space ~space_did:"did:example:space" ~space_type:"app.bsky.group"
      ~skey:"test"
  in
  OUnit2.assert_equal ~printer:(fun x -> x) official_space (Space.to_string s);
  let r =
    Space.record ~space_did:"did:example:space" ~space_type:"app.bsky.group"
      ~skey:"test" ~author_did:"did:example:alice"
      ~collection:"app.bsky.feed.post" ~rkey:"3jzfcijpj2z2a"
  in
  OUnit2.assert_equal ~printer:(fun x -> x) official_record (Space.to_string r)

let test_space_marker_distinguishes _ =
  OUnit2.assert_bool "space URI" (Space.is_space_uri official_space);
  OUnit2.assert_bool "record URI" (Space.is_space_uri official_record);
  OUnit2.assert_bool "public URI"
    (not
       (Space.is_space_uri
          "at://did:plc:xov3uvxfd4to6ev3ak5g5uxk/app.bsky.feed.post/3jyf6gx25eb27"));
  OUnit2.assert_bool "authority only"
    (not (Space.is_space_uri "at://did:example:space"))

let test_classify _ =
  (match classify official_space with
  | Space u ->
      OUnit2.assert_equal ~printer:(fun x -> x) official_space
        (Space.to_string u)
  | Public _ -> OUnit2.assert_failure "expected space");
  match
    classify
      "at://did:plc:xov3uvxfd4to6ev3ak5g5uxk/app.bsky.feed.post/3jyf6gx25eb27"
  with
  | Public u ->
      OUnit2.assert_equal (Some "app.bsky.feed.post") u.collection
  | Space _ -> OUnit2.assert_failure "expected public"

let test_public_parser_rejects_space_record _ =
  OUnit2.assert_raises
    (Failure "Uri.of_string: more than two path segments") (fun () ->
      ignore (Uri.of_string official_record))

let test_space_rejects_public _ =
  OUnit2.assert_raises
    (Space.Invalid "not a space URI (first path segment must be literal space)")
    (fun () ->
      ignore
        (Space.of_string
           "at://did:plc:xov3uvxfd4to6ev3ak5g5uxk/app.bsky.feed.post/3jyf6gx25eb27"))

let test_space_rejects_bad_count _ =
  OUnit2.assert_raises
    (Space.Invalid
       "space URI must be at://{did}/space/{type}/{skey} or \
        at://{did}/space/{type}/{skey}/{author}/{collection}/{rkey}") (fun () ->
      ignore
        (Space.of_string "at://did:example:space/space/app.bsky.group"))

let test_space_rejects_query_fragment_slash _ =
  OUnit2.assert_raises
    (Space.Invalid "query is not allowed on space URIs") (fun () ->
      ignore (Space.of_string (official_space ^ "?foo=1")));
  OUnit2.assert_raises
    (Space.Invalid "fragment is not allowed on space URIs") (fun () ->
      ignore (Space.of_string (official_space ^ "#frag")));
  OUnit2.assert_raises
    (Space.Invalid "trailing slash is not allowed") (fun () ->
      ignore (Space.of_string (official_space ^ "/")))

let test_space_rejects_invalid_components _ =
  OUnit2.assert_raises
    (Space.Invalid "invalid space authority DID not-a-did") (fun () ->
      ignore (Space.of_string "at://not-a-did/space/app.bsky.group/test"));
  OUnit2.assert_raises
    (Space.Invalid "invalid space type NSID not-an-nsid") (fun () ->
      ignore (Space.of_string "at://did:example:space/space/not-an-nsid/test"))

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
         "test_space_uri_roundtrip" >:: test_space_uri_roundtrip;
         "test_space_record_uri_roundtrip" >:: test_space_record_uri_roundtrip;
         "test_space_builders" >:: test_space_builders;
         "test_space_marker_distinguishes" >:: test_space_marker_distinguishes;
         "test_classify" >:: test_classify;
         "test_public_parser_rejects_space_record"
         >:: test_public_parser_rejects_space_record;
         "test_space_rejects_public" >:: test_space_rejects_public;
         "test_space_rejects_bad_count" >:: test_space_rejects_bad_count;
         "test_space_rejects_query_fragment_slash"
         >:: test_space_rejects_query_fragment_slash;
         "test_space_rejects_invalid_components"
         >:: test_space_rejects_invalid_components;
       ]

let () = run_test_tt_main suite
