open OUnit2
open Atproto.Oauth_scope

let test_atproto_and_transition _ =
  let scopes = Oauth_scope.parse "atproto transition:generic" in
  OUnit2.assert_equal 2 (List.length scopes);
  Oauth_scope.require_atproto scopes;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "atproto transition:generic"
    (Oauth_scope.serialize scopes)

let test_repo_and_rpc _ =
  let repo = Oauth_scope.parse_one "repo:app.bsky.feed.post" in
  OUnit2.assert_equal Oauth_scope.Repo repo.resource;
  OUnit2.assert_equal (Some "app.bsky.feed.post") repo.positional;
  let rpc =
    Oauth_scope.parse_one
      "rpc:com.atproto.moderation.createReport?aud=did:web:api.bsky.app%23bsky_appview"
  in
  OUnit2.assert_equal
    [ "com.atproto.moderation.createReport" ]
    (Oauth_scope.lxm_of rpc);
  OUnit2.assert_equal
    [ "did:web:api.bsky.app#bsky_appview" ]
    (Oauth_scope.aud_of rpc)

let test_rpc_double_wildcard_rejected _ =
  OUnit2.assert_raises
    (Oauth_scope.Invalid "rpc scope cannot wildcard both lxm and aud")
    (fun () -> ignore (Oauth_scope.parse_one "rpc:*?aud=*"))

let test_repo_glob_rejected _ =
  OUnit2.assert_raises
    (Oauth_scope.Invalid "repo collection globs are not allowed") (fun () ->
      ignore (Oauth_scope.parse_one "repo:app.bsky.*"))

let test_blob_and_include _ =
  let blob = Oauth_scope.parse_one "blob:*/*" in
  OUnit2.assert_equal Oauth_scope.Blob blob.resource;
  let inc = Oauth_scope.parse_one "include:app.bsky.authBasicFeatures?aud=*" in
  OUnit2.assert_equal Oauth_scope.Include inc.resource;
  OUnit2.assert_equal (Some "app.bsky.authBasicFeatures") inc.positional

let test_account_identity _ =
  let acc = Oauth_scope.parse_one "account:email?action=read" in
  OUnit2.assert_equal Oauth_scope.Account acc.resource;
  let id_ = Oauth_scope.parse_one "identity:handle" in
  OUnit2.assert_equal Oauth_scope.Identity id_.resource

let test_subset_and_require _ =
  OUnit2.assert_bool "subset"
    (Oauth_scope.is_subset ~requested:"atproto repo:app.bsky.feed.post"
       ~declared:"atproto repo:app.bsky.feed.post transition:generic");
  OUnit2.assert_bool "not subset"
    (not
       (Oauth_scope.is_subset ~requested:"atproto transition:generic"
          ~declared:"atproto"));
  OUnit2.assert_raises (Oauth_scope.Invalid "scope list must include atproto")
    (fun () -> ignore (Oauth_scope.parse_and_require "repo:app.bsky.feed.post"))

let test_metadata_parses_granular _ =
  let meta =
    Atproto.Oauth.Oauth.public_metadata
      ~client_id:"https://client.example/client-metadata.json"
      ~redirect_uris:[ "https://client.example/cb" ]
      ~scope:
        "atproto repo:app.bsky.actor.profile \
         rpc:app.bsky.feed.getTimeline?aud=*"
      ()
  in
  Atproto.Oauth.Oauth.validate_metadata meta

let suite =
  "oauth_scope"
  >::: [
         "test_atproto_and_transition" >:: test_atproto_and_transition;
         "test_repo_and_rpc" >:: test_repo_and_rpc;
         "test_rpc_double_wildcard_rejected"
         >:: test_rpc_double_wildcard_rejected;
         "test_repo_glob_rejected" >:: test_repo_glob_rejected;
         "test_blob_and_include" >:: test_blob_and_include;
         "test_account_identity" >:: test_account_identity;
         "test_subset_and_require" >:: test_subset_and_require;
         "test_metadata_parses_granular" >:: test_metadata_parses_granular;
       ]

let () = run_test_tt_main suite
