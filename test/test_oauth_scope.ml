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
      ignore (Oauth_scope.parse_one "repo:app.bsky.*"));
  OUnit2.assert_raises
    (Oauth_scope.Invalid "repo collection must be an NSID or *: not-a-nsid")
    (fun () -> ignore (Oauth_scope.parse_one "repo:not-a-nsid"))

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

let test_official_permission_sets _ =
  List.iter
    (fun nsid ->
      OUnit2.assert_bool nsid (Oauth_scope.is_official_include nsid);
      ignore (Oauth_scope.parse_one ("include:" ^ nsid)))
    Oauth_scope.official_include_nsids;
  OUnit2.assert_bool "unknown include"
    (not (Oauth_scope.is_official_include "app.bsky.authBasicFeatures"));
  (match Oauth_scope.expand_include_nsid "app.bsky.authCreatePosts" with
  | None -> OUnit2.assert_failure "expected bundled authCreatePosts"
  | Some scopes ->
      OUnit2.assert_bool "expands repo:post"
        (List.exists
           (fun s ->
             s.Oauth_scope.resource = Oauth_scope.Repo
             && s.Oauth_scope.positional = Some "app.bsky.feed.post")
           scopes);
      OUnit2.assert_bool "expands video rpc"
        (List.exists
           (fun s ->
             s.Oauth_scope.resource = Oauth_scope.Rpc
             && s.Oauth_scope.positional = Some "app.bsky.video.uploadVideo")
           scopes));
  (match Oauth_scope.expand_include_nsid "app.bsky.authFullApp" with
  | None -> OUnit2.assert_failure "expected bundled authFullApp"
  | Some scopes ->
      OUnit2.assert_bool "expands repo:referencelistoptout"
        (List.exists
           (fun s ->
             s.Oauth_scope.resource = Oauth_scope.Repo
             && s.Oauth_scope.positional
                = Some "app.bsky.graph.referencelistoptout")
           scopes);
      OUnit2.assert_bool "expands bookmark rpc"
        (List.exists
           (fun s ->
             s.Oauth_scope.resource = Oauth_scope.Rpc
             && s.Oauth_scope.positional
                = Some "app.bsky.bookmark.createBookmark")
           scopes));
  (match Oauth_scope.expand_include_nsid "app.bsky.authViewAll" with
  | None -> OUnit2.assert_failure "expected bundled authViewAll"
  | Some scopes ->
      OUnit2.assert_bool "authViewAll is rpc-only"
        (List.for_all
           (fun s -> s.Oauth_scope.resource = Oauth_scope.Rpc)
           scopes));
  match Oauth_scope.expand_include_nsid "app.bsky.authDeleteContent" with
  | None -> OUnit2.assert_failure "expected bundled authDeleteContent"
  | Some scopes ->
      OUnit2.assert_bool "authDeleteContent deletes posts"
        (List.exists
           (fun s ->
             s.Oauth_scope.resource = Oauth_scope.Repo
             && s.Oauth_scope.positional = Some "app.bsky.feed.post"
             && List.mem ("action", "delete") s.Oauth_scope.params)
           scopes)

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
         "test_official_permission_sets" >:: test_official_permission_sets;
       ]

let () = run_test_tt_main suite
