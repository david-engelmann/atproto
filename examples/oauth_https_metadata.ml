(* Offline sketch: publish HTTPS client-metadata and drive authorize →
   code → token. This is not a hosted login UI. An application still
   serves the JSON over HTTPS at the client_id URL and receives the
   browser redirect.

   Tiny server sketch (production must be HTTPS, HTTP 200, application/json):

     match path with
     | "/oauth-client-metadata.json" | "/client-metadata.json" ->
         Oauth.metadata_http_response meta
     | "/cb" -> (* parse the redirect query; then Oauth.complete_browser_login *)
         ...
     | _ -> 404
*)

open Atproto.Oauth

let example_client_id = "https://client.example/oauth-client-metadata.json"
let example_redirect = "https://client.example/cb"

let () =
  let client_id = Oauth.https_client_id ~host:"client.example" () in
  assert (client_id = example_client_id);
  let meta =
    Oauth.public_https_metadata ~client_id ~redirect_uris:[ example_redirect ]
      ~client_name:"Example AT Protocol client" ()
  in
  Oauth.validate_https_metadata meta;
  Oauth.expect_declared_redirect meta example_redirect;
  Oauth.expect_declared_scope meta ~requested:Oauth.default_scope;
  let served = Oauth.metadata_http_response meta in
  assert (served.status = 200);
  assert (List.assoc "Content-Type" served.headers = "application/json");
  let parsed = Oauth.metadata_of_json (Yojson.Safe.from_string served.body) in
  assert (parsed.client_id = client_id);
  (match Sys.argv with
  | [| _; path |] ->
      let json = Yojson.Safe.from_file path in
      let file_meta = Oauth.metadata_of_json json in
      Oauth.validate_https_metadata file_meta;
      assert (file_meta.client_id = example_client_id);
      assert (file_meta.redirect_uris = [ example_redirect ]);
      assert (file_meta.scope = Oauth.default_scope);
      assert (file_meta.token_endpoint_auth_method = "none");
      assert file_meta.dpop_bound_access_tokens
  | _ -> ());
  let authorize =
    Oauth.authorize_redirect_url
      ~authorization_endpoint:"https://bsky.social/oauth/authorize"
      ~client_id ~request_uri:"urn:ietf:params:oauth:request_uri:demo"
  in
  assert (String.contains authorize '?');
  (match
     Oauth.parse_redirect
       (example_redirect
      ^ "?code=splendid&state=abc&iss=https%3A%2F%2Fbsky.social")
   with
  | Oauth.Authorized { code; state; iss } ->
      assert (code = "splendid");
      assert (state = "abc");
      assert (iss = Some "https://bsky.social")
  | Oauth.Denied _ -> assert false);
  let token =
    Oauth.token_body ~client_id ~redirect_uri:example_redirect ~code:"splendid"
      ~code_verifier:"dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk" ()
  in
  assert (List.assoc "grant_type" token = "authorization_code");
  print_endline
    "oauth_https_metadata: HTTPS client-metadata + browser-login sketch ok"
