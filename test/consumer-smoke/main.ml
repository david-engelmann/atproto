(* Isolated downstream consumer: links the installed [atproto] package.
   No network, hosted services, or source-tree modules. *)

open Atproto.Tid
open Atproto.Syntax
open Atproto.Oauth

let () =
  assert (Tid.is_valid "3jzfcijpj2z2a");
  assert (Syntax.is_valid_nsid "app.bsky.feed.post");
  assert (Syntax.is_valid_handle "alice.test");
  assert (Syntax.is_valid_did "did:plc:abc123xyz0001112223333");
  let meta =
    Oauth.public_https_metadata
      ~client_id:"https://client.example/oauth-client-metadata.json"
      ~redirect_uris:[ "https://client.example/cb" ]
      ()
  in
  Oauth.validate_https_metadata meta;
  ignore (Oauth.metadata_http_response meta);
  print_endline "consumer-smoke: installed atproto package links"
