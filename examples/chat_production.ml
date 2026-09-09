(* Offline sketch: wire a production chat.bsky client against hosted
   Bluesky chat (did:web:api.bsky.chat#bsky_chat). This is not an OSS
   chat backend. Official @atproto/dev-env 0.6.4 TestNetwork does not
   start chat (ozone.chatUrl = localhost:2590, "must run separate chat
   service"). No network is used here.

   After OAuth (not createSession-only):

     1. Declare DM scopes on the HTTPS client-metadata document and
        PAR ([Oauth.default_chat_scope] or
        [Oauth_scope.full_chat_client_scope]).
     2. Complete browser login ([Oauth.start_browser_login] /
        [Oauth.complete_browser_login]).
     3. Mint getServiceAuth (aud = [Chat.service_aud], lxm = the
        chat.bsky.* NSID). DPoP cannot be sent through atproto-proxy.
     4. Call [Chat.list_convos_service] / [get_messages_service] /
        [send_message_service] on [Chat.default_host] with that JWT.

   Password sessions with a privileged app-password still use
   [Chat.list_convos] + [atproto-proxy] through the PDS.
*)

open Atproto.Chat
open Atproto.Oauth
open Atproto.Oauth_scope
open Atproto.Xrpc

let example_client_id = "https://client.example/oauth-client-metadata.json"
let example_redirect = "https://client.example/cb"

let () =
  assert (Oauth.default_scope = "atproto transition:generic");
  assert (
    Oauth.default_chat_scope = "atproto transition:generic transition:chat.bsky");
  assert (Oauth.default_chat_scope = Oauth_scope.default_chat_scope);
  assert (
    Oauth_scope.full_chat_client_scope
    = "atproto include:chat.bsky.authFullChatClient");
  ignore (Oauth_scope.parse_and_require Oauth.default_chat_scope);
  ignore (Oauth_scope.parse_and_require Oauth_scope.full_chat_client_scope);
  assert (Oauth_scope.has_chat Oauth.default_chat_scope);
  assert (Oauth_scope.has_chat Oauth_scope.full_chat_client_scope);
  assert (Oauth_scope.has_chat Oauth_scope.transition_chat);
  assert (not (Oauth_scope.has_chat Oauth.default_scope));
  let meta =
    Oauth.public_https_metadata ~client_id:example_client_id
      ~redirect_uris:[ example_redirect ] ~scope:Oauth.default_chat_scope ()
  in
  Oauth.validate_https_metadata meta;
  Oauth.expect_declared_scope meta ~requested:Oauth.default_chat_scope;
  Oauth.expect_declared_scope meta ~requested:Oauth_scope.transition_chat;
  assert (
    Xrpc.proxy_to_string (Chat.effective_proxy ())
    = "did:web:api.bsky.chat#bsky_chat");
  assert (Chat.default_host = "api.bsky.chat");
  assert (Chat.service_aud () = "did:web:api.bsky.chat");
  let override =
    { Xrpc.did = "did:web:chat.example.com"; service = "bsky_chat" }
  in
  assert (Chat.service_aud ~proxy:override () = "did:web:chat.example.com");
  assert (
    Xrpc.proxy_to_string (Chat.effective_proxy ~proxy:override ())
    = "did:web:chat.example.com#bsky_chat");
  let list = Chat.list_convos_body ~limit:10 ~status:"accepted" () in
  assert (List.assoc "limit" list = "10");
  assert (List.assoc "status" list = "accepted");
  let get = Chat.get_convo_body ~convo_id:"c1" () in
  assert (List.assoc "convoId" get = "c1");
  let members =
    Chat.get_convo_for_members_body
      ~members:[ "did:plc:abc123xyz0001112223333" ]
      ()
  in
  assert (List.assoc "members" members = "did:plc:abc123xyz0001112223333");
  let msgs = Chat.get_messages_body ~convo_id:"c1" ~limit:20 () in
  assert (List.assoc "convoId" msgs = "c1");
  assert (List.assoc "limit" msgs = "20");
  let send = Chat.send_message_body ~convo_id:"c1" ~text:"hello" () in
  let open Yojson.Safe.Util in
  assert (send |> member "convoId" |> to_string = "c1");
  assert (send |> member "message" |> member "text" |> to_string = "hello");
  let aud_body =
    Xrpc.service_auth_body ~aud:(Chat.service_aud ())
      ~lxm:"chat.bsky.convo.listConvos" ()
  in
  assert (aud_body |> member "aud" |> to_string = "did:web:api.bsky.chat");
  assert (aud_body |> member "lxm" |> to_string = "chat.bsky.convo.listConvos");
  print_endline "chat_production: hosted chat.bsky production wiring sketch ok"
