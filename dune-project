(lang dune 3.11)
(name atproto)
(version 1.0.0)
(generate_opam_files true)

(license MIT)
(authors "david-engelmann <david.engelmann44@gmail.com>")
(maintainers "david-engelmann <david.engelmann44@gmail.com>")
(source
 (github david-engelmann/atproto))
(documentation "https://david-engelmann.github.io/atproto/")

(package
 (name atproto)
 (synopsis "OCaml toolkit for the AT Protocol")
 (description
  "OCaml client and protocol library for the AT Protocol (XRPC, lexicons, repo sync, identity, AppView, Ozone, chat, site.standard, and com.germnetwork records). An HTTP/2 TLS client is included for XRPC calls that need status and response headers.\n\nPublic modules include Auth, Session, Actor, Feed, Graph, Bookmark, Video, Unspecced, Labeler, Chat, Ozone, Admin, Repo, Repo_sync, Server, Identity, Did_plc, Did_web, Did_key, Sync, Cid, Car, Dag_cbor, Mst, Tid, At_uri, Lexicon, Temp, Firehose, Websocket, Jetstream, Oauth, Oauth_scope, Label, Xrpc, Error, Syntax, Embed, Facet, Records, Notification, Draft, Contact, Ageassurance, Moderation, Site, Germnetwork, Http_client, Request, Response, and experimental Lt_hash / Space_commit / Space_credential / Space_xrpc / At_uri.Space (proposal 0016 commit digest, deniable signed commits, space URIs, space-credential JWT / DPoP, and draft com.atproto.space.* XRPC helpers; not a spaces product API). Password chat clients send atproto-proxy (default did:web:api.bsky.chat#bsky_chat, the session DID-document #bsky_chat service, or ATP_CHAT_DID). OAuth DPoP cannot be proxied; list_convos_service / get_messages_service / send_message_service call api.bsky.chat with a getServiceAuth JWT. This package does not host an OSS chat backend.\n\nVideo support is a client for the hosted video service (byte upload + job-status poll). This package does not host a PDS, OAuth client-metadata document, browser login, or video transcoder.\n\nThis package is published on the public opam-repository (`opam install atproto`). Pin the GitHub repository for development (see the README). Requires OCaml >= 4.14.1 and < 5.4 (CI: 4.14.1 and 5.3.0). Jane Street core / async / ppx_jane / zstandard are pinned to >= v0.16.0 and < v0.18~ (v0.16 on OCaml 4.14, v0.17 on OCaml 5.1-5.3). System libzstd is required.")
 (tags
  (atproto bluesky xrpc lexicon))
 (depends
  (ocaml
   (and (>= 4.14.1) (< 5.4)))
  (core
   (and
    (>= v0.16.0)
    (< v0.18~)))
  (async
   (and
    (>= v0.16.0)
    (< v0.18~)))
  (uri
   (>= 4.2.0))
  yojson
  ptime
  (lwt
   (>= 5.6.1))
  (h2
   (>= 0.10.0))
  (h2-lwt-unix
   (>= 0.10.0))
  (httpaf
   (>= 0.7.1))
  httpaf-lwt-unix
  (cohttp
   (>= 5.1.0))
  cohttp-lwt-unix
  cohttp-async
  ssl
  lwt_ssl
  jose
  digestif
  mirage-crypto-ec
  mirage-crypto-rng
  zarith
  (zstandard
   (and
    (>= v0.16.0)
    (< v0.18~)))
  (ppx_jane
   (and
    (>= v0.16.0)
    (< v0.18~)))
  (ounit2 :with-test)))
