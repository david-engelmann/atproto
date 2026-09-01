(lang dune 2.9)
(name atproto)
(version 0.1.0)
(generate_opam_files true)

(license MIT)
(authors "david-engelmann <david.engelmann44@gmail.com>")
(maintainers "david-engelmann <david.engelmann44@gmail.com>")
(source
 (github david-engelmann/atproto))
(documentation "https://github.com/david-engelmann/atproto")

(package
 (name atproto)
 (synopsis "OCaml toolkit for the AT Protocol")
 (description
  "OCaml client and protocol library for the AT Protocol (XRPC, lexicons, repo sync, identity, AppView, Ozone, chat, site.standard, and com.germnetwork records). An HTTP/2 TLS client is included for XRPC calls that need status and response headers.\n\nPublic modules include Auth, Session, Actor, Feed, Graph, Bookmark, Video, Unspecced, Labeler, Chat, Ozone, Admin, Repo, Repo_sync, Server, Identity, Did_plc, Did_web, Did_key, Sync, Cid, Car, Dag_cbor, Mst, Tid, At_uri, Lexicon, Temp, Firehose, Websocket, Jetstream, Oauth, Oauth_scope, Label, Xrpc, Error, Syntax, Embed, Facet, Records, Notification, Draft, Contact, Ageassurance, Moderation, Site, Germnetwork, Http_client, Request, and Response. Chat convo/actor clients always send atproto-proxy (default did:web:api.bsky.chat#bsky_chat, the session DID-document #bsky_chat service, or ATP_CHAT_DID).\n\nVideo support is a client for the hosted video service (byte upload + job-status poll). This package does not host a PDS, OAuth client-metadata document, browser login, or video transcoder.\n\nThis package is not published to the public opam-repository; pin the GitHub repository (see the README). Requires OCaml 4.14.1.")
 (tags
  (atproto bluesky xrpc lexicon))
 (depends
  (ocaml
   (>= 4.14.1))
  core
  async
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
  ppx_jane
  (ounit2 :with-test)
  (ocamlformat
   (and :with-test
        (= 0.25.1)))))
