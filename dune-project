(lang dune 3.11)
(name atproto)
(version 1.0.2)
(generate_opam_files true)

(license MIT)
(authors "david-engelmann <david.engelmann44@gmail.com>")
(maintainers "david-engelmann <david.engelmann44@gmail.com>")
(source
 (github david-engelmann/atproto))
(documentation "https://david-engelmann.github.io/atproto/")

(package
 (name atproto)
 (synopsis "Typed OCaml toolkit for the AT Protocol")
 (description
  "Typed OCaml toolkit for the AT Protocol. Resolve identities, read and write repositories, follow the firehose, and call AppView, Ozone, and hosted Bluesky products (chat, video, Jetstream) from one library. Protocol pieces (XRPC, CID/CAR/MST, lexicons, OAuth/DPoP) are implemented here, not left as raw HTTP.\n\nThis package is a client. It does not host a PDS, chat service, video transcoder, Tap, SMS gateway, or push backend.\n\nKey modules: Auth, Session, Identity, Actor, Feed, Graph, Repo, Records, Sync, Repo_sync, Firehose, Jetstream, Oauth, Chat, Video, Ozone, Notification, Contact, Lexicon, and experimental Lt_hash / At_uri.Space / Space_* (proposal 0016; not a spaces product API).\n\nUntil atproto.1.0.2 is published, pin the GitHub repository (see the README); then `opam install atproto`. Requires OCaml >= 4.14.1 and < 5.4 (CI: 4.14.1 and 5.3.0). Jane Street core / async / ppx_jane / zstandard are >= v0.16.0 and < v0.18~ (v0.16 on OCaml 4.14, v0.17 on OCaml 5.1-5.3). System libzstd is required.")
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
  (digestif
   (>= 1.1.2))
  (mirage-crypto-ec
   (>= 1.2.0))
  (mirage-crypto-rng
   (>= 1.2.0))
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
