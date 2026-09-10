open OUnit2
open Atproto.Auth

(** Shared skip for unauthenticated public-internet hops. Local
    TestNetwork stays on [ATP_LOCAL_PDS]; credential hops stay on
    [ATP_AUTH]. *)
let skip_unless_public () =
  skip_if
    (not Auth.public_live_enabled)
    "ATP_PUBLIC not set; public network hop skipped"
