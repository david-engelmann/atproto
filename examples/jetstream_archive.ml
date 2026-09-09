(* Offline sketch: wire Jetstream archive HTTP against a supplied
   operator API key. This is not an invented credential and does not
   hit the network.

   Bluesky-hosted replay / snapshot HTTP (planSnapshot, listSegments,
   getSegment, getBlock, planBackfill) needs a key from
   https://bsky.network/account. Official SDKs read JETSTREAM_API_KEY
   and send Authorization: Bearer. Live subscribeEvents stays
   unauthenticated and is not metered.

   Self-hosted or public archives may omit the key. This library never
   fabricates one; CI must not set a real key.

     1. Operator sets JETSTREAM_API_KEY (or JETSTREAM_ARCHIVE_TOKEN).
     2. [archive_token_from_env] / [resolve_archive_token] read it.
     3. [snapshot_header_pairs] / [try_*] send Bearer when present.
     4. [require_archive_token] / [~require_token:true] fail before
        HTTP when a hosted download needs a key the operator did not
        supply.
     5. Unauthenticated [try_plan_snapshot ()] stays skippable
        (Snapshot_gated 401/403) when the host requires a key.
*)

open Atproto.Jetstream

let getenv_of pairs name =
  try Some (List.assoc name pairs) with Not_found -> None

let () =
  assert (Jetstream.archive_api_key_env = "JETSTREAM_API_KEY");
  assert (Jetstream.archive_token_env = "JETSTREAM_ARCHIVE_TOKEN");
  let empty = getenv_of [] in
  assert (Jetstream.archive_token_from_env ~getenv:empty () = None);
  assert (Jetstream.resolve_archive_token ~getenv:empty () = None);
  assert (Jetstream.archive_authorization ~getenv:empty () = None);
  let unauth = Jetstream.snapshot_header_pairs ~getenv:empty () in
  assert (not (List.exists (fun (k, _) -> k = "Authorization") unauth));
  (match
     try
       ignore (Jetstream.require_archive_token ~getenv:empty ());
       None
     with Jetstream.Archive_token_required msg -> Some msg
   with
  | Some msg -> assert (msg = Jetstream.archive_token_required_message)
  | None -> assert false);
  let fixture = "fixture-operator-key-not-a-real-credential" in
  assert (
    Jetstream.resolve_archive_token ~token:fixture ~getenv:empty ()
    = Some fixture);
  (match Jetstream.archive_authorization ~token:fixture ~getenv:empty () with
  | Some ("Authorization", v) -> assert (v = "Bearer " ^ fixture)
  | _ -> assert false);
  let pairs =
    Jetstream.snapshot_header_pairs ~token:fixture
      ~range:(Jetstream.range_header ~first:0 ())
      ~getenv:empty ()
  in
  assert (List.mem ("Authorization", "Bearer " ^ fixture) pairs);
  assert (List.mem ("Range", "bytes=0-") pairs);
  let from_official = getenv_of [ (Jetstream.archive_api_key_env, fixture) ] in
  assert (
    Jetstream.archive_token_from_env ~getenv:from_official () = Some fixture);
  assert (Jetstream.require_archive_token ~getenv:from_official () = fixture);
  let alias_only =
    getenv_of [ (Jetstream.archive_token_env, "alias-fixture-not-real") ]
  in
  assert (
    Jetstream.archive_token_from_env ~getenv:alias_only ()
    = Some "alias-fixture-not-real");
  let both =
    getenv_of
      [
        (Jetstream.archive_api_key_env, "official-wins-not-real");
        (Jetstream.archive_token_env, "alias-ignored-not-real");
      ]
  in
  assert (
    Jetstream.archive_token_from_env ~getenv:both ()
    = Some "official-wins-not-real");
  assert (Jetstream.get_segment_url ~name:"seg_0000000000.jss" () <> "");
  assert (Jetstream.plan_snapshot_url () <> "");
  print_endline
    "jetstream_archive: operator token env / Bearer wiring sketch ok"
