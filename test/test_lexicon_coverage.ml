open OUnit2

(* Drift gate for official lexicon NSIDs pinned at bluesky-social/atproto
   60c4395951 (APP-2933). Re-run scripts/gen-official-nsids.py against a newer
   SHA to refresh lexicons/official-nsids.json; this test then fails until each
   new NSID has a client helper, record builder, bundled permission-set, or an
   explicit skip with a one-line reason. Hosted-only *servers* are not a reason
   to skip a public client NSID. *)

let expected_pin = "60c439595101fbcbe612463e6f23200590c5daaf"

let covered_types =
  [ "query"; "procedure"; "subscription"; "record"; "permission-set" ]

let endpoint_prefixes =
  [
    ("create_server_endpoint", "com.atproto.server");
    ("create_sync_endpoint", "com.atproto.sync");
    ("create_repo_endpoint", "com.atproto.repo");
    ("create_notification_endpoint", "app.bsky.notification");
    ("create_graph_endpoint", "app.bsky.graph");
    ("create_feed_endpoint", "app.bsky.feed");
    ("create_actor_endpoint", "app.bsky.actor");
    ("create_moderation_endpoint", "com.atproto.moderation");
    ("create_label_endpoint", "com.atproto.label");
    ("create_identity_endpoint", "com.atproto.identity");
  ]

(* Public client surfaces that must stay bound even when the matching host
   is not OSS (chat backend, video transcoder). *)
let never_skip_prefixes = [ "chat.bsky."; "app.bsky.video." ]

let rec parents_of dir =
  let parent = Filename.dirname dir in
  if parent = dir then [ dir ] else dir :: parents_of parent

let search_roots () =
  let cwd = Sys.getcwd () in
  [
    cwd;
    Filename.concat cwd "..";
    Filename.concat cwd "../..";
    Filename.concat cwd "../../..";
  ]
  @ parents_of cwd

let find_file rel =
  let rec go = function
    | [] ->
        failwith
          ("lexicon coverage: cannot find " ^ rel ^ " (cwd=" ^ Sys.getcwd ()
         ^ ")")
    | root :: rest ->
        let path = Filename.concat root rel in
        if Sys.file_exists path && not (Sys.is_directory path) then path
        else go rest
  in
  go (search_roots ())

let find_src () =
  let rec go = function
    | [] ->
        failwith
          ("lexicon coverage: cannot find src/lexicon.ml (cwd=" ^ Sys.getcwd ()
         ^ ")")
    | root :: rest ->
        let path = Filename.concat root "src/lexicon.ml" in
        if Sys.file_exists path then Filename.concat root "src" else go rest
  in
  go (search_roots ())

let rec collect_ml acc dir =
  Array.fold_left
    (fun acc name ->
      let path = Filename.concat dir name in
      if Sys.is_directory path then collect_ml acc path
      else if Filename.check_suffix name ".ml" then path :: acc
      else acc)
    acc (Sys.readdir dir)

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> true
  | _ -> false

let is_nsid s =
  let n = String.length s in
  if n < 5 then false
  else
    let dots = ref 0 in
    let ok = ref true in
    let i = ref 0 in
    while !i < n && !ok do
      let c = s.[!i] in
      if c = '.' then (
        incr dots;
        if !i = 0 || !i = n - 1 || s.[!i - 1] = '.' then ok := false)
      else if not (is_ident_char c) then ok := false;
      incr i
    done;
    !ok && !dots >= 2

let extract_quoted text =
  let n = String.length text in
  let acc = ref [] in
  let i = ref 0 in
  while !i < n do
    if text.[!i] = '"' then (
      incr i;
      let start = !i in
      while !i < n && text.[!i] <> '"' && text.[!i] <> '\n' do
        incr i
      done;
      if !i < n && text.[!i] = '"' then (
        let s = String.sub text start (!i - start) in
        if is_nsid s then acc := s :: !acc;
        incr i))
    else incr i
  done;
  !acc

let extract_after_needle text needle suffix_of =
  let n = String.length text in
  let fl = String.length needle in
  let acc = ref [] in
  let i = ref 0 in
  while !i + fl <= n do
    if String.sub text !i fl = needle then (
      let j = ref (!i + fl) in
      while
        !j < n && (text.[!j] = ' ' || text.[!j] = '\n' || text.[!j] = '\t')
      do
        incr j
      done;
      (match suffix_of text !j with Some s -> acc := s :: !acc | None -> ());
      i := !i + fl)
    else incr i
  done;
  !acc

let read_quoted text j =
  if j < String.length text && text.[j] = '"' then (
    let k = ref (j + 1) in
    while !k < String.length text && text.[!k] <> '"' do
      incr k
    done;
    if !k < String.length text then Some (String.sub text (j + 1) (!k - j - 1))
    else None)
  else None

let extract_endpoint_calls text =
  List.concat
    (List.map
       (fun (fn, prefix) ->
         List.filter_map
           (fun name -> if name = "" then None else Some (prefix ^ "." ^ name))
           (extract_after_needle text fn read_quoted))
       endpoint_prefixes)

let read_xrpc_nsid text j =
  let n = String.length text in
  if j >= n then None
  else
    let k = ref j in
    while !k < n && (is_ident_char text.[!k] || text.[!k] = '.') do
      incr k
    done;
    let s = String.sub text j (!k - j) in
    if is_nsid s then Some s else None

let extract_xrpc_urls text = extract_after_needle text "/xrpc/" read_xrpc_nsid

module SSet = Set.Make (String)

let implemented_nsids src_dir =
  collect_ml [] src_dir
  |> List.fold_left
       (fun acc path ->
         let text = read_file path in
         let found =
           extract_quoted text
           @ extract_endpoint_calls text
           @ extract_xrpc_urls text
         in
         List.fold_left (fun acc id -> SSet.add id acc) acc found)
       SSet.empty

let string_field json name =
  match Yojson.Safe.Util.member name json with
  | `String s -> s
  | _ -> failwith ("missing string field " ^ name)

let int_field json name =
  match Yojson.Safe.Util.member name json with
  | `Int n -> n
  | _ -> failwith ("missing int field " ^ name)

type official = { id : string; kind : string }
type skip = { id : string; reason : string }

let load_official path =
  let json = Yojson.Safe.from_file path in
  let sha = string_field json "sha" in
  let nsid_count = int_field json "nsid_count" in
  let nsids =
    match Yojson.Safe.Util.member "nsids" json with
    | `List xs ->
        List.map
          (fun x -> { id = string_field x "id"; kind = string_field x "type" })
          xs
    | _ -> failwith "manifest.nsids must be an array"
  in
  (sha, nsid_count, nsids)

let load_skips path =
  match Yojson.Safe.Util.member "skips" (Yojson.Safe.from_file path) with
  | `List xs ->
      List.map
        (fun x ->
          { id = string_field x "id"; reason = string_field x "reason" })
        xs
  | _ -> failwith "coverage-skips.skips must be an array"

let has_newline s =
  try
    ignore (String.index s '\n');
    true
  with Not_found -> false

let starts_with prefix s =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

let test_official_nsid_coverage _ =
  let sha, nsid_count, official =
    load_official (find_file "lexicons/official-nsids.json")
  in
  let skips = load_skips (find_file "lexicons/coverage-skips.json") in
  let implemented = implemented_nsids (find_src ()) in
  OUnit2.assert_equal ~printer:(fun x -> x) expected_pin sha;
  OUnit2.assert_equal ~printer:string_of_int nsid_count (List.length official);
  List.iter
    (fun (e : official) ->
      OUnit2.assert_bool
        ("unsupported main type for " ^ e.id ^ ": " ^ e.kind)
        (List.mem e.kind covered_types))
    official;
  let official_ids =
    List.fold_left
      (fun acc (e : official) -> SSet.add e.id acc)
      SSet.empty official
  in
  let skip_ids =
    List.fold_left
      (fun acc (s : skip) ->
        OUnit2.assert_bool
          (s.id ^ " skip reason must be a non-empty one-liner")
          (String.trim s.reason <> "" && not (has_newline s.reason));
        OUnit2.assert_bool
          (s.id ^ " is not in the official pin; remove the skip")
          (SSet.mem s.id official_ids);
        OUnit2.assert_bool
          (s.id
         ^ " is already represented in src/; skip list must not hide a bound \
            NSID")
          (not (SSet.mem s.id implemented));
        List.iter
          (fun prefix ->
            OUnit2.assert_bool
              (s.id ^ " is a public client NSID (" ^ prefix
             ^ "); hosted-only servers are not a skip reason")
              (not (starts_with prefix s.id)))
          never_skip_prefixes;
        SSet.add s.id acc)
      SSet.empty skips
  in
  let missing =
    List.filter_map
      (fun (e : official) ->
        if SSet.mem e.id implemented || SSet.mem e.id skip_ids then None
        else Some (e.kind ^ " " ^ e.id))
      official
  in
  (match missing with
  | [] -> ()
  | xs ->
      OUnit2.assert_failure
        ("official NSIDs missing a client helper, record builder, bundled \
          permission-set, or explicit skip:\n\
         \  " ^ String.concat "\n  " xs));
  let bound = List.length official - SSet.cardinal skip_ids in
  OUnit2.assert_bool "expected official NSIDs to be bound" (bound > 0)

let suite =
  "lexicon_coverage"
  >::: [ "test_official_nsid_coverage" >:: test_official_nsid_coverage ]

let () = run_test_tt_main suite
