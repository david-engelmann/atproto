(** AT URI scheme ([at://]) — public records
    ({{:https://atproto.com/specs/at-uri-scheme}AT URI spec}) and
    {e experimental} permissioned space URIs from proposal
    {{:https://github.com/bluesky-social/proposals/blob/main/0016-permissioned-data/README.md}0016}.

    Public URIs stay in {!Uri} (at most two path segments). Space URIs
    are in {!module-Space}: the first path segment is the literal [space]
    marker, not an NSID. The proposal is not final; {!module-Space} is not
    a stable spaces product API. *)

module Uri = struct
  type t = {
    authority : string;
    collection : string option;
    rkey : string option;
    query : (string * string) list option;
    fragment : string option;
  }

  (* Legacy record kept so existing sample tests and callers still type-check. *)
  type uri = {
    host : string;
    path_name : string;
    hash : string;
    search_params : (string * string) list option;
  }

  (** Convert an AT URI to the legacy [uri] record ([host] / [path_name] /
      [hash]). *)
  let to_legacy (u : t) : uri =
    {
      host = u.authority;
      path_name = Option.value ~default:"" u.collection;
      hash = Option.value ~default:"" u.rkey;
      search_params = u.query;
    }

  (** Convert a legacy [uri] record to an AT URI ([fragment] is [None]). *)
  let of_legacy (u : uri) : t =
    {
      authority = u.host;
      collection = (if u.path_name = "" then None else Some u.path_name);
      rkey = (if u.hash = "" then None else Some u.hash);
      query = u.search_params;
      fragment = None;
    }

  let split_once s sep =
    match String.index_opt s sep with
    | None -> (s, None)
    | Some i ->
        (String.sub s 0 i, Some (String.sub s (i + 1) (String.length s - i - 1)))

  let parse_query q =
    if q = "" then None
    else
      let pairs =
        String.split_on_char '&' q
        |> List.filter (fun p -> p <> "")
        |> List.map (fun p ->
               match split_once p '=' with
               | k, Some v -> (k, v)
               | k, None -> (k, ""))
      in
      Some pairs

  (** Parse an [at://] URI. Fails on a missing [at://] prefix, empty
      authority, trailing slash, or more than two path segments. *)
  let of_string (raw : string) : t =
    let s =
      if String.length raw >= 5 && String.sub raw 0 5 = "at://" then
        String.sub raw 5 (String.length raw - 5)
      else failwith "Uri.of_string: AT URIs must start with at://"
    in
    if s = "" then failwith "Uri.of_string: empty authority";
    let rest, fragment =
      match String.index_opt s '#' with
      | None -> (s, None)
      | Some i ->
          ( String.sub s 0 i,
            Some (String.sub s (i + 1) (String.length s - i - 1)) )
    in
    let rest, query =
      match String.index_opt rest '?' with
      | None -> (rest, None)
      | Some i ->
          ( String.sub rest 0 i,
            parse_query (String.sub rest (i + 1) (String.length rest - i - 1))
          )
    in
    if rest = "" then failwith "Uri.of_string: empty authority";
    if rest.[String.length rest - 1] = '/' && String.length rest > 0 then
      (* trailing slash after authority-only is invalid; also reject trailing slash generally *)
      if String.contains rest '/' && rest.[String.length rest - 1] = '/' then
        failwith "Uri.of_string: trailing slash is not allowed";
    let authority, path =
      match String.index_opt rest '/' with
      | None -> (rest, None)
      | Some i ->
          ( String.sub rest 0 i,
            Some (String.sub rest (i + 1) (String.length rest - i - 1)) )
    in
    if authority = "" then failwith "Uri.of_string: empty authority";
    let collection, rkey =
      match path with
      | None | Some "" -> (None, None)
      | Some p -> (
          match String.index_opt p '/' with
          | None -> (Some p, None)
          | Some i ->
              let col = String.sub p 0 i in
              let rkey = String.sub p (i + 1) (String.length p - i - 1) in
              if String.contains rkey '/' then
                failwith "Uri.of_string: more than two path segments";
              (Some col, Some rkey))
    in
    { authority; collection; rkey; query; fragment }

  (** Serialize [u] as
      [at://authority[/collection[/rkey]][?query][#fragment]]. *)
  let to_string (u : t) : string =
    let buf = Buffer.create 64 in
    Buffer.add_string buf "at://";
    Buffer.add_string buf u.authority;
    (match u.collection with
    | Some c -> (
        Buffer.add_char buf '/';
        Buffer.add_string buf c;
        match u.rkey with
        | Some r ->
            Buffer.add_char buf '/';
            Buffer.add_string buf r
        | None -> ())
    | None -> ());
    (match u.query with
    | Some pairs ->
        Buffer.add_char buf '?';
        Buffer.add_string buf
          (String.concat "&" (List.map (fun (k, v) -> k ^ "=" ^ v) pairs))
    | None -> ());
    (match u.fragment with
    | Some f ->
        Buffer.add_char buf '#';
        Buffer.add_string buf f
    | None -> ());
    Buffer.contents buf

  (** Build an AT URI for [authority] (DID or handle), optional
      [collection] NSID, and optional record key [rkey]. *)
  let record ?(collection = "") ?(rkey = "") (authority : string) : t =
    {
      authority;
      collection = (if collection = "" then None else Some collection);
      rkey = (if rkey = "" then None else Some rkey);
      query = None;
      fragment = None;
    }
end

(** Experimental space URI from AT Protocol proposal 0016
    (permissioned data / spaces).

    Tracks
    {{:https://github.com/bluesky-social/proposals/blob/main/0016-permissioned-data/README.md}0016
    § Addressing}. Distinguished from a public AT URI by the literal
    first path segment [space] (an NSID always contains at least two
    [.]; [space] contains none).

    {[
      Space:  at://{spaceDid}/space/{spaceType}/{skey}
      Record: at://{spaceDid}/space/{spaceType}/{skey}/{authorDid}/{collection}/{rkey}
    ]}

    The proposal is not final; this module may change and is {e not} a
    stable spaces product API. XRPC wrappers live in [Space_xrpc].
    This repo does not start or stub a space host. *)
module Space : sig
  type space = { space_did : string; space_type : string; skey : string }

  type record = {
    space_did : string;
    space_type : string;
    skey : string;
    author_did : string;
    collection : string;
    rkey : string;
  }

  type t = Space of space | Record of record

  exception Invalid of string

  val is_space_uri : string -> bool
  (** True when the first path segment after the authority is the
      literal [space] marker. Does not fully validate. *)

  val of_string : string -> t
  (** Parse a space or permissioned-record URI. Raises [Invalid] on a
      missing [at://] prefix, a first path segment other than [space],
      a segment count other than 3 or 6, a query or fragment, a
      trailing slash, or a component that fails DID / NSID / record-key
      syntax. *)

  val to_string : t -> string
  (** Serialize [t] without query or fragment. *)

  val space : space_did:string -> space_type:string -> skey:string -> t
  (** Build [at://{spaceDid}/space/{spaceType}/{skey}]. Validates
      components. *)

  val record :
    space_did:string ->
    space_type:string ->
    skey:string ->
    author_did:string ->
    collection:string ->
    rkey:string ->
    t
  (** Build a permissioned-record URI. Validates components. *)

  val space_of : t -> space
  (** Space triple from either a space or a record URI. *)

  val is_record : t -> bool
end = struct
  type space = { space_did : string; space_type : string; skey : string }

  type record = {
    space_did : string;
    space_type : string;
    skey : string;
    author_did : string;
    collection : string;
    rkey : string;
  }

  type t = Space of space | Record of record

  exception Invalid of string

  let fail msg = raise (Invalid msg)

  let ensure_did what s =
    if not (Syntax.Syntax.is_valid_did s) then
      fail ("invalid " ^ what ^ " DID " ^ s)

  let ensure_nsid what s =
    if not (Syntax.Syntax.is_valid_nsid s) then
      fail ("invalid " ^ what ^ " NSID " ^ s)

  let ensure_key what s =
    if not (Syntax.Syntax.is_valid_record_key s) then
      fail ("invalid " ^ what ^ " " ^ s)

  let strip_at s =
    if String.length s >= 5 && String.sub s 0 5 = "at://" then
      String.sub s 5 (String.length s - 5)
    else fail "AT URIs must start with at://"

  let path_after_scheme rest =
    let rest =
      match String.index_opt rest '#' with
      | None -> rest
      | Some i -> String.sub rest 0 i
    in
    match String.index_opt rest '?' with
    | None -> rest
    | Some i -> String.sub rest 0 i

  let first_path_segment rest =
    match String.index_opt rest '/' with
    | None -> None
    | Some i -> (
        let path = String.sub rest (i + 1) (String.length rest - i - 1) in
        if path = "" then Some ""
        else
          match String.index_opt path '/' with
          | None -> Some path
          | Some j -> Some (String.sub path 0 j))

  (** True when the first path segment is the literal [space] marker. *)
  let is_space_uri (raw : string) : bool =
    try
      let rest = path_after_scheme (strip_at raw) in
      match first_path_segment rest with Some "space" -> true | _ -> false
    with Invalid _ -> false

  let check_space_did space_did = ensure_did "space authority" space_did
  let check_space_type space_type = ensure_nsid "space type" space_type
  let check_skey skey = ensure_key "skey" skey
  let check_author author_did = ensure_did "author" author_did
  let check_collection collection = ensure_nsid "collection" collection
  let check_rkey rkey = ensure_key "rkey" rkey

  let space ~space_did ~space_type ~skey : t =
    check_space_did space_did;
    check_space_type space_type;
    check_skey skey;
    Space { space_did; space_type; skey }

  let record ~space_did ~space_type ~skey ~author_did ~collection ~rkey : t =
    check_space_did space_did;
    check_space_type space_type;
    check_skey skey;
    check_author author_did;
    check_collection collection;
    check_rkey rkey;
    Record { space_did; space_type; skey; author_did; collection; rkey }

  let of_string (raw : string) : t =
    let rest = strip_at raw in
    if String.contains raw '?' then fail "query is not allowed on space URIs";
    if String.contains raw '#' then fail "fragment is not allowed on space URIs";
    if rest = "" then fail "empty authority";
    if rest.[String.length rest - 1] = '/' then
      fail "trailing slash is not allowed";
    let parts = String.split_on_char '/' rest in
    if List.exists (fun p -> p = "") parts then fail "empty path segment";
    match parts with
    | [ space_did; "space"; space_type; skey ] ->
        space ~space_did ~space_type ~skey
    | [ space_did; "space"; space_type; skey; author_did; collection; rkey ] ->
        record ~space_did ~space_type ~skey ~author_did ~collection ~rkey
    | _ :: "space" :: _ ->
        fail
          "space URI must be at://{did}/space/{type}/{skey} or \
           at://{did}/space/{type}/{skey}/{author}/{collection}/{rkey}"
    | _ -> fail "not a space URI (first path segment must be literal space)"

  let to_string = function
    | Space s -> "at://" ^ s.space_did ^ "/space/" ^ s.space_type ^ "/" ^ s.skey
    | Record r ->
        "at://" ^ r.space_did ^ "/space/" ^ r.space_type ^ "/" ^ r.skey ^ "/"
        ^ r.author_did ^ "/" ^ r.collection ^ "/" ^ r.rkey

  let space_of = function
    | Space s -> s
    | Record r ->
        { space_did = r.space_did; space_type = r.space_type; skey = r.skey }

  let is_record = function Record _ -> true | Space _ -> false
end

type classified = Public of Uri.t | Space of Space.t

(** Route [raw] to {!module-Space} when the first path segment is [space],
    otherwise to the public {!Uri} parser. *)
let classify (raw : string) : classified =
  if Space.is_space_uri raw then Space (Space.of_string raw)
  else Public (Uri.of_string raw)
