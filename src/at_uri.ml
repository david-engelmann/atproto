(** AT URI scheme (at://) — https://atproto.com/specs/at-uri-scheme *)
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

  let to_legacy (u : t) : uri =
    {
      host = u.authority;
      path_name = Option.value ~default:"" u.collection;
      hash = Option.value ~default:"" u.rkey;
      search_params = u.query;
    }

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
            parse_query (String.sub rest (i + 1) (String.length rest - i - 1)) )
    in
    if rest = "" then failwith "Uri.of_string: empty authority";
    if rest.[String.length rest - 1] = '/' && String.length rest > 0 then
      (* trailing slash after authority-only is invalid; also reject trailing slash generally *)
      if
        String.contains rest '/'
        && rest.[String.length rest - 1] = '/'
      then failwith "Uri.of_string: trailing slash is not allowed";
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

  let to_string (u : t) : string =
    let buf = Buffer.create 64 in
    Buffer.add_string buf "at://";
    Buffer.add_string buf u.authority;
    (match u.collection with
    | Some c ->
        Buffer.add_char buf '/';
        Buffer.add_string buf c;
        (match u.rkey with
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

  let record ?(collection = "") ?(rkey = "") (authority : string) : t =
    {
      authority;
      collection = (if collection = "" then None else Some collection);
      rkey = (if rkey = "" then None else Some rkey);
      query = None;
      fragment = None;
    }
end
