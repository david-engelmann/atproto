(** AT Protocol OAuth authorization scopes (granular permissions).

    Grammar: [resource[:positional][?key=value&key=value]]
    https://github.com/bluesky-social/proposals/blob/main/0011-auth-scopes/README.md
    August 2025 implementation notes:
    https://github.com/bluesky-social/atproto/discussions/4118

    The [atproto] scope remains mandatory on every session. Transitional
    scopes ([transition:generic], [transition:chat.bsky], [transition:email])
    are still accepted. *)
module Oauth_scope = struct
  exception Invalid of string

  let fail msg = raise (Invalid msg)

  type resource_kind =
    | Atproto
    | Transition
    | Repo
    | Rpc
    | Blob
    | Identity
    | Account
    | Include
    | Other of string

  type t = {
    resource : resource_kind;
    positional : string option;
    params : (string * string) list;
  }

  let resource_name = function
    | Atproto -> "atproto"
    | Transition -> "transition"
    | Repo -> "repo"
    | Rpc -> "rpc"
    | Blob -> "blob"
    | Identity -> "identity"
    | Account -> "account"
    | Include -> "include"
    | Other s -> s

  let resource_of_string = function
    | "atproto" -> Atproto
    | "transition" -> Transition
    | "repo" -> Repo
    | "rpc" -> Rpc
    | "blob" -> Blob
    | "identity" -> Identity
    | "account" -> Account
    | "include" -> Include
    | other -> Other other

  let split_once s sep =
    match String.index_opt s sep with
    | None -> (s, None)
    | Some i ->
        (String.sub s 0 i, Some (String.sub s (i + 1) (String.length s - i - 1)))

  let pct_decode s = try Uri.pct_decode s with _ -> s
  let pct_encode s = Uri.pct_encode ~component:`Query_value s

  let parse_params qs =
    if qs = "" then []
    else
      String.split_on_char '&' qs
      |> List.filter (fun p -> p <> "")
      |> List.map (fun p ->
             let k, v = split_once p '=' in
             (pct_decode k, match v with Some x -> pct_decode x | None -> ""))

  let params_get key params =
    List.filter_map (fun (k, v) -> if k = key then Some v else None) params

  let param_one key params =
    match params_get key params with [] -> None | hd :: _ -> Some hd

  let collections_of (s : t) : string list =
    match s.positional with
    | Some p when p <> "" -> p :: params_get "collection" s.params
    | _ -> params_get "collection" s.params

  let actions_of (s : t) : string list =
    let acts = params_get "action" s.params in
    if acts = [] then [ "create"; "update"; "delete" ] else acts

  let lxm_of (s : t) : string list =
    match s.positional with
    | Some p when p <> "" -> p :: params_get "lxm" s.params
    | _ -> params_get "lxm" s.params

  let aud_of (s : t) : string list = params_get "aud" s.params
  let is_wildcard = function "*" -> true | _ -> false

  let validate (s : t) : unit =
    match s.resource with
    | Atproto ->
        if s.positional <> None || s.params <> [] then
          fail "atproto scope takes no parameters"
    | Transition -> (
        match s.positional with
        | Some ("generic" | "chat.bsky" | "email") -> ()
        | Some other -> fail ("unknown transition scope " ^ other)
        | None -> fail "transition scope requires a positional value")
    | Repo ->
        List.iter
          (fun c ->
            if c <> "*" && String.length c > 0 && c.[String.length c - 1] = '*'
            then fail "repo collection globs are not allowed";
            if (not (is_wildcard c)) && not (Syntax.Syntax.is_valid_nsid c) then
              fail ("repo collection must be an NSID or *: " ^ c))
          (collections_of s);
        List.iter
          (fun a ->
            match a with
            | "create" | "update" | "delete" | "*" -> ()
            | other ->
                fail ("repo action must be create|update|delete: " ^ other))
          (params_get "action" s.params)
    | Rpc ->
        let lxms = lxm_of s in
        let auds = aud_of s in
        if lxms = [] then fail "rpc scope requires lxm";
        if auds = [] then fail "rpc scope requires aud";
        let lxm_star = List.exists is_wildcard lxms in
        let aud_star = List.exists is_wildcard auds in
        if lxm_star && aud_star then
          fail "rpc scope cannot wildcard both lxm and aud";
        List.iter
          (fun l ->
            if (not (is_wildcard l)) && not (Syntax.Syntax.is_valid_nsid l) then
              fail ("rpc lxm must be an NSID or *: " ^ l))
          lxms
    | Identity -> (
        match (s.positional, param_one "attr" s.params) with
        | Some a, _ | None, Some a ->
            if not (a = "*" || a = "handle") then
              fail ("identity attr must be handle or *: " ^ a)
        | None, None -> fail "identity scope requires attr")
    | Account -> (
        match (s.positional, param_one "attr" s.params) with
        | Some a, _ | None, Some a ->
            if not (List.mem a [ "email"; "repo"; "status" ]) then
              fail ("account attr must be email|repo|status: " ^ a)
        | None, None -> fail "account scope requires attr")
    | Include -> (
        match s.positional with
        | Some nsid ->
            if not (Syntax.Syntax.is_valid_nsid nsid) then
              fail ("include positional must be an NSID: " ^ nsid)
        | None -> fail "include scope requires a permission-set NSID")
    | Blob | Other _ -> ()

  (* Official AT Protocol permission-set NSIDs referenced by include:. *)
  let official_include_nsids =
    [
      "app.bsky.authCreatePosts";
      "app.bsky.authDeleteContent";
      "app.bsky.authFullApp";
      "app.bsky.authManageFeedDeclarations";
      "app.bsky.authManageLabelerService";
      "app.bsky.authManageModeration";
      "app.bsky.authManageNotifications";
      "app.bsky.authManageProfile";
      "app.bsky.authViewAll";
      "chat.bsky.authFullChatClient";
    ]

  let is_official_include (nsid : string) : bool =
    List.mem nsid official_include_nsids

  let scopes_of_permission (p : Lexicon.Lexicon.permission) : t list =
    match p.resource with
    | "repo" ->
        let collections = if p.collection = [] then [ "*" ] else p.collection in
        let actions = if p.action = [] then [] else p.action in
        List.map
          (fun collection ->
            {
              resource = Repo;
              positional = Some collection;
              params = List.map (fun a -> ("action", a)) actions;
            })
          collections
    | "rpc" ->
        let lxms = if p.lxm = [] then [ "*" ] else p.lxm in
        let aud =
          match p.inherit_aud with Some true -> [ ("aud", "*") ] | _ -> []
        in
        List.map
          (fun lxm -> { resource = Rpc; positional = Some lxm; params = aud })
          lxms
    | _ -> []

  let expand_include (set : Lexicon.Lexicon.permission_set) : t list =
    List.concat (List.map scopes_of_permission set.permissions)

  let expand_include_nsid (nsid : string) : t list option =
    match List.assoc_opt nsid Lexicon.Lexicon.official_lexicons with
    | None -> None
    | Some body ->
        Some
          (expand_include
             (Lexicon.Lexicon.parse_permission_set
                (Yojson.Safe.from_string body)))

  let parse_one (raw : string) : t =
    let token = String.trim raw in
    if token = "" then fail "empty scope token";
    let head, query = split_once token '?' in
    let resource_s, positional = split_once head ':' in
    if resource_s = "" then fail "scope resource is empty";
    let s =
      {
        resource = resource_of_string resource_s;
        positional =
          (match positional with
          | Some p when p <> "" -> Some (pct_decode p)
          | _ -> None);
        params = (match query with Some q -> parse_params q | None -> []);
      }
    in
    validate s;
    s

  let split_scopes (scope : string) : string list =
    String.split_on_char ' ' scope |> List.filter (fun t -> t <> "")

  let parse (scope : string) : t list = List.map parse_one (split_scopes scope)

  let to_string (s : t) : string =
    let head =
      match s.positional with
      | Some p -> resource_name s.resource ^ ":" ^ pct_encode p
      | None -> resource_name s.resource
    in
    match s.params with
    | [] -> head
    | ps ->
        head ^ "?"
        ^ String.concat "&"
            (List.map (fun (k, v) -> pct_encode k ^ "=" ^ pct_encode v) ps)

  let serialize (scopes : t list) : string =
    String.concat " " (List.map to_string scopes)

  let has_atproto (scopes : t list) : bool =
    List.exists (fun s -> s.resource = Atproto) scopes

  let require_atproto (scopes : t list) : unit =
    if not (has_atproto scopes) then fail "scope list must include atproto"

  let token_set scope = split_scopes scope |> List.sort_uniq String.compare

  let is_subset ~requested ~declared =
    let dec = token_set declared in
    List.for_all (fun t -> List.mem t dec) (split_scopes requested)

  let parse_and_require (scope : string) : t list =
    let scopes = parse scope in
    require_atproto scopes;
    scopes
end
