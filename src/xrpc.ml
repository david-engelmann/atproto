open Base64url

(** XRPC protocol headers and service-auth JWT claims.
    https://atproto.com/specs/xrpc
    https://atproto.com/specs/label#labeler-http-headers *)
module Xrpc = struct
  exception Invalid of string

  let fail msg = raise (Invalid msg)

  let ascii_lower s =
    String.map
      (function 'A' .. 'Z' as c -> Char.chr (Char.code c + 32) | c -> c)
      s

  let trim s =
    let n = String.length s in
    let rec left i =
      if i >= n then n
      else match s.[i] with ' ' | '\t' -> left (i + 1) | _ -> i
    in
    let rec right i =
      if i < 0 then 0
      else match s.[i] with ' ' | '\t' -> right (i - 1) | _ -> i + 1
    in
    let lo = left 0 in
    let hi = right (n - 1) in
    if hi <= lo then "" else String.sub s lo (hi - lo)

  let split_commas s =
    String.split_on_char ',' s |> List.map trim
    |> List.filter (fun p -> p <> "")

  (* ---- atproto-proxy --------------------------------------------------- *)

  type proxy = { did : string; service : string }

  let parse_proxy (value : string) : proxy =
    let v = trim value in
    match String.index_opt v '#' with
    | None -> fail "atproto-proxy must be did#service"
    | Some i ->
        let did = String.sub v 0 i in
        let service = String.sub v (i + 1) (String.length v - i - 1) in
        if not (Syntax.Syntax.is_valid_did did) then
          fail ("atproto-proxy DID is invalid: " ^ did);
        if service = "" then fail "atproto-proxy service id is empty";
        { did; service }

  let proxy_to_string (p : proxy) : string = p.did ^ "#" ^ p.service

  let proxy_header (p : proxy) : string * string =
    ("atproto-proxy", proxy_to_string p)

  let labeler_proxy (did : string) : proxy =
    { did; service = "atproto_labeler" }

  (* ---- atproto-accept-labelers / atproto-content-labelers -------------- *)

  type labeler = { did : string; redact : bool }

  let parse_labeler_item (item : string) : labeler =
    let parts = String.split_on_char ';' item |> List.map trim in
    match parts with
    | [] -> fail "empty labeler item"
    | did :: params ->
        if not (Syntax.Syntax.is_valid_did did) then
          fail ("labeler DID is invalid: " ^ did);
        let redact = List.exists (fun p -> ascii_lower p = "redact") params in
        { did; redact }

  let parse_labelers (value : string) : labeler list =
    let items = split_commas value in
    let parsed = List.map parse_labeler_item items in
    (* de-duplicate by DID, unioning redact flags (spec: combine parameters) *)
    let acc = Hashtbl.create 8 in
    let order = ref [] in
    List.iter
      (fun (l : labeler) ->
        match Hashtbl.find_opt acc l.did with
        | None ->
            Hashtbl.add acc l.did l.redact;
            order := l.did :: !order
        | Some prev -> Hashtbl.replace acc l.did (prev || l.redact))
      parsed;
    List.rev_map (fun did -> { did; redact = Hashtbl.find acc did }) !order

  let labelers_to_string (ls : labeler list) : string =
    String.concat ", "
      (List.map
         (fun (l : labeler) -> if l.redact then l.did ^ ";redact" else l.did)
         ls)

  let accept_labelers_header (ls : labeler list) : string * string =
    ("atproto-accept-labelers", labelers_to_string ls)

  let content_labelers_header (ls : labeler list) : string * string =
    ("atproto-content-labelers", labelers_to_string ls)

  (* ---- Rate-limit and repo-rev headers -------------------------------- *)

  type rate_limit = {
    limit : int option;
    remaining : int option;
    reset : int64 option;
    policy : string option;
  }

  let header_value headers name =
    let lower = ascii_lower name in
    List.find_map
      (fun (k, v) -> if ascii_lower k = lower then Some (trim v) else None)
      headers

  let int_opt s = try Some (int_of_string s) with _ -> None
  let int64_opt s = try Some (Int64.of_string s) with _ -> None

  let parse_rate_limit (headers : (string * string) list) : rate_limit =
    {
      limit =
        (match header_value headers "RateLimit-Limit" with
        | Some s -> int_opt s
        | None -> None);
      remaining =
        (match header_value headers "RateLimit-Remaining" with
        | Some s -> int_opt s
        | None -> None);
      reset =
        (match header_value headers "RateLimit-Reset" with
        | Some s -> int64_opt s
        | None -> None);
      policy = header_value headers "RateLimit-Policy";
    }

  let repo_rev_header (rev : string) : string * string =
    ("atproto-repo-rev", rev)

  (* ---- Service-auth JWT (com.atproto.server.getServiceAuth) ----------- *)

  type service_auth = {
    iss : string;
    aud : string;
    exp : int64 option;
    lxm : string option;
    raw : string;
  }

  let split_jwt jwt =
    match String.split_on_char '.' jwt with
    | [ h; p; s ] -> (h, p, s)
    | _ -> fail "service-auth JWT must have three base64url parts"

  let parse_service_auth (jwt : string) : service_auth =
    let _, payload, _ = split_jwt jwt in
    let json = Yojson.Safe.from_string (Base64url.decode payload) in
    let open Yojson.Safe.Util in
    let iss =
      match json |> member "iss" with
      | `String s -> s
      | _ -> fail "service-auth JWT missing iss"
    in
    let aud =
      match json |> member "aud" with
      | `String s -> s
      | _ -> fail "service-auth JWT missing aud"
    in
    if not (Syntax.Syntax.is_valid_did iss) then
      fail "service-auth iss must be a DID";
    if not (Syntax.Syntax.is_valid_did aud) then
      fail "service-auth aud must be a DID";
    {
      iss;
      aud;
      exp =
        (match json |> member "exp" with
        | `Int n -> Some (Int64.of_int n)
        | `Intlit s -> Some (Int64.of_string s)
        | _ -> None);
      lxm =
        (match json |> member "lxm" with
        | `String s ->
            if not (Syntax.Syntax.is_valid_nsid s) then
              fail "service-auth lxm must be an NSID";
            Some s
        | _ -> None);
      raw = jwt;
    }

  let service_auth_body ~aud ?lxm ?exp () : Yojson.Safe.t =
    if not (Syntax.Syntax.is_valid_did aud) then
      fail "getServiceAuth aud must be a DID";
    (match lxm with
    | Some n ->
        if not (Syntax.Syntax.is_valid_nsid n) then
          fail "getServiceAuth lxm must be an NSID"
    | None -> ());
    let fields =
      [ ("aud", `String aud) ]
      @ (match lxm with Some n -> [ ("lxm", `String n) ] | None -> [])
      @
      match exp with
      | Some n -> [ ("exp", `Intlit (Int64.to_string n)) ]
      | None -> []
    in
    `Assoc fields
end
