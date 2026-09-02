(** XRPC error objects returned by AT Protocol HTTP endpoints. *)
module Error = struct
  type t = { error : string; message : string }
  type error = t
  type error_type = [ `RateLimitExceeded of t | `Xrpc of t ]

  let parse_error_from_json json : t =
    let open Yojson.Safe.Util in
    let error =
      match json |> member "error" with `String s -> s | _ -> "Unknown"
    in
    let message =
      match json |> member "message" with `String s -> s | _ -> ""
    in
    { error; message }

  (** Parse an XRPC [error] / [message] JSON object. *)
  let of_json = parse_error_from_json

  (** Parse [error] from an HTTP body, or [None] if it is not an XRPC
      error object. *)
  let of_body (body : string) : t option =
    try
      let json = Yojson.Safe.from_string body in
      match Yojson.Safe.Util.member "error" json with
      | `String _ -> Some (parse_error_from_json json)
      | _ -> None
    with _ -> None

  (** [Some error] when [json] has an [error] string field. *)
  let check_for_error json : string option =
    let open Yojson.Safe.Util in
    match json |> member "error" with `String s -> Some s | _ -> None

  (** Classify [json] as [RateLimitExceeded] or a generic XRPC error. *)
  let parse_error json : error_type =
    let e = parse_error_from_json json in
    match e.error with
    | "RateLimitExceeded" -> `RateLimitExceeded e
    | _ -> `Xrpc e

  (** [error] or [error: message]. *)
  let to_string (e : t) : string =
    if e.message = "" then e.error else e.error ^ ": " ^ e.message

  (** True for [MethodNotImplemented] / [MethodNotFound]. *)
  let is_not_implemented (e : t) : bool =
    e.error = "MethodNotImplemented" || e.error = "MethodNotFound"

  let contains_ci hay needle =
    let h = String.lowercase_ascii hay and n = String.lowercase_ascii needle in
    let rec aux i =
      if i + String.length n > String.length h then false
      else if String.sub h i (String.length n) = n then true
      else aux (i + 1)
    in
    aux 0

  (* Local AppView implements some NSIDs but keeps them flag-off
     (e.g. InvalidRequest: Search v2 is not enabled). A local PDS can
     also reject a record $type it has not bundled yet. *)
  let is_feature_disabled (e : t) : bool =
    e.error = "InvalidRequest"
    && (contains_ci e.message "not enabled"
       || contains_ci e.message "not available"
       || contains_ci e.message "unknown lexicon")

  (** True when the host does not serve the NSID (not implemented or
      flag-off). *)
  let is_not_served (e : t) : bool =
    is_not_implemented e || is_feature_disabled e

  let is_not_implemented_json json : bool =
    match check_for_error json with
    | Some "MethodNotImplemented" | Some "MethodNotFound" -> true
    | _ -> false

  let is_not_served_json json : bool =
    match check_for_error json with
    | None -> false
    | Some _ -> is_not_served (of_json json)

  (** Raise [Failure] for a classified XRPC error. *)
  let handle_error error_type =
    match error_type with
    | `RateLimitExceeded e -> failwith ("RateLimitExceeded: " ^ e.message)
    | `Xrpc e -> failwith (to_string e)
end
