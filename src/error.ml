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

  let of_json = parse_error_from_json

  let of_body (body : string) : t option =
    try
      let json = Yojson.Safe.from_string body in
      match Yojson.Safe.Util.member "error" json with
      | `String _ -> Some (parse_error_from_json json)
      | _ -> None
    with _ -> None

  let check_for_error json : string option =
    let open Yojson.Safe.Util in
    match json |> member "error" with `String s -> Some s | _ -> None

  let parse_error json : error_type =
    let e = parse_error_from_json json in
    match e.error with
    | "RateLimitExceeded" -> `RateLimitExceeded e
    | _ -> `Xrpc e

  let to_string (e : t) : string =
    if e.message = "" then e.error else e.error ^ ": " ^ e.message

  let handle_error error_type =
    match error_type with
    | `RateLimitExceeded e -> failwith ("RateLimitExceeded: " ^ e.message)
    | `Xrpc e -> failwith (to_string e)
end
