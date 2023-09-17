module Error = struct
  type error =
    {
      error : string;
      message : string;
    }

  type error_type =
    [
    | `RateLimitExceeded of error
    ]

  let parse_error_from_json json : error =
    let open Yojson.Safe.Util in
    let err = json |> member "error" |> to_string in
    let message = json |> member "message" |> to_string in
    { error=err; message }

  let parse_rate_limit_exceeded_error json : error_type =
    `RateLimitExceeded (parse_error_from_json json)

  let parse_error json : error_type =
    let open Yojson.Safe.Util in
    let err = json |> member "error" |> to_string in
    match err with
    | "RateLimitExceeded" -> parse_rate_limit_exceeded_error json
    | _ -> raise (Failure "Not a known error")

  let handle_error error_type =
    match error_type with
    | `RateLimitExceeded e -> raise (Failure ("error: " ^ e.error ^ " - message: " ^ e.message))
    | _ -> raise (Failure ("Not a known error: " ^ "fuck me"))
    (*
    | `RateLimitExceeded -> raise (Failure ("error: " ^ error_type.message))
    | _ -> raise (Failure ("Not a known error: " ^ error_type.error))
    *)

end
