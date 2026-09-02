(** HTTP methods used by [Request] and [Http_client]. *)
module Http_method = struct
  type http_method = Get | Post | Put | Delete | Patch

  (** Parse ["GET"] / ["POST"] / ["PUT"] / ["DELETE"] / ["PATCH"]. *)
  let lookup_http_method meth : http_method =
    match String.lowercase_ascii meth with
    | "get" -> Get
    | "post" -> Post
    | "put" -> Put
    | "delete" -> Delete
    | "patch" -> Patch
    | _ -> failwith "Not Recognized Method"

  (** Uppercase method name. *)
  let to_string = function
    | Get -> "GET"
    | Post -> "POST"
    | Put -> "PUT"
    | Delete -> "DELETE"
    | Patch -> "PATCH"
end
