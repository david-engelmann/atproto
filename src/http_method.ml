module HttpMethod = struct
  type http_method =
    | Get
    | Post
    (*| Put
    | Delete
    | Patch*)

  let lookup_method meth : http_method =
    match meth with
     | "get" -> Get
     | "post" -> Post
end


