module Method = struct
  type method =
    | Get
    | Post
    (*| Put
    | Delete
    | Patch*)

  let lookup_method meth =
    match meth with
     | "get" -> Get
     | "post" -> Post
end


