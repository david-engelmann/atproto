module Http_method = struct
  type http_method =
    | Get
    | Post
    (*| Put
    | Delete
    | Patch*)

  let lookup_http_method meth : http_method =
    match meth with
     | "get" -> Get
     | "post" -> Post
     | _ -> failwith "Not Recognized Method"
end


