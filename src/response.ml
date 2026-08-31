module Response = struct
  type response = {
    success : bool;
    status_code : int;
    content : bytes;
    headers : (string * string) list;
  }

  let make ~status_code ~content ?(headers = []) () : response =
    {
      success = status_code >= 200 && status_code < 300;
      status_code;
      content;
      headers;
    }

  let of_string ~status_code ?(headers = []) body : response =
    make ~status_code ~content:(Bytes.of_string body) ~headers ()

  let body_string (r : response) : string = Bytes.to_string r.content

  let header (r : response) name : string option =
    let lower = String.lowercase_ascii name in
    List.find_map
      (fun (k, v) -> if String.lowercase_ascii k = lower then Some v else None)
      r.headers

  let content_type (r : response) : string option = header r "content-type"
end
