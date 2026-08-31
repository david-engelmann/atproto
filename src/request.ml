open Http_method

module Request = struct
  type request = {
    method_ : Http_method.http_method;
    url : string;
    headers : (string * string) list;
    body : string option;
  }

  let user_agent = "david-engelmann/atproto (OCaml SDK)"

  let create ~method_ ~url ?(headers = []) ?body () : request =
    { method_; url; headers; body }

  let get url ?(headers = []) () : request =
    create ~method_:Http_method.Get ~url ~headers ()

  let post url ?(headers = []) ?body () : request =
    create ~method_:Http_method.Post ~url ~headers ?body ()

  let put url ?(headers = []) ?body () : request =
    create ~method_:Http_method.Put ~url ~headers ?body ()

  let delete url ?(headers = []) ?body () : request =
    create ~method_:Http_method.Delete ~url ~headers ?body ()

  let sample_request_with_body : request =
    {
      method_ = Http_method.Get;
      url = "https://github.com/david-engelmann";
      headers = [ ("User-Agent", user_agent) ];
      body = Some "{\"July\": \"Jackson\"}";
    }

  let sample_request_without_body : request =
    {
      method_ = Http_method.Get;
      url = "https://github.com/david-engelmann";
      headers = [ ("User-Agent", user_agent) ];
      body = None;
    }

  let test_get : Http_method.http_method = Http_method.Get
  let test_post : Http_method.http_method = Http_method.Post
end
