open Http_method

module Request = struct
    type request =
        {
          method_ : Http_method.http_method;
          url : string;
          headers : (string * string) list;
          body : string option;
        }

    let sample_request_with_body : request = {
        method_ = Http_method.Get;
        url = "https://github.com/david-engelmann";
        headers = [("User-Agent", "david-engelmann/bluesky (OCaml SDK)")];
        body = Some "{\"July\": \"Jackson\"}";
    }

    let sample_request_without_body : request = {
        method_ = Http_method.Get;
        url = "https://github.com/david-engelmann";
        headers = [("User-Agent", "david-engelmann/bluesky (OCaml SDK)")];
        body = None
    }

    let test_get : Http_method.http_method = Http_method.Get
    let test_post : Http_method.http_method = Http_method.Post
end
