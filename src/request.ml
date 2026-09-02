open Http_method

(** HTTP request record used by [Http_client] (method, URL, headers, body). *)
module Request = struct
  type request = {
    method_ : Http_method.http_method;
    url : string;
    headers : (string * string) list;
    body : string option;
  }

  (** User-Agent sent by [Http_client] ([david-engelmann/atproto]). *)
  let user_agent = "david-engelmann/atproto (OCaml SDK)"

  (** Build a request record ([method_], [url], optional [headers] /
      [body]). *)
  let create ~method_ ~url ?(headers = []) ?body () : request =
    { method_; url; headers; body }

  (** GET [url]. *)
  let get url ?(headers = []) () : request =
    create ~method_:Http_method.Get ~url ~headers ()

  (** POST [url] with optional [body]. *)
  let post url ?(headers = []) ?body () : request =
    create ~method_:Http_method.Post ~url ~headers ?body ()

  (** PUT [url] with optional [body]. *)
  let put url ?(headers = []) ?body () : request =
    create ~method_:Http_method.Put ~url ~headers ?body ()

  (** DELETE [url]. *)
  let delete url ?(headers = []) ?body () : request =
    create ~method_:Http_method.Delete ~url ~headers ?body ()

  (**/**)

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

  (**/**)
end
