open Bluesky.Http_method

module Request = struct
    type request =
        {
          method_ : Http_method.http_method;
          url : string;
          headers : (string * string) list;
          body : string option;
        }
end
