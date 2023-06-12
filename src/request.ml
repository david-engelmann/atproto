open Bluesky.Response

module Request = struct
    type request =
        {
          method_ : Response.response;
          url : string;
          headers : (string * string) list;
          body : string option;
        }
end
