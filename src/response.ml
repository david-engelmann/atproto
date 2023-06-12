module Response = struct
    type response =
        {
          success : bool;
          status_code : int;
          content : bytes;
          headers : (string * string) list;
        }
end
