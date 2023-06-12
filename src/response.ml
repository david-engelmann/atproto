module Response = struct
    type (string, string) response =
        {
          success : bool;
          status_code : int;
          content : bytes;
          headers : (string * string) list;
        }
end
