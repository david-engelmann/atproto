module Uri = struct
    type uri =
        {
          host : string;
          path_name : string;
          hash : string;
          search_params : (string, string) list;
        }
end
