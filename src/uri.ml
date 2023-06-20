module Uri = struct
    (*
     * at://did:plc:xov3uvxfd4to6ev3ak5g5uxk/app.bsky.feed.post/3jyf6gx25eb27
     *)
    type uri =
        {
          host : string;
          path_name : string;
          hash : string;
          search_params : (string * string) list option;
        }
end
