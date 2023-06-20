open Session
open Cohttp_client
open App

module Feed = struct
  let create_feed_endpoint (query_name : string) : string =
    "app.bsky.feed" ^ "." ^ query_name
end
