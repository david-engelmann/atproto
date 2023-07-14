open Cohttp_client
open App
open Session

module Moderation = struct
  let create_moderation_endpoint (query_name : string) : string =
    "com.atproto.moderation" ^ "." ^ query_name
end
