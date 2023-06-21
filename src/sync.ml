open Session
open Cohttp_client
open App

module Sync = struct
  let create_sync_endpoint (query_name : string) : string =
    "com.atproto.sync" ^ "." ^ query_name

end
