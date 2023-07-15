open Session
open Cohttp_client
open App

module Notification = struct
  let create_notification_endpoint (query_name : string) : string =
    "app.bsky.notification" ^ "." ^ query_name
end
