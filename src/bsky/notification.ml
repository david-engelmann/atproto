open Session
open Cohttp_client
open App

module Notification = struct
  let create_notification_endpoint (query_name : string) : string =
    "app.bsky.notification" ^ "." ^ query_name

  let get_unread_count (s : Session.session) (seen_at : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_unread_count_url = App.create_endpoint_url base_url (create_notification_endpoint "getUnreadCount") in
    let body = Cohttp_client.create_body_from_pairs [("seenAt", seen_at)] in
    let unread_count = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_unread_count_url body headers) in
    unread_count

  let list_notifications (s : Session.session) (limit : int) (seen_at : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let list_notifications_url = App.create_endpoint_url base_url (create_notification_endpoint "listNotifications") in
    let body = Cohttp_client.create_body_from_pairs [("limit", string_of_int limit); ("seenAt", seen_at)] in
    let notifications = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers list_notifications_url body headers) in
    notifications
end
