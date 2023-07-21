open Session
open Cohttp_client
open App
open Actor

module Notification = struct
  type strong_ref =
    {
      uri : string;
      cid : string;
    }
  type record =
    {
      record_type : string;
      subject : strong_ref;
      created_at : string;
    }
  type notification =
    {
      uri : string;
      cid : string;
      author : Actor.short_profile;
      reason : string;
      reason_subject : string;
      record : record;
      is_read : bool;
      indexed_at : string;
      labels : (string list) option;
    }

  let parse_strong_ref json : strong_ref =
    let open Yojson.Safe.Util in
    let uri = json |> member "uri" |> to_string in
    let cid = json |> member "cid" |> to_string in
    { uri; cid }

  let parse_record json : record =
    let open Yojson.Safe.Util in
    let record_type = json |> member "$type" |> to_string in
    let subject = json |> member "subject" |> parse_strong_ref in
    let created_at = json |> member "createdAt" |> to_string in
    { record_type; subject; created_at }

  let parse_notification json : notification =
    let open Yojson.Safe.Util in
    let uri = json |> member "uri" |> to_string in
    let cid = json |> member "cid" |> to_string in
    let author = json |> member "author" |> Actor.parse_short_profile in
    let reason = json |> member "reason" |> to_string in
    let reason_subject = json |> member "reasonSubject" |> to_string in
    let record = json |> member "record" |> parse_record in
    let is_read = json |> member "isRead" |> to_bool in
    let indexed_at = json |> member "indexedAt" |> to_string in
    let labels =
      match json |> member "labels" with
      | `Null -> None
      | `List labels_json -> Some (labels_json |> List.map to_string)
      | _ -> None
    in
    { uri; cid; author; reason; reason_subject; record; is_read;
      indexed_at; labels }

  let create_notification_endpoint (query_name : string) : string =
    "app.bsky.notification" ^ "." ^ query_name

  let convert_body_to_json (body : string) : Yojson.Safe.t =
    let json = Yojson.Safe.from_string body in
    json

  let get_unread_count (s : Session.session) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_unread_count_url = App.create_endpoint_url base_url (create_notification_endpoint "getUnreadCount") in
    let unread_count = Lwt_main.run (Cohttp_client.get_request_with_headers get_unread_count_url headers) in
    unread_count

  let list_notifications (s : Session.session) (limit : int) : notification list =
    let open Yojson.Safe.Util in
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let list_notifications_url = App.create_endpoint_url base_url (create_notification_endpoint "listNotifications") in
    let body = Cohttp_client.create_body_from_pairs [("limit", string_of_int limit)] in
    let notifications = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers list_notifications_url body headers) in
    Printf.printf "Notification from endpoint: %s\n" notifications;
    notifications |> convert_body_to_json |> member "notifications" |> to_list |> List.map parse_notification

  let update_seen (s : Session.session) (seen_at : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_update_seen_url = App.create_endpoint_url base_url (create_notification_endpoint "updateSeen") in
    let data = Printf.sprintf "{\"seenAt\": \"%s\"}" seen_at in
    let updated_seen = Lwt_main.run (Cohttp_client.post_data_with_headers get_update_seen_url data headers) in
    updated_seen
end
