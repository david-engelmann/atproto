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

  type like_record =
    {
      record_type : string;
      subject : strong_ref;
      created_at : string;
    }

  type follow_record =
    {
      record_type : string;
      subject : string;
      created_at : string;
    }

  type repost_record =
    {
      record_type : string;
      subject : strong_ref;
      created_at : string;
    }

  type reply =
    {
      root : strong_ref;
      parent : strong_ref;
    }

  type reply_record =
    {
      text : string;
      record_type : string;
      langs : string list;
      reply : reply;
      created_at : string;
   }

  type unread_count =
    {
      count : int;
    }

  type record =
    [
    | `Like of like_record
    | `Follow of follow_record
    | `Repost of repost_record
    | `Reply of reply_record
    ]

  type notification =
    {
      uri : string;
      cid : string;
      author : Actor.short_profile;
      reason : string;
      reason_subject : string option;
      record : record;
      is_read : bool;
      indexed_at : string;
      labels : (string list) option;
    }

  (*
  let lookup_record (r : string) : record =
    match r with
    | "like" -> Like
    | "follow" -> Follow
    | "repost" -> Repost
    | "reply" -> Reply

  let lookup_record_type (r : record) =
    match r with
    | Like -> like_record
    | Follow -> follow_record
    | Repost -> repost_record
    | Reply -> reply_record
    | Unknown -> like_record
  *)
  let parse_unread_count json : unread_count =
    let open Yojson.Safe.Util in
    let count = json |> member "count" |> to_int in
    { count }

  let parse_strong_ref json : strong_ref =
    let open Yojson.Safe.Util in
    let uri = json |> member "uri" |> to_string in
    let cid = json |> member "cid" |> to_string in
    { uri; cid }

  let parse_reply json : reply =
    let open Yojson.Safe.Util in
    let root = json |> member "root" |> parse_strong_ref in
    let parent = json |> member "parent" |> parse_strong_ref in
    { root; parent }

  let parse_record json reason : record =
    let open Yojson.Safe.Util in
    match reason with
    | "like" ->
        let record_type = json |> member "$type" |> to_string in
        let subject = json |> member "subject" |> parse_strong_ref in
        let created_at = json |> member "createdAt" |> to_string in
        `Like { record_type; subject; created_at }
    | "follow" ->
        let record_type = json |> member "$type" |> to_string in
        let subject = json |> member "subject" |> to_string in
        let created_at = json |> member "createdAt" |> to_string in
        `Follow { record_type; subject; created_at }
    | "repost" ->
        let record_type = json |> member "$type" |> to_string in
        let subject = json |> member "subject" |> parse_strong_ref in
        let created_at = json |> member "createdAt" |> to_string in
        `Repost { record_type; subject; created_at }
    | "reply" ->
        let text = json |> member "text" |> to_string in
        let record_type = json |> member "$type" |> to_string in
        let langs = json |> member "langs" |> to_list |> List.map to_string in
        let reply = json |> member "reply" |> parse_reply in
        let created_at = json |> member "createdAt" |> to_string in
        `Reply { text; record_type; langs; reply; created_at }
    | _ -> failwith ("Unknown Record Type: " ^ reason)


  let parse_notification json : notification =
    let open Yojson.Safe.Util in
    let uri = json |> member "uri" |> to_string in
    let cid = json |> member "cid" |> to_string in
    let author = json |> member "author" |> Actor.parse_short_profile in
    let reason = json |> member "reason" |> to_string in
    let reason_subject = Actor.extract_string_option json "reasonSubject" in
    let record_json = json |> member "record" in
    let record = parse_record record_json reason in
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

  let get_unread_count (s : Session.session) : unread_count =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_unread_count_url = App.create_endpoint_url base_url (create_notification_endpoint "getUnreadCount") in
    let unread_count = Lwt_main.run (Cohttp_client.get_request_with_headers get_unread_count_url headers) in
    unread_count |> convert_body_to_json |> parse_unread_count

  let list_notifications (s : Session.session) (limit : int) : notification list =
    let open Yojson.Safe.Util in
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let list_notifications_url = App.create_endpoint_url base_url (create_notification_endpoint "listNotifications") in
    let body = Cohttp_client.create_body_from_pairs [("limit", string_of_int limit)] in
    let notifications_req = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers list_notifications_url body headers) in
    let notification_list = notifications_req |> convert_body_to_json |> member "notifications" |> to_list in
    notification_list |> List.map parse_notification

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
