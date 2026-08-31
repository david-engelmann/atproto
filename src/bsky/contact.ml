open Session
open Client
open Actor

(** app.bsky.contact — phone-verified contact import / match (current lexicons). *)
module Contact = struct
  type match_and_index = {
    match_ : Actor.short_profile;
    contact_index : int;
  }

  type import_result = { matches : match_and_index list }
  type sync_status = { synced_at : string; matches_count : int }
  type sync_status_opt = { sync_status : sync_status option }
  type matches_page = { cursor : string option; matches : Actor.short_profile list }

  let parse_match_and_index json : match_and_index =
    let match_ =
      match Yojson.Safe.Util.member "match" json with
      | `Assoc _ as p -> Actor.parse_short_profile p
      | _ -> Actor.parse_short_profile json
    in
    { match_; contact_index = Client.int_member json "contactIndex" }

  let parse_import_result json : import_result =
    {
      matches =
        List.map parse_match_and_index
          (Client.list_member json "matchesAndContactIndexes");
    }

  let parse_sync_status json : sync_status =
    {
      synced_at = Client.string_member json "syncedAt";
      matches_count = Client.int_member json "matchesCount";
    }

  let parse_sync_status_opt json : sync_status_opt =
    {
      sync_status =
        (match Yojson.Safe.Util.member "syncStatus" json with
        | `Assoc _ as s -> Some (parse_sync_status s)
        | _ -> None);
    }

  let parse_matches_page json : matches_page =
    {
      cursor = Client.string_opt json "cursor";
      matches =
        List.map Actor.parse_short_profile (Client.list_member json "matches");
    }

  let import_contacts_body ~token ~contacts : Yojson.Safe.t =
    `Assoc
      [
        ("token", `String token);
        ("contacts", `List (List.map (fun p -> `String p) contacts));
      ]

  let dismiss_match_body ~subject : Yojson.Safe.t =
    `Assoc [ ("subject", `String subject) ]

  let start_phone_verification_body ~phone : Yojson.Safe.t =
    `Assoc [ ("phone", `String phone) ]

  let verify_phone_body ~phone ~code : Yojson.Safe.t =
    `Assoc [ ("phone", `String phone); ("code", `String code) ]

  let send_notification_body ~from ~to_ : Yojson.Safe.t =
    `Assoc [ ("from", `String from); ("to", `String to_) ]

  let get_matches (s : Session.session) ?limit ?cursor () : matches_page =
    Client.get_json ~session:s "app.bsky.contact.getMatches"
      (Client.opt_int "limit" limit @ Client.opt_pair "cursor" cursor)
    |> parse_matches_page

  let get_sync_status (s : Session.session) : sync_status_opt =
    Client.get_json ~session:s "app.bsky.contact.getSyncStatus" []
    |> parse_sync_status_opt

  let import_contacts (s : Session.session) ~token ~contacts () : import_result
      =
    Client.post_json ~session:s "app.bsky.contact.importContacts"
      (Yojson.Safe.to_string (import_contacts_body ~token ~contacts))
    |> parse_import_result

  let dismiss_match (s : Session.session) ~subject () : unit =
    ignore
      (Client.post_json ~session:s "app.bsky.contact.dismissMatch"
         (Yojson.Safe.to_string (dismiss_match_body ~subject)))

  let remove_data (s : Session.session) : unit =
    ignore (Client.post_json ~session:s "app.bsky.contact.removeData" "{}")

  let start_phone_verification (s : Session.session) ~phone () : unit =
    ignore
      (Client.post_json ~session:s "app.bsky.contact.startPhoneVerification"
         (Yojson.Safe.to_string (start_phone_verification_body ~phone)))

  let verify_phone (s : Session.session) ~phone ~code () : string =
    Client.post_json ~session:s "app.bsky.contact.verifyPhone"
      (Yojson.Safe.to_string (verify_phone_body ~phone ~code))
    |> fun json -> Client.string_member json "token"

  let send_notification (s : Session.session) ~from ~to_ () : unit =
    ignore
      (Client.post_json ~session:s "app.bsky.contact.sendNotification"
         (Yojson.Safe.to_string (send_notification_body ~from ~to_)))
end
