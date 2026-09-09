open Session
open Client
open Actor

(** app.bsky.contact — hosted Bluesky phone-verified contact import /
    match (current lexicons).

    This is a client for the hosted AppView contact surface. It does
    not send SMS and does not fake a phone gateway. Official
    TestNetwork does not start one. The production path is:

    1. Password session: [get_matches] / [get_sync_status] /
       [import_contacts] / … through the PDS / entryway
    2. OAuth DPoP: mint [getServiceAuth] ([aud] = AppView DID,
       [lxm] = the [app.bsky.contact.*] NSID) and call
       [get_matches_service] / [import_contacts_service] / … on the
       AppView host ([Client.appview_host_from_env]). DPoP cannot be
       sent as the AppView Bearer.
    3. Phone flow (hosted SMS): [start_phone_verification] →
       [verify_phone] (returns [token]) → [import_contacts]. Live
       hops need a real number ([ATP_PHONE] / [ATP_PHONE_NUMBER])
       and are not invented here.

    [com.atproto.temp.requestPhoneVerification] is a different,
    privileged signup-SMS client ([Temp.request_phone_verification])
    and is also not faked. *)
module Contact = struct
  let env_truthy name =
    match Sys.getenv_opt name with
    | Some v ->
        let v = String.lowercase_ascii (String.trim v) in
        List.mem v [ "1"; "true"; "yes"; "on" ]
    | None -> false

  (** True when live hosted-SMS hops may run ([ATP_PHONE] is truthy).
      Default off so CI never sends SMS. *)
  let phone_live_enabled : bool = env_truthy "ATP_PHONE"

  (** Optional E.164 number from [ATP_PHONE_NUMBER]. Empty / unset
      is [None]. This library does not invent a number. *)
  let phone_number_from_env : string option =
    match Sys.getenv_opt "ATP_PHONE_NUMBER" with
    | Some n ->
        let n = String.trim n in
        if n = "" then None else Some n
    | None -> None

  let appview_host ?host () =
    match host with Some h -> h | None -> Client.appview_host_from_env

  type match_and_index = { match_ : Actor.short_profile; contact_index : int }
  type import_result = { matches : match_and_index list }
  type sync_status = { synced_at : string; matches_count : int }
  type sync_status_opt = { sync_status : sync_status option }

  type matches_page = {
    cursor : string option;
    matches : Actor.short_profile list;
  }

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

  (** Query-string pairs for [app.bsky.contact.getMatches].
      Currently sent fields only: optional [limit] / [cursor]. *)
  let get_matches_body ?limit ?cursor () : (string * string) list =
    Client.opt_int "limit" limit @ Client.opt_pair "cursor" cursor

  (** JSON body for [app.bsky.contact.importContacts]
      ([token] from [verify_phone]). *)
  let import_contacts_body ~token ~contacts : Yojson.Safe.t =
    `Assoc
      [
        ("token", `String token);
        ("contacts", `List (List.map (fun p -> `String p) contacts));
      ]

  (** JSON body for [app.bsky.contact.dismissMatch]. *)
  let dismiss_match_body ~subject : Yojson.Safe.t =
    `Assoc [ ("subject", `String subject) ]

  (** JSON body for [app.bsky.contact.startPhoneVerification].
      Hosted SMS; this library does not send a text. *)
  let start_phone_verification_body ~phone : Yojson.Safe.t =
    `Assoc [ ("phone", `String phone) ]

  (** JSON body for [app.bsky.contact.verifyPhone]. *)
  let verify_phone_body ~phone ~code : Yojson.Safe.t =
    `Assoc [ ("phone", `String phone); ("code", `String code) ]

  (** JSON body for [app.bsky.contact.sendNotification]. *)
  let send_notification_body ~from ~to_ : Yojson.Safe.t =
    `Assoc [ ("from", `String from); ("to", `String to_) ]

  (** Empty JSON body for [app.bsky.contact.removeData]. *)
  let remove_data_body : Yojson.Safe.t = `Assoc []

  (** Matched contacts via [app.bsky.contact.getMatches]. Shares
      [get_matches_body]. *)
  let get_matches (s : Session.session) ?limit ?cursor () : matches_page =
    Client.get_json ~session:s "app.bsky.contact.getMatches"
      (get_matches_body ?limit ?cursor ())
    |> parse_matches_page

  (** [get_matches] on AppView. With a session, mints service-auth
      ([aud] = AppView DID, [lxm] = [getMatches]). *)
  let get_matches_appview ?session ?host ?aud ?limit ?cursor () : matches_page =
    Client.get_json_appview ?session ?host ?aud "app.bsky.contact.getMatches"
      (get_matches_body ?limit ?cursor ())
    |> parse_matches_page

  (** [get_matches] on the AppView host with a PDS-minted
      service-auth JWT (OAuth DPoP [getServiceAuth]). *)
  let get_matches_service ~bearer ?host ?limit ?cursor () : matches_page =
    Client.get_json ~bearer ~host:(appview_host ?host ())
      "app.bsky.contact.getMatches"
      (get_matches_body ?limit ?cursor ())
    |> parse_matches_page

  (** Contact import status via [app.bsky.contact.getSyncStatus]. *)
  let get_sync_status (s : Session.session) : sync_status_opt =
    Client.get_json ~session:s "app.bsky.contact.getSyncStatus" []
    |> parse_sync_status_opt

  (** [get_sync_status] on AppView (service-auth when [session] is
      present). *)
  let get_sync_status_appview ?session ?host ?aud () : sync_status_opt =
    Client.get_json_appview ?session ?host ?aud "app.bsky.contact.getSyncStatus"
      []
    |> parse_sync_status_opt

  (** [get_sync_status] on the AppView host with a service-auth JWT. *)
  let get_sync_status_service ~bearer ?host () : sync_status_opt =
    Client.get_json ~bearer ~host:(appview_host ?host ())
      "app.bsky.contact.getSyncStatus" []
    |> parse_sync_status_opt

  (** Import phone contacts via [app.bsky.contact.importContacts]
      ([token] from [verify_phone]). Shares [import_contacts_body]. *)
  let import_contacts (s : Session.session) ~token ~contacts () : import_result
      =
    Client.post_json ~session:s "app.bsky.contact.importContacts"
      (Yojson.Safe.to_string (import_contacts_body ~token ~contacts))
    |> parse_import_result

  (** [import_contacts] on the AppView host with a service-auth JWT. *)
  let import_contacts_service ~bearer ?host ~token ~contacts () : import_result
      =
    Client.post_json ~bearer ~host:(appview_host ?host ())
      "app.bsky.contact.importContacts"
      (Yojson.Safe.to_string (import_contacts_body ~token ~contacts))
    |> parse_import_result

  (** Dismiss match [subject] via [app.bsky.contact.dismissMatch]. *)
  let dismiss_match (s : Session.session) ~subject () : unit =
    ignore
      (Client.post_json ~session:s "app.bsky.contact.dismissMatch"
         (Yojson.Safe.to_string (dismiss_match_body ~subject)))

  (** [dismiss_match] on the AppView host with a service-auth JWT. *)
  let dismiss_match_service ~bearer ?host ~subject () : unit =
    ignore
      (Client.post_json ~bearer ~host:(appview_host ?host ())
         "app.bsky.contact.dismissMatch"
         (Yojson.Safe.to_string (dismiss_match_body ~subject)))

  (** Remove stored hashes and matches via [app.bsky.contact.removeData]. *)
  let remove_data (s : Session.session) : unit =
    ignore
      (Client.post_json ~session:s "app.bsky.contact.removeData"
         (Yojson.Safe.to_string remove_data_body))

  (** [remove_data] on the AppView host with a service-auth JWT. *)
  let remove_data_service ~bearer ?host () : unit =
    ignore
      (Client.post_json ~bearer ~host:(appview_host ?host ())
         "app.bsky.contact.removeData"
         (Yojson.Safe.to_string remove_data_body))

  (** Start SMS verification via [app.bsky.contact.startPhoneVerification].
      Hosted-only; this is a client wrapper and is not faked. *)
  let start_phone_verification (s : Session.session) ~phone () : unit =
    ignore
      (Client.post_json ~session:s "app.bsky.contact.startPhoneVerification"
         (Yojson.Safe.to_string (start_phone_verification_body ~phone)))

  (** [start_phone_verification] on the AppView host with a
      service-auth JWT. Hosted SMS; not faked. *)
  let start_phone_verification_service ~bearer ?host ~phone () : unit =
    ignore
      (Client.post_json ~bearer ~host:(appview_host ?host ())
         "app.bsky.contact.startPhoneVerification"
         (Yojson.Safe.to_string (start_phone_verification_body ~phone)))

  (** Verify [phone] / [code] via [app.bsky.contact.verifyPhone]
      (returns [token] for [import_contacts]). *)
  let verify_phone (s : Session.session) ~phone ~code () : string =
    Client.post_json ~session:s "app.bsky.contact.verifyPhone"
      (Yojson.Safe.to_string (verify_phone_body ~phone ~code))
    |> fun json -> Client.string_member json "token"

  (** [verify_phone] on the AppView host with a service-auth JWT. *)
  let verify_phone_service ~bearer ?host ~phone ~code () : string =
    Client.post_json ~bearer ~host:(appview_host ?host ())
      "app.bsky.contact.verifyPhone"
      (Yojson.Safe.to_string (verify_phone_body ~phone ~code))
    |> fun json -> Client.string_member json "token"

  (** Contact-import notification via [app.bsky.contact.sendNotification]. *)
  let send_notification (s : Session.session) ~from ~to_ () : unit =
    ignore
      (Client.post_json ~session:s "app.bsky.contact.sendNotification"
         (Yojson.Safe.to_string (send_notification_body ~from ~to_)))

  (** [send_notification] on the AppView host with a service-auth JWT. *)
  let send_notification_service ~bearer ?host ~from ~to_ () : unit =
    ignore
      (Client.post_json ~bearer ~host:(appview_host ?host ())
         "app.bsky.contact.sendNotification"
         (Yojson.Safe.to_string (send_notification_body ~from ~to_)))
end
