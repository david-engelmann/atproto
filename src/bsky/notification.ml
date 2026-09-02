open Session
open Cohttp_client
open App
open Actor

(** [app.bsky.notification] — list, prefs, activity subscriptions, and push. *)
module Notification = struct
  type strong_ref = { uri : string; cid : string }

  type like_record = {
    record_type : string;
    subject : strong_ref;
    created_at : string;
  }

  type follow_record = {
    record_type : string;
    subject : string;
    created_at : string;
  }

  type repost_record = {
    record_type : string;
    subject : strong_ref;
    created_at : string;
  }

  type reply = { root : strong_ref; parent : strong_ref }

  type reply_record = {
    text : string;
    record_type : string;
    langs : string list;
    reply : reply;
    created_at : string;
  }

  type unread_count = { count : int }
  type other_record = { reason : string; raw : Yojson.Safe.t }

  type quote_record = {
    text : string;
    record_type : string;
    langs : string list;
    embed : Embed.Embed.embed option;
    created_at : string;
  }

  type mention_record = {
    text : string;
    record_type : string;
    langs : string list;
    created_at : string;
  }

  type starterpack_record = {
    record_type : string;
    created_at : string option;
    raw : Yojson.Safe.t;
  }

  type verification_record = {
    record_type : string;
    created_at : string option;
    raw : Yojson.Safe.t;
  }

  type record =
    [ `Like of like_record
    | `Follow of follow_record
    | `Repost of repost_record
    | `Reply of reply_record
    | `Quote of quote_record
    | `Mention of mention_record
    | `Starterpack_joined of starterpack_record
    | `Verified of verification_record
    | `Unverified of verification_record
    | `Like_via_repost of like_record
    | `Repost_via_repost of repost_record
    | `Subscribed_post of mention_record
    | `Contact_match of other_record
    | `Other of other_record ]

  type notification = {
    uri : string;
    cid : string;
    author : Actor.short_profile;
    reason : string;
    reason_subject : string option;
    record : record;
    is_read : bool;
    indexed_at : string;
    labels : string list option;
    starter_pack : Yojson.Safe.t option;
  }

  type notification_page = {
    cursor : string option;
    notifications : notification list;
    priority : bool option;
    seen_at : string option;
  }

  type chat_preference = { include_ : string; push : bool }
  type filterable_preference = { include_ : string; list : bool; push : bool }
  type preference = { list : bool; push : bool }

  type preferences = {
    chat : chat_preference;
    follow : filterable_preference;
    like : filterable_preference;
    like_via_repost : filterable_preference;
    mention : filterable_preference;
    quote : filterable_preference;
    reply : filterable_preference;
    repost : filterable_preference;
    repost_via_repost : filterable_preference;
    starterpack_joined : preference;
    subscribed_post : preference;
    unverified : preference;
    verified : preference;
    original : Yojson.Safe.t;
  }

  type activity_subscription = { post : bool; reply : bool }

  type activity_subscription_page = {
    cursor : string option;
    subscriptions : Actor.short_profile list;
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

  let parse_reply_option json : reply option =
    let open Yojson.Safe.Util in
    try Some (json |> member "reply" |> parse_reply) with Type_error _ -> None

  let string_or_empty json field =
    match Yojson.Safe.Util.member field json with `String s -> s | _ -> ""

  let langs_of json =
    match Yojson.Safe.Util.member "langs" json with
    | `List items ->
        List.filter_map (function `String s -> Some s | _ -> None) items
    | _ -> []

  let parse_like_body json : like_record =
    let open Yojson.Safe.Util in
    {
      record_type = string_or_empty json "$type";
      subject = json |> member "subject" |> parse_strong_ref;
      created_at = string_or_empty json "createdAt";
    }

  let parse_repost_body json : repost_record =
    let open Yojson.Safe.Util in
    {
      record_type = string_or_empty json "$type";
      subject = json |> member "subject" |> parse_strong_ref;
      created_at = string_or_empty json "createdAt";
    }

  let parse_mention_body json : mention_record =
    {
      text = string_or_empty json "text";
      record_type = string_or_empty json "$type";
      langs = langs_of json;
      created_at = string_or_empty json "createdAt";
    }

  let parse_record json reason : record =
    let open Yojson.Safe.Util in
    match reason with
    | "like" -> `Like (parse_like_body json)
    | "like-via-repost" -> `Like_via_repost (parse_like_body json)
    | "follow" ->
        `Follow
          {
            record_type = string_or_empty json "$type";
            subject = string_or_empty json "subject";
            created_at = string_or_empty json "createdAt";
          }
    | "repost" -> `Repost (parse_repost_body json)
    | "repost-via-repost" -> `Repost_via_repost (parse_repost_body json)
    | "reply" ->
        let text = string_or_empty json "text" in
        let record_type = string_or_empty json "$type" in
        let langs = langs_of json in
        let reply =
          try json |> member "reply" |> parse_reply
          with Type_error _ ->
            { root = { uri = ""; cid = "" }; parent = { uri = ""; cid = "" } }
        in
        let created_at = string_or_empty json "createdAt" in
        `Reply { text; record_type; langs; reply; created_at }
    | "quote" ->
        `Quote
          {
            text = string_or_empty json "text";
            record_type = string_or_empty json "$type";
            langs = langs_of json;
            embed = Embed.Embed.parse_embed_option json;
            created_at = string_or_empty json "createdAt";
          }
    | "mention" -> `Mention (parse_mention_body json)
    | "subscribed-post" -> `Subscribed_post (parse_mention_body json)
    | "starterpack-joined" ->
        `Starterpack_joined
          {
            record_type = string_or_empty json "$type";
            created_at =
              (match json |> member "createdAt" with
              | `String s -> Some s
              | _ -> None);
            raw = json;
          }
    | "verified" ->
        `Verified
          {
            record_type = string_or_empty json "$type";
            created_at =
              (match json |> member "createdAt" with
              | `String s -> Some s
              | _ -> None);
            raw = json;
          }
    | "unverified" ->
        `Unverified
          {
            record_type = string_or_empty json "$type";
            created_at =
              (match json |> member "createdAt" with
              | `String s -> Some s
              | _ -> None);
            raw = json;
          }
    | "contact-match" -> `Contact_match { reason; raw = json }
    | _ -> `Other { reason; raw = json }

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
    let labels = Label.Label.parse_label_values (json |> member "labels") in
    let starter_pack =
      match json |> member "starterPack" with
      | `Null -> None
      | other -> Some other
    in
    {
      uri;
      cid;
      author;
      reason;
      reason_subject;
      record;
      is_read;
      indexed_at;
      labels;
      starter_pack;
    }

  let parse_notification_page json : notification_page =
    let open Yojson.Safe.Util in
    {
      cursor =
        (match json |> member "cursor" with `String s -> Some s | _ -> None);
      notifications =
        (match json |> member "notifications" with
        | `List xs -> List.map parse_notification xs
        | _ -> []);
      priority =
        (match json |> member "priority" with `Bool b -> Some b | _ -> None);
      seen_at =
        (match json |> member "seenAt" with `String s -> Some s | _ -> None);
    }

  let create_notification_endpoint (query_name : string) : string =
    "app.bsky.notification" ^ "." ^ query_name

  let convert_body_to_json (body : string) : Yojson.Safe.t =
    let json = Yojson.Safe.from_string body in
    json

  (** Unread count via [app.bsky.notification.getUnreadCount]. *)
  let get_unread_count (s : Session.session) : unread_count =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let get_unread_count_url =
      App.create_endpoint_url base_url
        (create_notification_endpoint "getUnreadCount")
    in
    let unread_count =
      Lwt_main.run
        (Cohttp_client.get_request_with_headers get_unread_count_url headers)
    in
    unread_count |> convert_body_to_json |> parse_unread_count

  (** Notifications for the session via
      [app.bsky.notification.listNotifications]. Optional [reasons] /
      [priority] / [cursor] / [seen_at] map to the lexicon query. *)
  let list_notifications (s : Session.session) ?reasons ?priority ?cursor
      ?seen_at (limit : int) : notification list =
    Client.Client.get_json ~session:s "app.bsky.notification.listNotifications"
      (("limit", string_of_int limit)
       :: Client.Client.repeat_param "reasons"
            (Option.value reasons ~default:[])
      @ Client.Client.opt_bool "priority" priority
      @ Client.Client.opt_pair "cursor" cursor
      @ Client.Client.opt_pair "seenAt" seen_at)
    |> parse_notification_page
    |> fun (p : notification_page) -> p.notifications

  (** Paginated notifications via
      [app.bsky.notification.listNotifications]. Optional [reasons] /
      [priority] / [cursor] / [seen_at] / [limit] map to the lexicon
      query. *)
  let list_notifications_page (s : Session.session) ?reasons ?priority ?cursor
      ?seen_at ?limit () : notification_page =
    Client.Client.get_json ~session:s "app.bsky.notification.listNotifications"
      (Client.Client.opt_int "limit" limit
      @ Client.Client.repeat_param "reasons" (Option.value reasons ~default:[])
      @ Client.Client.opt_bool "priority" priority
      @ Client.Client.opt_pair "cursor" cursor
      @ Client.Client.opt_pair "seenAt" seen_at)
    |> parse_notification_page

  (** Mark notifications seen at [seen_at] via
      [app.bsky.notification.updateSeen]. *)
  let update_seen (s : Session.session) (seen_at : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let get_update_seen_url =
      App.create_endpoint_url base_url
        (create_notification_endpoint "updateSeen")
    in
    let data = Printf.sprintf "{\"seenAt\": \"%s\"}" seen_at in
    let updated_seen =
      Lwt_main.run
        (Cohttp_client.post_data_with_headers get_update_seen_url data headers)
    in
    updated_seen

  let parse_chat_preference json : chat_preference =
    {
      include_ =
        (match Yojson.Safe.Util.member "include" json with
        | `String s -> s
        | _ -> "all");
      push =
        (match Yojson.Safe.Util.member "push" json with
        | `Bool b -> b
        | _ -> false);
    }

  let parse_filterable_preference json : filterable_preference =
    {
      include_ =
        (match Yojson.Safe.Util.member "include" json with
        | `String s -> s
        | _ -> "all");
      list =
        (match Yojson.Safe.Util.member "list" json with
        | `Bool b -> b
        | _ -> false);
      push =
        (match Yojson.Safe.Util.member "push" json with
        | `Bool b -> b
        | _ -> false);
    }

  let parse_preference json : preference =
    {
      list =
        (match Yojson.Safe.Util.member "list" json with
        | `Bool b -> b
        | _ -> false);
      push =
        (match Yojson.Safe.Util.member "push" json with
        | `Bool b -> b
        | _ -> false);
    }

  let empty_filterable : filterable_preference =
    { include_ = "all"; list = false; push = false }

  let empty_preference : preference = { list = false; push = false }

  let parse_preferences json : preferences =
    let open Yojson.Safe.Util in
    let prefs =
      match json |> member "preferences" with `Assoc _ as p -> p | _ -> json
    in
    {
      chat =
        (match prefs |> member "chat" with
        | `Assoc _ as c -> parse_chat_preference c
        | _ -> { include_ = "all"; push = false });
      follow =
        (match prefs |> member "follow" with
        | `Assoc _ as c -> parse_filterable_preference c
        | _ -> empty_filterable);
      like =
        (match prefs |> member "like" with
        | `Assoc _ as c -> parse_filterable_preference c
        | _ -> empty_filterable);
      like_via_repost =
        (match prefs |> member "likeViaRepost" with
        | `Assoc _ as c -> parse_filterable_preference c
        | _ -> empty_filterable);
      mention =
        (match prefs |> member "mention" with
        | `Assoc _ as c -> parse_filterable_preference c
        | _ -> empty_filterable);
      quote =
        (match prefs |> member "quote" with
        | `Assoc _ as c -> parse_filterable_preference c
        | _ -> empty_filterable);
      reply =
        (match prefs |> member "reply" with
        | `Assoc _ as c -> parse_filterable_preference c
        | _ -> empty_filterable);
      repost =
        (match prefs |> member "repost" with
        | `Assoc _ as c -> parse_filterable_preference c
        | _ -> empty_filterable);
      repost_via_repost =
        (match prefs |> member "repostViaRepost" with
        | `Assoc _ as c -> parse_filterable_preference c
        | _ -> empty_filterable);
      starterpack_joined =
        (match prefs |> member "starterpackJoined" with
        | `Assoc _ as c -> parse_preference c
        | _ -> empty_preference);
      subscribed_post =
        (match prefs |> member "subscribedPost" with
        | `Assoc _ as c -> parse_preference c
        | _ -> empty_preference);
      unverified =
        (match prefs |> member "unverified" with
        | `Assoc _ as c -> parse_preference c
        | _ -> empty_preference);
      verified =
        (match prefs |> member "verified" with
        | `Assoc _ as c -> parse_preference c
        | _ -> empty_preference);
      original = prefs;
    }

  let filterable_to_json (p : filterable_preference) : Yojson.Safe.t =
    `Assoc
      [
        ("include", `String p.include_);
        ("list", `Bool p.list);
        ("push", `Bool p.push);
      ]

  let preference_to_json (p : preference) : Yojson.Safe.t =
    `Assoc [ ("list", `Bool p.list); ("push", `Bool p.push) ]

  let chat_preference_to_json (p : chat_preference) : Yojson.Safe.t =
    `Assoc [ ("include", `String p.include_); ("push", `Bool p.push) ]

  let preferences_to_json (p : preferences) : Yojson.Safe.t =
    `Assoc
      [
        ("chat", chat_preference_to_json p.chat);
        ("follow", filterable_to_json p.follow);
        ("like", filterable_to_json p.like);
        ("likeViaRepost", filterable_to_json p.like_via_repost);
        ("mention", filterable_to_json p.mention);
        ("quote", filterable_to_json p.quote);
        ("reply", filterable_to_json p.reply);
        ("repost", filterable_to_json p.repost);
        ("repostViaRepost", filterable_to_json p.repost_via_repost);
        ("starterpackJoined", preference_to_json p.starterpack_joined);
        ("subscribedPost", preference_to_json p.subscribed_post);
        ("unverified", preference_to_json p.unverified);
        ("verified", preference_to_json p.verified);
      ]

  (** Notification preferences via [app.bsky.notification.getPreferences]. *)
  let get_preferences (s : Session.session) : preferences =
    Client.Client.get_json ~session:s "app.bsky.notification.getPreferences" []
    |> parse_preferences

  (** Set [priority] via [app.bsky.notification.putPreferences]. *)
  let put_preferences (s : Session.session) ~priority () : unit =
    ignore
      (Client.Client.post_json ~session:s "app.bsky.notification.putPreferences"
         (Yojson.Safe.to_string (`Assoc [ ("priority", `Bool priority) ])))

  (** Replace notification preferences via
      [app.bsky.notification.putPreferencesV2]. *)
  let put_preferences_v2 (s : Session.session) (prefs : preferences) :
      preferences =
    Client.Client.post_json ~session:s "app.bsky.notification.putPreferencesV2"
      (Yojson.Safe.to_string (preferences_to_json prefs))
    |> parse_preferences

  let parse_activity_subscription json : activity_subscription =
    {
      post =
        (match Yojson.Safe.Util.member "post" json with
        | `Bool b -> b
        | _ -> false);
      reply =
        (match Yojson.Safe.Util.member "reply" json with
        | `Bool b -> b
        | _ -> false);
    }

  let activity_subscription_to_json (a : activity_subscription) : Yojson.Safe.t
      =
    `Assoc [ ("post", `Bool a.post); ("reply", `Bool a.reply) ]

  let parse_activity_subscription_page json : activity_subscription_page =
    let open Yojson.Safe.Util in
    {
      cursor =
        (match json |> member "cursor" with `String s -> Some s | _ -> None);
      subscriptions =
        (match json |> member "subscriptions" with
        | `List xs -> List.map Actor.parse_short_profile xs
        | _ -> []);
    }

  (** Activity subscriptions via
      [app.bsky.notification.listActivitySubscriptions]. Optional
      [limit] / [cursor] map to the lexicon query. *)
  let list_activity_subscriptions (s : Session.session) ?limit ?cursor () :
      activity_subscription_page =
    Client.Client.get_json ~session:s
      "app.bsky.notification.listActivitySubscriptions"
      (Client.Client.opt_int "limit" limit
      @ Client.Client.opt_pair "cursor" cursor)
    |> parse_activity_subscription_page

  (** Put an activity subscription for [subject] via
      [app.bsky.notification.putActivitySubscription]. *)
  let put_activity_subscription (s : Session.session) ~subject
      ~(activity_subscription : activity_subscription) () :
      string * activity_subscription option =
    let json =
      Client.Client.post_json ~session:s
        "app.bsky.notification.putActivitySubscription"
        (Yojson.Safe.to_string
           (`Assoc
             [
               ("subject", `String subject);
               ( "activitySubscription",
                 activity_subscription_to_json activity_subscription );
             ]))
    in
    let open Yojson.Safe.Util in
    ( (match json |> member "subject" with `String s -> s | _ -> subject),
      match json |> member "activitySubscription" with
      | `Assoc _ as a -> Some (parse_activity_subscription a)
      | _ -> None )

  (** Register a push token via [app.bsky.notification.registerPush].
      Client wrapper for a hosted push service; this library does not
      send push or fake a live hop. Optional [age_restricted] maps to
      the lexicon body. *)
  let register_push (s : Session.session) ~service_did ~token ~platform ~app_id
      ?age_restricted () : unit =
    let fields =
      [
        ("serviceDid", `String service_did);
        ("token", `String token);
        ("platform", `String platform);
        ("appId", `String app_id);
      ]
      @
      match age_restricted with
      | Some b -> [ ("ageRestricted", `Bool b) ]
      | None -> []
    in
    ignore
      (Client.Client.post_json ~session:s "app.bsky.notification.registerPush"
         (Yojson.Safe.to_string (`Assoc fields)))

  (** Unregister a push token via [app.bsky.notification.unregisterPush].
      Client wrapper for a hosted push service; this library does not
      send push or fake a live hop. *)
  let unregister_push (s : Session.session) ~service_did ~token ~platform
      ~app_id () : unit =
    ignore
      (Client.Client.post_json ~session:s "app.bsky.notification.unregisterPush"
         (Yojson.Safe.to_string
            (`Assoc
              [
                ("serviceDid", `String service_did);
                ("token", `String token);
                ("platform", `String platform);
                ("appId", `String app_id);
              ])))
end
