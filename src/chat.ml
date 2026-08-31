open Session
open Client
open Xrpc
open Facet
open Embed

(** chat.bsky.convo — DMs. Requests must include atproto-proxy for the chat service. *)
module Chat = struct
  let default_proxy : Xrpc.proxy =
    { did = "did:web:api.bsky.chat"; service = "bsky_chat" }

  let proxy_headers ?(proxy = default_proxy) () = [ Xrpc.proxy_header proxy ]

  type member = {
    did : string;
    handle : string option;
    display_name : string option;
  }

  type reaction = { value : string; sender_did : string; created_at : string }

  type message = {
    id : string;
    rev : string;
    text : string;
    sender_did : string option;
    sent_at : string;
    deleted : bool;
    facets : Facet.facet list;
    reactions : reaction list;
    embed : Embed.embed option;
    reply_to_id : string option;
    original : Yojson.Safe.t;
  }

  type convo = {
    id : string;
    rev : string;
    muted : bool;
    unread_count : int;
    status : string option;
    members : member list;
    last_message : message option;
    original : Yojson.Safe.t;
  }

  type convos = { cursor : string option; convos : convo list }
  type messages = { cursor : string option; messages : message list }

  let parse_member json : member =
    {
      did = Client.string_member json "did";
      handle = Client.string_opt json "handle";
      display_name = Client.string_opt json "displayName";
    }

  let parse_reaction json : reaction =
    let sender_did =
      match Yojson.Safe.Util.member "sender" json with
      | `Assoc _ as s -> Client.string_member s "did"
      | _ -> Client.string_member json "sender"
    in
    {
      value = Client.string_member json "value";
      sender_did;
      created_at = Client.string_member json "createdAt";
    }

  let parse_message_facets json : Facet.facet list =
    List.map Facet.parse_facet (Client.list_member json "facets")

  let parse_reply_to_id json : string option =
    match Yojson.Safe.Util.member "replyTo" json with
    | `Assoc _ as r -> (
        match Client.string_opt r "messageId" with
        | Some id -> Some id
        | None -> Client.string_opt r "id")
    | `String s -> Some s
    | _ -> None

  let parse_message json : message =
    let ty = Client.string_opt json "$type" in
    let deleted =
      match ty with
      | Some t ->
          let n = String.length t in
          n >= 19 && String.sub t (n - 19) 19 = "deletedMessageView"
      | None ->
          Client.string_member json "text" = ""
          && Client.string_member json "id" <> ""
    in
    let sender_did =
      match Yojson.Safe.Util.member "sender" json with
      | `Assoc _ as s -> Client.string_opt s "did"
      | _ -> None
    in
    {
      id = Client.string_member json "id";
      rev = Client.string_member json "rev";
      text = Client.string_member json "text";
      sender_did;
      sent_at = Client.string_member json "sentAt";
      deleted;
      facets = parse_message_facets json;
      reactions = List.map parse_reaction (Client.list_member json "reactions");
      embed = Embed.parse_embed_option json;
      reply_to_id = parse_reply_to_id json;
      original = json;
    }

  let parse_convo json : convo =
    {
      id = Client.string_member json "id";
      rev = Client.string_member json "rev";
      muted = Client.bool_member json "muted";
      unread_count = Client.int_member json "unreadCount";
      status = Client.string_opt json "status";
      members = List.map parse_member (Client.list_member json "members");
      last_message =
        (match Yojson.Safe.Util.member "lastMessage" json with
        | `Assoc _ as m -> Some (parse_message m)
        | _ -> None);
      original = json;
    }

  let parse_convos json : convos =
    {
      cursor = Client.string_opt json "cursor";
      convos = List.map parse_convo (Client.list_member json "convos");
    }

  let parse_messages json : messages =
    {
      cursor = Client.string_opt json "cursor";
      messages = List.map parse_message (Client.list_member json "messages");
    }

  let message_input ?facets ?embed ?reply_to text : Yojson.Safe.t =
    let fields =
      [ ("text", `String text) ]
      @ (match facets with
        | Some fs -> [ ("facets", Facet.facets_to_json fs) ]
        | None -> [])
      @ (match embed with
        | Some e -> [ ("embed", Embed.embed_to_json e) ]
        | None -> [])
      @
      match reply_to with
      | Some id -> [ ("replyTo", `Assoc [ ("messageId", `String id) ]) ]
      | None -> []
    in
    `Assoc fields

  let send_message_body ~convo_id ~text ?facets ?embed ?reply_to () :
      Yojson.Safe.t =
    `Assoc
      [
        ("convoId", `String convo_id);
        ("message", message_input ?facets ?embed ?reply_to text);
      ]

  let update_read_body ~convo_id ?message_id () : Yojson.Safe.t =
    let fields =
      ("convoId", `String convo_id)
      ::
      (match message_id with
      | Some id -> [ ("messageId", `String id) ]
      | None -> [])
    in
    `Assoc fields

  let list_convos (s : Session.session) ?proxy ?limit ?cursor ?read_state
      ?status ?kind () : convos =
    Client.get_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.convo.listConvos"
      (Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor
      @ Client.opt_pair "readState" read_state
      @ Client.opt_pair "status" status
      @ Client.opt_pair "kind" kind)
    |> parse_convos

  let get_convo (s : Session.session) ?proxy ~convo_id () : convo =
    Client.get_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.convo.getConvo"
      [ ("convoId", convo_id) ]
    |> fun json ->
    match Yojson.Safe.Util.member "convo" json with
    | `Assoc _ as c -> parse_convo c
    | _ -> parse_convo json

  let get_convo_for_members (s : Session.session) ?proxy ~members () : convo =
    Client.get_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.convo.getConvoForMembers"
      (Client.repeat_param "members" members)
    |> fun json ->
    match Yojson.Safe.Util.member "convo" json with
    | `Assoc _ as c -> parse_convo c
    | _ -> parse_convo json

  let get_messages (s : Session.session) ?proxy ~convo_id ?limit ?cursor () :
      messages =
    Client.get_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.convo.getMessages"
      ([ ("convoId", convo_id) ]
      @ Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor)
    |> parse_messages

  let send_message (s : Session.session) ?proxy ~convo_id ~text ?facets ?embed
      ?reply_to () : message =
    Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.convo.sendMessage"
      (Yojson.Safe.to_string
         (send_message_body ~convo_id ~text ?facets ?embed ?reply_to ()))
    |> parse_message

  let update_read (s : Session.session) ?proxy ~convo_id ?message_id () : convo
      =
    Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.convo.updateRead"
      (Yojson.Safe.to_string (update_read_body ~convo_id ?message_id ()))
    |> fun json ->
    match Yojson.Safe.Util.member "convo" json with
    | `Assoc _ as c -> parse_convo c
    | _ -> parse_convo json

  let mute_convo (s : Session.session) ?proxy ~convo_id () : convo =
    Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.convo.muteConvo"
      (Yojson.Safe.to_string (`Assoc [ ("convoId", `String convo_id) ]))
    |> fun json ->
    match Yojson.Safe.Util.member "convo" json with
    | `Assoc _ as c -> parse_convo c
    | _ -> parse_convo json

  let unmute_convo (s : Session.session) ?proxy ~convo_id () : convo =
    Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.convo.unmuteConvo"
      (Yojson.Safe.to_string (`Assoc [ ("convoId", `String convo_id) ]))
    |> fun json ->
    match Yojson.Safe.Util.member "convo" json with
    | `Assoc _ as c -> parse_convo c
    | _ -> parse_convo json

  let convo_id_body convo_id : Yojson.Safe.t =
    `Assoc [ ("convoId", `String convo_id) ]

  let unwrap_convo json : convo =
    match Yojson.Safe.Util.member "convo" json with
    | `Assoc _ as c -> parse_convo c
    | _ -> parse_convo json

  let unwrap_message json : message =
    match Yojson.Safe.Util.member "message" json with
    | `Assoc _ as m -> parse_message m
    | _ -> parse_message json

  type convo_availability = { can_chat : bool; convo : convo option }
  type accept_result = { rev : string option }
  type leave_result = { convo_id : string; rev : string }

  type log_entry = {
    type_ : string;
    convo_id : string option;
    rev : string option;
    original : Yojson.Safe.t;
  }

  type logs = { cursor : string option; logs : log_entry list }
  type unread_counts = { unread_accepted : int; unread_request : int }

  type batch_item = {
    convo_id : string;
    text : string;
    facets : Facet.facet list option;
    embed : Embed.embed option;
    reply_to : string option;
  }

  let parse_availability json : convo_availability =
    {
      can_chat = Client.bool_member json "canChat";
      convo =
        (match Yojson.Safe.Util.member "convo" json with
        | `Assoc _ as c -> Some (parse_convo c)
        | _ -> None);
    }

  let parse_leave json : leave_result =
    {
      convo_id = Client.string_member json "convoId";
      rev = Client.string_member json "rev";
    }

  let parse_log_entry json : log_entry =
    {
      type_ = Client.string_member json "$type";
      convo_id = Client.string_opt json "convoId";
      rev = Client.string_opt json "rev";
      original = json;
    }

  let parse_logs json : logs =
    {
      cursor = Client.string_opt json "cursor";
      logs = List.map parse_log_entry (Client.list_member json "logs");
    }

  let parse_unread_counts json : unread_counts =
    {
      unread_accepted = Client.int_member json "unreadAcceptedConvos";
      unread_request = Client.int_member json "unreadRequestConvos";
    }

  let parse_accept json : accept_result = { rev = Client.string_opt json "rev" }

  let parse_requests json : convos =
    {
      cursor = Client.string_opt json "cursor";
      convos =
        List.map parse_convo
          (match Yojson.Safe.Util.member "requests" json with
          | `List xs -> xs
          | _ -> Client.list_member json "convos");
    }

  let accept_convo (s : Session.session) ?proxy ~convo_id () : accept_result =
    Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.convo.acceptConvo"
      (Yojson.Safe.to_string (convo_id_body convo_id))
    |> parse_accept

  let leave_convo (s : Session.session) ?proxy ~convo_id () : leave_result =
    Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.convo.leaveConvo"
      (Yojson.Safe.to_string (convo_id_body convo_id))
    |> parse_leave

  let add_reaction (s : Session.session) ?proxy ~convo_id ~message_id ~value ()
      : message =
    Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.convo.addReaction"
      (Yojson.Safe.to_string
         (`Assoc
           [
             ("convoId", `String convo_id);
             ("messageId", `String message_id);
             ("value", `String value);
           ]))
    |> unwrap_message

  let remove_reaction (s : Session.session) ?proxy ~convo_id ~message_id ~value
      () : message =
    Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.convo.removeReaction"
      (Yojson.Safe.to_string
         (`Assoc
           [
             ("convoId", `String convo_id);
             ("messageId", `String message_id);
             ("value", `String value);
           ]))
    |> unwrap_message

  let delete_message_for_self (s : Session.session) ?proxy ~convo_id ~message_id
      () : message =
    Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.convo.deleteMessageForSelf"
      (Yojson.Safe.to_string
         (`Assoc
           [ ("convoId", `String convo_id); ("messageId", `String message_id) ]))
    |> unwrap_message

  let get_convo_availability (s : Session.session) ?proxy ~members () :
      convo_availability =
    Client.get_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.convo.getConvoAvailability"
      (Client.repeat_param "members" members)
    |> parse_availability

  let get_log (s : Session.session) ?proxy ?cursor () : logs =
    Client.get_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.convo.getLog"
      (Client.opt_pair "cursor" cursor)
    |> parse_logs

  let get_unread_counts (s : Session.session) ?proxy ?include_group_chats () :
      unread_counts =
    Client.get_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.convo.getUnreadCounts"
      (Client.opt_bool "includeGroupChats" include_group_chats)
    |> parse_unread_counts

  let list_convo_requests (s : Session.session) ?proxy ?limit ?cursor () :
      convos =
    Client.get_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.convo.listConvoRequests"
      (Client.opt_int "limit" limit @ Client.opt_pair "cursor" cursor)
    |> parse_requests

  let send_message_batch (s : Session.session) ?proxy ~items () : message list =
    let payload =
      `Assoc
        [
          ( "items",
            `List
              (List.map
                 (fun (i : batch_item) ->
                   `Assoc
                     [
                       ("convoId", `String i.convo_id);
                       ( "message",
                         message_input ?facets:i.facets ?embed:i.embed
                           ?reply_to:i.reply_to i.text );
                     ])
                 items) );
        ]
    in
    Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.convo.sendMessageBatch"
      (Yojson.Safe.to_string payload)
    |> fun json -> List.map parse_message (Client.list_member json "items")

  let update_all_read (s : Session.session) ?proxy ?status () : int =
    Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.convo.updateAllRead"
      (Yojson.Safe.to_string
         (`Assoc
           (match status with
           | Some s -> [ ("status", `String s) ]
           | None -> [])))
    |> fun json ->
    match Yojson.Safe.Util.member "updatedCount" json with
    | `Int n -> n
    | _ -> 0

  let message_input_with_facets ?facets ?embed ?reply_to text : Yojson.Safe.t =
    message_input ?facets ?embed ?reply_to text

  let lock_convo (s : Session.session) ?proxy ~convo_id () : convo =
    Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.convo.lockConvo"
      (Yojson.Safe.to_string (convo_id_body convo_id))
    |> unwrap_convo

  let unlock_convo (s : Session.session) ?proxy ~convo_id () : convo =
    Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.convo.unlockConvo"
      (Yojson.Safe.to_string (convo_id_body convo_id))
    |> unwrap_convo

  type add_members_result = { convo : convo; added_members : member list }

  let add_members (s : Session.session) ?proxy ~convo_id ~members () :
      add_members_result =
    let json =
      Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
        "chat.bsky.group.addMembers"
        (Yojson.Safe.to_string
           (`Assoc
             [
               ("convoId", `String convo_id);
               ("members", `List (List.map (fun d -> `String d) members));
             ]))
    in
    {
      convo = unwrap_convo json;
      added_members =
        List.map parse_member (Client.list_member json "addedMembers");
    }

  let remove_members (s : Session.session) ?proxy ~convo_id ~members () : convo
      =
    Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.group.removeMembers"
      (Yojson.Safe.to_string
         (`Assoc
           [
             ("convoId", `String convo_id);
             ("members", `List (List.map (fun d -> `String d) members));
           ]))
    |> unwrap_convo

  let edit_group (s : Session.session) ?proxy ~convo_id ~name () : convo =
    Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.group.editGroup"
      (Yojson.Safe.to_string
         (`Assoc [ ("convoId", `String convo_id); ("name", `String name) ]))
    |> unwrap_convo

  type chat_pref = { include_ : string; push : bool }

  type notification_preferences = {
    chat : chat_pref;
    chat_request : chat_pref;
    original : Yojson.Safe.t;
  }

  let parse_chat_pref json : chat_pref =
    {
      include_ =
        (match Client.string_opt json "include" with
        | Some s -> s
        | None -> "all");
      push = Client.bool_member json "push";
    }

  let chat_pref_to_json (p : chat_pref) : Yojson.Safe.t =
    `Assoc [ ("include", `String p.include_); ("push", `Bool p.push) ]

  let parse_notification_preferences json : notification_preferences =
    let prefs =
      match Yojson.Safe.Util.member "preferences" json with
      | `Assoc _ as p -> p
      | _ -> json
    in
    {
      chat =
        (match Yojson.Safe.Util.member "chat" prefs with
        | `Assoc _ as c -> parse_chat_pref c
        | _ -> { include_ = "all"; push = false });
      chat_request =
        (match Yojson.Safe.Util.member "chatRequest" prefs with
        | `Assoc _ as c -> parse_chat_pref c
        | _ -> { include_ = "all"; push = false });
      original = prefs;
    }

  let get_notification_preferences (s : Session.session) ?proxy () :
      notification_preferences =
    Client.get_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.notification.getPreferences" []
    |> parse_notification_preferences

  let put_notification_preferences (s : Session.session) ?proxy ?chat
      ?chat_request () : notification_preferences =
    let fields =
      (match chat with
      | Some p -> [ ("chat", chat_pref_to_json p) ]
      | None -> [])
      @
      match chat_request with
      | Some p -> [ ("chatRequest", chat_pref_to_json p) ]
      | None -> []
    in
    Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.notification.putPreferences"
      (Yojson.Safe.to_string (`Assoc fields))
    |> parse_notification_preferences

  (* chat.bsky.actor — viewer status, declaration record, account export/delete. *)

  type actor_status = {
    chat_disabled : bool;
    can_create_groups : bool;
    group_member_limit : int;
  }

  type declaration = {
    allow_incoming : string;
    allow_group_invites : string option;
  }

  let parse_actor_status json : actor_status =
    {
      chat_disabled = Client.bool_member json "chatDisabled";
      can_create_groups = Client.bool_member json "canCreateGroups";
      group_member_limit = Client.int_member json "groupMemberLimit";
    }

  let parse_declaration json : declaration =
    {
      allow_incoming =
        (match Client.string_opt json "allowIncoming" with
        | Some s -> s
        | None -> "all");
      allow_group_invites = Client.string_opt json "allowGroupInvites";
    }

  let declaration_json ~allow_incoming ?allow_group_invites () : Yojson.Safe.t =
    let fields =
      [
        ("$type", `String "chat.bsky.actor.declaration");
        ("allowIncoming", `String allow_incoming);
      ]
      @
      match allow_group_invites with
      | Some s -> [ ("allowGroupInvites", `String s) ]
      | None -> []
    in
    `Assoc fields

  let get_actor_status (s : Session.session) ?proxy () : actor_status =
    Client.get_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.actor.getStatus" []
    |> parse_actor_status

  let delete_account (s : Session.session) ?proxy () : unit =
    ignore
      (Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
         "chat.bsky.actor.deleteAccount" "{}")

  let export_account_data (s : Session.session) ?proxy () : string =
    Client.get_text ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.actor.exportAccountData" []

  (* chat.bsky.moderation — operator views of convos / actor access. *)

  type actor_metadata_window = {
    messages_sent : int;
    messages_received : int;
    convos : int;
    convos_started : int;
  }

  type actor_metadata = {
    day : actor_metadata_window;
    month : actor_metadata_window;
    all : actor_metadata_window;
  }

  type mod_group = {
    created_at : string;
    join_request_count : int;
    lock_status : string option;
    member_count : int;
    member_limit : int;
    name : string;
  }

  type mod_convo_kind =
    [ `Direct | `Group of mod_group | `Unknown of Yojson.Safe.t ]

  type mod_convo = {
    id : string;
    rev : string;
    kind : mod_convo_kind;
    original : Yojson.Safe.t;
  }

  type mod_members = { cursor : string option; members : member list }

  let parse_metadata_window json : actor_metadata_window =
    {
      messages_sent = Client.int_member json "messagesSent";
      messages_received = Client.int_member json "messagesReceived";
      convos = Client.int_member json "convos";
      convos_started = Client.int_member json "convosStarted";
    }

  let parse_actor_metadata json : actor_metadata =
    let window field =
      match Yojson.Safe.Util.member field json with
      | `Assoc _ as w -> parse_metadata_window w
      | _ ->
          {
            messages_sent = 0;
            messages_received = 0;
            convos = 0;
            convos_started = 0;
          }
    in
    { day = window "day"; month = window "month"; all = window "all" }

  let parse_lock_status json : string option =
    match Yojson.Safe.Util.member "lockStatus" json with
    | `String s -> Some s
    | `Assoc _ as o -> (
        match Client.string_opt o "status" with
        | Some s -> Some s
        | None -> Client.string_opt o "$type")
    | _ -> None

  let parse_mod_group json : mod_group =
    {
      created_at = Client.string_member json "createdAt";
      join_request_count = Client.int_member json "joinRequestCount";
      lock_status = parse_lock_status json;
      member_count = Client.int_member json "memberCount";
      member_limit = Client.int_member json "memberLimit";
      name = Client.string_member json "name";
    }

  let parse_mod_kind json : mod_convo_kind =
    match Yojson.Safe.Util.member "kind" json with
    | `Assoc _ as k ->
        let ty = Client.string_opt k "$type" |> Option.value ~default:"" in
        if
          let n = String.length ty in
          n >= 10 && String.sub ty (n - 10) 10 = "groupConvo"
        then `Group (parse_mod_group k)
        else if
          let n = String.length ty in
          n >= 11 && String.sub ty (n - 11) 11 = "directConvo"
        then `Direct
        else if Client.string_member k "name" <> "" then
          `Group (parse_mod_group k)
        else `Unknown k
    | _ -> `Direct

  let parse_mod_convo json : mod_convo =
    {
      id = Client.string_member json "id";
      rev = Client.string_member json "rev";
      kind = parse_mod_kind json;
      original = json;
    }

  let unwrap_mod_convo json : mod_convo =
    match Yojson.Safe.Util.member "convo" json with
    | `Assoc _ as c -> parse_mod_convo c
    | _ -> parse_mod_convo json

  let parse_mod_convos json : mod_convo list =
    List.map parse_mod_convo (Client.list_member json "convos")

  let parse_mod_members json : mod_members =
    {
      cursor = Client.string_opt json "cursor";
      members = List.map parse_member (Client.list_member json "members");
    }

  let update_actor_access_body ~actor ~allow_access ?ref () : Yojson.Safe.t =
    let fields =
      [ ("actor", `String actor); ("allowAccess", `Bool allow_access) ]
      @ match ref with Some r -> [ ("ref", `String r) ] | None -> []
    in
    `Assoc fields

  let get_actor_metadata (s : Session.session) ?proxy ~actor () : actor_metadata
      =
    Client.get_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.moderation.getActorMetadata"
      [ ("actor", actor) ]
    |> parse_actor_metadata

  let get_mod_convo (s : Session.session) ?proxy ~convo_id () : mod_convo =
    Client.get_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.moderation.getConvo"
      [ ("convoId", convo_id) ]
    |> unwrap_mod_convo

  let get_mod_convos (s : Session.session) ?proxy ~convo_ids () : mod_convo list
      =
    Client.get_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.moderation.getConvos"
      (Client.repeat_param "convoIds" convo_ids)
    |> parse_mod_convos

  let get_mod_convo_members (s : Session.session) ?proxy ~convo_id ?limit
      ?cursor () : mod_members =
    Client.get_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.moderation.getConvoMembers"
      ([ ("convoId", convo_id) ]
      @ Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor)
    |> parse_mod_members

  let get_message_context (s : Session.session) ?proxy ~message_id ?convo_id
      ?before ?after ?max_interleaved_system_messages () : message list =
    Client.get_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.moderation.getMessageContext"
      ([ ("messageId", message_id) ]
      @ Client.opt_pair "convoId" convo_id
      @ Client.opt_int "before" before
      @ Client.opt_int "after" after
      @ Client.opt_int "maxInterleavedSystemMessages"
          max_interleaved_system_messages)
    |> fun json -> List.map parse_message (Client.list_member json "messages")

  let update_actor_access (s : Session.session) ?proxy ~actor ~allow_access ?ref
      () : unit =
    ignore
      (Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
         "chat.bsky.moderation.updateActorAccess"
         (Yojson.Safe.to_string
            (update_actor_access_body ~actor ~allow_access ?ref ())))
end
