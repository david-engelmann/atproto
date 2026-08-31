open Session
open Client
open Xrpc

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

  type message = {
    id : string;
    rev : string;
    text : string;
    sender_did : string option;
    sent_at : string;
    deleted : bool;
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

  let message_input ?facets ?reply_to text : Yojson.Safe.t =
    let fields =
      [ ("text", `String text) ]
      @ (match facets with Some f -> [ ("facets", f) ] | None -> [])
      @
      match reply_to with
      | Some id -> [ ("replyTo", `Assoc [ ("messageId", `String id) ]) ]
      | None -> []
    in
    `Assoc fields

  let send_message_body ~convo_id ~text ?facets ?reply_to () : Yojson.Safe.t =
    `Assoc
      [
        ("convoId", `String convo_id);
        ("message", message_input ?facets ?reply_to text);
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

  let send_message (s : Session.session) ?proxy ~convo_id ~text ?facets
      ?reply_to () : message =
    Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.convo.sendMessage"
      (Yojson.Safe.to_string
         (send_message_body ~convo_id ~text ?facets ?reply_to ()))
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
    facets : Yojson.Safe.t option;
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
                         message_input ?facets:i.facets ?reply_to:i.reply_to
                           i.text );
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

  let message_input_with_facets ?facets ?reply_to text : Yojson.Safe.t =
    let facets_json =
      match facets with
      | Some (`List _ as f) -> Some f
      | Some f -> Some f
      | None -> None
    in
    message_input ?facets:facets_json ?reply_to text
end
