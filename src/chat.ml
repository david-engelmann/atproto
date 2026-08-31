open Session
open Client
open Xrpc
open Facet
open Embed
open Cid
open Firehose
open Dag_cbor
open Websocket

(** chat.bsky.convo — DMs. Requests must include atproto-proxy for the chat service. *)
module Chat = struct
  let default_proxy : Xrpc.proxy =
    { did = "did:web:api.bsky.chat"; service = "bsky_chat" }

  let proxy_headers ?(proxy = default_proxy) () = [ Xrpc.proxy_header proxy ]

  let ends_with suffix s =
    let n = String.length s and m = String.length suffix in
    n >= m && String.sub s (n - m) m = suffix

  type member_kind = [ `Direct | `Group | `Past | `Unknown of string ]

  (* chat.bsky.actor.defs#profileViewBasic + #groupConvoMember extras. *)
  type member = {
    did : string;
    handle : string option;
    display_name : string option;
    chat_disabled : bool option;
    role : string option;
    added_by : member option;
    kind : member_kind option;
  }

  let empty_member =
    {
      did = "";
      handle = None;
      display_name = None;
      chat_disabled = None;
      role = None;
      added_by = None;
      kind = None;
    }

  type reaction = { value : string; sender_did : string; created_at : string }

  (* chat.bsky.convo.defs#systemMessageReferredUser *)
  type referred_user = { did : string }

  type system_message_data =
    [ `Add_member of referred_user * string * referred_user
    | `Remove_member of referred_user * referred_user
    | `Member_join of referred_user * string * referred_user option
    | `Member_leave of referred_user
    | `Lock of referred_user
    | `Unlock of referred_user
    | `Lock_permanently of referred_user
    | `Edit_group of string option * string option
    | `Create_join_link
    | `Edit_join_link
    | `Enable_join_link
    | `Disable_join_link
    | `Unknown of Yojson.Safe.t ]

  type message = {
    id : string;
    rev : string;
    text : string;
    sender_did : string option;
    sent_at : string;
    deleted : bool;
    is_system : bool;
    system : system_message_data option;
    facets : Facet.facet list;
    reactions : reaction list;
    embed : Embed.embed option;
    reply_to_id : string option;
    original : Yojson.Safe.t;
  }

  type last_reaction = { message : message; reaction : reaction }

  (* chat.bsky.group.defs#joinLinkView — also used on #groupConvo.joinLink. *)
  type join_link = {
    code : string;
    enabled_status : string;
    require_approval : bool;
    join_rule : string;
    created_at : string;
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
    last_reaction : last_reaction option;
    unread_join_request_count : int option;
    lock_status : string option;
    lock_status_moderation_override : bool option;
    member_count : int option;
    member_limit : int option;
    join_request_count : int option;
    created_at : string option;
    join_link : join_link option;
    group_name : string option;
    original : Yojson.Safe.t;
  }

  type convos = { cursor : string option; convos : convo list }

  type messages = {
    cursor : string option;
    messages : message list;
    related_profiles : member list;
  }

  let rec parse_member json : member =
    let kind_json =
      match Yojson.Safe.Util.member "kind" json with
      | `Assoc _ as k -> Some k
      | _ -> None
    in
    let kind_src = match kind_json with Some k -> k | None -> json in
    let kind =
      match kind_json with
      | None -> None
      | Some k ->
          let ty = Option.value ~default:"" (Client.string_opt k "$type") in
          if ends_with "pastGroupConvoMember" ty then Some `Past
          else if ends_with "groupConvoMember" ty then Some `Group
          else if ends_with "directConvoMember" ty then Some `Direct
          else if ty = "" then None
          else Some (`Unknown ty)
    in
    {
      did = Client.string_member json "did";
      handle = Client.string_opt json "handle";
      display_name = Client.string_opt json "displayName";
      chat_disabled = Client.bool_opt json "chatDisabled";
      role = Client.string_opt kind_src "role";
      added_by =
        (match Yojson.Safe.Util.member "addedBy" kind_src with
        | `Assoc _ as m -> Some (parse_member m)
        | _ -> None);
      kind;
    }

  let parse_join_link json : join_link =
    {
      code = Client.string_member json "code";
      enabled_status = Client.string_member json "enabledStatus";
      require_approval = Client.bool_member json "requireApproval";
      join_rule = Client.string_member json "joinRule";
      created_at = Client.string_member json "createdAt";
      original = json;
    }

  let parse_referred_user json : referred_user =
    { did = Client.string_member json "did" }

  let referred_opt json field =
    match Yojson.Safe.Util.member field json with
    | `Assoc _ as u -> Some (parse_referred_user u)
    | _ -> None

  let referred_req json field =
    match Yojson.Safe.Util.member field json with
    | `Assoc _ as u -> parse_referred_user u
    | _ -> { did = Client.string_member json field }

  let parse_system_data json : system_message_data =
    let ty = Option.value ~default:"" (Client.string_opt json "$type") in
    if ends_with "systemMessageDataAddMember" ty then
      `Add_member
        ( referred_req json "member",
          Client.string_member json "role",
          referred_req json "addedBy" )
    else if ends_with "systemMessageDataRemoveMember" ty then
      `Remove_member (referred_req json "member", referred_req json "removedBy")
    else if ends_with "systemMessageDataMemberJoin" ty then
      `Member_join
        ( referred_req json "member",
          Client.string_member json "role",
          referred_opt json "approvedBy" )
    else if ends_with "systemMessageDataMemberLeave" ty then
      `Member_leave (referred_req json "member")
    else if ends_with "systemMessageDataLockConvoPermanently" ty then
      `Lock_permanently (referred_req json "lockedBy")
    else if ends_with "systemMessageDataLockConvo" ty then
      `Lock (referred_req json "lockedBy")
    else if ends_with "systemMessageDataUnlockConvo" ty then
      `Unlock (referred_req json "unlockedBy")
    else if ends_with "systemMessageDataEditGroup" ty then
      `Edit_group
        (Client.string_opt json "oldName", Client.string_opt json "newName")
    else if ends_with "systemMessageDataCreateJoinLink" ty then `Create_join_link
    else if ends_with "systemMessageDataEditJoinLink" ty then `Edit_join_link
    else if ends_with "systemMessageDataEnableJoinLink" ty then
      `Enable_join_link
    else if ends_with "systemMessageDataDisableJoinLink" ty then
      `Disable_join_link
    else `Unknown json

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
    let is_system =
      match ty with
      | Some t -> ends_with "systemMessageView" t
      | None -> (
          match Yojson.Safe.Util.member "data" json with
          | `Assoc _ -> true
          | _ -> false)
    in
    let deleted =
      (not is_system)
      &&
      match ty with
      | Some t -> ends_with "deletedMessageView" t
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
      is_system;
      system =
        (match Yojson.Safe.Util.member "data" json with
        | `Assoc _ as d -> Some (parse_system_data d)
        | _ -> None);
      facets = parse_message_facets json;
      reactions = List.map parse_reaction (Client.list_member json "reactions");
      embed = Embed.parse_embed_option json;
      reply_to_id = parse_reply_to_id json;
      original = json;
    }

  let parse_last_reaction json : last_reaction option =
    match json with
    | `Assoc _ -> (
        let message = Yojson.Safe.Util.member "message" json in
        let reaction = Yojson.Safe.Util.member "reaction" json in
        match (message, reaction) with
        | `Assoc _, `Assoc _ ->
            Some
              {
                message = parse_message message;
                reaction = parse_reaction reaction;
              }
        | _ -> None)
    | _ -> None

  let parse_group_kind json =
    match Yojson.Safe.Util.member "kind" json with
    | `Assoc _ as k -> k
    | _ -> `Null

  let parse_convo json : convo =
    let kind = parse_group_kind json in
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
      last_reaction =
        parse_last_reaction (Yojson.Safe.Util.member "lastReaction" json);
      unread_join_request_count =
        (match kind with
        | `Assoc _ -> Client.int_opt kind "unreadJoinRequestCount"
        | _ -> Client.int_opt json "unreadJoinRequestCount");
      lock_status =
        (match kind with
        | `Assoc _ -> Client.string_opt kind "lockStatus"
        | _ -> Client.string_opt json "lockStatus");
      lock_status_moderation_override =
        (match kind with
        | `Assoc _ -> Client.bool_opt kind "lockStatusModerationOverride"
        | _ -> Client.bool_opt json "lockStatusModerationOverride");
      member_count =
        (match kind with
        | `Assoc _ -> Client.int_opt kind "memberCount"
        | _ -> Client.int_opt json "memberCount");
      member_limit =
        (match kind with
        | `Assoc _ -> Client.int_opt kind "memberLimit"
        | _ -> Client.int_opt json "memberLimit");
      join_request_count =
        (match kind with
        | `Assoc _ -> Client.int_opt kind "joinRequestCount"
        | _ -> Client.int_opt json "joinRequestCount");
      created_at =
        (match kind with
        | `Assoc _ -> Client.string_opt kind "createdAt"
        | _ -> Client.string_opt json "createdAt");
      join_link =
        (let src = match kind with `Assoc _ -> kind | _ -> json in
         match Yojson.Safe.Util.member "joinLink" src with
         | `Assoc _ as j -> Some (parse_join_link j)
         | _ -> None);
      group_name =
        (match kind with
        | `Assoc _ -> Client.string_opt kind "name"
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
      related_profiles =
        List.map parse_member (Client.list_member json "relatedProfiles");
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
    message : message option;
    related_profiles : member list;
    member : member option;
    reaction : reaction option;
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
      message =
        (match Yojson.Safe.Util.member "message" json with
        | `Assoc _ as m -> Some (parse_message m)
        | _ -> None);
      related_profiles =
        List.map parse_member (Client.list_member json "relatedProfiles");
      member =
        (match Yojson.Safe.Util.member "member" json with
        | `Assoc _ as m -> Some (parse_member m)
        | _ -> None);
      reaction =
        (match Yojson.Safe.Util.member "reaction" json with
        | `Assoc _ as r -> Some (parse_reaction r)
        | _ -> None);
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

  (* chat.bsky.group.defs#joinRequestConvoView — requester's view in listConvoRequests. *)
  type join_request_convo = {
    convo_id : string;
    name : string;
    owner : member;
    member_count : int;
    member_limit : int;
    requested_at : string option;
    original : Yojson.Safe.t;
  }

  type convo_request =
    [ `Convo of convo
    | `Join_request of join_request_convo
    | `Unknown of Yojson.Safe.t ]

  type convo_requests = { cursor : string option; requests : convo_request list }

  let parse_join_request_convo json : join_request_convo =
    {
      convo_id = Client.string_member json "convoId";
      name = Client.string_member json "name";
      owner =
        (match Yojson.Safe.Util.member "owner" json with
        | `Assoc _ as o -> parse_member o
        | _ -> empty_member);
      member_count = Client.int_member json "memberCount";
      member_limit = Client.int_member json "memberLimit";
      requested_at =
        (match Yojson.Safe.Util.member "viewer" json with
        | `Assoc _ as v -> Client.string_opt v "requestedAt"
        | _ -> None);
      original = json;
    }

  let parse_convo_request json : convo_request =
    let ty = Option.value ~default:"" (Client.string_opt json "$type") in
    if
      ends_with "joinRequestConvoView" ty
      || (Client.string_member json "convoId" <> ""
         && Client.string_member json "id" = "")
    then `Join_request (parse_join_request_convo json)
    else if Client.string_member json "id" <> "" || ends_with "convoView" ty then
      `Convo (parse_convo json)
    else `Unknown json

  let parse_convo_requests json : convo_requests =
    {
      cursor = Client.string_opt json "cursor";
      requests =
        List.map parse_convo_request
          (match Yojson.Safe.Util.member "requests" json with
          | `List xs -> xs
          | _ -> Client.list_member json "convos");
    }

  let parse_requests json : convos =
    {
      cursor = Client.string_opt json "cursor";
      convos =
        List.filter_map
          (function `Convo c -> Some c | _ -> None)
          (parse_convo_requests json).requests;
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
      convo_requests =
    Client.get_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.convo.listConvoRequests"
      (Client.opt_int "limit" limit @ Client.opt_pair "cursor" cursor)
    |> parse_convo_requests

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

  type members_page = { cursor : string option; members : member list }

  type join_request = {
    convo_id : string;
    requested_by : member;
    requested_at : string;
  }

  type join_requests = { cursor : string option; requests : join_request list }

  type join_link_preview = {
    convo_id : string;
    code : string;
    name : string;
    owner : member;
    member_count : int;
    member_limit : int;
    require_approval : bool;
    join_rule : string;
    requested_at : string option;
    convo : convo option;
  }

  type join_preview =
    [ `Preview of join_link_preview
    | `Disabled of string
    | `Invalid of string
    | `Unknown of Yojson.Safe.t ]

  type request_join_result = { status : string; convo : convo option }

  let parse_members_page json : members_page =
    {
      cursor = Client.string_opt json "cursor";
      members = List.map parse_member (Client.list_member json "members");
    }

  let unwrap_join_link json : join_link =
    match Yojson.Safe.Util.member "joinLink" json with
    | `Assoc _ as j -> parse_join_link j
    | _ -> parse_join_link json

  let parse_join_request json : join_request =
    {
      convo_id = Client.string_member json "convoId";
      requested_by =
        (match Yojson.Safe.Util.member "requestedBy" json with
        | `Assoc _ as m -> parse_member m
        | _ -> { empty_member with did = Client.string_member json "requestedBy" });
      requested_at = Client.string_member json "requestedAt";
    }

  let parse_join_requests json : join_requests =
    {
      cursor = Client.string_opt json "cursor";
      requests =
        List.map parse_join_request (Client.list_member json "requests");
    }

  let parse_join_preview json : join_preview =
    let ty = Option.value ~default:"" (Client.string_opt json "$type") in
    let ends_with suffix =
      let n = String.length ty and m = String.length suffix in
      n >= m && String.sub ty (n - m) m = suffix
    in
    if ends_with "disabledJoinLinkPreviewView" then
      `Disabled (Client.string_member json "code")
    else if ends_with "invalidJoinLinkPreviewView" then
      `Invalid (Client.string_member json "code")
    else if
      ends_with "joinLinkPreviewView"
      || Client.string_member json "convoId" <> ""
    then
      `Preview
        {
          convo_id = Client.string_member json "convoId";
          code = Client.string_member json "code";
          name = Client.string_member json "name";
          owner =
            (match Yojson.Safe.Util.member "owner" json with
            | `Assoc _ as o -> parse_member o
            | _ -> empty_member);
          member_count = Client.int_member json "memberCount";
          member_limit = Client.int_member json "memberLimit";
          require_approval = Client.bool_member json "requireApproval";
          join_rule = Client.string_member json "joinRule";
          requested_at =
            (match Yojson.Safe.Util.member "viewer" json with
            | `Assoc _ as v -> Client.string_opt v "requestedAt"
            | _ -> None);
          convo =
            (match Yojson.Safe.Util.member "convo" json with
            | `Assoc _ as c -> Some (parse_convo c)
            | _ -> None);
        }
    else `Unknown json

  let get_convo_members (s : Session.session) ?proxy ~convo_id ?limit ?cursor ()
      : members_page =
    Client.get_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.convo.getConvoMembers"
      ((("convoId", convo_id) :: Client.opt_int "limit" limit)
      @ Client.opt_pair "cursor" cursor)
    |> parse_members_page

  let create_group (s : Session.session) ?proxy ~members ~name () : convo =
    Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.group.createGroup"
      (Yojson.Safe.to_string
         (`Assoc
           [
             ("members", `List (List.map (fun d -> `String d) members));
             ("name", `String name);
           ]))
    |> unwrap_convo

  let create_join_link (s : Session.session) ?proxy ~convo_id ~join_rule
      ?(require_approval = false) () : join_link =
    Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.group.createJoinLink"
      (Yojson.Safe.to_string
         (`Assoc
           [
             ("convoId", `String convo_id);
             ("joinRule", `String join_rule);
             ("requireApproval", `Bool require_approval);
           ]))
    |> unwrap_join_link

  let edit_join_link (s : Session.session) ?proxy ~convo_id ?join_rule
      ?require_approval () : join_link =
    let fields =
      ("convoId", `String convo_id)
      ::
      (match join_rule with
      | Some r -> [ ("joinRule", `String r) ]
      | None -> [])
      @
      match require_approval with
      | Some b -> [ ("requireApproval", `Bool b) ]
      | None -> []
    in
    Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.group.editJoinLink"
      (Yojson.Safe.to_string (`Assoc fields))
    |> unwrap_join_link

  let enable_join_link (s : Session.session) ?proxy ~convo_id () : join_link =
    Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.group.enableJoinLink"
      (Yojson.Safe.to_string (convo_id_body convo_id))
    |> unwrap_join_link

  let disable_join_link (s : Session.session) ?proxy ~convo_id () : join_link =
    Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.group.disableJoinLink"
      (Yojson.Safe.to_string (convo_id_body convo_id))
    |> unwrap_join_link

  let get_join_link_previews ?session ?proxy ?host ~codes () : join_preview list
      =
    Client.get_json ?session ?host
      ~extra:
        (match session with Some _ -> proxy_headers ?proxy () | None -> [])
      "chat.bsky.group.getJoinLinkPreviews"
      (Client.repeat_param "codes" codes)
    |> fun json ->
    List.map parse_join_preview (Client.list_member json "joinLinkPreviews")

  let list_join_requests (s : Session.session) ?proxy ~convo_id ?limit ?cursor
      () : join_requests =
    Client.get_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.group.listJoinRequests"
      ((("convoId", convo_id) :: Client.opt_int "limit" limit)
      @ Client.opt_pair "cursor" cursor)
    |> parse_join_requests

  let list_mutual_groups (s : Session.session) ?proxy ~subject ?limit ?cursor ()
      : convos =
    Client.get_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.group.listMutualGroups"
      ((("subject", subject) :: Client.opt_int "limit" limit)
      @ Client.opt_pair "cursor" cursor)
    |> parse_convos

  let approve_join_request (s : Session.session) ?proxy ~convo_id ~member () :
      convo =
    Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
      "chat.bsky.group.approveJoinRequest"
      (Yojson.Safe.to_string
         (`Assoc [ ("convoId", `String convo_id); ("member", `String member) ]))
    |> unwrap_convo

  let reject_join_request (s : Session.session) ?proxy ~convo_id ~member () :
      unit =
    ignore
      (Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
         "chat.bsky.group.rejectJoinRequest"
         (Yojson.Safe.to_string
            (`Assoc
              [ ("convoId", `String convo_id); ("member", `String member) ])))

  let request_join (s : Session.session) ?proxy ~code () : request_join_result =
    let json =
      Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
        "chat.bsky.group.requestJoin"
        (Yojson.Safe.to_string (`Assoc [ ("code", `String code) ]))
    in
    {
      status = Client.string_member json "status";
      convo =
        (match Yojson.Safe.Util.member "convo" json with
        | `Assoc _ as c -> Some (parse_convo c)
        | _ -> None);
    }

  let withdraw_join_request (s : Session.session) ?proxy ~convo_id () : unit =
    ignore
      (Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
         "chat.bsky.group.withdrawJoinRequest"
         (Yojson.Safe.to_string (convo_id_body convo_id)))

  let update_join_requests_read (s : Session.session) ?proxy ~convo_id () : unit
      =
    ignore
      (Client.post_json ~session:s ~extra:(proxy_headers ?proxy ())
         "chat.bsky.group.updateJoinRequestsRead"
         (Yojson.Safe.to_string (convo_id_body convo_id)))

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

  (* chat.bsky.moderation.subscribeModEvents — private operator firehose. *)

  type mod_event_convo_first_message = {
    convo_id : string;
    created_at : string;
    rev : string;
    user : string;
    recipients : string list;
    message_id : string option;
  }

  type mod_event_group_created = {
    actor_did : string;
    convo_created_at : string;
    convo_id : string;
    created_at : string;
    group_member_count : int;
    group_name : string;
    initial_member_dids : string list;
    owner_did : string;
    rev : string;
  }

  type mod_event_member_added = {
    actor_did : string;
    convo_created_at : string;
    convo_id : string;
    created_at : string;
    group_member_count : int;
    group_name : string;
    owner_did : string;
    request_members_count : int;
    rev : string;
    subject_did : string;
    subject_follows_owner : bool;
  }

  type mod_event_member_joined = {
    actor_did : string;
    convo_created_at : string;
    convo_id : string;
    created_at : string;
    group_member_count : int;
    group_name : string;
    join_link_code : string;
    owner_did : string;
    rev : string;
    subject_follows_owner : bool;
  }

  type mod_event_join_request = mod_event_member_joined

  type mod_event_join_decision = {
    actor_did : string;
    convo_created_at : string;
    convo_id : string;
    created_at : string;
    group_member_count : int;
    group_name : string;
    owner_did : string;
    rev : string;
    subject_did : string;
  }

  type mod_event_chat_accepted = {
    actor_did : string;
    convo_created_at : string;
    convo_id : string;
    created_at : string;
    method_ : string;
    rev : string;
    group_member_count : int option;
    group_name : string option;
    owner_did : string option;
  }

  type mod_event_member_left = {
    actor_did : string;
    convo_created_at : string;
    convo_id : string;
    created_at : string;
    group_member_count : int;
    group_name : string;
    leave_method : string;
    owner_did : string;
    rev : string;
    subject_did : string;
  }

  type mod_event_group_updated = {
    actor_did : string;
    convo_created_at : string;
    convo_id : string;
    created_at : string;
    group_member_count : int;
    group_name : string;
    owner_did : string;
    rev : string;
    update_type : string;
    join_link_code : string option;
    join_link_followers_only : bool option;
    join_link_requires_approval : bool option;
    lock_reason : string option;
    new_name : string option;
    old_name : string option;
  }

  type mod_event_rate_limit = {
    actor_did : string;
    created_at : string;
    endpoint : string;
    rev : string;
  }

  type mod_event =
    [ `Convo_first_message of mod_event_convo_first_message
    | `Group_chat_created of mod_event_group_created
    | `Group_chat_member_added of mod_event_member_added
    | `Group_chat_member_joined of mod_event_member_joined
    | `Group_chat_join_request of mod_event_join_request
    | `Group_chat_join_request_approved of mod_event_join_decision
    | `Group_chat_join_request_rejected of mod_event_join_decision
    | `Chat_accepted of mod_event_chat_accepted
    | `Group_chat_member_left of mod_event_member_left
    | `Group_chat_updated of mod_event_group_updated
    | `Rate_limit_exceeded of mod_event_rate_limit
    | `Error of string * string option
    | `Unknown of string * Yojson.Safe.t ]

  let default_mod_events_host = "api.bsky.chat"

  let subscribe_mod_events_url ?(host = default_mod_events_host) ?cursor () =
    let base =
      Printf.sprintf "wss://%s/xrpc/chat.bsky.moderation.subscribeModEvents"
        host
    in
    match cursor with
    | None -> base
    | Some c -> base ^ "?cursor=" ^ Uri.pct_encode c

  let string_list json field =
    List.filter_map
      (function `String s -> Some s | _ -> None)
      (Client.list_member json field)

  let ends_with_event suffix ty =
    let n = String.length ty and m = String.length suffix in
    n >= m && String.sub ty (n - m) m = suffix

  let parse_convo_first_message json : mod_event_convo_first_message =
    {
      convo_id = Client.string_member json "convoId";
      created_at = Client.string_member json "createdAt";
      rev = Client.string_member json "rev";
      user = Client.string_member json "user";
      recipients = string_list json "recipients";
      message_id = Client.string_opt json "messageId";
    }

  let parse_group_created json : mod_event_group_created =
    {
      actor_did = Client.string_member json "actorDid";
      convo_created_at = Client.string_member json "convoCreatedAt";
      convo_id = Client.string_member json "convoId";
      created_at = Client.string_member json "createdAt";
      group_member_count = Client.int_member json "groupMemberCount";
      group_name = Client.string_member json "groupName";
      initial_member_dids = string_list json "initialMemberDids";
      owner_did = Client.string_member json "ownerDid";
      rev = Client.string_member json "rev";
    }

  let parse_member_added json : mod_event_member_added =
    {
      actor_did = Client.string_member json "actorDid";
      convo_created_at = Client.string_member json "convoCreatedAt";
      convo_id = Client.string_member json "convoId";
      created_at = Client.string_member json "createdAt";
      group_member_count = Client.int_member json "groupMemberCount";
      group_name = Client.string_member json "groupName";
      owner_did = Client.string_member json "ownerDid";
      request_members_count = Client.int_member json "requestMembersCount";
      rev = Client.string_member json "rev";
      subject_did = Client.string_member json "subjectDid";
      subject_follows_owner = Client.bool_member json "subjectFollowsOwner";
    }

  let parse_member_joined json : mod_event_member_joined =
    {
      actor_did = Client.string_member json "actorDid";
      convo_created_at = Client.string_member json "convoCreatedAt";
      convo_id = Client.string_member json "convoId";
      created_at = Client.string_member json "createdAt";
      group_member_count = Client.int_member json "groupMemberCount";
      group_name = Client.string_member json "groupName";
      join_link_code = Client.string_member json "joinLinkCode";
      owner_did = Client.string_member json "ownerDid";
      rev = Client.string_member json "rev";
      subject_follows_owner = Client.bool_member json "subjectFollowsOwner";
    }

  let parse_join_decision json : mod_event_join_decision =
    {
      actor_did = Client.string_member json "actorDid";
      convo_created_at = Client.string_member json "convoCreatedAt";
      convo_id = Client.string_member json "convoId";
      created_at = Client.string_member json "createdAt";
      group_member_count = Client.int_member json "groupMemberCount";
      group_name = Client.string_member json "groupName";
      owner_did = Client.string_member json "ownerDid";
      rev = Client.string_member json "rev";
      subject_did = Client.string_member json "subjectDid";
    }

  let parse_chat_accepted json : mod_event_chat_accepted =
    {
      actor_did = Client.string_member json "actorDid";
      convo_created_at = Client.string_member json "convoCreatedAt";
      convo_id = Client.string_member json "convoId";
      created_at = Client.string_member json "createdAt";
      method_ = Client.string_member json "method";
      rev = Client.string_member json "rev";
      group_member_count = Client.int_opt json "groupMemberCount";
      group_name = Client.string_opt json "groupName";
      owner_did = Client.string_opt json "ownerDid";
    }

  let parse_member_left json : mod_event_member_left =
    {
      actor_did = Client.string_member json "actorDid";
      convo_created_at = Client.string_member json "convoCreatedAt";
      convo_id = Client.string_member json "convoId";
      created_at = Client.string_member json "createdAt";
      group_member_count = Client.int_member json "groupMemberCount";
      group_name = Client.string_member json "groupName";
      leave_method = Client.string_member json "leaveMethod";
      owner_did = Client.string_member json "ownerDid";
      rev = Client.string_member json "rev";
      subject_did = Client.string_member json "subjectDid";
    }

  let parse_group_updated json : mod_event_group_updated =
    {
      actor_did = Client.string_member json "actorDid";
      convo_created_at = Client.string_member json "convoCreatedAt";
      convo_id = Client.string_member json "convoId";
      created_at = Client.string_member json "createdAt";
      group_member_count = Client.int_member json "groupMemberCount";
      group_name = Client.string_member json "groupName";
      owner_did = Client.string_member json "ownerDid";
      rev = Client.string_member json "rev";
      update_type = Client.string_member json "updateType";
      join_link_code = Client.string_opt json "joinLinkCode";
      join_link_followers_only = Client.bool_opt json "joinLinkFollowersOnly";
      join_link_requires_approval =
        Client.bool_opt json "joinLinkRequiresApproval";
      lock_reason = Client.string_opt json "lockReason";
      new_name = Client.string_opt json "newName";
      old_name = Client.string_opt json "oldName";
    }

  let parse_rate_limit json : mod_event_rate_limit =
    {
      actor_did = Client.string_member json "actorDid";
      created_at = Client.string_member json "createdAt";
      endpoint = Client.string_member json "endpoint";
      rev = Client.string_member json "rev";
    }

  let parse_mod_event_type ~type_ json : mod_event =
    if ends_with_event "eventConvoFirstMessage" type_ then
      `Convo_first_message (parse_convo_first_message json)
    else if ends_with_event "eventGroupChatCreated" type_ then
      `Group_chat_created (parse_group_created json)
    else if ends_with_event "eventGroupChatMemberAdded" type_ then
      `Group_chat_member_added (parse_member_added json)
    else if ends_with_event "eventGroupChatMemberJoined" type_ then
      `Group_chat_member_joined (parse_member_joined json)
    else if ends_with_event "eventGroupChatJoinRequestApproved" type_ then
      `Group_chat_join_request_approved (parse_join_decision json)
    else if ends_with_event "eventGroupChatJoinRequestRejected" type_ then
      `Group_chat_join_request_rejected (parse_join_decision json)
    else if ends_with_event "eventGroupChatJoinRequest" type_ then
      `Group_chat_join_request (parse_member_joined json)
    else if ends_with_event "eventChatAccepted" type_ then
      `Chat_accepted (parse_chat_accepted json)
    else if ends_with_event "eventGroupChatMemberLeft" type_ then
      `Group_chat_member_left (parse_member_left json)
    else if ends_with_event "eventGroupChatUpdated" type_ then
      `Group_chat_updated (parse_group_updated json)
    else if ends_with_event "eventRateLimitExceeded" type_ then
      `Rate_limit_exceeded (parse_rate_limit json)
    else `Unknown (type_, json)

  let parse_mod_event json : mod_event =
    let type_ =
      match Client.string_opt json "$type" with
      | Some s -> s
      | None -> Option.value ~default:"" (Client.string_opt json "t")
    in
    parse_mod_event_type ~type_ json

  let rec cbor_to_json (v : Dag_cbor.value) : Yojson.Safe.t =
    match v with
    | Dag_cbor.Null -> `Null
    | Dag_cbor.Bool b -> `Bool b
    | Dag_cbor.Int n -> `Int n
    | Dag_cbor.Int64 n -> `Intlit (Int64.to_string n)
    | Dag_cbor.Text s -> `String s
    | Dag_cbor.Bytes b -> `String b
    | Dag_cbor.Array xs -> `List (List.map cbor_to_json xs)
    | Dag_cbor.Map fields ->
        `Assoc (List.map (fun (k, x) -> (k, cbor_to_json x)) fields)
    | Dag_cbor.Tag (_, inner) -> cbor_to_json inner
    | Dag_cbor.Cid c -> `String (Cid.to_string c)

  let decode_mod_event_frame (bytes : string) : Firehose.header * mod_event =
    match Dag_cbor.decode_sequence bytes with
    | header_v :: body :: _ ->
        let header = Firehose.parse_header header_v in
        let json = cbor_to_json body in
        let event =
          if header.op = -1 then
            let err =
              match Yojson.Safe.Util.member "error" json with
              | `String s -> s
              | _ -> "error"
            in
            let msg =
              match Yojson.Safe.Util.member "message" json with
              | `String s -> Some s
              | _ -> None
            in
            `Error (err, msg)
          else
            let type_ =
              match header.t with
              | Some t ->
                  if String.length t > 0 && t.[0] = '#' then
                    String.sub t 1 (String.length t - 1)
                  else t
              | None -> ""
            in
            parse_mod_event_type ~type_ json
        in
        (header, event)
    | _ ->
        failwith
          "Chat.decode_mod_event_frame: expected header and body CBOR values"

  let subscribe_mod_events ?session ?proxy ?(host = default_mod_events_host)
      ?cursor ?max_messages f =
    let extra =
      proxy_headers ?proxy ()
      @
      match session with
      | Some s -> [ Session.bearer_token_from_session s ]
      | None -> []
    in
    let url = subscribe_mod_events_url ~host ?cursor () in
    Websocket.with_connection ~extra_headers:extra url (fun ws ->
        let rec loop n =
          match max_messages with
          | Some m when n >= m -> ()
          | _ -> (
              match Websocket.recv_message ws with
              | Websocket.Binary payload | Websocket.Text payload ->
                  f (decode_mod_event_frame payload);
                  loop (n + 1)
              | Websocket.Close _ -> ()
              | Websocket.Ping _ | Websocket.Pong _ -> loop n)
        in
        loop 0)
end
