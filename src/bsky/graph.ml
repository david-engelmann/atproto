open Session
open Cohttp_client
open App
open Actor
open Label

(** [app.bsky.graph] — follows, blocks, mutes, lists, and starter packs. *)
module Graph = struct
  (* Official app.bsky.graph.getFollows / getFollowers `sort` knownValues. *)
  let sort_latest = "latest"
  let sort_top = "top"

  type followers = {
    subject : Actor.short_profile;
    followers : Actor.short_profile list;
    cursor : string option;
  }

  type follows = {
    subject : Actor.short_profile;
    follows : Actor.short_profile list;
    cursor : string option;
  }

  type blocks = { blocks : Actor.block_profile list; cursor : string }
  type mutes = { mutes : Actor.block_profile list; cursor : string }

  let create_graph_endpoint (query_name : string) : string =
    "app.bsky.graph" ^ "." ^ query_name

  let convert_body_to_json (body : string) : Yojson.Safe.t =
    let json = Yojson.Safe.from_string body in
    json

  let string_opt json field =
    match Yojson.Safe.Util.member field json with
    | `String s -> Some s
    | _ -> None

  let parse_followers json : followers =
    let open Yojson.Safe.Util in
    let subject = json |> member "subject" |> Actor.parse_short_profile in
    let followers =
      json |> member "followers" |> to_list
      |> List.map Actor.parse_short_profile
    in
    { subject; followers; cursor = string_opt json "cursor" }

  let parse_follows json : follows =
    let open Yojson.Safe.Util in
    let subject = json |> member "subject" |> Actor.parse_short_profile in
    let follows =
      json |> member "follows" |> to_list |> List.map Actor.parse_short_profile
    in
    { subject; follows; cursor = string_opt json "cursor" }

  (* Official getFollows / getFollowers query params, including `sort`. *)
  let follow_page_pairs ~actor ?limit ?cursor ?sort () =
    (("actor", actor) :: Client.Client.opt_int "limit" limit)
    @ Client.Client.opt_pair "cursor" cursor
    @ Client.Client.opt_pair "sort" sort

  let parse_blocks json : blocks =
    let open Yojson.Safe.Util in
    let blocks =
      json |> member "blocks" |> to_list |> List.map Actor.parse_block_profile
    in
    let cursor =
      match json |> member "cursor" with `String s -> s | _ -> ""
    in
    { blocks; cursor }

  let parse_mutes json : mutes =
    let open Yojson.Safe.Util in
    let mutes =
      json |> member "mutes" |> to_list |> List.map Actor.parse_block_profile
    in
    let cursor =
      match json |> member "cursor" with `String s -> s | _ -> ""
    in
    { mutes; cursor }

  (** Accounts the session blocks via [app.bsky.graph.getBlocks]. *)
  let get_blocks (s : Session.session) (limit : int) : blocks =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let get_blocks_url =
      App.create_endpoint_url base_url (create_graph_endpoint "getBlocks")
    in
    let body =
      Cohttp_client.create_body_from_pairs [ ("limit", string_of_int limit) ]
    in
    let blocks =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers get_blocks_url body
           headers)
    in
    blocks |> convert_body_to_json |> parse_blocks

  (** Followers of [actor] (handle or DID) via [app.bsky.graph.getFollowers]. *)
  let get_followers (s : Session.session) (actor : string) (limit : int) :
      followers =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let get_followers_url =
      App.create_endpoint_url base_url (create_graph_endpoint "getFollowers")
    in
    let body =
      Cohttp_client.create_body_from_pairs
        [ ("actor", actor); ("limit", string_of_int limit) ]
    in
    let followers =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers get_followers_url body
           headers)
    in
    followers |> convert_body_to_json |> parse_followers

  (** Paginated followers of [actor] via [app.bsky.graph.getFollowers].
      Optional [limit] / [cursor] / [sort] map to the lexicon query
      ([sort_latest] / [sort_top]). Works without a session against public
      AppView. *)
  let get_followers_page ?session ?host ~actor ?limit ?cursor ?sort () :
      followers =
    Client.Client.get_json ?session ?host "app.bsky.graph.getFollowers"
      (follow_page_pairs ~actor ?limit ?cursor ?sort ())
    |> parse_followers

  (** Accounts [actor] follows via [app.bsky.graph.getFollows]. *)
  let get_follows (s : Session.session) (actor : string) (limit : int) : follows
      =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let get_follows_url =
      App.create_endpoint_url base_url (create_graph_endpoint "getFollows")
    in
    let body =
      Cohttp_client.create_body_from_pairs
        [ ("actor", actor); ("limit", string_of_int limit) ]
    in
    let follows =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers get_follows_url body
           headers)
    in
    follows |> convert_body_to_json |> parse_follows

  (** Paginated accounts [actor] follows via [app.bsky.graph.getFollows].
      Optional [limit] / [cursor] / [sort] map to the lexicon query
      ([sort_latest] / [sort_top]). Works without a session against public
      AppView. *)
  let get_follows_page ?session ?host ~actor ?limit ?cursor ?sort () : follows =
    Client.Client.get_json ?session ?host "app.bsky.graph.getFollows"
      (follow_page_pairs ~actor ?limit ?cursor ?sort ())
    |> parse_follows

  (** Accounts the session mutes via [app.bsky.graph.getMutes]. *)
  let get_mutes (s : Session.session) (limit : int) : mutes =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let get_mutes_url =
      App.create_endpoint_url base_url (create_graph_endpoint "getMutes")
    in
    let body =
      Cohttp_client.create_body_from_pairs [ ("limit", string_of_int limit) ]
    in
    let mutes =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers get_mutes_url body
           headers)
    in
    mutes |> convert_body_to_json |> parse_mutes

  (* app.bsky.graph.muteActor — optional onlyReposts / onlyQuoteposts replace
     a full mute with a scoped mute. Repeat calls replace the stored scope. *)

  (** JSON body for [app.bsky.graph.muteActor]. Optional [only_reposts] /
      [only_quoteposts] store a scoped mute; later calls replace the stored
      scope. *)
  let mute_actor_body ~actor ?only_reposts ?only_quoteposts () : Yojson.Safe.t =
    let fields =
      ("actor", `String actor)
      ::
      (match only_reposts with
      | Some b -> [ ("onlyReposts", `Bool b) ]
      | None -> [])
      @
      match only_quoteposts with
      | Some b -> [ ("onlyQuoteposts", `Bool b) ]
      | None -> []
    in
    `Assoc fields

  (** Mute [actor] via [app.bsky.graph.muteActor]. Optional [only_reposts] /
      [only_quoteposts] store a scoped mute; later calls replace the stored
      scope. *)
  let mute_actor (s : Session.session) ?only_reposts ?only_quoteposts
      (actor : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let get_muted_actor_url =
      App.create_endpoint_url base_url (create_graph_endpoint "muteActor")
    in
    let data =
      Yojson.Safe.to_string
        (mute_actor_body ~actor ?only_reposts ?only_quoteposts ())
    in
    let muted_actor =
      Lwt_main.run
        (Cohttp_client.post_data_with_headers get_muted_actor_url data headers)
    in
    muted_actor

  (** Unmute [actor] via [app.bsky.graph.unmuteActor]. *)
  let unmute_actor (s : Session.session) (actor : string) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let get_unmuted_actor_url =
      App.create_endpoint_url base_url (create_graph_endpoint "unmuteActor")
    in
    let data = Printf.sprintf "{\"actor\": \"%s\"}" actor in
    let unmuted_actor =
      Lwt_main.run
        (Cohttp_client.post_data_with_headers get_unmuted_actor_url data headers)
    in
    unmuted_actor

  (* ---- lists, starter packs, relationships ----------------------------- *)

  (* app.bsky.graph.defs#listViewerState — APP-2933 adds referenceListOptOut. *)
  type list_viewer = {
    muted : bool option;
    blocked : string option;
    reference_list_opt_out : string option;
  }

  type list_view = {
    uri : string;
    cid : string;
    name : string;
    purpose : string;
    creator_did : string option;
    description : string option;
    list_item_count : int option;
    indexed_at : string;
    viewer : list_viewer option;
    original : Yojson.Safe.t;
  }

  type list_item_subject = {
    did : string;
    handle : string;
    display_name : string option;
  }

  (* app.bsky.graph.defs#listItemView — subjectOptedOut is const true when set. *)
  type list_item = {
    uri : string;
    subject : list_item_subject;
    subject_opted_out : bool option;
  }

  type list_page = {
    cursor : string option;
    list : list_view;
    items : list_item list;
  }

  type lists = { cursor : string option; lists : list_view list }

  type starter_pack_feed = {
    uri : string;
    cid : string;
    display_name : string option;
  }

  type starter_pack = {
    uri : string;
    cid : string;
    name : string option;
    creator_did : string option;
    list : list_view option;
    list_uri : string option;
    list_item_count : int option;
    joined_week_count : int option;
    joined_all_time_count : int option;
    list_items_sample : list_item list;
    feeds : starter_pack_feed list;
    labels : Label.label list;
    indexed_at : string;
    original : Yojson.Safe.t;
  }

  type starter_packs = {
    cursor : string option;
    starter_packs : starter_pack list;
    hits_total : int option;
  }

  type list_with_membership = { list : list_view; list_item : list_item option }

  type lists_with_membership = {
    cursor : string option;
    lists : list_with_membership list;
  }

  type starter_pack_with_membership = {
    starter_pack : starter_pack;
    list_item : list_item option;
  }

  type starter_packs_with_membership = {
    cursor : string option;
    starter_packs : starter_pack_with_membership list;
  }

  (* Official app.bsky.graph.getSuggestedFollowsByActor output, including recIdStr. *)
  type suggested_follows = {
    suggestions : list_item_subject list;
    rec_id_str : string option;
    rec_id : string option;
  }

  type relationship = {
    did : string;
    following : string option;
    followed_by : string option;
    blocking : string option;
    blocked_by : string option;
    blocking_by_list : string option;
    blocked_by_list : string option;
    not_found : bool;
    original : Yojson.Safe.t;
  }

  type relationships = {
    actor : string option;
    relationships : relationship list;
  }

  let parse_list_viewer json : list_viewer =
    {
      muted =
        (match Yojson.Safe.Util.member "muted" json with
        | `Bool b -> Some b
        | _ -> None);
      blocked =
        (match Yojson.Safe.Util.member "blocked" json with
        | `String s -> Some s
        | _ -> None);
      reference_list_opt_out =
        (match Yojson.Safe.Util.member "referenceListOptOut" json with
        | `String s -> Some s
        | _ -> None);
    }

  let parse_list_view json : list_view =
    let creator_did =
      match Yojson.Safe.Util.member "creator" json with
      | `Assoc _ as c -> (
          match Yojson.Safe.Util.member "did" c with
          | `String s -> Some s
          | _ -> None)
      | _ -> None
    in
    {
      uri =
        (match Yojson.Safe.Util.member "uri" json with
        | `String s -> s
        | _ -> "");
      cid =
        (match Yojson.Safe.Util.member "cid" json with
        | `String s -> s
        | _ -> "");
      name =
        (match Yojson.Safe.Util.member "name" json with
        | `String s -> s
        | _ -> "");
      purpose =
        (match Yojson.Safe.Util.member "purpose" json with
        | `String s -> s
        | _ -> "");
      creator_did;
      description =
        (match Yojson.Safe.Util.member "description" json with
        | `String s -> Some s
        | _ -> None);
      list_item_count =
        (match Yojson.Safe.Util.member "listItemCount" json with
        | `Int n -> Some n
        | _ -> None);
      indexed_at =
        (match Yojson.Safe.Util.member "indexedAt" json with
        | `String s -> s
        | _ -> "");
      viewer =
        (match Yojson.Safe.Util.member "viewer" json with
        | `Assoc _ as v -> Some (parse_list_viewer v)
        | _ -> None);
      original = json;
    }

  let parse_list_item json : list_item =
    let subject_json = Yojson.Safe.Util.member "subject" json in
    {
      uri =
        (match Yojson.Safe.Util.member "uri" json with
        | `String s -> s
        | _ -> "");
      subject =
        {
          did =
            (match Yojson.Safe.Util.member "did" subject_json with
            | `String s -> s
            | _ -> "");
          handle =
            (match Yojson.Safe.Util.member "handle" subject_json with
            | `String s -> s
            | _ -> "");
          display_name =
            (match Yojson.Safe.Util.member "displayName" subject_json with
            | `String s -> Some s
            | _ -> None);
        };
      subject_opted_out =
        (match Yojson.Safe.Util.member "subjectOptedOut" json with
        | `Bool b -> Some b
        | _ -> None);
    }

  let parse_list_page json : list_page =
    {
      cursor =
        (match Yojson.Safe.Util.member "cursor" json with
        | `String s -> Some s
        | _ -> None);
      list = parse_list_view (Yojson.Safe.Util.member "list" json);
      items =
        List.map parse_list_item
          (match Yojson.Safe.Util.member "items" json with
          | `List xs -> xs
          | _ -> []);
    }

  let parse_lists json : lists =
    {
      cursor =
        (match Yojson.Safe.Util.member "cursor" json with
        | `String s -> Some s
        | _ -> None);
      lists =
        List.map parse_list_view
          (match Yojson.Safe.Util.member "lists" json with
          | `List xs -> xs
          | _ -> []);
    }

  let parse_starter_pack json : starter_pack =
    let record = Yojson.Safe.Util.member "record" json in
    let creator_did =
      match Yojson.Safe.Util.member "creator" json with
      | `Assoc _ as c -> (
          match Yojson.Safe.Util.member "did" c with
          | `String s -> Some s
          | _ -> None)
      | _ -> None
    in
    let nested_list =
      match Yojson.Safe.Util.member "list" json with
      | `Assoc _ as l -> Some (parse_list_view l)
      | _ -> None
    in
    let list_uri =
      match nested_list with
      | Some l when l.uri <> "" -> Some l.uri
      | _ -> (
          match record with
          | `Assoc _ -> (
              match Yojson.Safe.Util.member "list" record with
              | `String s -> Some s
              | _ -> None)
          | _ -> None)
    in
    {
      uri =
        (match Yojson.Safe.Util.member "uri" json with
        | `String s -> s
        | _ -> "");
      cid =
        (match Yojson.Safe.Util.member "cid" json with
        | `String s -> s
        | _ -> "");
      name =
        (match record with
        | `Assoc _ -> (
            match Yojson.Safe.Util.member "name" record with
            | `String s -> Some s
            | _ -> None)
        | _ -> None);
      creator_did;
      list = nested_list;
      list_uri;
      list_item_count =
        (match Yojson.Safe.Util.member "listItemCount" json with
        | `Int n -> Some n
        | _ -> None);
      joined_week_count =
        (match Yojson.Safe.Util.member "joinedWeekCount" json with
        | `Int n -> Some n
        | _ -> None);
      joined_all_time_count =
        (match Yojson.Safe.Util.member "joinedAllTimeCount" json with
        | `Int n -> Some n
        | _ -> None);
      list_items_sample =
        List.map parse_list_item
          (match Yojson.Safe.Util.member "listItemsSample" json with
          | `List xs -> xs
          | _ -> []);
      feeds =
        List.filter_map
          (function
            | `Assoc _ as f ->
                Some
                  {
                    uri =
                      (match Yojson.Safe.Util.member "uri" f with
                      | `String s -> s
                      | _ -> "");
                    cid =
                      (match Yojson.Safe.Util.member "cid" f with
                      | `String s -> s
                      | _ -> "");
                    display_name =
                      (match Yojson.Safe.Util.member "displayName" f with
                      | `String s -> Some s
                      | _ -> None);
                  }
            | _ -> None)
          (match Yojson.Safe.Util.member "feeds" json with
          | `List xs -> xs
          | _ -> []);
      labels =
        List.filter_map
          (function `Assoc _ as l -> Some (Label.parse_label l) | _ -> None)
          (match Yojson.Safe.Util.member "labels" json with
          | `List xs -> xs
          | _ -> []);
      indexed_at =
        (match Yojson.Safe.Util.member "indexedAt" json with
        | `String s -> s
        | _ -> "");
      original = json;
    }

  let parse_starter_packs json : starter_packs =
    {
      cursor =
        (match Yojson.Safe.Util.member "cursor" json with
        | `String s -> Some s
        | _ -> None);
      starter_packs =
        List.map parse_starter_pack
          (match Yojson.Safe.Util.member "starterPacks" json with
          | `List xs -> xs
          | _ -> []);
      hits_total =
        (match Yojson.Safe.Util.member "hitsTotal" json with
        | `Int n -> Some n
        | `Intlit s -> ( try Some (int_of_string s) with _ -> None)
        | _ -> None);
    }

  let parse_list_item_opt json : list_item option =
    match Yojson.Safe.Util.member "listItem" json with
    | `Assoc _ as item -> Some (parse_list_item item)
    | _ -> None

  let parse_list_with_membership json : list_with_membership =
    {
      list =
        (match Yojson.Safe.Util.member "list" json with
        | `Assoc _ as l -> parse_list_view l
        | _ -> parse_list_view json);
      list_item = parse_list_item_opt json;
    }

  let parse_lists_with_membership json : lists_with_membership =
    {
      cursor =
        (match Yojson.Safe.Util.member "cursor" json with
        | `String s -> Some s
        | _ -> None);
      lists =
        List.map parse_list_with_membership
          (match Yojson.Safe.Util.member "listsWithMembership" json with
          | `List xs -> xs
          | _ -> []);
    }

  let parse_starter_pack_with_membership json : starter_pack_with_membership =
    {
      starter_pack =
        (match Yojson.Safe.Util.member "starterPack" json with
        | `Assoc _ as sp -> parse_starter_pack sp
        | _ -> parse_starter_pack json);
      list_item = parse_list_item_opt json;
    }

  let parse_starter_packs_with_membership json : starter_packs_with_membership =
    {
      cursor =
        (match Yojson.Safe.Util.member "cursor" json with
        | `String s -> Some s
        | _ -> None);
      starter_packs =
        List.map parse_starter_pack_with_membership
          (match Yojson.Safe.Util.member "starterPacksWithMembership" json with
          | `List xs -> xs
          | _ -> []);
    }

  let parse_relationship json : relationship =
    let not_found =
      match Yojson.Safe.Util.member "notFound" json with
      | `Bool b -> b
      | _ -> false
    in
    {
      did =
        (match Yojson.Safe.Util.member "did" json with
        | `String s -> s
        | _ -> (
            match Yojson.Safe.Util.member "actor" json with
            | `String s -> s
            | _ -> ""));
      following =
        (match Yojson.Safe.Util.member "following" json with
        | `String s -> Some s
        | _ -> None);
      followed_by =
        (match Yojson.Safe.Util.member "followedBy" json with
        | `String s -> Some s
        | _ -> None);
      blocking =
        (match Yojson.Safe.Util.member "blocking" json with
        | `String s -> Some s
        | _ -> None);
      blocked_by =
        (match Yojson.Safe.Util.member "blockedBy" json with
        | `String s -> Some s
        | _ -> None);
      blocking_by_list =
        (match Yojson.Safe.Util.member "blockingByList" json with
        | `String s -> Some s
        | _ -> None);
      blocked_by_list =
        (match Yojson.Safe.Util.member "blockedByList" json with
        | `String s -> Some s
        | _ -> None);
      not_found;
      original = json;
    }

  let parse_relationships json : relationships =
    {
      actor =
        (match Yojson.Safe.Util.member "actor" json with
        | `String s -> Some s
        | _ -> None);
      relationships =
        List.map parse_relationship
          (match Yojson.Safe.Util.member "relationships" json with
          | `List xs -> xs
          | _ -> []);
    }

  (** List view and items for [list] (AT URI) via [app.bsky.graph.getList].
      Works without a session against public AppView. *)
  let get_list ?session ?host ~list ?limit ?cursor () : list_page =
    Client.Client.get_json ?session ?host "app.bsky.graph.getList"
      ((("list", list) :: Client.Client.opt_int "limit" limit)
      @ Client.Client.opt_pair "cursor" cursor)
    |> parse_list_page

  (** Lists created by [actor] via [app.bsky.graph.getLists]. *)
  let get_lists ?session ?host ~actor ?limit ?cursor () : lists =
    Client.Client.get_json ?session ?host "app.bsky.graph.getLists"
      ((("actor", actor) :: Client.Client.opt_int "limit" limit)
      @ Client.Client.opt_pair "cursor" cursor)
    |> parse_lists

  (** Lists the session mutes via [app.bsky.graph.getListMutes]. Optional
      [limit] / [cursor] map to the lexicon query. *)
  let get_list_mutes (s : Session.session) ?limit ?cursor () : lists =
    Client.Client.get_json ~session:s "app.bsky.graph.getListMutes"
      (Client.Client.opt_int "limit" limit
      @ Client.Client.opt_pair "cursor" cursor)
    |> parse_lists

  (** Lists the session blocks via [app.bsky.graph.getListBlocks]. Optional
      [limit] / [cursor] map to the lexicon query. *)
  let get_list_blocks (s : Session.session) ?limit ?cursor () : lists =
    Client.Client.get_json ~session:s "app.bsky.graph.getListBlocks"
      (Client.Client.opt_int "limit" limit
      @ Client.Client.opt_pair "cursor" cursor)
    |> parse_lists

  (** Mute list [list] (AT URI) via [app.bsky.graph.muteActorList]. *)
  let mute_actor_list (s : Session.session) ~list () : unit =
    ignore
      (Client.Client.post_json ~session:s "app.bsky.graph.muteActorList"
         (Yojson.Safe.to_string (`Assoc [ ("list", `String list) ])))

  (** Unmute list [list] (AT URI) via [app.bsky.graph.unmuteActorList]. *)
  let unmute_actor_list (s : Session.session) ~list () : unit =
    ignore
      (Client.Client.post_json ~session:s "app.bsky.graph.unmuteActorList"
         (Yojson.Safe.to_string (`Assoc [ ("list", `String list) ])))

  (** Mute the thread rooted at [root] (AT URI) via
      [app.bsky.graph.muteThread]. *)
  let mute_thread (s : Session.session) ~root () : unit =
    ignore
      (Client.Client.post_json ~session:s "app.bsky.graph.muteThread"
         (Yojson.Safe.to_string (`Assoc [ ("root", `String root) ])))

  (** Unmute the thread rooted at [root] (AT URI) via
      [app.bsky.graph.unmuteThread]. *)
  let unmute_thread (s : Session.session) ~root () : unit =
    ignore
      (Client.Client.post_json ~session:s "app.bsky.graph.unmuteThread"
         (Yojson.Safe.to_string (`Assoc [ ("root", `String root) ])))

  (** Starter pack [starter_pack] (AT URI) via [app.bsky.graph.getStarterPack].
      Works without a session against public AppView. *)
  let get_starter_pack ?session ?host ~starter_pack () : starter_pack =
    Client.Client.get_json ?session ?host "app.bsky.graph.getStarterPack"
      [ ("starterPack", starter_pack) ]
    |> fun json ->
    match Yojson.Safe.Util.member "starterPack" json with
    | `Assoc _ as sp -> parse_starter_pack sp
    | _ -> parse_starter_pack json

  (** Starter packs for [uris] via [app.bsky.graph.getStarterPacks]. Works
      without a session against public AppView. *)
  let get_starter_packs ?session ?host ~uris () : starter_pack list =
    Client.Client.get_json ?session ?host "app.bsky.graph.getStarterPacks"
      (Client.Client.repeat_param "uris" uris)
    |> parse_starter_packs
    |> fun (p : starter_packs) -> p.starter_packs

  (** Starter packs created by [actor] via
      [app.bsky.graph.getActorStarterPacks]. Optional [limit] / [cursor]
      map to the lexicon query. Works without a session against public
      AppView. *)
  let get_actor_starter_packs ?session ?host ~actor ?limit ?cursor () :
      starter_packs =
    Client.Client.get_json ?session ?host "app.bsky.graph.getActorStarterPacks"
      ((("actor", actor) :: Client.Client.opt_int "limit" limit)
      @ Client.Client.opt_pair "cursor" cursor)
    |> parse_starter_packs

  (** Search starter packs for [q] via [app.bsky.graph.searchStarterPacks].
      Optional [limit] / [cursor] map to the lexicon query. Works without a
      session against public AppView. *)
  let search_starter_packs ?session ?host ~q ?limit ?cursor () : starter_packs =
    Client.Client.get_json ?session ?host "app.bsky.graph.searchStarterPacks"
      ((("q", q) :: Client.Client.opt_int "limit" limit)
      @ Client.Client.opt_pair "cursor" cursor)
    |> parse_starter_packs

  (** Search starter packs for [q] via [app.bsky.graph.searchStarterPacksV2].
      Optional [limit] / [cursor] map to the lexicon query. Works without a
      session against public AppView. *)
  let search_starter_packs_v2 ?session ?host ~q ?limit ?cursor () :
      starter_packs =
    Client.Client.get_json ?session ?host "app.bsky.graph.searchStarterPacksV2"
      ((("q", q) :: Client.Client.opt_int "limit" limit)
      @ Client.Client.opt_pair "cursor" cursor)
    |> parse_starter_packs

  (** [actor]'s lists plus the session's membership via
      [app.bsky.graph.getListsWithMembership]. Optional [limit] / [cursor]
      / [purposes] map to the lexicon query. *)
  let get_lists_with_membership (s : Session.session) ~actor ?limit ?cursor
      ?(purposes = []) () : lists_with_membership =
    Client.Client.get_json ~session:s "app.bsky.graph.getListsWithMembership"
      ((("actor", actor) :: Client.Client.opt_int "limit" limit)
      @ Client.Client.opt_pair "cursor" cursor
      @ Client.Client.repeat_param "purposes" purposes)
    |> parse_lists_with_membership

  (** [actor]'s starter packs plus the session's membership via
      [app.bsky.graph.getStarterPacksWithMembership]. Optional [limit] /
      [cursor] map to the lexicon query. *)
  let get_starter_packs_with_membership (s : Session.session) ~actor ?limit
      ?cursor () : starter_packs_with_membership =
    Client.Client.get_json ~session:s
      "app.bsky.graph.getStarterPacksWithMembership"
      ((("actor", actor) :: Client.Client.opt_int "limit" limit)
      @ Client.Client.opt_pair "cursor" cursor)
    |> parse_starter_packs_with_membership

  (** Follow/block relationships for [actor] vs [others] via
      [app.bsky.graph.getRelationships]. *)
  let get_relationships ?session ?host ~actor ?others () : relationships =
    Client.Client.get_json ?session ?host "app.bsky.graph.getRelationships"
      (("actor", actor)
      :: Client.Client.repeat_param "others" (Option.value others ~default:[]))
    |> parse_relationships

  (** Followers of [actor] that the session also follows via
      [app.bsky.graph.getKnownFollowers]. Optional [limit] / [cursor] map
      to the lexicon query. *)
  let get_known_followers ?session ?host ~actor ?limit ?cursor () : followers =
    Client.Client.get_json ?session ?host "app.bsky.graph.getKnownFollowers"
      ((("actor", actor) :: Client.Client.opt_int "limit" limit)
      @ Client.Client.opt_pair "cursor" cursor)
    |> parse_followers

  let parse_suggested_follow_subject json : list_item_subject =
    {
      did =
        (match Yojson.Safe.Util.member "did" json with
        | `String s -> s
        | _ -> "");
      handle =
        (match Yojson.Safe.Util.member "handle" json with
        | `String s -> s
        | _ -> "");
      display_name =
        (match Yojson.Safe.Util.member "displayName" json with
        | `String s -> Some s
        | _ -> None);
    }

  let parse_suggested_follows json : suggested_follows =
    let items =
      match Yojson.Safe.Util.member "suggestions" json with
      | `List xs -> xs
      | _ -> (
          match Yojson.Safe.Util.member "actors" json with
          | `List xs -> xs
          | _ -> [])
    in
    {
      suggestions = List.map parse_suggested_follow_subject items;
      rec_id_str = Client.Client.string_opt json "recIdStr";
      rec_id =
        (match Yojson.Safe.Util.member "recId" json with
        | `String s -> Some s
        | `Int n -> Some (string_of_int n)
        | _ -> None);
    }

  (** Suggested follows for [actor] via
      [app.bsky.graph.getSuggestedFollowsByActor]. Works without a session
      against public AppView. *)
  let get_suggested_follows_by_actor ?session ?host ~actor () :
      suggested_follows =
    Client.Client.get_json ?session ?host
      "app.bsky.graph.getSuggestedFollowsByActor"
      [ ("actor", actor) ]
    |> parse_suggested_follows
end
