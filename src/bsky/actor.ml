open Session
open Cohttp_client
open App

(** [app.bsky.actor] — profiles, search, suggestions, and preferences. *)
module Actor = struct
  (* app.bsky.actor.defs#knownFollowers — subject's followers the viewer also follows. *)
  type known_follower = {
    did : string;
    handle : string;
    display_name : string option;
  }

  type known_followers = { count : int; followers : known_follower list }

  (* app.bsky.notification.defs#activitySubscription on viewerState. *)
  type viewer_activity_subscription = { post : bool; reply : bool }
  type list_ref = { uri : string; cid : string option; name : string option }

  (* app.bsky.actor.defs#viewerState — includes scoped mutes (onlyReposts / onlyQuoteposts). *)
  type viewer_status = {
    muted : bool;
    muted_only_reposts : bool option;
    muted_only_quoteposts : bool option;
    muted_by_list : list_ref option;
    blocked_by : bool;
    blocking : string option;
    blocking_by_list : list_ref option;
    following : string option;
    followed_by : string option;
    known_followers : known_followers option;
    activity_subscription : viewer_activity_subscription option;
  }

  type profile_associated_chat = {
    allow_incoming : string;
    allow_group_invites : string option;
  }

  (* app.bsky.actor.defs#profileAssociatedGerm — Germ Network button on a profile. *)
  type profile_associated_germ = {
    show_button_to : string;
    message_me_url : string;
  }

  type profile_associated_activity = { allow_subscriptions : string }

  type profile_associated = {
    lists : int option;
    feedgens : int option;
    starter_packs : int option;
    labeler : bool option;
    chat : profile_associated_chat option;
    activity_subscription : profile_associated_activity option;
    germ : profile_associated_germ option;
  }

  type verification_view = {
    issuer : string;
    issuer_display_name : string option;
    issuer_handle : string option;
    uri : string;
    is_valid : bool;
    created_at : string;
  }

  type verification_state = {
    verifications : verification_view list;
    verified_status : string;
    trusted_verifier_status : string;
  }

  type status_view = {
    uri : string option;
    cid : string option;
    status : string;
    expires_at : string option;
    is_active : bool option;
    is_disabled : bool option;
  }

  type profile = {
    did : string;
    handle : string;
    display_name : string option;
    description : string option;
    pronouns : string option;
    website : string option;
    avatar : string option;
    banner : string option;
    follows_count : int;
    followers_count : int;
    posts_count : int;
    indexed_at : string;
    created_at : string option;
    pinned_post_uri : string option;
    joined_via_starter_pack_uri : string option;
    associated : profile_associated option;
    verification : verification_state option;
    status : status_view option;
    viewer : viewer_status;
    labels : string list option;
  }

  type short_profile = {
    did : string;
    handle : string;
    display_name : string option;
    description : string option;
    avatar : string option;
    indexed_at : string;
    viewer : viewer_status;
    labels : string list option;
  }

  type short_profile_without_description = {
    did : string;
    handle : string;
    display_name : string option;
    avatar : string option;
    indexed_at : string;
    viewer : viewer_status;
    labels : string list option;
  }

  type typeahead_profile = {
    did : string;
    handle : string;
    display_name : string option;
    avatar : string option;
    viewer : viewer_status;
    labels : string list option;
  }

  type block_profile = {
    did : string;
    handle : string;
    viewer : viewer_status;
    labels : string list option;
  }

  let extract_string_option json field : string option =
    let open Yojson.Safe.Util in
    try Some (to_string (member field json)) with Type_error _ -> None

  let extract_bool_option json field : bool option =
    match Yojson.Safe.Util.member field json with
    | `Bool b -> Some b
    | _ -> None

  let extract_int_option json field : int option =
    match Yojson.Safe.Util.member field json with
    | `Int n -> Some n
    | `Intlit s -> ( try Some (int_of_string s) with _ -> None)
    | _ -> None

  let empty_viewer_status : viewer_status =
    {
      muted = false;
      muted_only_reposts = None;
      muted_only_quoteposts = None;
      muted_by_list = None;
      blocked_by = false;
      blocking = None;
      blocking_by_list = None;
      following = None;
      followed_by = None;
      known_followers = None;
      activity_subscription = None;
    }

  let parse_list_ref json : list_ref option =
    match json with
    | `Assoc _ -> (
        match extract_string_option json "uri" with
        | None -> None
        | Some uri ->
            Some
              {
                uri;
                cid = extract_string_option json "cid";
                name = extract_string_option json "name";
              })
    | _ -> None

  let parse_known_follower json : known_follower =
    {
      did =
        (match extract_string_option json "did" with Some s -> s | None -> "");
      handle =
        (match extract_string_option json "handle" with
        | Some s -> s
        | None -> "");
      display_name = extract_string_option json "displayName";
    }

  let parse_known_followers_opt json : known_followers option =
    match json with
    | `Assoc _ ->
        Some
          {
            count =
              (match extract_int_option json "count" with
              | Some n -> n
              | None -> 0);
            followers =
              (match Yojson.Safe.Util.member "followers" json with
              | `List xs -> List.map parse_known_follower xs
              | _ -> []);
          }
    | _ -> None

  let parse_viewer_activity_subscription json :
      viewer_activity_subscription option =
    match json with
    | `Assoc _ ->
        Some
          {
            post =
              (match extract_bool_option json "post" with
              | Some b -> b
              | None -> false);
            reply =
              (match extract_bool_option json "reply" with
              | Some b -> b
              | None -> false);
          }
    | _ -> None

  let parse_viewer_status json : viewer_status =
    let open Yojson.Safe.Util in
    match json with
    | `Null -> empty_viewer_status
    | _ ->
        let muted =
          match json |> member "muted" with `Bool b -> b | _ -> false
        in
        let blocked_by =
          match json |> member "blockedBy" with `Bool b -> b | _ -> false
        in
        {
          muted;
          muted_only_reposts = extract_bool_option json "mutedOnlyReposts";
          muted_only_quoteposts = extract_bool_option json "mutedOnlyQuoteposts";
          muted_by_list = parse_list_ref (json |> member "mutedByList");
          blocked_by;
          blocking = extract_string_option json "blocking";
          blocking_by_list = parse_list_ref (json |> member "blockingByList");
          following = extract_string_option json "following";
          followed_by = extract_string_option json "followedBy";
          known_followers =
            parse_known_followers_opt (json |> member "knownFollowers");
          activity_subscription =
            parse_viewer_activity_subscription
              (json |> member "activitySubscription");
        }

  let parse_associated_chat json : profile_associated_chat option =
    match extract_string_option json "allowIncoming" with
    | None -> None
    | Some allow_incoming ->
        Some
          {
            allow_incoming;
            allow_group_invites = extract_string_option json "allowGroupInvites";
          }

  let parse_associated_germ json : profile_associated_germ option =
    match
      ( extract_string_option json "showButtonTo",
        extract_string_option json "messageMeUrl" )
    with
    | Some show_button_to, Some message_me_url ->
        Some { show_button_to; message_me_url }
    | _ -> None

  let parse_associated json : profile_associated option =
    match json with
    | `Assoc _ ->
        Some
          {
            lists = extract_int_option json "lists";
            feedgens = extract_int_option json "feedgens";
            starter_packs = extract_int_option json "starterPacks";
            labeler = extract_bool_option json "labeler";
            chat =
              (match Yojson.Safe.Util.member "chat" json with
              | `Assoc _ as c -> parse_associated_chat c
              | _ -> None);
            activity_subscription =
              (match Yojson.Safe.Util.member "activitySubscription" json with
              | `Assoc _ as a -> (
                  match extract_string_option a "allowSubscriptions" with
                  | Some s -> Some { allow_subscriptions = s }
                  | None -> None)
              | _ -> None);
            germ =
              (match Yojson.Safe.Util.member "germ" json with
              | `Assoc _ as g -> parse_associated_germ g
              | _ -> None);
          }
    | _ -> None

  let parse_verification_view json : verification_view =
    {
      issuer =
        (match extract_string_option json "issuer" with
        | Some s -> s
        | None -> "");
      issuer_display_name = extract_string_option json "issuerDisplayName";
      issuer_handle = extract_string_option json "issuerHandle";
      uri =
        (match extract_string_option json "uri" with Some s -> s | None -> "");
      is_valid =
        (match extract_bool_option json "isValid" with
        | Some b -> b
        | None -> false);
      created_at =
        (match extract_string_option json "createdAt" with
        | Some s -> s
        | None -> "");
    }

  let parse_verification_state json : verification_state option =
    match json with
    | `Assoc _ ->
        Some
          {
            verifications =
              (match Yojson.Safe.Util.member "verifications" json with
              | `List xs -> List.map parse_verification_view xs
              | _ -> []);
            verified_status =
              (match extract_string_option json "verifiedStatus" with
              | Some s -> s
              | None -> "");
            trusted_verifier_status =
              (match extract_string_option json "trustedVerifierStatus" with
              | Some s -> s
              | None -> "");
          }
    | _ -> None

  let parse_status_view json : status_view option =
    match extract_string_option json "status" with
    | None -> None
    | Some status ->
        Some
          {
            uri = extract_string_option json "uri";
            cid = extract_string_option json "cid";
            status;
            expires_at = extract_string_option json "expiresAt";
            is_active = extract_bool_option json "isActive";
            is_disabled = extract_bool_option json "isDisabled";
          }

  let parse_profile json : profile =
    let open Yojson.Safe.Util in
    let did = json |> member "did" |> to_string in
    let handle = json |> member "handle" |> to_string in
    let display_name = extract_string_option json "displayName" in
    let description = extract_string_option json "description" in
    let avatar = extract_string_option json "avatar" in
    let banner = extract_string_option json "banner" in
    let follows_count =
      match json |> member "followsCount" with `Int n -> n | _ -> 0
    in
    let followers_count =
      match json |> member "followersCount" with `Int n -> n | _ -> 0
    in
    let posts_count =
      match json |> member "postsCount" with `Int n -> n | _ -> 0
    in
    let indexed_at =
      match json |> member "indexedAt" with `String s -> s | _ -> ""
    in
    let viewer =
      match json |> member "viewer" with
      | `Null -> empty_viewer_status
      | v -> parse_viewer_status v
    in
    let labels = Label.Label.parse_label_values (json |> member "labels") in
    let pinned_post_uri =
      match json |> member "pinnedPost" with
      | `Assoc _ as p -> extract_string_option p "uri"
      | _ -> None
    in
    let joined_via_starter_pack_uri =
      match json |> member "joinedViaStarterPack" with
      | `Assoc _ as p -> extract_string_option p "uri"
      | `String s -> Some s
      | _ -> None
    in
    {
      did;
      handle;
      display_name;
      description;
      pronouns = extract_string_option json "pronouns";
      website = extract_string_option json "website";
      avatar;
      banner;
      follows_count;
      followers_count;
      posts_count;
      indexed_at;
      created_at = extract_string_option json "createdAt";
      pinned_post_uri;
      joined_via_starter_pack_uri;
      associated = parse_associated (json |> member "associated");
      verification = parse_verification_state (json |> member "verification");
      status = parse_status_view (json |> member "status");
      viewer;
      labels;
    }

  let parse_short_profile_without_description json :
      short_profile_without_description =
    let open Yojson.Safe.Util in
    let did = json |> member "did" |> to_string in
    let handle = json |> member "handle" |> to_string in
    let display_name = extract_string_option json "displayName" in
    let avatar = extract_string_option json "avatar" in
    let indexed_at =
      match json |> member "indexedAt" with `String s -> s | _ -> ""
    in
    let viewer = json |> member "viewer" |> parse_viewer_status in
    let labels = Label.Label.parse_label_values (json |> member "labels") in
    { did; handle; display_name; avatar; indexed_at; viewer; labels }

  let parse_short_profile json : short_profile =
    let open Yojson.Safe.Util in
    let did = json |> member "did" |> to_string in
    let handle = json |> member "handle" |> to_string in
    let display_name = extract_string_option json "displayName" in
    let description = extract_string_option json "description" in
    let avatar = extract_string_option json "avatar" in
    let indexed_at =
      match json |> member "indexedAt" with `String s -> s | _ -> ""
    in
    let viewer = json |> member "viewer" |> parse_viewer_status in
    let labels = Label.Label.parse_label_values (json |> member "labels") in
    {
      did;
      handle;
      display_name;
      description;
      avatar;
      indexed_at;
      viewer;
      labels;
    }

  let parse_typeahead_profile json : typeahead_profile =
    let open Yojson.Safe.Util in
    let did = json |> member "did" |> to_string in
    let handle = json |> member "handle" |> to_string in
    let display_name = extract_string_option json "displayName" in
    let avatar = extract_string_option json "avatar" in
    let viewer = json |> member "viewer" |> parse_viewer_status in
    let labels = Label.Label.parse_label_values (json |> member "labels") in
    { did; handle; display_name; avatar; viewer; labels }

  let parse_block_profile json : block_profile =
    let open Yojson.Safe.Util in
    let did = json |> member "did" |> to_string in
    let handle = json |> member "handle" |> to_string in
    let viewer = json |> member "viewer" |> parse_viewer_status in
    let labels = Label.Label.parse_label_values (json |> member "labels") in
    { did; handle; viewer; labels }

  let parse_profiles json : profile list =
    let open Yojson.Safe.Util in
    let profiles = json |> member "profiles" |> to_list in
    List.map parse_profile profiles

  let parse_short_profiles json : short_profile list =
    let open Yojson.Safe.Util in
    let profiles = json |> member "actors" |> to_list in
    List.map parse_short_profile profiles

  let parse_short_profile_without_descriptions json :
      short_profile_without_description list =
    let open Yojson.Safe.Util in
    let profiles = json |> member "actors" |> to_list in
    List.map parse_short_profile_without_description profiles

  let parse_typeahead_profiles json : typeahead_profile list =
    let open Yojson.Safe.Util in
    let profiles = json |> member "actors" |> to_list in
    List.map parse_typeahead_profile profiles

  let convert_body_to_json (body : string) : Yojson.Safe.t =
    let json = Yojson.Safe.from_string body in
    json

  let create_actor_endpoint (query_name : string) : string =
    "app.bsky.actor" ^ "." ^ query_name

  (** Profile view for [actor] (handle or DID) via
      [app.bsky.actor.getProfile]. *)
  let get_profile (s : Session.session) (actor : string) : profile =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let get_profile_url =
      App.create_endpoint_url base_url (create_actor_endpoint "getProfile")
    in
    let body = Cohttp_client.create_body_from_pairs [ ("actor", actor) ] in
    let profile =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers get_profile_url body
           headers)
    in
    let profile_json = profile |> convert_body_to_json in
    profile_json |> parse_profile

  (** Profile views for several actors via [app.bsky.actor.getProfiles]. *)
  let get_profiles (s : Session.session) (actors : string list) : profile list =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let get_profiles_url =
      App.create_endpoint_url base_url (create_actor_endpoint "getProfiles")
    in
    let body = Cohttp_client.add_query_params "actors" actors in
    let profiles =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers get_profiles_url body
           headers)
    in
    let profiles_json = profiles |> convert_body_to_json in
    profiles_json |> parse_profiles

  (** Suggested accounts via [app.bsky.actor.getSuggestions]. *)
  let get_suggestions (s : Session.session) (limit : int) : short_profile list =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let get_suggestions_url =
      App.create_endpoint_url base_url (create_actor_endpoint "getSuggestions")
    in
    let body =
      Cohttp_client.create_body_from_pairs [ ("limit", string_of_int limit) ]
    in
    let suggestions =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers get_suggestions_url
           body headers)
    in
    suggestions |> convert_body_to_json |> parse_short_profiles

  (** Actor search via [app.bsky.actor.searchActors] ([q] = [term]). *)
  let search_actors (s : Session.session) (term : string) (limit : int) :
      short_profile list =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let search_actors_url =
      App.create_endpoint_url base_url (create_actor_endpoint "searchActors")
    in
    let body =
      Cohttp_client.create_body_from_pairs
        [ ("q", term); ("limit", string_of_int limit) ]
    in
    let profiles =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers search_actors_url body
           headers)
    in
    profiles |> convert_body_to_json |> parse_short_profiles

  (** Typeahead via [app.bsky.actor.searchActorsTypeahead]. *)
  let search_actors_typeahead (s : Session.session) (term : string)
      (limit : int) : typeahead_profile list =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers =
      Cohttp_client.create_headers_from_pairs [ application_json; bearer_token ]
    in
    let base_url = App.create_base_url s in
    let search_actors_typeahead_url =
      App.create_endpoint_url base_url
        (create_actor_endpoint "searchActorsTypeahead")
    in
    let body =
      Cohttp_client.create_body_from_pairs
        [ ("q", term); ("limit", string_of_int limit) ]
    in
    let profiles =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers
           search_actors_typeahead_url body headers)
    in
    profiles |> convert_body_to_json |> parse_typeahead_profiles

  type saved_feed = {
    id : string;
    type_ : string;
    value : string;
    pinned : bool;
  }

  type adult_content_pref = { enabled : bool }

  type content_label_pref = {
    label : string;
    visibility : string;
    labeler_did : string option;
  }

  type saved_feeds_v2_pref = { items : saved_feed list }

  type saved_feeds_pref = {
    pinned : string list;
    saved : string list;
    timeline_index : int option;
  }

  type personal_details_pref = { birth_date : string option }

  type declared_age_pref = {
    is_over_age_13 : bool option;
    is_over_age_16 : bool option;
    is_over_age_18 : bool option;
  }

  type feed_view_pref = {
    feed : string;
    hide_replies : bool option;
    hide_replies_by_unfollowed : bool option;
    hide_replies_by_like_count : int option;
    hide_reposts : bool option;
    hide_quote_posts : bool option;
  }

  type thread_view_pref = { sort : string option }
  type interests_pref = { tags : string list; updated_at : string option }

  type muted_word = {
    id : string option;
    value : string;
    targets : string list;
    actor_target : string option;
    expires_at : string option;
  }

  type muted_words_pref = { items : muted_word list }
  type hidden_posts_pref = { items : string list }
  type labeler_pref_item = { did : string }
  type labelers_pref = { labelers : labeler_pref_item list }

  type nux = {
    id : string;
    completed : bool;
    data : string option;
    expires_at : string option;
  }

  type bsky_app_state_pref = {
    active_progress_guide : string option;
    is_beta_user : bool option;
    queued_nudges : string list;
    nuxs : nux list;
  }

  type threadgate_rule =
    [ `Mention
    | `Follower
    | `Following
    | `List of string
    | `Unknown of Yojson.Safe.t ]

  type postgate_rule = [ `Disable | `Unknown of Yojson.Safe.t ]

  type post_interaction_pref = {
    threadgate_allow_rules : threadgate_rule list option;
    postgate_embedding_rules : postgate_rule list option;
  }

  type verification_pref = { hide_badges : bool }

  type live_event_pref = {
    hidden_feed_ids : string list;
    hide_all_feeds : bool;
  }

  type preference_kind =
    [ `Adult_content of adult_content_pref
    | `Content_label of content_label_pref
    | `Saved_feeds of saved_feeds_pref
    | `Saved_feeds_v2 of saved_feeds_v2_pref
    | `Personal_details of personal_details_pref
    | `Declared_age of declared_age_pref
    | `Feed_view of feed_view_pref
    | `Thread_view of thread_view_pref
    | `Interests of interests_pref
    | `Muted_words of muted_words_pref
    | `Hidden_posts of hidden_posts_pref
    | `Bsky_app_state of bsky_app_state_pref
    | `Labelers of labelers_pref
    | `Post_interaction of post_interaction_pref
    | `Verification of verification_pref
    | `Live_event of live_event_pref
    | `Other ]

  type preference = {
    type_ : string;
    kind : preference_kind;
    original : Yojson.Safe.t;
  }

  type preferences = { preferences : preference list }

  let ends_with suffix s =
    let n = String.length s and m = String.length suffix in
    n >= m && String.sub s (n - m) m = suffix

  let parse_saved_feed json : saved_feed =
    {
      id = Client.Client.string_member json "id";
      type_ = Client.Client.string_member json "type";
      value = Client.Client.string_member json "value";
      pinned = Client.Client.bool_member json "pinned";
    }

  let string_list json field =
    List.filter_map
      (function `String s -> Some s | _ -> None)
      (Client.Client.list_member json field)

  let parse_muted_word json : muted_word =
    {
      id = Client.Client.string_opt json "id";
      value = Client.Client.string_member json "value";
      targets = string_list json "targets";
      actor_target = Client.Client.string_opt json "actorTarget";
      expires_at = Client.Client.string_opt json "expiresAt";
    }

  let parse_nux json : nux =
    {
      id = Client.Client.string_member json "id";
      completed = Client.Client.bool_member json "completed";
      data = Client.Client.string_opt json "data";
      expires_at = Client.Client.string_opt json "expiresAt";
    }

  let parse_threadgate_rule json : threadgate_rule =
    let ty =
      match Yojson.Safe.Util.member "$type" json with `String s -> s | _ -> ""
    in
    if ends_with "mentionRule" ty then `Mention
    else if ends_with "followerRule" ty then `Follower
    else if ends_with "followingRule" ty then `Following
    else if ends_with "listRule" ty then
      `List (Client.Client.string_member json "list")
    else `Unknown json

  let parse_postgate_rule json : postgate_rule =
    let ty =
      match Yojson.Safe.Util.member "$type" json with `String s -> s | _ -> ""
    in
    if ends_with "disableRule" ty then `Disable else `Unknown json

  let parse_preference_kind ~type_ json : preference_kind =
    if ends_with "adultContentPref" type_ then
      `Adult_content { enabled = Client.Client.bool_member json "enabled" }
    else if ends_with "contentLabelPref" type_ then
      `Content_label
        {
          label = Client.Client.string_member json "label";
          visibility = Client.Client.string_member json "visibility";
          labeler_did = Client.Client.string_opt json "labelerDid";
        }
    else if ends_with "savedFeedsPrefV2" type_ then
      `Saved_feeds_v2
        {
          items =
            List.map parse_saved_feed (Client.Client.list_member json "items");
        }
    else if ends_with "savedFeedsPref" type_ then
      `Saved_feeds
        {
          pinned = string_list json "pinned";
          saved = string_list json "saved";
          timeline_index = Client.Client.int_opt json "timelineIndex";
        }
    else if ends_with "personalDetailsPref" type_ then
      `Personal_details
        { birth_date = Client.Client.string_opt json "birthDate" }
    else if ends_with "declaredAgePref" type_ then
      `Declared_age
        {
          is_over_age_13 = Client.Client.bool_opt json "isOverAge13";
          is_over_age_16 = Client.Client.bool_opt json "isOverAge16";
          is_over_age_18 = Client.Client.bool_opt json "isOverAge18";
        }
    else if ends_with "feedViewPref" type_ then
      `Feed_view
        {
          feed = Client.Client.string_member json "feed";
          hide_replies = Client.Client.bool_opt json "hideReplies";
          hide_replies_by_unfollowed =
            Client.Client.bool_opt json "hideRepliesByUnfollowed";
          hide_replies_by_like_count =
            Client.Client.int_opt json "hideRepliesByLikeCount";
          hide_reposts = Client.Client.bool_opt json "hideReposts";
          hide_quote_posts = Client.Client.bool_opt json "hideQuotePosts";
        }
    else if ends_with "threadViewPref" type_ then
      `Thread_view { sort = Client.Client.string_opt json "sort" }
    else if ends_with "interestsPref" type_ then
      `Interests
        {
          tags = string_list json "tags";
          updated_at = Client.Client.string_opt json "updatedAt";
        }
    else if ends_with "mutedWordsPref" type_ then
      `Muted_words
        {
          items =
            List.map parse_muted_word (Client.Client.list_member json "items");
        }
    else if ends_with "hiddenPostsPref" type_ then
      `Hidden_posts { items = string_list json "items" }
    else if ends_with "bskyAppStatePref" type_ then
      `Bsky_app_state
        {
          active_progress_guide =
            (match Yojson.Safe.Util.member "activeProgressGuide" json with
            | `Assoc _ as g -> Client.Client.string_opt g "guide"
            | `String s -> Some s
            | _ -> None);
          is_beta_user = Client.Client.bool_opt json "isBetaUser";
          queued_nudges = string_list json "queuedNudges";
          nuxs = List.map parse_nux (Client.Client.list_member json "nuxs");
        }
    else if ends_with "labelersPref" type_ then
      `Labelers
        {
          labelers =
            List.map
              (fun item -> { did = Client.Client.string_member item "did" })
              (Client.Client.list_member json "labelers");
        }
    else if ends_with "postInteractionSettingsPref" type_ then
      `Post_interaction
        {
          threadgate_allow_rules =
            (match Yojson.Safe.Util.member "threadgateAllowRules" json with
            | `List xs -> Some (List.map parse_threadgate_rule xs)
            | _ -> None);
          postgate_embedding_rules =
            (match Yojson.Safe.Util.member "postgateEmbeddingRules" json with
            | `List xs -> Some (List.map parse_postgate_rule xs)
            | _ -> None);
        }
    else if ends_with "verificationPrefs" type_ then
      `Verification
        { hide_badges = Client.Client.bool_member json "hideBadges" }
    else if ends_with "liveEventPreferences" type_ then
      `Live_event
        {
          hidden_feed_ids = string_list json "hiddenFeedIds";
          hide_all_feeds = Client.Client.bool_member json "hideAllFeeds";
        }
    else `Other

  let parse_preference json : preference =
    let type_ =
      match Yojson.Safe.Util.member "$type" json with `String s -> s | _ -> ""
    in
    { type_; kind = parse_preference_kind ~type_ json; original = json }

  let parse_preferences json : preferences =
    {
      preferences =
        List.map parse_preference
          (match Yojson.Safe.Util.member "preferences" json with
          | `List xs -> xs
          | _ -> []);
    }

  let put_preferences_body preferences : Yojson.Safe.t =
    `Assoc [ ("preferences", `List preferences) ]

  (** The session's stored preferences ([app.bsky.actor.getPreferences]). *)
  let get_preferences (s : Session.session) : preferences =
    Client.Client.get_json ~session:s "app.bsky.actor.getPreferences" []
    |> parse_preferences

  (** Replace stored preferences via [app.bsky.actor.putPreferences]. *)
  let put_preferences (s : Session.session) preferences : unit =
    ignore
      (Client.Client.post_json ~session:s "app.bsky.actor.putPreferences"
         (Yojson.Safe.to_string (put_preferences_body preferences)))
end
