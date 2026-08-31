open Session
open Cohttp_client
open App

module Actor = struct
  type viewer_status = {
    muted : bool;
    blocked_by : bool;
    following : string option;
    followed_by : string option;
  }

  type profile = {
    did : string;
    handle : string;
    display_name : string option;
    description : string option;
    avatar : string option;
    banner : string option;
    follows_count : int;
    followers_count : int;
    posts_count : int;
    indexed_at : string;
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

  let parse_viewer_status json : viewer_status =
    let open Yojson.Safe.Util in
    match json with
    | `Null ->
        {
          muted = false;
          blocked_by = false;
          following = None;
          followed_by = None;
        }
    | _ ->
        let muted =
          match json |> member "muted" with `Bool b -> b | _ -> false
        in
        let blocked_by =
          match json |> member "blockedBy" with `Bool b -> b | _ -> false
        in
        let following = extract_string_option json "following" in
        let followed_by = extract_string_option json "followedBy" in
        { muted; blocked_by; following; followed_by }

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
      | `Null ->
          {
            muted = false;
            blocked_by = false;
            following = None;
            followed_by = None;
          }
      | v -> parse_viewer_status v
    in
    let labels = Label.Label.parse_label_values (json |> member "labels") in
    {
      did;
      handle;
      display_name;
      description;
      avatar;
      banner;
      follows_count;
      followers_count;
      posts_count;
      indexed_at;
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
  type interests_pref = { tags : string list }

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
      `Interests { tags = string_list json "tags" }
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

  let get_preferences (s : Session.session) : preferences =
    Client.Client.get_json ~session:s "app.bsky.actor.getPreferences" []
    |> parse_preferences

  let put_preferences (s : Session.session) preferences : unit =
    ignore
      (Client.Client.post_json ~session:s "app.bsky.actor.putPreferences"
         (Yojson.Safe.to_string (put_preferences_body preferences)))
end
