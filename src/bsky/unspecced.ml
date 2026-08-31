open Session
open Client
open Actor
open Graph

(** app.bsky.unspecced — public search skeletons, popular feeds, trends. *)
module Unspecced = struct
  type skeleton_post = { uri : string }
  type skeleton_actor = { did : string }
  type skeleton_starter_pack = { uri : string }

  type skeleton_posts = {
    cursor : string option;
    hits_total : int option;
    posts : skeleton_post list;
  }

  type skeleton_actors = {
    cursor : string option;
    hits_total : int option;
    actors : skeleton_actor list;
  }

  type skeleton_starter_packs = {
    cursor : string option;
    hits_total : int option;
    starter_packs : skeleton_starter_pack list;
  }

  type trending_topic = {
    topic : string;
    display_name : string option;
    description : string option;
    link : string;
  }

  type trending_topics = {
    topics : trending_topic list;
    suggested : trending_topic list;
  }

  type live_now = { did : string; domains : string list }

  type config = {
    check_email_confirmed : bool option;
    live_now : live_now list;
  }

  type generator_view = {
    uri : string;
    cid : string;
    did : string;
    display_name : string;
    description : string option;
    creator_did : string option;
    like_count : int option;
    indexed_at : string;
  }

  type generators = { cursor : string option; feeds : generator_view list }

  let parse_skeleton_post json : skeleton_post =
    { uri = Client.string_member json "uri" }

  let parse_skeleton_actor json : skeleton_actor =
    { did = Client.string_member json "did" }

  let parse_skeleton_starter_pack json : skeleton_starter_pack =
    { uri = Client.string_member json "uri" }

  let parse_skeleton_posts json : skeleton_posts =
    {
      cursor = Client.string_opt json "cursor";
      hits_total = Client.int_opt json "hitsTotal";
      posts = List.map parse_skeleton_post (Client.list_member json "posts");
    }

  let parse_skeleton_actors json : skeleton_actors =
    {
      cursor = Client.string_opt json "cursor";
      hits_total = Client.int_opt json "hitsTotal";
      actors = List.map parse_skeleton_actor (Client.list_member json "actors");
    }

  let parse_skeleton_starter_packs json : skeleton_starter_packs =
    {
      cursor = Client.string_opt json "cursor";
      hits_total = Client.int_opt json "hitsTotal";
      starter_packs =
        List.map parse_skeleton_starter_pack
          (Client.list_member json "starterPacks");
    }

  let parse_trending_topic json : trending_topic =
    {
      topic = Client.string_member json "topic";
      display_name = Client.string_opt json "displayName";
      description = Client.string_opt json "description";
      link = Client.string_member json "link";
    }

  let parse_trending_topics json : trending_topics =
    {
      topics = List.map parse_trending_topic (Client.list_member json "topics");
      suggested =
        List.map parse_trending_topic (Client.list_member json "suggested");
    }

  let parse_live_now json : live_now =
    {
      did = Client.string_member json "did";
      domains =
        List.filter_map
          (function `String s -> Some s | _ -> None)
          (Client.list_member json "domains");
    }

  let parse_config json : config =
    {
      check_email_confirmed = Client.bool_opt json "checkEmailConfirmed";
      live_now = List.map parse_live_now (Client.list_member json "liveNow");
    }

  let parse_generator_view json : generator_view =
    let creator_did =
      match Yojson.Safe.Util.member "creator" json with
      | `Assoc _ as c -> Client.string_opt c "did"
      | _ -> None
    in
    {
      uri = Client.string_member json "uri";
      cid = Client.string_member json "cid";
      did = Client.string_member json "did";
      display_name = Client.string_member json "displayName";
      description = Client.string_opt json "description";
      creator_did;
      like_count = Client.int_opt json "likeCount";
      indexed_at = Client.string_member json "indexedAt";
    }

  let parse_generators json : generators =
    {
      cursor = Client.string_opt json "cursor";
      feeds = List.map parse_generator_view (Client.list_member json "feeds");
    }

  let search_posts_skeleton ?session ?host ~q ?sort ?since ?until ?mentions
      ?author ?lang ?domain ?url ?viewer ?limit ?cursor () : skeleton_posts =
    Client.get_json ?session ?host "app.bsky.unspecced.searchPostsSkeleton"
      ([ ("q", q) ]
      @ Client.opt_pair "sort" sort
      @ Client.opt_pair "since" since
      @ Client.opt_pair "until" until
      @ Client.opt_pair "mentions" mentions
      @ Client.opt_pair "author" author
      @ Client.opt_pair "lang" lang
      @ Client.opt_pair "domain" domain
      @ Client.opt_pair "url" url
      @ Client.opt_pair "viewer" viewer
      @ Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor)
    |> parse_skeleton_posts

  let search_actors_skeleton ?session ?host ~q ?viewer ?typeahead ?limit ?cursor
      () : skeleton_actors =
    Client.get_json ?session ?host "app.bsky.unspecced.searchActorsSkeleton"
      ([ ("q", q) ]
      @ Client.opt_pair "viewer" viewer
      @ Client.opt_bool "typeahead" typeahead
      @ Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor)
    |> parse_skeleton_actors

  let search_starter_packs_skeleton ?session ?host ~q ?viewer ?limit ?cursor ()
      : skeleton_starter_packs =
    Client.get_json ?session ?host
      "app.bsky.unspecced.searchStarterPacksSkeleton"
      ([ ("q", q) ]
      @ Client.opt_pair "viewer" viewer
      @ Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor)
    |> parse_skeleton_starter_packs

  let get_trending_topics ?session ?host ?viewer ?limit () : trending_topics =
    Client.get_json ?session ?host "app.bsky.unspecced.getTrendingTopics"
      (Client.opt_pair "viewer" viewer @ Client.opt_int "limit" limit)
    |> parse_trending_topics

  let get_config ?session ?host () : config =
    Client.get_json ?session ?host "app.bsky.unspecced.getConfig" []
    |> parse_config

  let get_popular_feed_generators ?session ?host ?query ?limit ?cursor () :
      generators =
    Client.get_json ?session ?host "app.bsky.unspecced.getPopularFeedGenerators"
      (Client.opt_pair "query" query
      @ Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor)
    |> parse_generators

  type tagged_suggestion = {
    tag : string;
    subject_type : string;
    subject : string;
  }

  type tagged_suggestions = { suggestions : tagged_suggestion list }

  type age_assurance_state = {
    status : string;
    last_initiated_at : string option;
  }

  type skeleton_trend = {
    topic : string;
    display_name : string;
    description : string option;
    link : string;
    started_at : string;
    post_count : int;
    status : string option;
    category : string option;
    dids : string list;
  }

  type trend_view = {
    topic : string;
    display_name : string;
    description : string option;
    link : string;
    started_at : string;
    post_count : int;
    status : string option;
    category : string option;
    actor_dids : string list;
  }

  type trends = { trends : trend_view list; rec_id_str : string option }

  type trends_skeleton = {
    trends : skeleton_trend list;
    rec_id_str : string option;
  }

  type suggestions_skeleton = {
    cursor : string option;
    actors : skeleton_actor list;
    relative_to_did : string option;
    rec_id : int option;
    rec_id_str : string option;
  }

  type suggested_users = {
    actors : Actor.short_profile list;
    rec_id : string option;
    rec_id_str : string option;
  }

  type suggested_feeds = { feeds : generator_view list }
  type uri_skeleton = { uris : string list }
  type did_skeleton = { dids : string list; rec_id_str : string option }

  let parse_tagged_suggestion json : tagged_suggestion =
    {
      tag = Client.string_member json "tag";
      subject_type = Client.string_member json "subjectType";
      subject = Client.string_member json "subject";
    }

  let parse_tagged_suggestions json : tagged_suggestions =
    {
      suggestions =
        List.map parse_tagged_suggestion (Client.list_member json "suggestions");
    }

  let parse_age_assurance_state json : age_assurance_state =
    {
      status = Client.string_member json "status";
      last_initiated_at = Client.string_opt json "lastInitiatedAt";
    }

  let string_list json field =
    List.filter_map
      (function `String s -> Some s | _ -> None)
      (Client.list_member json field)

  let parse_skeleton_trend json : skeleton_trend =
    {
      topic = Client.string_member json "topic";
      display_name = Client.string_member json "displayName";
      description = Client.string_opt json "description";
      link = Client.string_member json "link";
      started_at = Client.string_member json "startedAt";
      post_count = Client.int_member json "postCount";
      status = Client.string_opt json "status";
      category = Client.string_opt json "category";
      dids = string_list json "dids";
    }

  let parse_trend_view json : trend_view =
    let actor_dids =
      List.filter_map
        (function
          | `Assoc _ as a -> Client.string_opt a "did"
          | `String s -> Some s
          | _ -> None)
        (Client.list_member json "actors")
    in
    {
      topic = Client.string_member json "topic";
      display_name = Client.string_member json "displayName";
      description = Client.string_opt json "description";
      link = Client.string_member json "link";
      started_at = Client.string_member json "startedAt";
      post_count = Client.int_member json "postCount";
      status = Client.string_opt json "status";
      category = Client.string_opt json "category";
      actor_dids;
    }

  let parse_trends json : trends =
    {
      trends = List.map parse_trend_view (Client.list_member json "trends");
      rec_id_str = Client.string_opt json "recIdStr";
    }

  let parse_trends_skeleton json : trends_skeleton =
    {
      trends = List.map parse_skeleton_trend (Client.list_member json "trends");
      rec_id_str = Client.string_opt json "recIdStr";
    }

  let parse_suggestions_skeleton json : suggestions_skeleton =
    {
      cursor = Client.string_opt json "cursor";
      actors = List.map parse_skeleton_actor (Client.list_member json "actors");
      relative_to_did = Client.string_opt json "relativeToDid";
      rec_id = Client.int_opt json "recId";
      rec_id_str = Client.string_opt json "recIdStr";
    }

  let parse_suggested_users json : suggested_users =
    {
      actors =
        List.map Actor.parse_short_profile (Client.list_member json "actors");
      rec_id =
        (match Yojson.Safe.Util.member "recId" json with
        | `String s -> Some s
        | `Int n -> Some (string_of_int n)
        | _ -> None);
      rec_id_str = Client.string_opt json "recIdStr";
    }

  let parse_suggested_feeds json : suggested_feeds =
    { feeds = List.map parse_generator_view (Client.list_member json "feeds") }

  let parse_uri_list json field : uri_skeleton =
    { uris = string_list json field }

  let parse_did_skeleton json : did_skeleton =
    {
      dids = string_list json "dids";
      rec_id_str = Client.string_opt json "recIdStr";
    }

  let get_tagged_suggestions ?session ?host () : tagged_suggestions =
    Client.get_json ?session ?host "app.bsky.unspecced.getTaggedSuggestions" []
    |> parse_tagged_suggestions

  let get_age_assurance_state (s : Session.session) : age_assurance_state =
    Client.get_json ~session:s "app.bsky.unspecced.getAgeAssuranceState" []
    |> parse_age_assurance_state

  let init_age_assurance_body ~email ~language ~country_code : Yojson.Safe.t =
    `Assoc
      [
        ("email", `String email);
        ("language", `String language);
        ("countryCode", `String country_code);
      ]

  let init_age_assurance (s : Session.session) ~email ~language ~country_code ()
      : age_assurance_state =
    Client.post_json ~session:s "app.bsky.unspecced.initAgeAssurance"
      (Yojson.Safe.to_string
         (init_age_assurance_body ~email ~language ~country_code))
    |> parse_age_assurance_state

  let get_trends ?session ?host ?limit () : trends =
    Client.get_json ?session ?host "app.bsky.unspecced.getTrends"
      (Client.opt_int "limit" limit)
    |> parse_trends

  let get_trends_skeleton ?session ?host ?viewer ?limit () : trends_skeleton =
    Client.get_json ?session ?host "app.bsky.unspecced.getTrendsSkeleton"
      (Client.opt_pair "viewer" viewer @ Client.opt_int "limit" limit)
    |> parse_trends_skeleton

  let get_suggestions_skeleton ?session ?host ?viewer ?limit ?cursor
      ?relative_to_did () : suggestions_skeleton =
    Client.get_json ?session ?host "app.bsky.unspecced.getSuggestionsSkeleton"
      (Client.opt_pair "viewer" viewer
      @ Client.opt_int "limit" limit
      @ Client.opt_pair "cursor" cursor
      @ Client.opt_pair "relativeToDid" relative_to_did)
    |> parse_suggestions_skeleton

  let get_suggested_feeds ?session ?host ?limit () : suggested_feeds =
    Client.get_json ?session ?host "app.bsky.unspecced.getSuggestedFeeds"
      (Client.opt_int "limit" limit)
    |> parse_suggested_feeds

  let get_suggested_feeds_skeleton ?session ?host ?viewer ?limit () :
      uri_skeleton =
    Client.get_json ?session ?host
      "app.bsky.unspecced.getSuggestedFeedsSkeleton"
      (Client.opt_pair "viewer" viewer @ Client.opt_int "limit" limit)
    |> fun json -> parse_uri_list json "feeds"

  let get_suggested_users ?session ?host ?category ?limit () : suggested_users =
    Client.get_json ?session ?host "app.bsky.unspecced.getSuggestedUsers"
      (Client.opt_pair "category" category @ Client.opt_int "limit" limit)
    |> parse_suggested_users

  let get_suggested_users_skeleton ?session ?host ?viewer ?category ?limit () :
      did_skeleton =
    Client.get_json ?session ?host
      "app.bsky.unspecced.getSuggestedUsersSkeleton"
      (Client.opt_pair "viewer" viewer
      @ Client.opt_pair "category" category
      @ Client.opt_int "limit" limit)
    |> parse_did_skeleton

  let get_suggested_starter_packs ?session ?host ?limit () :
      Graph.starter_pack list =
    Client.get_json ?session ?host "app.bsky.unspecced.getSuggestedStarterPacks"
      (Client.opt_int "limit" limit)
    |> fun json ->
    List.map Graph.parse_starter_pack (Client.list_member json "starterPacks")

  let get_suggested_starter_packs_skeleton ?session ?host ?viewer ?limit () :
      uri_skeleton =
    Client.get_json ?session ?host
      "app.bsky.unspecced.getSuggestedStarterPacksSkeleton"
      (Client.opt_pair "viewer" viewer @ Client.opt_int "limit" limit)
    |> fun json -> parse_uri_list json "starterPacks"

  let get_onboarding_suggested_starter_packs ?session ?host ?limit () :
      Graph.starter_pack list =
    Client.get_json ?session ?host
      "app.bsky.unspecced.getOnboardingSuggestedStarterPacks"
      (Client.opt_int "limit" limit)
    |> fun json ->
    List.map Graph.parse_starter_pack (Client.list_member json "starterPacks")

  let get_onboarding_suggested_starter_packs_skeleton ?session ?host ?viewer
      ?limit () : uri_skeleton =
    Client.get_json ?session ?host
      "app.bsky.unspecced.getOnboardingSuggestedStarterPacksSkeleton"
      (Client.opt_pair "viewer" viewer @ Client.opt_int "limit" limit)
    |> fun json -> parse_uri_list json "starterPacks"
end
