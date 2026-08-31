open Client

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

  let get_tagged_suggestions ?session ?host () : Yojson.Safe.t =
    Client.get_json ?session ?host "app.bsky.unspecced.getTaggedSuggestions" []
end
