open Embed
open Facet
open Notification

(** Typed builders and parsers for common Bluesky repo records. *)
module Records = struct
  let nsid_post = "app.bsky.feed.post"
  let nsid_like = "app.bsky.feed.like"
  let nsid_repost = "app.bsky.feed.repost"
  let nsid_follow = "app.bsky.graph.follow"
  let nsid_block = "app.bsky.graph.block"
  let nsid_listblock = "app.bsky.graph.listblock"
  let nsid_list = "app.bsky.graph.list"
  let nsid_listitem = "app.bsky.graph.listitem"
  let nsid_starterpack = "app.bsky.graph.starterpack"
  let nsid_profile = "app.bsky.actor.profile"
  let nsid_chat_declaration = "chat.bsky.actor.declaration"
  let nsid_status = "app.bsky.actor.status"
  let nsid_content_visibility = "app.bsky.actor.contentVisibilityDeclaration"
  let nsid_verification = "app.bsky.graph.verification"
  let nsid_threadgate = "app.bsky.feed.threadgate"
  let nsid_postgate = "app.bsky.feed.postgate"
  let nsid_generator = "app.bsky.feed.generator"
  let nsid_labeler_service = "app.bsky.labeler.service"
  let nsid_notification_declaration = "app.bsky.notification.declaration"
  let status_live = "app.bsky.actor.status#live"
  let purpose_modlist = "app.bsky.graph.defs#modlist"
  let purpose_curatelist = "app.bsky.graph.defs#curatelist"
  let purpose_referencelist = "app.bsky.graph.defs#referencelist"

  let strong_ref ~uri ~cid : Yojson.Safe.t =
    `Assoc [ ("uri", `String uri); ("cid", `String cid) ]

  let parse_strong_ref json : Embed.strong_ref = Embed.parse_strong_ref json
  let via_fields via = match via with Some v -> [ ("via", v) ] | None -> []

  let post ~text ~created_at ?langs ?facets ?embed ?reply ?tags ?self_labels ()
      : Yojson.Safe.t =
    let fields =
      [
        ("$type", `String nsid_post);
        ("text", `String text);
        ("createdAt", `String created_at);
      ]
      @ (match langs with
        | Some xs -> [ ("langs", `List (List.map (fun s -> `String s) xs)) ]
        | None -> [])
      @ (match facets with
        | Some fs -> [ ("facets", Facet.facets_to_json fs) ]
        | None -> [])
      @ (match embed with
        | Some e -> [ ("embed", Embed.embed_to_json e) ]
        | None -> [])
      @ (match reply with
        | Some (r : Notification.reply) ->
            [
              ( "reply",
                `Assoc
                  [
                    ("root", strong_ref ~uri:r.root.uri ~cid:r.root.cid);
                    ("parent", strong_ref ~uri:r.parent.uri ~cid:r.parent.cid);
                  ] );
            ]
        | None -> [])
      @ (match tags with
        | Some xs -> [ ("tags", `List (List.map (fun s -> `String s) xs)) ]
        | None -> [])
      @
      match self_labels with
      | Some xs -> [ ("labels", Label.Label.self_labels_to_json xs) ]
      | None -> []
    in
    `Assoc fields

  let like ~uri ~cid ~created_at ?via () : Yojson.Safe.t =
    `Assoc
      ([
         ("$type", `String nsid_like);
         ("subject", strong_ref ~uri ~cid);
         ("createdAt", `String created_at);
       ]
      @ via_fields via)

  let repost ~uri ~cid ~created_at ?via () : Yojson.Safe.t =
    `Assoc
      ([
         ("$type", `String nsid_repost);
         ("subject", strong_ref ~uri ~cid);
         ("createdAt", `String created_at);
       ]
      @ via_fields via)

  let follow ~subject ~created_at ?via () : Yojson.Safe.t =
    `Assoc
      ([
         ("$type", `String nsid_follow);
         ("subject", `String subject);
         ("createdAt", `String created_at);
       ]
      @ via_fields via)

  let block ~subject ~created_at () : Yojson.Safe.t =
    `Assoc
      [
        ("$type", `String nsid_block);
        ("subject", `String subject);
        ("createdAt", `String created_at);
      ]

  let listblock ~subject ~created_at () : Yojson.Safe.t =
    `Assoc
      [
        ("$type", `String nsid_listblock);
        ("subject", `String subject);
        ("createdAt", `String created_at);
      ]

  let list ~name ~purpose ~created_at ?description ?description_facets ?avatar
      ?self_labels () : Yojson.Safe.t =
    let fields =
      [
        ("$type", `String nsid_list);
        ("name", `String name);
        ("purpose", `String purpose);
        ("createdAt", `String created_at);
      ]
      @ (match description with
        | Some s -> [ ("description", `String s) ]
        | None -> [])
      @ (match description_facets with
        | Some fs -> [ ("descriptionFacets", Facet.facets_to_json fs) ]
        | None -> [])
      @ (match avatar with Some b -> [ ("avatar", b) ] | None -> [])
      @
      match self_labels with
      | Some xs -> [ ("labels", Label.Label.self_labels_to_json xs) ]
      | None -> []
    in
    `Assoc fields

  let listitem ~subject ~list ~created_at () : Yojson.Safe.t =
    `Assoc
      [
        ("$type", `String nsid_listitem);
        ("subject", `String subject);
        ("list", `String list);
        ("createdAt", `String created_at);
      ]

  let starterpack ~name ~list ~created_at ?description ?description_facets
      ?feeds () : Yojson.Safe.t =
    let fields =
      [
        ("$type", `String nsid_starterpack);
        ("name", `String name);
        ("list", `String list);
        ("createdAt", `String created_at);
      ]
      @ (match description with
        | Some s -> [ ("description", `String s) ]
        | None -> [])
      @ (match description_facets with
        | Some fs -> [ ("descriptionFacets", Facet.facets_to_json fs) ]
        | None -> [])
      @
      match feeds with
      | Some uris ->
          [
            ( "feeds",
              `List (List.map (fun uri -> `Assoc [ ("uri", `String uri) ]) uris)
            );
          ]
      | None -> []
    in
    `Assoc fields

  let profile ?display_name ?description ?pronouns ?website ?avatar ?banner
      ?self_labels ?pinned_post ?created_at () : Yojson.Safe.t =
    let fields =
      [ ("$type", `String nsid_profile) ]
      @ (match display_name with
        | Some s -> [ ("displayName", `String s) ]
        | None -> [])
      @ (match description with
        | Some s -> [ ("description", `String s) ]
        | None -> [])
      @ (match pronouns with
        | Some s -> [ ("pronouns", `String s) ]
        | None -> [])
      @ (match website with Some s -> [ ("website", `String s) ] | None -> [])
      @ (match avatar with Some b -> [ ("avatar", b) ] | None -> [])
      @ (match banner with Some b -> [ ("banner", b) ] | None -> [])
      @ (match self_labels with
        | Some xs -> [ ("labels", Label.Label.self_labels_to_json xs) ]
        | None -> [])
      @ (match pinned_post with Some r -> [ ("pinnedPost", r) ] | None -> [])
      @
      match created_at with
      | Some s -> [ ("createdAt", `String s) ]
      | None -> []
    in
    `Assoc fields

  let status ~status ~created_at ?embed ?duration_minutes () : Yojson.Safe.t =
    let fields =
      [
        ("$type", `String nsid_status);
        ("status", `String status);
        ("createdAt", `String created_at);
      ]
      @ (match embed with Some e -> [ ("embed", e) ] | None -> [])
      @
      match duration_minutes with
      | Some n -> [ ("durationMinutes", `Int n) ]
      | None -> []
    in
    `Assoc fields

  let content_visibility_declaration ~hide_from_algorithmic_recommendations () :
      Yojson.Safe.t =
    `Assoc
      [
        ("$type", `String nsid_content_visibility);
        ( "hideFromAlgorithmicRecommendations",
          `Bool hide_from_algorithmic_recommendations );
      ]

  let verification ~subject ~handle ~display_name ~created_at () : Yojson.Safe.t
      =
    `Assoc
      [
        ("$type", `String nsid_verification);
        ("subject", `String subject);
        ("handle", `String handle);
        ("displayName", `String display_name);
        ("createdAt", `String created_at);
      ]

  type threadgate_rule =
    [ `Mention
    | `Follower
    | `Following
    | `List of string
    | `Unknown of Yojson.Safe.t ]

  type postgate_rule = [ `Disable | `Unknown of Yojson.Safe.t ]

  let threadgate_rule_json = function
    | `Mention ->
        `Assoc [ ("$type", `String (nsid_threadgate ^ "#mentionRule")) ]
    | `Follower ->
        `Assoc [ ("$type", `String (nsid_threadgate ^ "#followerRule")) ]
    | `Following ->
        `Assoc [ ("$type", `String (nsid_threadgate ^ "#followingRule")) ]
    | `List list ->
        `Assoc
          [
            ("$type", `String (nsid_threadgate ^ "#listRule"));
            ("list", `String list);
          ]
    | `Unknown json -> json

  let postgate_rule_json = function
    | `Disable -> `Assoc [ ("$type", `String (nsid_postgate ^ "#disableRule")) ]
    | `Unknown json -> json

  let threadgate ~post ~created_at ?allow ?hidden_replies () : Yojson.Safe.t =
    let fields =
      [
        ("$type", `String nsid_threadgate);
        ("post", `String post);
        ("createdAt", `String created_at);
      ]
      @ (match allow with
        | Some xs -> [ ("allow", `List (List.map threadgate_rule_json xs)) ]
        | None -> [])
      @
      match hidden_replies with
      | Some uris ->
          [ ("hiddenReplies", `List (List.map (fun s -> `String s) uris)) ]
      | None -> []
    in
    `Assoc fields

  let postgate ~post ~created_at ?detached_embedding_uris ?embedding_rules () :
      Yojson.Safe.t =
    let fields =
      [
        ("$type", `String nsid_postgate);
        ("post", `String post);
        ("createdAt", `String created_at);
      ]
      @ (match detached_embedding_uris with
        | Some uris ->
            [
              ( "detachedEmbeddingUris",
                `List (List.map (fun s -> `String s) uris) );
            ]
        | None -> [])
      @
      match embedding_rules with
      | Some xs ->
          [ ("embeddingRules", `List (List.map postgate_rule_json xs)) ]
      | None -> []
    in
    `Assoc fields

  let generator ~did ~display_name ~created_at ?description ?description_facets
      ?avatar ?accepts_interactions ?self_labels ?content_mode () :
      Yojson.Safe.t =
    let fields =
      [
        ("$type", `String nsid_generator);
        ("did", `String did);
        ("displayName", `String display_name);
        ("createdAt", `String created_at);
      ]
      @ (match description with
        | Some s -> [ ("description", `String s) ]
        | None -> [])
      @ (match description_facets with
        | Some fs -> [ ("descriptionFacets", Facet.facets_to_json fs) ]
        | None -> [])
      @ (match avatar with Some b -> [ ("avatar", b) ] | None -> [])
      @ (match accepts_interactions with
        | Some b -> [ ("acceptsInteractions", `Bool b) ]
        | None -> [])
      @ (match self_labels with
        | Some xs -> [ ("labels", Label.Label.self_labels_to_json xs) ]
        | None -> [])
      @
      match content_mode with
      | Some s -> [ ("contentMode", `String s) ]
      | None -> []
    in
    `Assoc fields

  let labeler_service ~policies ~created_at ?self_labels ?reason_types
      ?subject_types ?subject_collections () : Yojson.Safe.t =
    let fields =
      [
        ("$type", `String nsid_labeler_service);
        ("policies", policies);
        ("createdAt", `String created_at);
      ]
      @ (match self_labels with
        | Some xs -> [ ("labels", Label.Label.self_labels_to_json xs) ]
        | None -> [])
      @ (match reason_types with
        | Some xs ->
            [ ("reasonTypes", `List (List.map (fun s -> `String s) xs)) ]
        | None -> [])
      @ (match subject_types with
        | Some xs ->
            [ ("subjectTypes", `List (List.map (fun s -> `String s) xs)) ]
        | None -> [])
      @
      match subject_collections with
      | Some xs ->
          [ ("subjectCollections", `List (List.map (fun s -> `String s) xs)) ]
      | None -> []
    in
    `Assoc fields

  let notification_declaration ~allow_subscriptions () : Yojson.Safe.t =
    `Assoc
      [
        ("$type", `String nsid_notification_declaration);
        ("allowSubscriptions", `String allow_subscriptions);
      ]

  let chat_declaration ~allow_incoming ?allow_group_invites () : Yojson.Safe.t =
    let fields =
      [
        ("$type", `String nsid_chat_declaration);
        ("allowIncoming", `String allow_incoming);
      ]
      @
      match allow_group_invites with
      | Some s -> [ ("allowGroupInvites", `String s) ]
      | None -> []
    in
    `Assoc fields

  type like_record = {
    subject : Embed.strong_ref;
    created_at : string;
    via : Embed.strong_ref option;
  }

  type follow_record = {
    subject : string;
    created_at : string;
    via : Embed.strong_ref option;
  }

  type block_record = { subject : string; created_at : string }

  type list_record = {
    name : string;
    purpose : string;
    description : string option;
    description_facets : Facet.facet list option;
    created_at : string;
    self_labels : string list option;
  }

  type listitem_record = {
    subject : string;
    list : string;
    created_at : string;
  }

  type starterpack_record = {
    name : string;
    list : string;
    description : string option;
    description_facets : Facet.facet list option;
    feeds : string list;
    created_at : string;
  }

  type chat_declaration_record = {
    allow_incoming : string;
    allow_group_invites : string option;
  }

  type status_record = {
    status : string;
    created_at : string;
    duration_minutes : int option;
  }

  type content_visibility_record = {
    hide_from_algorithmic_recommendations : bool;
  }

  type verification_record = {
    subject : string;
    handle : string;
    display_name : string;
    created_at : string;
  }

  type threadgate_record = {
    post : string;
    created_at : string;
    allow : threadgate_rule list option;
    hidden_replies : string list;
  }

  type postgate_record = {
    post : string;
    created_at : string;
    detached_embedding_uris : string list;
    embedding_rules : postgate_rule list;
  }

  type generator_record = {
    did : string;
    display_name : string;
    description : string option;
    created_at : string;
    accepts_interactions : bool option;
    content_mode : string option;
  }

  type notification_declaration_record = { allow_subscriptions : string }

  let parse_via json : Embed.strong_ref option =
    match Yojson.Safe.Util.member "via" json with
    | `Assoc _ as v -> Some (parse_strong_ref v)
    | _ -> None

  let parse_like json : like_record =
    let open Yojson.Safe.Util in
    {
      subject = json |> member "subject" |> parse_strong_ref;
      created_at =
        (match json |> member "createdAt" with `String s -> s | _ -> "");
      via = parse_via json;
    }

  let parse_repost json : like_record = parse_like json

  let parse_follow json : follow_record =
    let open Yojson.Safe.Util in
    {
      subject = (match json |> member "subject" with `String s -> s | _ -> "");
      created_at =
        (match json |> member "createdAt" with `String s -> s | _ -> "");
      via = parse_via json;
    }

  let parse_block json : block_record =
    let open Yojson.Safe.Util in
    {
      subject = (match json |> member "subject" with `String s -> s | _ -> "");
      created_at =
        (match json |> member "createdAt" with `String s -> s | _ -> "");
    }

  let parse_listblock json : block_record = parse_block json

  let parse_facets_option json field : Facet.facet list option =
    match Yojson.Safe.Util.member field json with
    | `List xs -> Some (List.map Facet.parse_facet xs)
    | _ -> None

  let parse_list json : list_record =
    let open Yojson.Safe.Util in
    {
      name = (match json |> member "name" with `String s -> s | _ -> "");
      purpose = (match json |> member "purpose" with `String s -> s | _ -> "");
      description =
        (match json |> member "description" with
        | `String s -> Some s
        | _ -> None);
      description_facets = parse_facets_option json "descriptionFacets";
      created_at =
        (match json |> member "createdAt" with `String s -> s | _ -> "");
      self_labels = Label.Label.parse_self_labels (json |> member "labels");
    }

  let parse_listitem json : listitem_record =
    let open Yojson.Safe.Util in
    {
      subject = (match json |> member "subject" with `String s -> s | _ -> "");
      list = (match json |> member "list" with `String s -> s | _ -> "");
      created_at =
        (match json |> member "createdAt" with `String s -> s | _ -> "");
    }

  let parse_starterpack json : starterpack_record =
    let open Yojson.Safe.Util in
    let feeds =
      match json |> member "feeds" with
      | `List items ->
          List.filter_map
            (fun item ->
              match item |> member "uri" with `String s -> Some s | _ -> None)
            items
      | _ -> []
    in
    {
      name = (match json |> member "name" with `String s -> s | _ -> "");
      list = (match json |> member "list" with `String s -> s | _ -> "");
      description =
        (match json |> member "description" with
        | `String s -> Some s
        | _ -> None);
      description_facets = parse_facets_option json "descriptionFacets";
      feeds;
      created_at =
        (match json |> member "createdAt" with `String s -> s | _ -> "");
    }

  let parse_chat_declaration json : chat_declaration_record =
    let open Yojson.Safe.Util in
    {
      allow_incoming =
        (match json |> member "allowIncoming" with
        | `String s -> s
        | _ -> "all");
      allow_group_invites =
        (match json |> member "allowGroupInvites" with
        | `String s -> Some s
        | _ -> None);
    }

  let ends_with suffix s =
    let n = String.length s and m = String.length suffix in
    n >= m && String.sub s (n - m) m = suffix

  let parse_threadgate_rule json : threadgate_rule =
    let ty =
      match Yojson.Safe.Util.member "$type" json with `String s -> s | _ -> ""
    in
    if ends_with "mentionRule" ty then `Mention
    else if ends_with "followerRule" ty then `Follower
    else if ends_with "followingRule" ty then `Following
    else if ends_with "listRule" ty then
      `List
        (match Yojson.Safe.Util.member "list" json with
        | `String s -> s
        | _ -> "")
    else `Unknown json

  let parse_postgate_rule json : postgate_rule =
    let ty =
      match Yojson.Safe.Util.member "$type" json with `String s -> s | _ -> ""
    in
    if ends_with "disableRule" ty then `Disable else `Unknown json

  let parse_status json : status_record =
    let open Yojson.Safe.Util in
    {
      status = (match json |> member "status" with `String s -> s | _ -> "");
      created_at =
        (match json |> member "createdAt" with `String s -> s | _ -> "");
      duration_minutes =
        (match json |> member "durationMinutes" with
        | `Int n -> Some n
        | _ -> None);
    }

  let parse_content_visibility json : content_visibility_record =
    {
      hide_from_algorithmic_recommendations =
        (match
           Yojson.Safe.Util.member "hideFromAlgorithmicRecommendations" json
         with
        | `Bool b -> b
        | _ -> false);
    }

  let parse_verification json : verification_record =
    let open Yojson.Safe.Util in
    {
      subject = (match json |> member "subject" with `String s -> s | _ -> "");
      handle = (match json |> member "handle" with `String s -> s | _ -> "");
      display_name =
        (match json |> member "displayName" with `String s -> s | _ -> "");
      created_at =
        (match json |> member "createdAt" with `String s -> s | _ -> "");
    }

  let parse_threadgate json : threadgate_record =
    let open Yojson.Safe.Util in
    {
      post = (match json |> member "post" with `String s -> s | _ -> "");
      created_at =
        (match json |> member "createdAt" with `String s -> s | _ -> "");
      allow =
        (match json |> member "allow" with
        | `List xs -> Some (List.map parse_threadgate_rule xs)
        | _ -> None);
      hidden_replies =
        (match json |> member "hiddenReplies" with
        | `List xs ->
            List.filter_map (function `String s -> Some s | _ -> None) xs
        | _ -> []);
    }

  let parse_postgate json : postgate_record =
    let open Yojson.Safe.Util in
    {
      post = (match json |> member "post" with `String s -> s | _ -> "");
      created_at =
        (match json |> member "createdAt" with `String s -> s | _ -> "");
      detached_embedding_uris =
        (match json |> member "detachedEmbeddingUris" with
        | `List xs ->
            List.filter_map (function `String s -> Some s | _ -> None) xs
        | _ -> []);
      embedding_rules =
        (match json |> member "embeddingRules" with
        | `List xs -> List.map parse_postgate_rule xs
        | _ -> []);
    }

  let parse_generator json : generator_record =
    let open Yojson.Safe.Util in
    {
      did = (match json |> member "did" with `String s -> s | _ -> "");
      display_name =
        (match json |> member "displayName" with `String s -> s | _ -> "");
      description =
        (match json |> member "description" with
        | `String s -> Some s
        | _ -> None);
      created_at =
        (match json |> member "createdAt" with `String s -> s | _ -> "");
      accepts_interactions =
        (match json |> member "acceptsInteractions" with
        | `Bool b -> Some b
        | _ -> None);
      content_mode =
        (match json |> member "contentMode" with
        | `String s -> Some s
        | _ -> None);
    }

  let parse_notification_declaration json : notification_declaration_record =
    {
      allow_subscriptions =
        (match Yojson.Safe.Util.member "allowSubscriptions" json with
        | `String s -> s
        | _ -> "followers");
    }
end
