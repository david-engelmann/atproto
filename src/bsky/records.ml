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
  let nsid_profile = "app.bsky.actor.profile"

  let strong_ref ~uri ~cid : Yojson.Safe.t =
    `Assoc [ ("uri", `String uri); ("cid", `String cid) ]

  let parse_strong_ref json : Embed.strong_ref = Embed.parse_strong_ref json

  let via_fields via =
    match via with Some v -> [ ("via", v) ] | None -> []

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
      @ (match pinned_post with
        | Some r -> [ ("pinnedPost", r) ]
        | None -> [])
      @
      match created_at with
      | Some s -> [ ("createdAt", `String s) ]
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
      subject =
        (match json |> member "subject" with `String s -> s | _ -> "");
      created_at =
        (match json |> member "createdAt" with `String s -> s | _ -> "");
      via = parse_via json;
    }

  let parse_block json : block_record =
    let open Yojson.Safe.Util in
    {
      subject =
        (match json |> member "subject" with `String s -> s | _ -> "");
      created_at =
        (match json |> member "createdAt" with `String s -> s | _ -> "");
    }

  let parse_listblock json : block_record = parse_block json
end
