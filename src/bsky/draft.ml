open Session
open Client

(** app.bsky.draft — private stash drafts (current lexicons). *)
module Draft = struct
  type local_ref = { path : string }
  type caption = { lang : string; content : string }
  type embed_image = { local_ref : local_ref; alt : string option }

  type embed_video = {
    local_ref : local_ref;
    alt : string option;
    captions : caption list;
  }

  type embed_external = { uri : string }
  type embed_record = { uri : string; cid : string }
  type embed_gallery = { items : embed_image list }

  type threadgate_rule =
    [ `Mention
    | `Follower
    | `Following
    | `List of string
    | `Unknown of Yojson.Safe.t ]

  type postgate_rule = [ `Disable | `Unknown of Yojson.Safe.t ]

  type draft_post = {
    text : string;
    labels : string list option;
    embed_images : embed_image list;
    embed_gallery : embed_gallery option;
    embed_videos : embed_video list;
    embed_externals : embed_external list;
    embed_records : embed_record list;
  }

  type draft = {
    device_id : string option;
    device_name : string option;
    posts : draft_post list;
    langs : string list;
    postgate_embedding_rules : postgate_rule list;
    threadgate_allow : threadgate_rule list;
    original : Yojson.Safe.t;
  }

  type draft_view = {
    id : string;
    draft : draft;
    created_at : string;
    updated_at : string;
  }

  type drafts_page = { cursor : string option; drafts : draft_view list }
  type create_result = { id : string }

  let ends_with suffix s =
    let n = String.length s and m = String.length suffix in
    n >= m && String.sub s (n - m) m = suffix

  let string_list json field =
    List.filter_map
      (function `String s -> Some s | _ -> None)
      (Client.list_member json field)

  let parse_local_ref json : local_ref =
    match json with
    | `Assoc _ -> { path = Client.string_member json "path" }
    | `String s -> { path = s }
    | _ -> { path = "" }

  let parse_caption json : caption =
    {
      lang = Client.string_member json "lang";
      content = Client.string_member json "content";
    }

  let parse_embed_image json : embed_image =
    {
      local_ref =
        (match Yojson.Safe.Util.member "localRef" json with
        | `Null -> { path = "" }
        | v -> parse_local_ref v);
      alt = Client.string_opt json "alt";
    }

  let parse_embed_video json : embed_video =
    {
      local_ref =
        (match Yojson.Safe.Util.member "localRef" json with
        | `Null -> { path = "" }
        | v -> parse_local_ref v);
      alt = Client.string_opt json "alt";
      captions = List.map parse_caption (Client.list_member json "captions");
    }

  let parse_embed_external json : embed_external =
    { uri = Client.string_member json "uri" }

  let parse_embed_record json : embed_record =
    let record =
      match Yojson.Safe.Util.member "record" json with
      | `Assoc _ as r -> r
      | _ -> json
    in
    {
      uri = Client.string_member record "uri";
      cid = Client.string_member record "cid";
    }

  let parse_embed_gallery json : embed_gallery =
    let items =
      match Yojson.Safe.Util.member "items" json with
      | `List xs -> List.map parse_embed_image xs
      | `Assoc _ as g ->
          List.map parse_embed_image (Client.list_member g "items")
      | _ -> List.map parse_embed_image (Client.list_member json "items")
    in
    { items }

  let parse_threadgate_rule json : threadgate_rule =
    let ty = Client.string_opt json "$type" |> Option.value ~default:"" in
    if ends_with "mentionRule" ty then `Mention
    else if ends_with "followerRule" ty then `Follower
    else if ends_with "followingRule" ty then `Following
    else if ends_with "listRule" ty then
      `List (Client.string_member json "list")
    else `Unknown json

  let parse_postgate_rule json : postgate_rule =
    let ty = Client.string_opt json "$type" |> Option.value ~default:"" in
    if ends_with "disableRule" ty then `Disable else `Unknown json

  let parse_draft_post json : draft_post =
    {
      text = Client.string_member json "text";
      labels =
        Label.Label.parse_self_labels (Yojson.Safe.Util.member "labels" json);
      embed_images =
        List.map parse_embed_image (Client.list_member json "embedImages");
      embed_gallery =
        (match Yojson.Safe.Util.member "embedGallery" json with
        | `Assoc _ as g -> Some (parse_embed_gallery g)
        | _ -> None);
      embed_videos =
        List.map parse_embed_video (Client.list_member json "embedVideos");
      embed_externals =
        List.map parse_embed_external (Client.list_member json "embedExternals");
      embed_records =
        List.map parse_embed_record (Client.list_member json "embedRecords");
    }

  let parse_draft json : draft =
    {
      device_id = Client.string_opt json "deviceId";
      device_name = Client.string_opt json "deviceName";
      posts = List.map parse_draft_post (Client.list_member json "posts");
      langs = string_list json "langs";
      postgate_embedding_rules =
        List.map parse_postgate_rule
          (Client.list_member json "postgateEmbeddingRules");
      threadgate_allow =
        List.map parse_threadgate_rule
          (Client.list_member json "threadgateAllow");
      original = json;
    }

  let parse_draft_view json : draft_view =
    {
      id = Client.string_member json "id";
      draft =
        (match Yojson.Safe.Util.member "draft" json with
        | `Assoc _ as d -> parse_draft d
        | _ -> parse_draft json);
      created_at = Client.string_member json "createdAt";
      updated_at = Client.string_member json "updatedAt";
    }

  let parse_drafts_page json : drafts_page =
    {
      cursor = Client.string_opt json "cursor";
      drafts = List.map parse_draft_view (Client.list_member json "drafts");
    }

  let local_ref_json (r : local_ref) : Yojson.Safe.t =
    `Assoc [ ("path", `String r.path) ]

  let caption_json (c : caption) : Yojson.Safe.t =
    `Assoc [ ("lang", `String c.lang); ("content", `String c.content) ]

  let embed_image_json (i : embed_image) : Yojson.Safe.t =
    let fields =
      ("localRef", local_ref_json i.local_ref)
      :: (match i.alt with Some a -> [ ("alt", `String a) ] | None -> [])
    in
    `Assoc fields

  let embed_video_json (v : embed_video) : Yojson.Safe.t =
    let fields =
      [ ("localRef", local_ref_json v.local_ref) ]
      @ (match v.alt with Some a -> [ ("alt", `String a) ] | None -> [])
      @
      match v.captions with
      | [] -> []
      | cs -> [ ("captions", `List (List.map caption_json cs)) ]
    in
    `Assoc fields

  let threadgate_rule_json (r : threadgate_rule) : Yojson.Safe.t =
    match r with
    | `Mention ->
        `Assoc [ ("$type", `String "app.bsky.feed.threadgate#mentionRule") ]
    | `Follower ->
        `Assoc [ ("$type", `String "app.bsky.feed.threadgate#followerRule") ]
    | `Following ->
        `Assoc [ ("$type", `String "app.bsky.feed.threadgate#followingRule") ]
    | `List uri ->
        `Assoc
          [
            ("$type", `String "app.bsky.feed.threadgate#listRule");
            ("list", `String uri);
          ]
    | `Unknown j -> j

  let postgate_rule_json (r : postgate_rule) : Yojson.Safe.t =
    match r with
    | `Disable ->
        `Assoc [ ("$type", `String "app.bsky.feed.postgate#disableRule") ]
    | `Unknown j -> j

  let draft_post_json (p : draft_post) : Yojson.Safe.t =
    let fields =
      [ ("text", `String p.text) ]
      @ (match p.embed_images with
        | [] -> []
        | xs -> [ ("embedImages", `List (List.map embed_image_json xs)) ])
      @ (match p.embed_gallery with
        | Some g ->
            [
              ( "embedGallery",
                `Assoc [ ("items", `List (List.map embed_image_json g.items)) ]
              );
            ]
        | None -> [])
      @ (match p.embed_videos with
        | [] -> []
        | xs -> [ ("embedVideos", `List (List.map embed_video_json xs)) ])
      @ (match p.embed_externals with
        | [] -> []
        | xs ->
            [
              ( "embedExternals",
                `List
                  (List.map
                     (fun (e : embed_external) ->
                       `Assoc [ ("uri", `String e.uri) ])
                     xs) );
            ])
      @
      match p.embed_records with
      | [] -> []
      | xs ->
          [
            ( "embedRecords",
              `List
                (List.map
                   (fun (r : embed_record) ->
                     `Assoc
                       [
                         ( "record",
                           `Assoc
                             [ ("uri", `String r.uri); ("cid", `String r.cid) ]
                         );
                       ])
                   xs) );
          ]
    in
    `Assoc fields

  let draft_json ?(device_id : string option) ?(device_name : string option)
      ?(langs = []) ?(postgate_embedding_rules = []) ?(threadgate_allow = [])
      ~posts () : Yojson.Safe.t =
    let fields =
      [ ("posts", `List (List.map draft_post_json posts)) ]
      @ (match device_id with
        | Some id -> [ ("deviceId", `String id) ]
        | None -> [])
      @ (match device_name with
        | Some n -> [ ("deviceName", `String n) ]
        | None -> [])
      @ (match langs with
        | [] -> []
        | xs -> [ ("langs", `List (List.map (fun s -> `String s) xs)) ])
      @ (match postgate_embedding_rules with
        | [] -> []
        | xs ->
            [
              ("postgateEmbeddingRules", `List (List.map postgate_rule_json xs));
            ])
      @
      match threadgate_allow with
      | [] -> []
      | xs -> [ ("threadgateAllow", `List (List.map threadgate_rule_json xs)) ]
    in
    `Assoc fields

  let create_draft_body draft : Yojson.Safe.t = `Assoc [ ("draft", draft) ]

  let update_draft_body ~id draft : Yojson.Safe.t =
    `Assoc [ ("draft", `Assoc [ ("id", `String id); ("draft", draft) ]) ]

  let delete_draft_body ~id : Yojson.Safe.t = `Assoc [ ("id", `String id) ]

  let get_drafts (s : Session.session) ?limit ?cursor () : drafts_page =
    Client.get_json ~session:s "app.bsky.draft.getDrafts"
      (Client.opt_int "limit" limit @ Client.opt_pair "cursor" cursor)
    |> parse_drafts_page

  let create_draft (s : Session.session) draft : create_result =
    Client.post_json ~session:s "app.bsky.draft.createDraft"
      (Yojson.Safe.to_string (create_draft_body draft))
    |> fun json -> { id = Client.string_member json "id" }

  let update_draft (s : Session.session) ~id draft : unit =
    ignore
      (Client.post_json ~session:s "app.bsky.draft.updateDraft"
         (Yojson.Safe.to_string (update_draft_body ~id draft)))

  let delete_draft (s : Session.session) ~id () : unit =
    ignore
      (Client.post_json ~session:s "app.bsky.draft.deleteDraft"
         (Yojson.Safe.to_string (delete_draft_body ~id)))
end
