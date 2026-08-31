module Embed = struct
  (*
   * external
   * images
   * record
   * record_with_media
   * video
   *)
  type ref = { ref_link : string }

  type image = {
    image_type : string;
    ref : ref;
    mime_type : string;
    size : int;
  }

  type thumb = {
    thumb_type : string;
    ref : ref;
    mime_type : string;
    size : int;
  }

  type ext = {
    uri : string;
    thumb : thumb;
    title : string;
    description : string;
  }

  type ext_view = {
    uri : string;
    title : string;
    description : string;
    thumb : string;
  }

  type image_data = { alt : string; image : image }
  type image_view = { thumb : string; fullsize : string; alt : string }
  type image_embed = { embed_type : string; images : image_data list }
  type image_view_embed = { embed_type : string; images : image_view list }
  type ext_embed = { embed_type : string; ext : ext }
  type ext_view_embed = { embed_type : string; ext : ext_view }
  type strong_ref = { uri : string; cid : string }
  type record_embed = { embed_type : string; record : strong_ref }
  type aspect_ratio = { width : int; height : int }

  type video_blob = {
    cid : string;
    mime_type : string;
    size : int;
    original : Yojson.Safe.t;
  }

  type video_embed = {
    embed_type : string;
    video : video_blob;
    alt : string option;
    aspect_ratio : aspect_ratio option;
  }

  type video_view_embed = {
    embed_type : string;
    cid : string;
    playlist : string;
    thumbnail : string option;
    alt : string option;
    aspect_ratio : aspect_ratio option;
  }

  type gallery_image = {
    image : image;
    alt : string;
    aspect_ratio : aspect_ratio option;
  }

  type gallery_embed = { embed_type : string; items : gallery_image list }

  type gallery_view_image = {
    thumbnail : string;
    fullsize : string;
    alt : string;
    aspect_ratio : aspect_ratio option;
  }

  type gallery_view_embed = {
    embed_type : string;
    items : gallery_view_image list;
  }

  type view_not_found = { uri : string; not_found : bool }

  type view_blocked = {
    uri : string;
    blocked : bool;
    author_did : string option;
  }

  type view_detached = { uri : string; detached : bool }

  type named_view = {
    type_ : string;
    uri : string;
    cid : string;
    original : Yojson.Safe.t;
  }

  type media =
    [ `Image of image_embed
    | `ImageView of image_view_embed
    | `External of ext_embed
    | `ExternalView of ext_view_embed
    | `Video of video_embed
    | `VideoView of video_view_embed
    | `Gallery of gallery_embed
    | `GalleryView of gallery_view_embed ]

  type record_with_media = {
    embed_type : string;
    record : record_embed;
    media : media;
  }

  type view_record = {
    uri : string;
    cid : string;
    author_did : string option;
    author_handle : string option;
    value : Yojson.Safe.t;
    labels : string list option;
    reply_count : int option;
    repost_count : int option;
    like_count : int option;
    quote_count : int option;
    embeds : embed list;
    indexed_at : string;
  }

  and record_view_union =
    [ `ViewRecord of view_record
    | `ViewNotFound of view_not_found
    | `ViewBlocked of view_blocked
    | `ViewDetached of view_detached
    | `NamedView of named_view
    | `Unknown of Yojson.Safe.t ]

  and record_view_embed = { embed_type : string; record : record_view_union }

  and embed =
    [ `Image of image_embed
    | `ImageView of image_view_embed
    | `External of ext_embed
    | `ExternalView of ext_view_embed
    | `Record of record_embed
    | `RecordView of record_view_embed
    | `RecordWithMedia of record_with_media
    | `Video of video_embed
    | `VideoView of video_view_embed
    | `Gallery of gallery_embed
    | `GalleryView of gallery_view_embed
    | `Unknown of Yojson.Safe.t ]

  type embed_external_view = {
    view : ext_view_embed option;
    associated_refs : strong_ref list;
    associated_records : Yojson.Safe.t list;
  }

  let string_member json field =
    match Yojson.Safe.Util.member field json with `String s -> s | _ -> ""

  let string_opt json field =
    match Yojson.Safe.Util.member field json with
    | `String s -> Some s
    | _ -> None

  let int_member json field =
    match Yojson.Safe.Util.member field json with `Int n -> n | _ -> 0

  let parse_ref json : ref =
    let open Yojson.Safe.Util in
    let ref_link =
      match json |> member "$link" with
      | `String s -> s
      | _ -> ( match json with `String s -> s | _ -> "")
    in
    { ref_link }

  let parse_image json : image =
    let open Yojson.Safe.Util in
    let image_type = string_member json "$type" in
    let ref = json |> member "ref" |> parse_ref in
    let mime_type = string_member json "mimeType" in
    let size = int_member json "size" in
    { image_type; ref; mime_type; size }

  let parse_thumb json : thumb =
    let image_type = string_member json "$type" in
    let ref = Yojson.Safe.Util.member "ref" json |> parse_ref in
    let mime_type = string_member json "mimeType" in
    let size = int_member json "size" in
    { thumb_type = image_type; ref; mime_type; size }

  let parse_ext json : ext =
    let open Yojson.Safe.Util in
    let uri = string_member json "uri" in
    let thumb = json |> member "thumb" |> parse_thumb in
    let title = string_member json "title" in
    let description = string_member json "description" in
    { uri; thumb; title; description }

  let parse_ext_view json : ext_view =
    {
      uri = string_member json "uri";
      title = string_member json "title";
      description = string_member json "description";
      thumb = string_member json "thumb";
    }

  let parse_ext_embed json : ext_embed =
    let open Yojson.Safe.Util in
    let embed_type = string_member json "$type" in
    let ext = json |> member "external" |> parse_ext in
    { embed_type; ext }

  let parse_ext_view_embed json : ext_view_embed =
    let open Yojson.Safe.Util in
    let embed_type = string_member json "$type" in
    let ext = json |> member "external" |> parse_ext_view in
    { embed_type; ext }

  let parse_image_data json : image_data =
    let open Yojson.Safe.Util in
    let alt = string_member json "alt" in
    let image = json |> member "image" |> parse_image in
    { alt; image }

  let parse_image_view json : image_view =
    {
      thumb = string_member json "thumb";
      fullsize = string_member json "fullsize";
      alt = string_member json "alt";
    }

  let parse_image_embed json : image_embed =
    let open Yojson.Safe.Util in
    let embed_type = string_member json "$type" in
    let images =
      match json |> member "images" with
      | `List items -> List.map parse_image_data items
      | _ -> []
    in
    { embed_type; images }

  let parse_image_view_embed json : image_view_embed =
    let open Yojson.Safe.Util in
    let embed_type = string_member json "$type" in
    let images =
      match json |> member "images" with
      | `List items -> List.map parse_image_view items
      | _ -> []
    in
    { embed_type; images }

  let check_for_field field json =
    match json with
    | `Assoc fields -> List.exists (fun (key, _) -> key = field) fields
    | _ -> false

  let parse_strong_ref json : strong_ref =
    { uri = string_member json "uri"; cid = string_member json "cid" }

  let parse_record_embed json : record_embed =
    let open Yojson.Safe.Util in
    let embed_type = string_member json "$type" in
    let record =
      match json |> member "record" with
      | `Assoc _ as rec_ ->
          if check_for_field "uri" rec_ then parse_strong_ref rec_
          else
            (* embed.record nests the strongRef one more level *)
            parse_strong_ref (rec_ |> member "record")
      | _ -> parse_strong_ref json
    in
    { embed_type; record }

  let parse_aspect_ratio json : aspect_ratio option =
    match json with
    | `Assoc _ ->
        let w = int_member json "width" in
        let h = int_member json "height" in
        if w = 0 && h = 0 then None else Some { width = w; height = h }
    | _ -> None

  let parse_video_blob json : video_blob =
    let open Yojson.Safe.Util in
    let cid =
      match json |> member "ref" with
      | `Assoc _ as ref_ -> (
          match ref_ |> member "$link" with `String s -> s | _ -> "")
      | `String s -> s
      | _ -> string_member json "cid"
    in
    {
      cid;
      mime_type = string_member json "mimeType";
      size = int_member json "size";
      original = json;
    }

  let check_field_is_string field json : bool =
    match Yojson.Safe.Util.member field json with
    | `String _ -> true
    | _ -> false

  let parse_video_embed json : video_embed =
    let open Yojson.Safe.Util in
    {
      embed_type = string_member json "$type";
      video = json |> member "video" |> parse_video_blob;
      alt = string_opt json "alt";
      aspect_ratio = parse_aspect_ratio (json |> member "aspectRatio");
    }

  let parse_video_view_embed json : video_view_embed =
    {
      embed_type = string_member json "$type";
      cid = string_member json "cid";
      playlist = string_member json "playlist";
      thumbnail = string_opt json "thumbnail";
      alt = string_opt json "alt";
      aspect_ratio =
        parse_aspect_ratio (Yojson.Safe.Util.member "aspectRatio" json);
    }

  let parse_to_correct_external_type json =
    let open Yojson.Safe.Util in
    let thumb_check =
      check_field_is_string "thumb" (json |> member "external")
    in
    match thumb_check with
    | false -> `External (parse_ext_embed json)
    | true -> `ExternalView (parse_ext_view_embed json)

  let parse_to_correct_image_type json =
    let open Yojson.Safe.Util in
    match json |> member "images" with
    | `List (hd :: _) ->
        if check_for_field "image" hd then `Image (parse_image_embed json)
        else `ImageView (parse_image_view_embed json)
    | _ -> `ImageView (parse_image_view_embed json)

  let parse_to_correct_video_type json =
    if check_for_field "playlist" json || check_for_field "cid" json then
      `VideoView (parse_video_view_embed json)
    else `Video (parse_video_embed json)

  let parse_gallery_image json : gallery_image =
    let open Yojson.Safe.Util in
    {
      image = json |> member "image" |> parse_image;
      alt = string_member json "alt";
      aspect_ratio = parse_aspect_ratio (json |> member "aspectRatio");
    }

  let parse_gallery_view_image json : gallery_view_image =
    let open Yojson.Safe.Util in
    {
      thumbnail = string_member json "thumbnail";
      fullsize = string_member json "fullsize";
      alt = string_member json "alt";
      aspect_ratio = parse_aspect_ratio (json |> member "aspectRatio");
    }

  let parse_gallery_embed json : gallery_embed =
    let open Yojson.Safe.Util in
    {
      embed_type = string_member json "$type";
      items =
        (match json |> member "items" with
        | `List xs -> List.map parse_gallery_image xs
        | _ -> []);
    }

  let parse_gallery_view_embed json : gallery_view_embed =
    let open Yojson.Safe.Util in
    {
      embed_type = string_member json "$type";
      items =
        (match json |> member "items" with
        | `List xs -> List.map parse_gallery_view_image xs
        | _ -> []);
    }

  let parse_to_correct_gallery_type json =
    let open Yojson.Safe.Util in
    match json |> member "items" with
    | `List (hd :: _) ->
        if check_for_field "image" hd then `Gallery (parse_gallery_embed json)
        else `GalleryView (parse_gallery_view_embed json)
    | _ ->
        let typ = string_member json "$type" in
        if typ = "app.bsky.embed.gallery#view" then
          `GalleryView (parse_gallery_view_embed json)
        else `Gallery (parse_gallery_embed json)

  let type_suffix suffix typ =
    let n = String.length suffix in
    String.length typ >= n && String.sub typ (String.length typ - n) n = suffix

  let rec parse_media json : media =
    match parse_embed json with
    | `Image e -> `Image e
    | `ImageView e -> `ImageView e
    | `External e -> `External e
    | `ExternalView e -> `ExternalView e
    | `Video e -> `Video e
    | `VideoView e -> `VideoView e
    | `Gallery e -> `Gallery e
    | `GalleryView e -> `GalleryView e
    | _ -> `External (parse_ext_embed json)

  and parse_record_with_media json : record_with_media =
    let open Yojson.Safe.Util in
    {
      embed_type = string_member json "$type";
      record = parse_record_embed json;
      media = parse_media (json |> member "media");
    }

  and parse_view_record json : view_record =
    let open Yojson.Safe.Util in
    let author = json |> member "author" in
    {
      uri = string_member json "uri";
      cid = string_member json "cid";
      author_did = string_opt author "did";
      author_handle = string_opt author "handle";
      value = json |> member "value";
      labels = Label.Label.parse_label_values (json |> member "labels");
      reply_count =
        (match json |> member "replyCount" with `Int n -> Some n | _ -> None);
      repost_count =
        (match json |> member "repostCount" with `Int n -> Some n | _ -> None);
      like_count =
        (match json |> member "likeCount" with `Int n -> Some n | _ -> None);
      quote_count =
        (match json |> member "quoteCount" with `Int n -> Some n | _ -> None);
      embeds =
        (match json |> member "embeds" with
        | `List xs -> List.map parse_embed xs
        | _ -> []);
      indexed_at = string_member json "indexedAt";
    }

  and parse_record_view_union json : record_view_union =
    let typ = string_member json "$type" in
    if typ = "app.bsky.embed.record#viewRecord" || type_suffix "#viewRecord" typ
    then `ViewRecord (parse_view_record json)
    else if
      typ = "app.bsky.embed.record#viewNotFound"
      || type_suffix "#viewNotFound" typ
    then `ViewNotFound { uri = string_member json "uri"; not_found = true }
    else if
      typ = "app.bsky.embed.record#viewBlocked"
      || type_suffix "#viewBlocked" typ
    then
      let author_did =
        match Yojson.Safe.Util.member "author" json with
        | `Assoc _ as a -> string_opt a "did"
        | _ -> None
      in
      `ViewBlocked
        { uri = string_member json "uri"; blocked = true; author_did }
    else if
      typ = "app.bsky.embed.record#viewDetached"
      || type_suffix "#viewDetached" typ
    then `ViewDetached { uri = string_member json "uri"; detached = true }
    else if
      type_suffix "#generatorView" typ
      || type_suffix "#listView" typ
      || type_suffix "#labelerView" typ
      || type_suffix "#starterPackViewBasic" typ
      || type_suffix "#starterPackView" typ
    then
      `NamedView
        {
          type_ = typ;
          uri = string_member json "uri";
          cid = string_member json "cid";
          original = json;
        }
    else if check_for_field "notFound" json then
      `ViewNotFound { uri = string_member json "uri"; not_found = true }
    else if check_for_field "blocked" json then
      `ViewBlocked
        { uri = string_member json "uri"; blocked = true; author_did = None }
    else if check_for_field "detached" json then
      `ViewDetached { uri = string_member json "uri"; detached = true }
    else if check_for_field "value" json && check_for_field "uri" json then
      `ViewRecord (parse_view_record json)
    else `Unknown json

  and parse_record_view_embed json : record_view_embed =
    let open Yojson.Safe.Util in
    {
      embed_type = string_member json "$type";
      record = parse_record_view_union (json |> member "record");
    }

  and parse_embed json : embed =
    let typ = string_member json "$type" in
    if typ = "app.bsky.embed.video" || typ = "app.bsky.embed.video#main" then
      `Video (parse_video_embed json)
    else if typ = "app.bsky.embed.video#view" then
      `VideoView (parse_video_view_embed json)
    else if
      typ = "app.bsky.embed.recordWithMedia"
      || typ = "app.bsky.embed.recordWithMedia#main"
      || typ = "app.bsky.embed.recordWithMedia#view"
    then `RecordWithMedia (parse_record_with_media json)
    else if typ = "app.bsky.embed.record#view" then
      `RecordView (parse_record_view_embed json)
    else if typ = "app.bsky.embed.record" || typ = "app.bsky.embed.record#main"
    then `Record (parse_record_embed json)
    else if typ = "app.bsky.embed.images" || typ = "app.bsky.embed.images#main"
    then parse_to_correct_image_type json
    else if typ = "app.bsky.embed.images#view" then
      `ImageView (parse_image_view_embed json)
    else if
      typ = "app.bsky.embed.gallery" || typ = "app.bsky.embed.gallery#main"
    then parse_to_correct_gallery_type json
    else if typ = "app.bsky.embed.gallery#view" then
      `GalleryView (parse_gallery_view_embed json)
    else if
      typ = "app.bsky.embed.external" || typ = "app.bsky.embed.external#main"
    then parse_to_correct_external_type json
    else if typ = "app.bsky.embed.external#view" then
      `ExternalView (parse_ext_view_embed json)
    else if check_for_field "images" json then parse_to_correct_image_type json
    else if check_for_field "items" json then parse_to_correct_gallery_type json
    else if check_for_field "external" json then
      parse_to_correct_external_type json
    else if check_for_field "video" json || check_for_field "playlist" json then
      parse_to_correct_video_type json
    else if check_for_field "record" json && check_for_field "media" json then
      `RecordWithMedia (parse_record_with_media json)
    else if check_for_field "record" json then
      let inner = Yojson.Safe.Util.member "record" json in
      if
        check_for_field "value" inner
        || check_for_field "notFound" inner
        || check_for_field "blocked" inner
        || check_for_field "detached" inner
      then `RecordView (parse_record_view_embed json)
      else `Record (parse_record_embed json)
    else `Unknown json

  let parse_embed_option json : embed option =
    let open Yojson.Safe.Util in
    match json |> member "embed" with
    | `Null -> None
    | `Assoc _ as inner -> ( try Some (parse_embed inner) with _ -> None)
    | _ -> None

  let blob_to_json ?(type_ = "blob") ~cid ~mime_type ~size () : Yojson.Safe.t =
    `Assoc
      [
        ("$type", `String type_);
        ("ref", `Assoc [ ("$link", `String cid) ]);
        ("mimeType", `String mime_type);
        ("size", `Int size);
      ]

  let strong_ref_to_json (r : strong_ref) : Yojson.Safe.t =
    `Assoc [ ("uri", `String r.uri); ("cid", `String r.cid) ]

  let aspect_ratio_to_json (a : aspect_ratio) : Yojson.Safe.t =
    `Assoc [ ("width", `Int a.width); ("height", `Int a.height) ]

  let image_to_json (i : image) : Yojson.Safe.t =
    blob_to_json
      ~type_:(if i.image_type = "" then "blob" else i.image_type)
      ~cid:i.ref.ref_link ~mime_type:i.mime_type ~size:i.size ()

  let rec embed_to_json = function
    | `Image (e : image_embed) ->
        `Assoc
          [
            ( "$type",
              `String
                (if e.embed_type = "" then "app.bsky.embed.images"
                 else e.embed_type) );
            ( "images",
              `List
                (List.map
                   (fun (d : image_data) ->
                     `Assoc
                       [
                         ("alt", `String d.alt); ("image", image_to_json d.image);
                       ])
                   e.images) );
          ]
    | `ImageView (e : image_view_embed) ->
        `Assoc
          [
            ("$type", `String "app.bsky.embed.images#view");
            ( "images",
              `List
                (List.map
                   (fun (v : image_view) ->
                     `Assoc
                       [
                         ("thumb", `String v.thumb);
                         ("fullsize", `String v.fullsize);
                         ("alt", `String v.alt);
                       ])
                   e.images) );
          ]
    | `External (e : ext_embed) ->
        `Assoc
          [
            ( "$type",
              `String
                (if e.embed_type = "" then "app.bsky.embed.external"
                 else e.embed_type) );
            ( "external",
              `Assoc
                [
                  ("uri", `String e.ext.uri);
                  ("title", `String e.ext.title);
                  ("description", `String e.ext.description);
                  ( "thumb",
                    blob_to_json
                      ~type_:
                        (if e.ext.thumb.thumb_type = "" then "blob"
                         else e.ext.thumb.thumb_type)
                      ~cid:e.ext.thumb.ref.ref_link
                      ~mime_type:e.ext.thumb.mime_type ~size:e.ext.thumb.size ()
                  );
                ] );
          ]
    | `ExternalView (e : ext_view_embed) ->
        `Assoc
          [
            ("$type", `String "app.bsky.embed.external#view");
            ( "external",
              `Assoc
                [
                  ("uri", `String e.ext.uri);
                  ("title", `String e.ext.title);
                  ("description", `String e.ext.description);
                  ("thumb", `String e.ext.thumb);
                ] );
          ]
    | `Record (e : record_embed) ->
        `Assoc
          [
            ( "$type",
              `String
                (if e.embed_type = "" then "app.bsky.embed.record"
                 else e.embed_type) );
            ("record", strong_ref_to_json e.record);
          ]
    | `RecordView (e : record_view_embed) ->
        `Assoc
          [
            ("$type", `String "app.bsky.embed.record#view");
            ("record", record_view_union_to_json e.record);
          ]
    | `RecordWithMedia (e : record_with_media) ->
        `Assoc
          [
            ( "$type",
              `String
                (if e.embed_type = "" then "app.bsky.embed.recordWithMedia"
                 else e.embed_type) );
            ( "record",
              `Assoc
                [
                  ("$type", `String "app.bsky.embed.record");
                  ("record", strong_ref_to_json e.record.record);
                ] );
            ("media", media_to_json e.media);
          ]
    | `Video (e : video_embed) ->
        let fields =
          [
            ( "$type",
              `String
                (if e.embed_type = "" then "app.bsky.embed.video"
                 else e.embed_type) );
            ( "video",
              blob_to_json ~cid:e.video.cid ~mime_type:e.video.mime_type
                ~size:e.video.size () );
          ]
          @ (match e.alt with Some a -> [ ("alt", `String a) ] | None -> [])
          @
          match e.aspect_ratio with
          | Some a -> [ ("aspectRatio", aspect_ratio_to_json a) ]
          | None -> []
        in
        `Assoc fields
    | `VideoView (e : video_view_embed) ->
        let fields =
          [
            ("$type", `String "app.bsky.embed.video#view");
            ("cid", `String e.cid);
            ("playlist", `String e.playlist);
          ]
          @ (match e.thumbnail with
            | Some t -> [ ("thumbnail", `String t) ]
            | None -> [])
          @ (match e.alt with Some a -> [ ("alt", `String a) ] | None -> [])
          @
          match e.aspect_ratio with
          | Some a -> [ ("aspectRatio", aspect_ratio_to_json a) ]
          | None -> []
        in
        `Assoc fields
    | `Gallery (e : gallery_embed) ->
        `Assoc
          [
            ( "$type",
              `String
                (if e.embed_type = "" then "app.bsky.embed.gallery"
                 else e.embed_type) );
            ( "items",
              `List
                (List.map
                   (fun (item : gallery_image) ->
                     let fields =
                       [
                         ("image", image_to_json item.image);
                         ("alt", `String item.alt);
                       ]
                       @
                       match item.aspect_ratio with
                       | Some a -> [ ("aspectRatio", aspect_ratio_to_json a) ]
                       | None -> []
                     in
                     `Assoc fields)
                   e.items) );
          ]
    | `GalleryView (e : gallery_view_embed) ->
        `Assoc
          [
            ("$type", `String "app.bsky.embed.gallery#view");
            ( "items",
              `List
                (List.map
                   (fun (item : gallery_view_image) ->
                     let fields =
                       [
                         ("thumbnail", `String item.thumbnail);
                         ("fullsize", `String item.fullsize);
                         ("alt", `String item.alt);
                       ]
                       @
                       match item.aspect_ratio with
                       | Some a -> [ ("aspectRatio", aspect_ratio_to_json a) ]
                       | None -> []
                     in
                     `Assoc fields)
                   e.items) );
          ]
    | `Unknown json -> json

  and media_to_json (m : media) : Yojson.Safe.t =
    match m with
    | `Image e -> embed_to_json (`Image e)
    | `ImageView e -> embed_to_json (`ImageView e)
    | `External e -> embed_to_json (`External e)
    | `ExternalView e -> embed_to_json (`ExternalView e)
    | `Video e -> embed_to_json (`Video e)
    | `VideoView e -> embed_to_json (`VideoView e)
    | `Gallery e -> embed_to_json (`Gallery e)
    | `GalleryView e -> embed_to_json (`GalleryView e)

  and record_view_union_to_json = function
    | `ViewRecord v ->
        let fields =
          [
            ("$type", `String "app.bsky.embed.record#viewRecord");
            ("uri", `String v.uri);
            ("cid", `String v.cid);
            ("value", v.value);
            ("indexedAt", `String v.indexed_at);
          ]
          @ (match v.author_did with
            | Some did ->
                [
                  ( "author",
                    `Assoc
                      ([ ("did", `String did) ]
                      @
                      match v.author_handle with
                      | Some h -> [ ("handle", `String h) ]
                      | None -> []) );
                ]
            | None -> [])
          @
          match v.embeds with
          | [] -> []
          | xs -> [ ("embeds", `List (List.map embed_to_json xs)) ]
        in
        `Assoc fields
    | `ViewNotFound { uri; _ } ->
        `Assoc
          [
            ("$type", `String "app.bsky.embed.record#viewNotFound");
            ("uri", `String uri);
            ("notFound", `Bool true);
          ]
    | `ViewBlocked { uri; author_did; _ } ->
        let fields =
          [
            ("$type", `String "app.bsky.embed.record#viewBlocked");
            ("uri", `String uri);
            ("blocked", `Bool true);
          ]
          @
          match author_did with
          | Some did -> [ ("author", `Assoc [ ("did", `String did) ]) ]
          | None -> []
        in
        `Assoc fields
    | `ViewDetached { uri; _ } ->
        `Assoc
          [
            ("$type", `String "app.bsky.embed.record#viewDetached");
            ("uri", `String uri);
            ("detached", `Bool true);
          ]
    | `NamedView { original; _ } -> original
    | `Unknown json -> json

  let parse_embed_external_view json : embed_external_view =
    let open Yojson.Safe.Util in
    {
      view =
        (match json |> member "view" with
        | `Assoc _ as v -> Some (parse_ext_view_embed v)
        | _ -> None);
      associated_refs =
        (match json |> member "associatedRefs" with
        | `List xs -> List.map parse_strong_ref xs
        | _ -> []);
      associated_records =
        (match json |> member "associatedRecords" with
        | `List xs -> xs
        | _ -> []);
    }

  let get_embed_external_view ?session ?host ~url ~uris () : embed_external_view
      =
    Client.Client.get_json ?session ?host "app.bsky.embed.getEmbedExternalView"
      (("url", url) :: Client.Client.repeat_param "uris" uris)
    |> parse_embed_external_view
end
