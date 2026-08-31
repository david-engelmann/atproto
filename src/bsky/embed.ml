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

  type media =
    [ `Image of image_embed
    | `ImageView of image_view_embed
    | `External of ext_embed
    | `ExternalView of ext_view_embed
    | `Video of video_embed
    | `VideoView of video_view_embed ]

  type record_with_media = {
    embed_type : string;
    record : record_embed;
    media : media;
  }

  type embed =
    [ `Image of image_embed
    | `ImageView of image_view_embed
    | `External of ext_embed
    | `ExternalView of ext_view_embed
    | `Record of record_embed
    | `RecordWithMedia of record_with_media
    | `Video of video_embed
    | `VideoView of video_view_embed
    | `Unknown of Yojson.Safe.t ]

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

  let rec parse_media json : media =
    match parse_embed json with
    | `Image e -> `Image e
    | `ImageView e -> `ImageView e
    | `External e -> `External e
    | `ExternalView e -> `ExternalView e
    | `Video e -> `Video e
    | `VideoView e -> `VideoView e
    | _ -> `External (parse_ext_embed json)

  and parse_record_with_media json : record_with_media =
    let open Yojson.Safe.Util in
    {
      embed_type = string_member json "$type";
      record = parse_record_embed json;
      media = parse_media (json |> member "media");
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
    else if typ = "app.bsky.embed.record" || typ = "app.bsky.embed.record#main"
    then `Record (parse_record_embed json)
    else if typ = "app.bsky.embed.images" || typ = "app.bsky.embed.images#main"
    then parse_to_correct_image_type json
    else if typ = "app.bsky.embed.images#view" then
      `ImageView (parse_image_view_embed json)
    else if
      typ = "app.bsky.embed.external" || typ = "app.bsky.embed.external#main"
    then parse_to_correct_external_type json
    else if typ = "app.bsky.embed.external#view" then
      `ExternalView (parse_ext_view_embed json)
    else if check_for_field "images" json then parse_to_correct_image_type json
    else if check_for_field "external" json then
      parse_to_correct_external_type json
    else if check_for_field "video" json || check_for_field "playlist" json then
      parse_to_correct_video_type json
    else if check_for_field "record" json && check_for_field "media" json then
      `RecordWithMedia (parse_record_with_media json)
    else if check_for_field "record" json then `Record (parse_record_embed json)
    else `Unknown json

  let parse_embed_option json : embed option =
    let open Yojson.Safe.Util in
    match json |> member "embed" with
    | `Null -> None
    | `Assoc _ as inner -> ( try Some (parse_embed inner) with _ -> None)
    | _ -> None
end
