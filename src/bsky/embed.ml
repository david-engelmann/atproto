module Embed = struct
    (*
     * external
     * images
     * record
     * record_with_media
     *
     * need to parse embed similar to notification but with that bit about if
     * the field is missing
     *
     * *)
  type ref =
    {
     ref_link : string;
    }

  type image =
    {
      image_type : string;
      ref : ref;
      mime_type : string;
      size : int;
    }

  type thumb =
    {
      thumb_type : string;
      ref : ref;
      mime_type : string;
      size : int;

    }

  type ext =
    {
      uri : string;
      thumb : thumb;
      title : string;
      description : string;
    }

  type ext_view =
    {
     uri : string;
     title : string;
     description : string;
     thumb : string;
    }

  type image_data =
    {
      alt : string;
      image : image;
    }

  type image_view =
    {
      thumb : string;
      fullsize : string;
      alt : string;
    }

  type image_embed =
    {
      embed_type : string;
      images : image_data list;
    }

  type image_view_embed =
    {
      embed_type : string;
      images : image_view list;
    }

  type ext_embed =
    {
      embed_type : string;
      ext : ext;
    }

  type ext_view_embed =
    {
      embed_type : string;
      ext : ext_view;
    }

  type embed = (* GONNA CHANGE TO | style *)
    [
    | `Image of image_embed
    | `ImageView of image_view_embed
    | `External of ext_embed
    | `ExternalView of ext_view_embed
    ]

  let parse_ref json : ref =
    let open Yojson.Safe.Util in
    let ref_link = json |> member "$link" |> to_string in
    { ref_link }

  let parse_image json : image =
    let open Yojson.Safe.Util in
    let image_type = json |> member "$type" |> to_string in
    let ref = json |> member "ref" |> parse_ref in
    let mime_type = json |> member "mimeType" |> to_string in
    let size = json |> member "size" |> to_int in
    { image_type; ref; mime_type; size }

  let parse_thumb json : thumb =
    let open Yojson.Safe.Util in
    let thumb_type = json |> member "$type" |> to_string in
    let ref = json |> member "ref" |> parse_ref in
    let mime_type = json |> member "mimeType" |> to_string in
    let size = json |> member "size" |> to_int in
    { thumb_type; ref; mime_type; size }

  let parse_ext json : ext =
    let open Yojson.Safe.Util in
    let uri = json |> member "uri" |> to_string in
    let thumb = json |> member "thumb" |> parse_thumb in
    let title = json |> member "title" |> to_string in
    let description = json |> member "description" |> to_string in
    { uri; thumb; title; description }

  let parse_ext_view json : ext_view =
    let open Yojson.Safe.Util in
    let uri = json |> member "uri" |> to_string in
    let title = json |> member "title" |> to_string in
    let description = json |> member "description" |> to_string in
    let thumb = json |> member "thumb" |> to_string in
    { uri; title; description; thumb }

  let parse_ext_embed json : ext_embed =
    let open Yojson.Safe.Util in
    let embed_type = json |> member "$type" |> to_string in
    let ext = json |> member "external" |> parse_ext in
    { embed_type; ext }

  let parse_ext_view_embed json : ext_view_embed =
    let open Yojson.Safe.Util in
    let embed_type = json |> member "$type" |> to_string in
    let ext = json |> member "external" |> parse_ext_view in
    { embed_type; ext }

  let parse_image_data json : image_data =
    let open Yojson.Safe.Util in
    let alt = json |> member "alt" |> to_string in
    let image = json |> member "image" |> parse_image in
    { alt; image }

  let parse_image_view json : image_view =
    let open Yojson.Safe.Util in
    let thumb = json |> member "thumb" |> to_string in
    let fullsize = json |> member "fullsize" |> to_string in
    let alt = json |> member "alt" |> to_string in
    { thumb; fullsize; alt }

  let parse_image_embed json : image_embed =
    let open Yojson.Safe.Util in
    let embed_type = json |> member "$type" |> to_string in
    let images = json |> member "images" |> to_list |> List.map parse_image_data in
    { embed_type; images }

  let parse_image_view_embed json : image_view_embed =
    let open Yojson.Safe.Util in
    let embed_type = json |> member "$type" |> to_string in
    let images = json |> member "images" |> to_list |> List.map parse_image_view in
    { embed_type; images }
end
