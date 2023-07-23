module Embed = struct
    (*
     * external
     * images
     * record
     * record_with_media
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

  type image_data =
    {
      alt : string;
      image : image;
    }

  type image_embed =
    {
      embed_type : string;
      images : image_data list;
    }

  type embed = (* GONNA CHANGE TO | style *)
    [
    | `Image of image_embed
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

  let parse_image_data json : image_data =
    let open Yojson.Safe.Util in
    let alt = json |> member "alt" |> to_string in
    let image = json |> member "image" |> parse_image in
    { alt; image }

  let parse_image_embed json : image_embed =
    let open Yojson.Safe.Util in
    let embed_type = json |> member "$type" |> to_string in
    let images = json |> member "images" |> to_list |> List.map parse_image_data in
    { embed_type; images }
end
