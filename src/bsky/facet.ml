module Facet = struct
  type facet_index = { byte_end : int; byte_start : int }
  type mention_feature = { did : string; mention_type : string }
  type link_feature = { uri : string; cid : string; link_type : string }
  type tag_feature = { tag : string; tag_type : string }

  type mention_facet = {
    facet_type : string;
    facet_index : facet_index;
    features : mention_feature list;
  }

  type link_facet = { facet_index : facet_index; features : link_feature list }
  type tag_facet = { facet_index : facet_index; features : tag_feature list }

  type facet =
    [ `Mention of mention_facet | `Link of link_facet | `Tag of tag_facet ]

  let string_member json field =
    match Yojson.Safe.Util.member field json with `String s -> s | _ -> ""

  let parse_link_feature json : link_feature =
    let uri = string_member json "uri" in
    let cid = string_member json "cid" in
    let link_type = string_member json "$type" in
    { uri; cid; link_type }

  let parse_mention_feature json : mention_feature =
    let did = string_member json "did" in
    let mention_type = string_member json "$type" in
    { did; mention_type }

  let parse_tag_feature json : tag_feature =
    let tag = string_member json "tag" in
    let tag_type = string_member json "$type" in
    { tag; tag_type }

  let parse_facet_index json : facet_index =
    let open Yojson.Safe.Util in
    let byte_end = match json |> member "byteEnd" with `Int n -> n | _ -> 0 in
    let byte_start =
      match json |> member "byteStart" with `Int n -> n | _ -> 0
    in
    { byte_end; byte_start }

  let convert_body_to_json (body : string) : Yojson.Safe.t =
    let json = Yojson.Safe.from_string body in
    json

  let feature_type json = string_member json "$type"

  let is_type suffix json =
    let t = feature_type json in
    let n = String.length suffix in
    String.length t >= n && String.sub t (String.length t - n) n = suffix

  let parse_facet json : facet =
    let open Yojson.Safe.Util in
    let facet_index = json |> member "index" |> parse_facet_index in
    let features_json =
      match json |> member "features" with `List xs -> xs | _ -> []
    in
    let first = match features_json with hd :: _ -> hd | [] -> `Null in
    if is_type "#mention" first || is_type "mention" first then
      let facet_type = string_member json "$type" in
      let features = List.map parse_mention_feature features_json in
      `Mention { facet_type; facet_index; features }
    else if is_type "#tag" first || is_type "tag" first then
      let features = List.map parse_tag_feature features_json in
      `Tag { facet_index; features }
    else
      let features = List.map parse_link_feature features_json in
      `Link { facet_index; features }
end
