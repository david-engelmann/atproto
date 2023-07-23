module Facet = struct
  (* link, mention
   * link has no type in the facet list but it says link in the features->$type, use uri
   * mention has type in the facet list and in features->$type, use did
   *
   * *)
  type facet_index =
    {
      byte_end : int;
      byte_start : int;
    }

  type mention_feature =
    {
      did : string;
      mention_type : string;
    }

  type link_feature =
    {
      cid : string;
      link_type : string;
    }

  type mention_facet =
    {
      facet_type : string;
      facet_index : facet_index;
      features : mention_feature list;
    }

  type link_facet =
    {
      facet_index : facet_index;
      features : link_feature list;
    }

  type facet =
    [
    | `Mention of mention_facet
    | `Link of link_facet
    ]

  let parse_link_feature json : link_feature =
    let open Yojson.Safe.Util in
    let cid = json |> member "cid" |> to_string in
    let link_type = json |> member "$type" |> to_string in
    { cid; link_type }

  let parse_mention_feature json : mention_feature =
    let open Yojson.Safe.Util in
    let did = json |> member "did" |> to_string in
    let mention_type = json |> member "$type" |> to_string in
    { did; mention_type }

  let parse_facet_index json : facet_index =
    let open Yojson.Safe.Util in
    let byte_end = json |> member "byteEnd" |> to_int in
    let byte_start = json |> member "byteStart" |> to_int in
    { byte_end; byte_start }

  let convert_body_to_json (body : string) : Yojson.Safe.t =
    let json = Yojson.Safe.from_string body in
    json

  let check_for_facet_type_presence json =
    match Yojson.Safe.Util.member "$type" json with
    | `Null -> false
    | _ -> true

  let parse_facet json : facet =
    let open Yojson.Safe.Util in
    let facet_check = check_for_facet_type_presence json in
    match facet_check with
    | false ->
      let facet_index = json |> member "index" |> parse_facet_index in
      let features = json |> member "features" |> to_list |> List.map parse_link_feature in
      `Link { facet_index; features }
    | true ->
      let facet_type = json |> member "$type" |> to_string in
      let facet_index = json |> member "index" |> parse_facet_index in
      let features = json |> member "features" |> to_list |> List.map parse_mention_feature in
      `Mention { facet_type; facet_index; features }
end
