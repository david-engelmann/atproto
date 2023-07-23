module Facet = struct
  (* link, mention *)
  type facet_index =
    {
      byte_end : int;
      byte_start : int;
    }

  let parse_facet_index json : facet_index =
    let open Yojson.Safe.Util in
    let byte_end = json |> member "byteEnd" |> to_int in
    let byte_start = json |> member "byteStart" |> to_int in
    { byte_end; byte_start }

  let convert_body_to_json (body : string) : Yojson.Safe.t =
    let json = Yojson.Safe.from_string body in
    json


end
