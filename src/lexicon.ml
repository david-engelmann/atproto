(** Lexicon 1 documents — https://atproto.com/specs/lexicon *)
module Lexicon = struct
  type primitive =
    | Boolean
    | Number
    | Integer
    | String
    | Ref
    | Union
    | Unknown
    | Cid_link
    | Bytes

  type definition =
    | Record
    | Query
    | Procedure
    | Subscription
    | Params
    | Token
    | Object
    | Blob
    | Array
    | String_def
    | Unknown_def of string

  type def = {
    name : string;
    kind : definition;
    description : string option;
    required : string list;
    properties : (string * primitive) list;
  }

  type document = {
    lexicon : int;
    id : string;
    description : string option;
    defs : def list;
  }

  let lookup_primitive prim =
    match prim with
    | "boolean" -> Boolean
    | "number" -> Number
    | "integer" -> Integer
    | "string" -> String
    | "ref" -> Ref
    | "union" -> Union
    | "unknown" -> Unknown
    | "cid-link" -> Cid_link
    | "bytes" -> Bytes
    | _ -> Unknown

  let lookup_definition def =
    match def with
    | "record" -> Record
    | "query" -> Query
    | "procedure" -> Procedure
    | "subscription" -> Subscription
    | "params" -> Params
    | "token" -> Token
    | "object" -> Object
    | "blob" -> Blob
    | "array" -> Array
    | "string" -> String_def
    | other -> Unknown_def other

  let string_opt json field =
    match Yojson.Safe.Util.member field json with
    | `String s -> Some s
    | _ -> None

  let parse_property json : primitive =
    let open Yojson.Safe.Util in
    match json |> member "type" with
    | `String "ref" -> Ref
    | `String "union" -> Union
    | `String t -> lookup_primitive t
    | _ -> Unknown

  let parse_properties json : (string * primitive) list =
    match Yojson.Safe.Util.member "properties" json with
    | `Assoc fields ->
        List.map (fun (name, body) -> (name, parse_property body)) fields
    | _ -> []

  let parse_required json : string list =
    match Yojson.Safe.Util.member "required" json with
    | `List items ->
        List.filter_map (function `String s -> Some s | _ -> None) items
    | _ -> []

  let parse_def name json : def =
    let open Yojson.Safe.Util in
    let kind =
      match json |> member "type" with
      | `String t -> lookup_definition t
      | _ -> Unknown_def "unknown"
    in
    {
      name;
      kind;
      description = string_opt json "description";
      required = parse_required json;
      properties = parse_properties json;
    }

  let of_json json : document =
    let open Yojson.Safe.Util in
    let lexicon = match json |> member "lexicon" with `Int n -> n | _ -> 1 in
    let id =
      match json |> member "id" with
      | `String s -> s
      | _ -> failwith "Lexicon.of_json: missing id"
    in
    let defs =
      match json |> member "defs" with
      | `Assoc fields ->
          List.map (fun (name, body) -> parse_def name body) fields
      | _ -> []
    in
    { lexicon; id; description = string_opt json "description"; defs }

  let of_string (body : string) : document =
    of_json (Yojson.Safe.from_string body)

  let main (doc : document) : def option =
    List.find_opt (fun d -> d.name = "main") doc.defs
end
