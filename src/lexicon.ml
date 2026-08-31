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

  type schema_shape = {
    encoding : string option;
    required : string list;
    properties : (string * primitive) list;
  }

  type def = {
    name : string;
    kind : definition;
    description : string option;
    required : string list;
    properties : (string * primitive) list;
    input : schema_shape;
    output : schema_shape;
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
    | `String "array" -> (
        match json |> member "items" with
        | `Assoc _ as items -> (
            match items |> member "type" with
            | `String "ref" -> Ref
            | `String "union" -> Union
            | `String t -> lookup_primitive t
            | _ -> Unknown)
        | _ -> Unknown)
    | `String "blob" -> Bytes
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

  let empty_shape : schema_shape =
    { encoding = None; required = []; properties = [] }

  let unwrap_schema json =
    match Yojson.Safe.Util.member "schema" json with
    | `Assoc _ as schema -> schema
    | _ -> json

  let parse_io_schema json field : schema_shape =
    match Yojson.Safe.Util.member field json with
    | `Assoc _ as obj ->
        let body = unwrap_schema obj in
        {
          encoding = string_opt obj "encoding";
          required = parse_required body;
          properties = parse_properties body;
        }
    | _ -> empty_shape

  let shape_json json =
    let open Yojson.Safe.Util in
    match json |> member "type" with
    | `String "record" -> (
        match json |> member "record" with `Assoc _ as obj -> obj | _ -> json)
    | `String "query" | `String "procedure" | `String "subscription" -> (
        match json |> member "parameters" with
        | `Assoc _ as obj -> obj
        | _ -> json)
    | _ -> json

  let parse_def name json : def =
    let open Yojson.Safe.Util in
    let kind =
      match json |> member "type" with
      | `String t -> lookup_definition t
      | _ -> Unknown_def "unknown"
    in
    let body = shape_json json in
    {
      name;
      kind;
      description = string_opt json "description";
      required = parse_required body;
      properties = parse_properties body;
      input = parse_io_schema json "input";
      output = parse_io_schema json "output";
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

  let ocaml_ident (s : string) : string =
    let buf = Buffer.create (String.length s) in
    String.iter
      (function
        | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') as c -> Buffer.add_char buf c
        | _ -> Buffer.add_char buf '_')
      s;
    let id = Buffer.contents buf in
    if id = "" then "lexicon"
    else if id.[0] >= '0' && id.[0] <= '9' then "_" ^ id
    else id

  let primitive_to_ocaml = function
    | Boolean -> "bool"
    | Number -> "float"
    | Integer -> "int"
    | String | Ref | Cid_link | Bytes -> "string"
    | Union | Unknown -> "Yojson.Safe.t"

  let kind_label = function
    | Record -> "record"
    | Query -> "query"
    | Procedure -> "procedure"
    | Subscription -> "subscription"
    | Params -> "params"
    | Token -> "token"
    | Object -> "object"
    | Blob -> "blob"
    | Array -> "array"
    | String_def -> "string"
    | Unknown_def s -> s

  let to_ocaml (doc : document) : string =
    let buf = Buffer.create 256 in
    let modname = String.capitalize_ascii (ocaml_ident doc.id) in
    Printf.bprintf buf "(** generated from %s *)\n" doc.id;
    Printf.bprintf buf "module %s = struct\n" modname;
    Printf.bprintf buf "  let id = %S\n" doc.id;
    Printf.bprintf buf "  let lexicon = %d\n\n" doc.lexicon;
    let emit_record buf indent name required props =
      match props with
      | [] -> Printf.bprintf buf "%stype %s = unit\n" indent name
      | props ->
          Printf.bprintf buf "%stype %s = {\n" indent name;
          List.iter
            (fun (n, prim) ->
              let optional = not (List.mem n required) in
              Printf.bprintf buf "%s  %s : %s%s;\n" indent (ocaml_ident n)
                (primitive_to_ocaml prim)
                (if optional then " option" else ""))
            props;
          Printf.bprintf buf "%s}\n" indent
    in
    List.iter
      (fun (d : def) ->
        let inner = String.capitalize_ascii (ocaml_ident d.name) in
        Printf.bprintf buf "  (** %s *)\n" (kind_label d.kind);
        Printf.bprintf buf "  module %s = struct\n" inner;
        Printf.bprintf buf "    let name = %S\n" d.name;
        emit_record buf "    " "t" d.required d.properties;
        (match d.input.encoding with
        | Some enc -> Printf.bprintf buf "    let input_encoding = %S\n" enc
        | None -> ());
        (match d.input.properties with
        | [] -> ()
        | props -> emit_record buf "    " "input" d.input.required props);
        (match d.output.encoding with
        | Some enc -> Printf.bprintf buf "    let output_encoding = %S\n" enc
        | None -> ());
        (match d.output.properties with
        | [] -> ()
        | props -> emit_record buf "    " "output" d.output.required props);
        Printf.bprintf buf "  end\n\n")
      doc.defs;
    Printf.bprintf buf "end\n";
    Buffer.contents buf

  let primitive_matches prim json =
    match (prim, json) with
    | Boolean, `Bool _ -> true
    | Number, (`Float _ | `Int _) -> true
    | Integer, (`Int _ | `Intlit _) -> true
    | (String | Ref | Cid_link | Bytes), `String _ -> true
    | (Union | Unknown), _ -> true
    | _ -> false

  let validate (d : def) (json : Yojson.Safe.t) : (unit, string) result =
    match json with
    | `Assoc fields ->
        let missing =
          List.filter (fun req -> not (List.mem_assoc req fields)) d.required
        in
        if missing <> [] then
          Error ("missing required: " ^ String.concat ", " missing)
        else
          let rec check = function
            | [] -> Ok ()
            | (name, prim) :: rest -> (
                match List.assoc_opt name fields with
                | None -> check rest
                | Some v ->
                    if primitive_matches prim v then check rest
                    else Error ("field " ^ name ^ " has the wrong type"))
          in
          check d.properties
    | _ -> Error "expected JSON object"
end
