(** Lexicon 1 documents — https://atproto.com/specs/lexicon *)
module Lexicon = struct
  type primitive =
    | Boolean
    | Number
    | Integer
    | String
    | Ref
    | Union of string list
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
    | Permission_set
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
    | "union" -> Union []
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
    | "permission-set" -> Permission_set
    | other -> Unknown_def other

  let string_opt json field =
    match Yojson.Safe.Util.member field json with
    | `String s -> Some s
    | _ -> None

  let parse_union_refs json : string list =
    match Yojson.Safe.Util.member "refs" json with
    | `List items ->
        List.filter_map (function `String s -> Some s | _ -> None) items
    | _ -> []

  let parse_property json : primitive =
    let open Yojson.Safe.Util in
    match json |> member "type" with
    | `String "ref" -> Ref
    | `String "union" -> Union (parse_union_refs json)
    | `String "array" -> (
        match json |> member "items" with
        | `Assoc _ as items -> (
            match items |> member "type" with
            | `String "ref" -> Ref
            | `String "union" -> Union (parse_union_refs items)
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

  (** Parse a Lexicon 1 document from a JSON string. Fails when [id] is
      missing. *)
  let of_string (body : string) : document =
    of_json (Yojson.Safe.from_string body)

  type permission = {
    resource : string;
    inherit_aud : bool option;
    lxm : string list;
    collection : string list;
    action : string list;
  }

  type permission_set = {
    id : string option;
    title : string option;
    detail : string option;
    permissions : permission list;
  }

  let string_list_field json field =
    match Yojson.Safe.Util.member field json with
    | `List items ->
        List.filter_map (function `String s -> Some s | _ -> None) items
    | `String s -> [ s ]
    | _ -> []

  let parse_permission json : permission =
    {
      resource =
        (match Yojson.Safe.Util.member "resource" json with
        | `String s -> s
        | _ -> "");
      inherit_aud =
        (match Yojson.Safe.Util.member "inheritAud" json with
        | `Bool b -> Some b
        | _ -> None);
      lxm = string_list_field json "lxm";
      collection = string_list_field json "collection";
      action = string_list_field json "action";
    }

  let permission_set_body json =
    match Yojson.Safe.Util.member "defs" json with
    | `Assoc fields -> (
        match List.assoc_opt "main" fields with Some m -> m | None -> json)
    | _ -> json

  let parse_permission_set json : permission_set =
    let body = permission_set_body json in
    {
      id =
        (match Yojson.Safe.Util.member "id" json with
        | `String s -> Some s
        | _ -> None);
      title = string_opt body "title";
      detail = string_opt body "detail";
      permissions =
        (match Yojson.Safe.Util.member "permissions" body with
        | `List xs -> List.map parse_permission xs
        | _ -> []);
    }

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
    | Union refs ->
        if refs = [] then "Yojson.Safe.t"
        else
          let variant r = "`" ^ ocaml_ident r ^ " of Yojson.Safe.t" in
          "[ "
          ^ String.concat " | " (List.map variant refs)
          ^ " | `Unknown of Yojson.Safe.t ]"
    | Unknown -> "Yojson.Safe.t"

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
    | Permission_set -> "permission-set"
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
    | (Union _ | Unknown), _ -> true
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

  let union_refs = function Union refs -> refs | _ -> []

  let official_listitem =
    {|{"lexicon":1,"id":"app.bsky.graph.listitem","defs":{"main":{"type":"record","description":"Record representing an account's inclusion on a specific list.","key":"tid","record":{"type":"object","required":["subject","list","createdAt"],"properties":{"subject":{"type":"string","format":"did"},"list":{"type":"string","format":"at-uri"},"createdAt":{"type":"string","format":"datetime"}}}}}}|}

  let official_starterpack =
    {|{"lexicon":1,"id":"app.bsky.graph.starterpack","defs":{"main":{"type":"record","description":"Record defining a starter pack of actors and feeds for new users.","key":"tid","record":{"type":"object","required":["name","list","createdAt"],"properties":{"name":{"type":"string"},"description":{"type":"string"},"descriptionFacets":{"type":"array","items":{"type":"ref","ref":"app.bsky.richtext.facet"}},"list":{"type":"string","format":"at-uri"},"feeds":{"type":"array","items":{"type":"ref","ref":"#feedItem"}},"createdAt":{"type":"string","format":"datetime"}}}},"feedItem":{"type":"object","required":["uri"],"properties":{"uri":{"type":"string","format":"at-uri"}}}}}|}

  let official_list =
    {|{"lexicon":1,"id":"app.bsky.graph.list","defs":{"main":{"type":"record","description":"Record representing a list of accounts.","key":"tid","record":{"type":"object","required":["name","purpose","createdAt"],"properties":{"purpose":{"type":"ref","ref":"app.bsky.graph.defs#listPurpose"},"name":{"type":"string"},"description":{"type":"string"},"descriptionFacets":{"type":"array","items":{"type":"ref","ref":"app.bsky.richtext.facet"}},"avatar":{"type":"blob"},"labels":{"type":"union","refs":["com.atproto.label.defs#selfLabels"]},"createdAt":{"type":"string","format":"datetime"}}}}}}|}

  let official_chat_notification_defs =
    {|{"lexicon":1,"id":"chat.bsky.notification.defs","defs":{"preferences":{"type":"object","required":["chat","chatRequest"],"properties":{"chat":{"type":"ref","ref":"#chatPreference"},"chatRequest":{"type":"ref","ref":"#chatPreference"}}},"chatPreference":{"type":"object","required":["include","push"],"properties":{"include":{"type":"string","knownValues":["all","follows"]},"push":{"type":"boolean"}}}}}|}

  let official_get_post_thread =
    {|{"lexicon":1,"id":"app.bsky.feed.getPostThread","defs":{"main":{"type":"query","description":"Get posts in a thread.","parameters":{"type":"params","required":["uri"],"properties":{"uri":{"type":"string","format":"at-uri"},"depth":{"type":"integer"},"parentHeight":{"type":"integer"}}},"output":{"encoding":"application/json","schema":{"type":"object","required":["thread"],"properties":{"thread":{"type":"union","refs":["app.bsky.feed.defs#threadViewPost","app.bsky.feed.defs#notFoundPost","app.bsky.feed.defs#blockedPost"]},"threadgate":{"type":"ref","ref":"app.bsky.feed.defs#threadgateView"}}}}}}}|}

  let official_get_post_thread_v2 =
    {|{"lexicon":1,"id":"app.bsky.unspecced.getPostThreadV2","defs":{"main":{"type":"query","description":"Get posts in a thread (unspecced v2).","parameters":{"type":"params","required":["anchor"],"properties":{"anchor":{"type":"string","format":"at-uri"},"above":{"type":"boolean"},"below":{"type":"integer"},"branchingFactor":{"type":"integer"},"sort":{"type":"string"}}},"output":{"encoding":"application/json","schema":{"type":"object","required":["thread","hasOtherReplies"],"properties":{"thread":{"type":"array"},"threadgate":{"type":"ref","ref":"app.bsky.feed.defs#threadgateView"},"hasOtherReplies":{"type":"boolean"}}}}}}}|}

  let official_subscribe_mod_events =
    {|{"lexicon":1,"id":"chat.bsky.moderation.subscribeModEvents","defs":{"main":{"type":"subscription","description":"Subscribe to stream of chat events targeted to moderation.","parameters":{"type":"params","properties":{"cursor":{"type":"string"}}},"message":{"schema":{"type":"union","refs":["#eventConvoFirstMessage","#eventGroupChatCreated","#eventGroupChatMemberAdded","#eventGroupChatMemberJoined","#eventGroupChatJoinRequest","#eventGroupChatJoinRequestApproved","#eventGroupChatJoinRequestRejected","#eventChatAccepted","#eventGroupChatMemberLeft","#eventGroupChatUpdated","#eventRateLimitExceeded"]}}},"eventConvoFirstMessage":{"type":"object","required":["createdAt","rev","convoId","user","recipients"],"properties":{"convoId":{"type":"string"},"createdAt":{"type":"string"},"rev":{"type":"string"},"user":{"type":"string"},"recipients":{"type":"array"}}}}}|}

  let official_ageassurance_get_state =
    {|{"lexicon":1,"id":"app.bsky.ageassurance.getState","defs":{"main":{"type":"query","description":"Returns server-computed Age Assurance state.","parameters":{"type":"params","required":["countryCode"],"properties":{"countryCode":{"type":"string"},"regionCode":{"type":"string"}}},"output":{"encoding":"application/json","schema":{"type":"object","required":["state","metadata"],"properties":{"state":{"type":"ref","ref":"app.bsky.ageassurance.defs#state"},"metadata":{"type":"ref","ref":"app.bsky.ageassurance.defs#stateMetadata"}}}}}}}|}

  let official_draft_create =
    {|{"lexicon":1,"id":"app.bsky.draft.createDraft","defs":{"main":{"type":"procedure","description":"Inserts a draft using private storage.","input":{"encoding":"application/json","schema":{"type":"object","required":["draft"],"properties":{"draft":{"type":"ref","ref":"app.bsky.draft.defs#draft"}}}},"output":{"encoding":"application/json","schema":{"type":"object","required":["id"],"properties":{"id":{"type":"string","format":"tid"}}}}}}}|}

  type resolved_lexicon = {
    uri : string;
    cid : string;
    schema : Yojson.Safe.t;
    document : document option;
  }

  let parse_resolved_lexicon json : resolved_lexicon =
    let schema =
      match Yojson.Safe.Util.member "schema" json with
      | `Assoc _ as s -> s
      | _ -> json
    in
    let document = try Some (of_json schema) with _ -> None in
    {
      uri = string_opt json "uri" |> Option.value ~default:"";
      cid = string_opt json "cid" |> Option.value ~default:"";
      schema;
      document;
    }

  let resolve_lexicon ?session ?host ~nsid () : resolved_lexicon =
    Client.Client.get_json ?session ?host "com.atproto.lexicon.resolveLexicon"
      [ ("nsid", nsid) ]
    |> parse_resolved_lexicon

  let official_search_posts_v2 =
    {|{"lexicon":1,"id":"app.bsky.feed.searchPostsV2","defs":{"main":{"type":"query","description":"Find posts matching a search query or filters.","parameters":{"type":"params","properties":{"query":{"type":"string"},"sort":{"type":"string"},"limit":{"type":"integer"},"cursor":{"type":"string"}}},"output":{"encoding":"application/json","schema":{"type":"object","required":["posts"],"properties":{"posts":{"type":"array"},"cursor":{"type":"string"},"hitsTotal":{"type":"integer"},"detectedQueryLanguages":{"type":"array"}}}}}}}|}

  let official_search_starter_packs_v2 =
    {|{"lexicon":1,"id":"app.bsky.graph.searchStarterPacksV2","defs":{"main":{"type":"query","description":"Find starter packs matching search criteria.","parameters":{"type":"params","required":["q"],"properties":{"q":{"type":"string"},"limit":{"type":"integer"},"cursor":{"type":"string"}}},"output":{"encoding":"application/json","schema":{"type":"object","required":["starterPacks"],"properties":{"starterPacks":{"type":"array"},"cursor":{"type":"string"},"hitsTotal":{"type":"integer"}}}}}}}|}

  let official_resolve_lexicon =
    {|{"lexicon":1,"id":"com.atproto.lexicon.resolveLexicon","defs":{"main":{"type":"query","description":"Resolves an atproto lexicon (NSID) to a schema.","parameters":{"type":"params","required":["nsid"],"properties":{"nsid":{"type":"string","format":"nsid"}}},"output":{"encoding":"application/json","schema":{"type":"object","required":["uri","cid","schema"],"properties":{"uri":{"type":"string"},"cid":{"type":"string"},"schema":{"type":"ref","ref":"com.atproto.lexicon.schema#main"}}}}}}}|}

  let official_check_handle =
    {|{"lexicon":1,"id":"com.atproto.temp.checkHandleAvailability","defs":{"main":{"type":"query","description":"Checks whether the provided handle is available.","parameters":{"type":"params","required":["handle"],"properties":{"handle":{"type":"string"},"email":{"type":"string"},"birthDate":{"type":"string"}}},"output":{"encoding":"application/json","schema":{"type":"object","required":["handle","result"],"properties":{"handle":{"type":"string"},"result":{"type":"union","refs":["#resultAvailable","#resultUnavailable"]}}}}}}}|}

  let official_create_group =
    {|{"lexicon":1,"id":"chat.bsky.group.createGroup","defs":{"main":{"type":"procedure","description":"Creates a group convo.","input":{"encoding":"application/json","schema":{"type":"object","required":["members","name"],"properties":{"members":{"type":"array"},"name":{"type":"string"}}}},"output":{"encoding":"application/json","schema":{"type":"object","required":["convo"],"properties":{"convo":{"type":"ref","ref":"chat.bsky.convo.defs#convoView"}}}}}}}|}

  let official_list_queues =
    {|{"lexicon":1,"id":"tools.ozone.queue.listQueues","defs":{"main":{"type":"query","description":"List all configured moderation queues with statistics.","parameters":{"type":"params","properties":{"enabled":{"type":"boolean"},"limit":{"type":"integer"},"cursor":{"type":"string"}}},"output":{"encoding":"application/json","schema":{"type":"object","required":["queues"],"properties":{"queues":{"type":"array"},"cursor":{"type":"string"}}}}}}}|}

  let official_create_queue =
    {|{"lexicon":1,"id":"tools.ozone.queue.createQueue","defs":{"main":{"type":"procedure","description":"Create a new moderation queue.","input":{"encoding":"application/json","schema":{"type":"object","required":["name"],"properties":{"name":{"type":"string"},"subjectTypes":{"type":"array"},"reportTypes":{"type":"array"}}}},"output":{"encoding":"application/json","schema":{"type":"object","required":["queue"],"properties":{"queue":{"type":"ref","ref":"tools.ozone.queue.defs#queueView"}}}}}}}|}

  let official_query_reports =
    {|{"lexicon":1,"id":"tools.ozone.report.queryReports","defs":{"main":{"type":"query","description":"View moderation reports.","parameters":{"type":"params","required":["status"],"properties":{"status":{"type":"string"},"queueId":{"type":"integer"},"limit":{"type":"integer"},"cursor":{"type":"string"}}},"output":{"encoding":"application/json","schema":{"type":"object","required":["reports"],"properties":{"reports":{"type":"array"},"cursor":{"type":"string"}}}}}}}|}

  let official_check_signup =
    {|{"lexicon":1,"id":"com.atproto.temp.checkSignupQueue","defs":{"main":{"type":"query","description":"Check accounts location in signup queue.","output":{"encoding":"application/json","schema":{"type":"object","required":["activated"],"properties":{"activated":{"type":"boolean"},"placeInQueue":{"type":"integer"},"estimatedTimeMs":{"type":"integer"}}}}}}}|}

  let official_deref_scope =
    {|{"lexicon":1,"id":"com.atproto.temp.dereferenceScope","defs":{"main":{"type":"query","description":"Allows finding the oauth permission scope from a reference.","parameters":{"type":"params","required":["scope"],"properties":{"scope":{"type":"string"}}},"output":{"encoding":"application/json","schema":{"type":"object","required":["scope"],"properties":{"scope":{"type":"string"}}}}}}}|}

  let official_revoke_creds =
    {|{"lexicon":1,"id":"com.atproto.temp.revokeAccountCredentials","defs":{"main":{"type":"procedure","description":"Revoke sessions, password, and app passwords associated with account.","input":{"encoding":"application/json","schema":{"type":"object","required":["account"],"properties":{"account":{"type":"string"}}}}}}}|}

  let official_site_recommend =
    {|{"lexicon":1,"id":"site.standard.graph.recommend","defs":{"main":{"type":"record","description":"Record declaring a recommendation of a document.","key":"tid","record":{"type":"object","required":["document","createdAt"],"properties":{"document":{"type":"string","format":"at-uri"},"createdAt":{"type":"string","format":"datetime"}}}}}}|}

  let official_site_subscription =
    {|{"lexicon":1,"id":"site.standard.graph.subscription","defs":{"main":{"type":"record","description":"Record declaring a subscription to a publication.","key":"tid","record":{"type":"object","required":["publication"],"properties":{"publication":{"type":"string","format":"at-uri"},"createdAt":{"type":"string","format":"datetime"}}}}}}|}

  let official_germ_declaration =
    {|{"lexicon":1,"id":"com.germnetwork.declaration","defs":{"main":{"type":"record","description":"A declaration of a Germ Network account","key":"literal:self","record":{"type":"object","required":["version","currentKey"],"properties":{"version":{"type":"string"},"currentKey":{"type":"bytes"},"messageMe":{"type":"ref","ref":"#messageMe"}}}},"messageMe":{"type":"object","required":["showButtonTo","messageMeUrl"],"properties":{"showButtonTo":{"type":"string","knownValues":["none","usersIFollow","everyone"]},"messageMeUrl":{"type":"string","format":"uri"}}}}}|}

  let official_safelink_query_events =
    {|{"lexicon":1,"id":"tools.ozone.safelink.queryEvents","defs":{"main":{"type":"procedure","description":"Query URL safety audit events","input":{"encoding":"application/json","schema":{"type":"object","properties":{"cursor":{"type":"string"},"limit":{"type":"integer"},"urls":{"type":"array","items":{"type":"string"}},"patternType":{"type":"string"},"sortDirection":{"type":"string"}}}},"output":{"encoding":"application/json","schema":{"type":"object","required":["events"],"properties":{"cursor":{"type":"string"},"events":{"type":"array","items":{"type":"ref","ref":"tools.ozone.safelink.defs#event"}}}}}}}}|}

  let official_admin_signing_key =
    {|{"lexicon":1,"id":"com.atproto.admin.updateAccountSigningKey","defs":{"main":{"type":"procedure","description":"Administrative action to update an account's signing key in their Did document.","input":{"encoding":"application/json","schema":{"type":"object","required":["did","signingKey"],"properties":{"did":{"type":"string","format":"did"},"signingKey":{"type":"string","format":"did"}}}}}}}|}

  let official_join_link =
    {|{"lexicon":1,"id":"chat.bsky.embed.joinLink","defs":{"main":{"type":"object","required":["code"],"properties":{"code":{"type":"string"}}},"view":{"type":"object","required":["joinLinkPreview"],"properties":{"joinLinkPreview":{"type":"union","refs":["chat.bsky.group.defs#joinLinkPreviewView"]}}}}}|}

  let official_confirm_email =
    {|{"lexicon":1,"id":"com.atproto.server.confirmEmail","defs":{"main":{"type":"procedure","description":"Confirm an email using a token from com.atproto.server.requestEmailConfirmation.","input":{"encoding":"application/json","schema":{"type":"object","required":["email","token"],"properties":{"email":{"type":"string"},"token":{"type":"string"}}}}}}}|}

  let official_feed_known_likers =
    {|{"lexicon":1,"id":"app.bsky.feed.defs","defs":{"knownLikers":{"type":"object","description":"The post's likers whom you also follow","required":["count","actors"],"properties":{"count":{"type":"integer"},"actors":{"type":"array","items":{"type":"ref","ref":"app.bsky.actor.defs#profileViewBasic"}}}},"viewerState":{"type":"object","properties":{"repost":{"type":"string"},"like":{"type":"string"},"bookmarked":{"type":"boolean"},"threadMuted":{"type":"boolean"},"replyDisabled":{"type":"boolean"},"embeddingDisabled":{"type":"boolean"},"pinned":{"type":"boolean"},"knownLikers":{"type":"ref","ref":"#knownLikers"}}},"replyRef":{"type":"object","required":["root","parent"],"properties":{"root":{"type":"union"},"parent":{"type":"union"},"grandparentAuthor":{"type":"ref","ref":"app.bsky.actor.defs#profileViewBasic"}}}}}|}

  let official_embed_video =
    {|{"lexicon":1,"id":"app.bsky.embed.video","defs":{"main":{"type":"object","required":["video"],"properties":{"video":{"type":"blob"},"alt":{"type":"string"},"aspectRatio":{"type":"ref","ref":"app.bsky.embed.defs#aspectRatio"},"presentation":{"type":"string","knownValues":["default","gif"]}}},"view":{"type":"object","required":["cid","playlist"],"properties":{"cid":{"type":"string"},"playlist":{"type":"string"},"alt":{"type":"string"},"presentation":{"type":"string","knownValues":["default","gif"]}}}}}|}

  let official_lexicon_schema =
    {|{"lexicon":1,"id":"com.atproto.lexicon.schema","defs":{"main":{"type":"record","description":"Representation of Lexicon schemas themselves, when published as atproto records.","key":"nsid","record":{"type":"object","required":["lexicon"],"properties":{"lexicon":{"type":"integer"}}}}}}|}

  let official_mute_actor =
    {|{"lexicon":1,"id":"app.bsky.graph.muteActor","defs":{"main":{"type":"procedure","description":"Creates a mute relationship for the specified account.","input":{"encoding":"application/json","schema":{"type":"object","required":["actor"],"properties":{"actor":{"type":"string"},"onlyReposts":{"type":"boolean"},"onlyQuoteposts":{"type":"boolean"}}}}}}}|}

  let official_get_follows =
    {|{"lexicon":1,"id":"app.bsky.graph.getFollows","defs":{"main":{"type":"query","description":"Enumerates accounts which a specified account (actor) follows.","parameters":{"type":"params","required":["actor"],"properties":{"actor":{"type":"string"},"limit":{"type":"integer"},"cursor":{"type":"string"},"sort":{"type":"string","knownValues":["latest","top"]}}},"output":{"encoding":"application/json","schema":{"type":"object","required":["subject","follows"],"properties":{"subject":{"type":"ref","ref":"app.bsky.actor.defs#profileView"},"cursor":{"type":"string"},"follows":{"type":"array"}}}}}}}|}

  let official_get_followers =
    {|{"lexicon":1,"id":"app.bsky.graph.getFollowers","defs":{"main":{"type":"query","description":"Enumerates accounts which follow a specified account (actor).","parameters":{"type":"params","required":["actor"],"properties":{"actor":{"type":"string"},"limit":{"type":"integer"},"cursor":{"type":"string"},"sort":{"type":"string","knownValues":["latest","top"]}}},"output":{"encoding":"application/json","schema":{"type":"object","required":["subject","followers"],"properties":{"subject":{"type":"ref","ref":"app.bsky.actor.defs#profileView"},"cursor":{"type":"string"},"followers":{"type":"array"}}}}}}}|}

  let official_get_suggested_follows_by_actor =
    {|{"lexicon":1,"id":"app.bsky.graph.getSuggestedFollowsByActor","defs":{"main":{"type":"query","description":"Enumerates follows similar to a given account (actor).","parameters":{"type":"params","required":["actor"],"properties":{"actor":{"type":"string"}}},"output":{"encoding":"application/json","schema":{"type":"object","required":["suggestions"],"properties":{"suggestions":{"type":"array"},"recIdStr":{"type":"string"},"isFallback":{"type":"boolean"},"recId":{"type":"integer"}}}}}}}|}

  let official_actor_defs =
    {|{"lexicon":1,"id":"app.bsky.actor.defs","defs":{"viewerState":{"type":"object","properties":{"muted":{"type":"boolean"},"mutedOnlyReposts":{"type":"boolean"},"mutedOnlyQuoteposts":{"type":"boolean"},"blocking":{"type":"string"},"knownFollowers":{"type":"ref","ref":"#knownFollowers"}}},"profileAssociatedGerm":{"type":"object","required":["showButtonTo","messageMeUrl"],"properties":{"showButtonTo":{"type":"string"},"messageMeUrl":{"type":"string"}}},"profileAssociated":{"type":"object","properties":{"germ":{"type":"ref","ref":"#profileAssociatedGerm"}}},"profileViewDetailed":{"type":"object","required":["did","handle"],"properties":{"joinedViaStarterPack":{"type":"ref","ref":"app.bsky.graph.defs#starterPackViewBasic"}}}}}|}

  let official_create_session =
    {|{"lexicon":1,"id":"com.atproto.server.createSession","defs":{"main":{"type":"procedure","description":"Create an authentication session.","input":{"encoding":"application/json","schema":{"type":"object","required":["identifier","password"],"properties":{"identifier":{"type":"string"},"password":{"type":"string"},"authFactorToken":{"type":"string"},"allowTakendown":{"type":"boolean"}}}}}}}|}

  let official_create_account =
    {|{"lexicon":1,"id":"com.atproto.server.createAccount","defs":{"main":{"type":"procedure","description":"Create an account.","input":{"encoding":"application/json","schema":{"type":"object","required":["handle"],"properties":{"handle":{"type":"string"},"email":{"type":"string"},"did":{"type":"string"},"inviteCode":{"type":"string"},"verificationCode":{"type":"string"},"verificationPhone":{"type":"string"},"password":{"type":"string"},"recoveryKey":{"type":"string"},"plcOp":{"type":"unknown"}}}}}}}|}

  let official_get_session =
    {|{"lexicon":1,"id":"com.atproto.server.getSession","defs":{"main":{"type":"query","description":"Get information about the current auth session.","output":{"encoding":"application/json","schema":{"type":"object","required":["handle","did"],"properties":{"handle":{"type":"string"},"did":{"type":"string"},"email":{"type":"string"},"emailConfirmed":{"type":"boolean"},"emailAuthFactor":{"type":"boolean"},"active":{"type":"boolean"},"status":{"type":"string"}}}}}}}|}

  let official_create_app_password =
    {|{"lexicon":1,"id":"com.atproto.server.createAppPassword","defs":{"main":{"type":"procedure","description":"Create an App Password.","input":{"encoding":"application/json","schema":{"type":"object","required":["name"],"properties":{"name":{"type":"string"},"privileged":{"type":"boolean"}}}}}}}|}

  let official_embed_external =
    {|{"lexicon":1,"id":"app.bsky.embed.external","defs":{"main":{"type":"object","required":["external"],"properties":{"external":{"type":"ref","ref":"#external"}}},"external":{"type":"object","required":["uri","title","description"],"properties":{"associatedRefs":{"type":"array"}}},"viewExternal":{"type":"object","required":["uri","title","description"],"properties":{"readingTime":{"type":"integer"},"associatedProfiles":{"type":"array"},"source":{"type":"ref","ref":"#viewExternalSource"}}},"viewExternalSourceTheme":{"type":"object","properties":{"backgroundRGB":{"type":"ref","ref":"#colorRGB"},"foregroundRGB":{"type":"ref","ref":"#colorRGB"},"accentRGB":{"type":"ref","ref":"#colorRGB"},"accentForegroundRGB":{"type":"ref","ref":"#colorRGB"}}}}}|}

  let official_get_author_feed =
    {|{"lexicon":1,"id":"app.bsky.feed.getAuthorFeed","defs":{"main":{"type":"query","description":"Get a view of an actor's author feed.","parameters":{"type":"params","required":["actor"],"properties":{"actor":{"type":"string"},"limit":{"type":"integer"},"cursor":{"type":"string"},"filter":{"type":"string","knownValues":["posts_with_replies","posts_no_replies","posts_with_media","posts_and_author_threads","posts_with_video"]},"includePins":{"type":"boolean"}}},"output":{"encoding":"application/json","schema":{"type":"object","required":["feed"],"properties":{"cursor":{"type":"string"},"feed":{"type":"array"}}}}}}}|}

  let official_graph_relationship =
    {|{"lexicon":1,"id":"app.bsky.graph.defs","defs":{"relationship":{"type":"object","required":["did"],"properties":{"did":{"type":"string"},"blockingByList":{"type":"string"},"blockedByList":{"type":"string"}}},"starterPackView":{"type":"object","required":["uri","cid","record","creator","indexedAt"],"properties":{"listItemsSample":{"type":"array"}}},"listViewerState":{"type":"object","properties":{"muted":{"type":"boolean"},"blocked":{"type":"string"},"referenceListOptOut":{"type":"string"}}},"listItemView":{"type":"object","required":["uri","subject"],"properties":{"uri":{"type":"string"},"subject":{"type":"ref"},"subjectOptedOut":{"type":"boolean"}}}}}|}

  let official_check_account_status =
    {|{"lexicon":1,"id":"com.atproto.server.checkAccountStatus","defs":{"main":{"type":"query","description":"Returns the status of an account, especially as pertaining to import or recovery.","output":{"encoding":"application/json","schema":{"type":"object","required":["activated","validDid","repoCommit","repoRev","repoBlocks","indexedRecords","privateStateValues","expectedBlobs","importedBlobs"],"properties":{"activated":{"type":"boolean"},"validDid":{"type":"boolean"},"repoCommit":{"type":"string"},"repoRev":{"type":"string"},"repoBlocks":{"type":"integer"},"indexedRecords":{"type":"integer"},"privateStateValues":{"type":"integer"},"expectedBlobs":{"type":"integer"},"importedBlobs":{"type":"integer"}}}}}}}|}

  let official_label_value_definition =
    {|{"lexicon":1,"id":"com.atproto.label.defs","defs":{"labelValueDefinition":{"type":"object","required":["identifier","severity","blurs","locales"],"properties":{"identifier":{"type":"string"},"severity":{"type":"string"},"blurs":{"type":"string"},"defaultSetting":{"type":"string"},"adultOnly":{"type":"boolean"},"locales":{"type":"array"}}}}}|}

  let official_get_messages =
    {|{"lexicon":1,"id":"chat.bsky.convo.getMessages","defs":{"main":{"type":"query","description":"Returns a page of messages from a conversation.","parameters":{"type":"params","required":["convoId"],"properties":{"convoId":{"type":"string"}}},"output":{"encoding":"application/json","schema":{"type":"object","required":["messages"],"properties":{"messages":{"type":"array"},"relatedProfiles":{"type":"array"}}}}}}}|}

  let official_convo_system_messages =
    {|{"lexicon":1,"id":"chat.bsky.convo.defs","defs":{"systemMessageReferredUser":{"type":"object","required":["did"],"properties":{"did":{"type":"string"}}},"systemMessageDataAddMember":{"type":"object","required":["member","role","addedBy"],"properties":{"member":{"type":"ref"},"role":{"type":"ref"},"addedBy":{"type":"ref"}}},"systemMessageDataRemoveMember":{"type":"object","required":["member","removedBy"],"properties":{"member":{"type":"ref"},"removedBy":{"type":"ref"}}},"systemMessageDataUnlockConvo":{"type":"object","required":["unlockedBy"],"properties":{"unlockedBy":{"type":"ref"}}},"groupConvo":{"type":"object","required":["createdAt","lockStatus","memberCount","memberLimit","name"],"properties":{"createdAt":{"type":"string"},"joinLink":{"type":"ref"},"joinRequestCount":{"type":"integer"},"memberLimit":{"type":"integer"}}}}}|}

  let official_chat_actor_member =
    {|{"lexicon":1,"id":"chat.bsky.actor.defs","defs":{"groupConvoMember":{"type":"object","required":["role"],"properties":{"addedBy":{"type":"ref","ref":"#profileViewBasic"},"role":{"type":"ref","ref":"#memberRole"}}},"profileViewBasic":{"type":"object","required":["did","handle"],"properties":{"chatDisabled":{"type":"boolean"},"kind":{"type":"union","refs":["#directConvoMember","#groupConvoMember","#pastGroupConvoMember"]}}}}}|}

  let official_list_convo_requests =
    {|{"lexicon":1,"id":"chat.bsky.convo.listConvoRequests","defs":{"main":{"type":"query","description":"Returns incoming conversation requests. Direct convo requests are convoView; group join requests made by the user are joinRequestConvoView.","output":{"encoding":"application/json","schema":{"type":"object","required":["requests"],"properties":{"requests":{"type":"array"}}}}}}}|}

  let official_join_request_convo =
    {|{"lexicon":1,"id":"chat.bsky.group.defs","defs":{"joinRequestConvoView":{"type":"object","required":["convoId","name","owner","memberCount","memberLimit","viewer"],"properties":{"convoId":{"type":"string"},"name":{"type":"string"},"owner":{"type":"ref"},"memberCount":{"type":"integer"},"memberLimit":{"type":"integer"},"viewer":{"type":"ref","ref":"#joinLinkViewerState"}}},"joinLinkViewerState":{"type":"object","properties":{"requestedAt":{"type":"string"}}}}}|}

  let official_ageassurance_event =
    {|{"lexicon":1,"id":"app.bsky.ageassurance.defs","defs":{"event":{"type":"object","required":["createdAt","status","access","attemptId","countryCode"],"properties":{"initIp":{"type":"string"},"initUa":{"type":"string"},"completeIp":{"type":"string"},"completeUa":{"type":"string"}}}}}|}

  let official_referencelistoptout =
    {|{"lexicon":1,"id":"app.bsky.graph.referencelistoptout","defs":{"main":{"type":"record","description":"Record requesting that its author be omitted from the public presentation of a reference list.","key":"tid","record":{"type":"object","required":["subject","createdAt"],"properties":{"subject":{"type":"string","format":"at-uri"},"createdAt":{"type":"string","format":"datetime"}}}}}}|}

  let official_auth_create_posts =
    {|{"lexicon":1,"id":"app.bsky.authCreatePosts","defs":{"main":{"type":"permission-set","title":"Create Bluesky Posts","detail":"Can not update or delete posts.","permissions":[{"type":"permission","resource":"rpc","inheritAud":true,"lxm":["app.bsky.video.uploadVideo","app.bsky.video.getJobStatus","app.bsky.video.getUploadLimits","app.bsky.video.startUpload","app.bsky.video.uploadPart","app.bsky.video.finishUpload","app.bsky.video.abortUpload","app.bsky.video.getUploadStatus"]},{"type":"permission","resource":"repo","action":["create"],"collection":["app.bsky.feed.post","app.bsky.feed.postgate","app.bsky.feed.threadgate"]}]}}}|}

  let official_auth_full_chat =
    {|{"lexicon":1,"id":"chat.bsky.authFullChatClient","defs":{"main":{"type":"permission-set","title":"Full Chat Client (All Conversations)","detail":"Control of all chat conversations and configuration management.","permissions":[{"type":"permission","resource":"rpc","inheritAud":true,"lxm":["chat.bsky.convo.listConvos","chat.bsky.convo.sendMessage"]},{"type":"permission","resource":"repo","action":["create","update","delete"],"collection":["chat.bsky.actor.declaration"]}]}}}|}

  let official_auth_full_app =
    {|{"lexicon":1,"id":"app.bsky.authFullApp","defs":{"main":{"type":"permission-set","title":"Full Bluesky Social App Permissions","detail":"Manage all public content and interactions, private preferences and subscriptions, and other Bluesky-specific app features and data.","permissions":[{"type":"permission","resource":"rpc","inheritAud":true,"lxm":["app.bsky.actor.getPreferences","app.bsky.actor.getProfile","app.bsky.actor.getProfiles","app.bsky.actor.getSuggestions","app.bsky.actor.putPreferences","app.bsky.actor.searchActors","app.bsky.actor.searchActorsTypeahead","app.bsky.bookmark.createBookmark","app.bsky.bookmark.deleteBookmark","app.bsky.bookmark.getBookmarks","app.bsky.contact.dismissMatch","app.bsky.contact.getMatches","app.bsky.contact.getSyncStatus","app.bsky.contact.importContacts","app.bsky.contact.removeData","app.bsky.contact.startPhoneVerification","app.bsky.contact.verifyPhone","app.bsky.feed.describeFeedGenerator","app.bsky.feed.getActorFeeds","app.bsky.feed.getActorLikes","app.bsky.feed.getAuthorFeed","app.bsky.feed.getFeed","app.bsky.feed.getFeedGenerator","app.bsky.feed.getFeedGenerators","app.bsky.feed.getFeedSkeleton","app.bsky.feed.getLikes","app.bsky.feed.getListFeed","app.bsky.feed.getPostThread","app.bsky.feed.getPosts","app.bsky.feed.getQuotes","app.bsky.feed.getRepostedBy","app.bsky.feed.getSuggestedFeeds","app.bsky.feed.getTimeline","app.bsky.feed.searchPosts","app.bsky.feed.searchPostsV2","app.bsky.feed.sendInteractions","app.bsky.graph.getActorStarterPacks","app.bsky.graph.getBlocks","app.bsky.graph.getFollowers","app.bsky.graph.getFollows","app.bsky.graph.getKnownFollowers","app.bsky.graph.getList","app.bsky.graph.getListBlocks","app.bsky.graph.getListMutes","app.bsky.graph.getLists","app.bsky.graph.getListsWithMembership","app.bsky.graph.getMutes","app.bsky.graph.getRelationships","app.bsky.graph.getStarterPack","app.bsky.graph.getStarterPacks","app.bsky.graph.getStarterPacksWithMembership","app.bsky.graph.getSuggestedFollowsByActor","app.bsky.graph.muteActor","app.bsky.graph.muteActorList","app.bsky.graph.muteThread","app.bsky.graph.searchStarterPacks","app.bsky.graph.searchStarterPacksV2","app.bsky.graph.unmuteActor","app.bsky.graph.unmuteActorList","app.bsky.graph.unmuteThread","app.bsky.labeler.getServices","app.bsky.notification.getPreferences","app.bsky.notification.getUnreadCount","app.bsky.notification.listActivitySubscriptions","app.bsky.notification.listNotifications","app.bsky.notification.putActivitySubscription","app.bsky.notification.putPreferences","app.bsky.notification.putPreferencesV2","app.bsky.notification.registerPush","app.bsky.notification.unregisterPush","app.bsky.notification.updateSeen","app.bsky.unspecced.getAgeAssuranceState","app.bsky.unspecced.getConfig","app.bsky.unspecced.getOnboardingSuggestedStarterPacks","app.bsky.unspecced.getPopularFeedGenerators","app.bsky.unspecced.getPostThreadOtherV2","app.bsky.unspecced.getPostThreadV2","app.bsky.unspecced.getSuggestedFeeds","app.bsky.unspecced.getSuggestedFeedsSkeleton","app.bsky.unspecced.getSuggestedStarterPacks","app.bsky.unspecced.getSuggestedStarterPacksSkeleton","app.bsky.unspecced.getSuggestedUsers","app.bsky.unspecced.getSuggestedUsersSkeleton","app.bsky.unspecced.getSuggestionsSkeleton","app.bsky.unspecced.getTaggedSuggestions","app.bsky.unspecced.getTrendingTopics","app.bsky.unspecced.getTrends","app.bsky.unspecced.getTrendsSkeleton","app.bsky.unspecced.initAgeAssurance","app.bsky.unspecced.searchActorsSkeleton","app.bsky.unspecced.searchPostsSkeleton","app.bsky.unspecced.searchStarterPacksSkeleton","app.bsky.video.getJobStatus","app.bsky.video.getUploadLimits","app.bsky.video.uploadVideo","app.bsky.video.startUpload","app.bsky.video.uploadPart","app.bsky.video.finishUpload","app.bsky.video.abortUpload","app.bsky.video.getUploadStatus"]},{"type":"permission","resource":"repo","action":["create","update","delete"],"collection":["app.bsky.actor.profile","app.bsky.actor.status","app.bsky.feed.like","app.bsky.feed.post","app.bsky.feed.postgate","app.bsky.feed.repost","app.bsky.feed.threadgate","app.bsky.graph.block","app.bsky.graph.follow","app.bsky.graph.list","app.bsky.graph.listblock","app.bsky.graph.listitem","app.bsky.graph.referencelistoptout","app.bsky.graph.starterpack","app.bsky.notification.declaration"]}]}}}|}

  let official_auth_view_all =
    {|{"lexicon":1,"id":"app.bsky.authViewAll","defs":{"main":{"type":"permission-set","title":"Read-only access to all content","detail":"View Bluesky network content from account perspective, and read all notifications and preferences.","permissions":[{"type":"permission","resource":"rpc","inheritAud":true,"lxm":["app.bsky.actor.getPreferences","app.bsky.actor.getProfile","app.bsky.actor.getProfiles","app.bsky.actor.getSuggestions","app.bsky.actor.searchActors","app.bsky.actor.searchActorsTypeahead","app.bsky.bookmark.getBookmarks","app.bsky.feed.describeFeedGenerator","app.bsky.feed.getActorFeeds","app.bsky.feed.getActorLikes","app.bsky.feed.getAuthorFeed","app.bsky.feed.getFeed","app.bsky.feed.getFeedGenerator","app.bsky.feed.getFeedGenerators","app.bsky.feed.getFeedSkeleton","app.bsky.feed.getLikes","app.bsky.feed.getListFeed","app.bsky.feed.getPostThread","app.bsky.feed.getPosts","app.bsky.feed.getQuotes","app.bsky.feed.getRepostedBy","app.bsky.feed.getSuggestedFeeds","app.bsky.feed.getTimeline","app.bsky.feed.searchPosts","app.bsky.feed.searchPostsV2","app.bsky.graph.getActorStarterPacks","app.bsky.graph.getBlocks","app.bsky.graph.getFollowers","app.bsky.graph.getFollows","app.bsky.graph.getKnownFollowers","app.bsky.graph.getListBlocks","app.bsky.graph.getListMutes","app.bsky.graph.getLists","app.bsky.graph.getListsWithMembership","app.bsky.graph.getMutes","app.bsky.graph.getRelationships","app.bsky.graph.getStarterPack","app.bsky.graph.getStarterPacks","app.bsky.graph.getStarterPacksWithMembership","app.bsky.graph.getSuggestedFollowsByActor","app.bsky.graph.searchStarterPacks","app.bsky.graph.searchStarterPacksV2","app.bsky.labeler.getServices","app.bsky.notification.getPreferences","app.bsky.notification.getUnreadCount","app.bsky.notification.listActivitySubscriptions","app.bsky.notification.listNotifications","app.bsky.notification.updateSeen","app.bsky.unspecced.getAgeAssuranceState","app.bsky.unspecced.getConfig","app.bsky.unspecced.getOnboardingSuggestedStarterPacks","app.bsky.unspecced.getPopularFeedGenerators","app.bsky.unspecced.getPostThreadOtherV2","app.bsky.unspecced.getPostThreadV2","app.bsky.unspecced.getSuggestedFeeds","app.bsky.unspecced.getSuggestedFeedsSkeleton","app.bsky.unspecced.getSuggestedStarterPacks","app.bsky.unspecced.getSuggestedStarterPacksSkeleton","app.bsky.unspecced.getSuggestedUsers","app.bsky.unspecced.getSuggestedUsersSkeleton","app.bsky.unspecced.getSuggestionsSkeleton","app.bsky.unspecced.getTaggedSuggestions","app.bsky.unspecced.getTrendingTopics","app.bsky.unspecced.getTrends","app.bsky.unspecced.getTrendsSkeleton","app.bsky.unspecced.searchActorsSkeleton","app.bsky.unspecced.searchPostsSkeleton","app.bsky.unspecced.searchStarterPacksSkeleton","app.bsky.video.getUploadLimits"]}]}}}|}

  let official_auth_delete_content =
    {|{"lexicon":1,"id":"app.bsky.authDeleteContent","defs":{"main":{"type":"permission-set","title":"Delete Bluesky Content","detail":"Clean up public account history: posts, reposts, and likes.","permissions":[{"type":"permission","resource":"repo","action":["delete"],"collection":["app.bsky.feed.like","app.bsky.feed.post","app.bsky.feed.postgate","app.bsky.feed.repost","app.bsky.feed.threadgate"]}]}}}|}

  let official_auth_manage_feed =
    {|{"lexicon":1,"id":"app.bsky.authManageFeedDeclarations","defs":{"main":{"type":"permission-set","title":"Manage Hosted Feeds","detail":"Configure feed generator declaration records.","permissions":[{"type":"permission","resource":"repo","action":["create","update","delete"],"collection":["app.bsky.feed.generator"]}]}}}|}

  let official_auth_manage_labeler =
    {|{"lexicon":1,"id":"app.bsky.authManageLabelerService","defs":{"main":{"type":"permission-set","title":"Manage Hosted Labeling Service","detail":"Configure labeler declaration records.","permissions":[{"type":"permission","resource":"repo","action":["create","update","delete"],"collection":["app.bsky.labeler.service"]}]}}}|}

  let official_auth_manage_moderation =
    {|{"lexicon":1,"id":"app.bsky.authManageModeration","defs":{"main":{"type":"permission-set","title":"Manage Personal Moderation","detail":"Control over blocks, mutes, mod lists, mod services, and preferences.","permissions":[{"type":"permission","resource":"rpc","inheritAud":true,"lxm":["app.bsky.actor.getPreferences","app.bsky.actor.putPreferences","app.bsky.graph.muteActor","app.bsky.graph.muteActorList","app.bsky.graph.muteThread","app.bsky.graph.unmuteActor","app.bsky.graph.unmuteActorList","app.bsky.graph.unmuteThread"]},{"type":"permission","resource":"repo","action":["create","update","delete"],"collection":["app.bsky.graph.block","app.bsky.graph.listblock"]}]}}}|}

  let official_auth_manage_notifications =
    {|{"lexicon":1,"id":"app.bsky.authManageNotifications","defs":{"main":{"type":"permission-set","title":"Manage Bluesky Notifications","detail":"View and configure notifications for the Bluesky app.","permissions":[{"type":"permission","resource":"rpc","inheritAud":true,"lxm":["app.bsky.notification.getPreferences","app.bsky.notification.getUnreadCount","app.bsky.notification.listActivitySubscriptions","app.bsky.notification.listNotifications","app.bsky.notification.putActivitySubscription","app.bsky.notification.putPreferences","app.bsky.notification.putPreferencesV2","app.bsky.notification.registerPush","app.bsky.notification.unregisterPush","app.bsky.notification.updateSeen"]}]}}}|}

  let official_auth_manage_profile =
    {|{"lexicon":1,"id":"app.bsky.authManageProfile","defs":{"main":{"type":"permission-set","title":"Manage Bluesky Profile","detail":"Update profile data, as well as status and public chat visibility.","permissions":[{"type":"permission","resource":"repo","action":["create","update","delete"],"collection":["app.bsky.actor.profile","app.bsky.actor.status","app.bsky.notification.declaration"]}]}}}|}

  let official_lexicons : (string * string) list =
    [
      ("app.bsky.graph.listitem", official_listitem);
      ("app.bsky.graph.starterpack", official_starterpack);
      ("app.bsky.graph.list", official_list);
      ("chat.bsky.notification.defs", official_chat_notification_defs);
      ("app.bsky.feed.getPostThread", official_get_post_thread);
      ("app.bsky.unspecced.getPostThreadV2", official_get_post_thread_v2);
      ("chat.bsky.moderation.subscribeModEvents", official_subscribe_mod_events);
      ("app.bsky.ageassurance.getState", official_ageassurance_get_state);
      ("app.bsky.draft.createDraft", official_draft_create);
      ("app.bsky.feed.searchPostsV2", official_search_posts_v2);
      ("app.bsky.graph.searchStarterPacksV2", official_search_starter_packs_v2);
      ("com.atproto.lexicon.resolveLexicon", official_resolve_lexicon);
      ("com.atproto.temp.checkHandleAvailability", official_check_handle);
      ("chat.bsky.group.createGroup", official_create_group);
      ("tools.ozone.queue.listQueues", official_list_queues);
      ("tools.ozone.queue.createQueue", official_create_queue);
      ("tools.ozone.report.queryReports", official_query_reports);
      ("com.atproto.temp.checkSignupQueue", official_check_signup);
      ("com.atproto.temp.dereferenceScope", official_deref_scope);
      ("com.atproto.temp.revokeAccountCredentials", official_revoke_creds);
      ("site.standard.graph.recommend", official_site_recommend);
      ("site.standard.graph.subscription", official_site_subscription);
      ("com.germnetwork.declaration", official_germ_declaration);
      ("tools.ozone.safelink.queryEvents", official_safelink_query_events);
      ("com.atproto.admin.updateAccountSigningKey", official_admin_signing_key);
      ("chat.bsky.embed.joinLink", official_join_link);
      ("com.atproto.server.confirmEmail", official_confirm_email);
      ("app.bsky.feed.defs", official_feed_known_likers);
      ("app.bsky.embed.video", official_embed_video);
      ("com.atproto.lexicon.schema", official_lexicon_schema);
      ("app.bsky.graph.muteActor", official_mute_actor);
      ("app.bsky.graph.getFollows", official_get_follows);
      ("app.bsky.graph.getFollowers", official_get_followers);
      ( "app.bsky.graph.getSuggestedFollowsByActor",
        official_get_suggested_follows_by_actor );
      ("app.bsky.actor.defs", official_actor_defs);
      ("com.atproto.server.createSession", official_create_session);
      ("com.atproto.server.createAccount", official_create_account);
      ("com.atproto.server.getSession", official_get_session);
      ("com.atproto.server.createAppPassword", official_create_app_password);
      ("app.bsky.embed.external", official_embed_external);
      ("app.bsky.feed.getAuthorFeed", official_get_author_feed);
      ("app.bsky.graph.defs", official_graph_relationship);
      ("com.atproto.server.checkAccountStatus", official_check_account_status);
      ("com.atproto.label.defs", official_label_value_definition);
      ("chat.bsky.convo.getMessages", official_get_messages);
      ("chat.bsky.convo.defs", official_convo_system_messages);
      ("chat.bsky.actor.defs", official_chat_actor_member);
      ("chat.bsky.convo.listConvoRequests", official_list_convo_requests);
      ("chat.bsky.group.defs", official_join_request_convo);
      ("app.bsky.ageassurance.defs", official_ageassurance_event);
      ("app.bsky.graph.referencelistoptout", official_referencelistoptout);
      ("app.bsky.authCreatePosts", official_auth_create_posts);
      ("chat.bsky.authFullChatClient", official_auth_full_chat);
      ("app.bsky.authFullApp", official_auth_full_app);
      ("app.bsky.authViewAll", official_auth_view_all);
      ("app.bsky.authDeleteContent", official_auth_delete_content);
      ("app.bsky.authManageFeedDeclarations", official_auth_manage_feed);
      ("app.bsky.authManageLabelerService", official_auth_manage_labeler);
      ("app.bsky.authManageModeration", official_auth_manage_moderation);
      ("app.bsky.authManageNotifications", official_auth_manage_notifications);
      ("app.bsky.authManageProfile", official_auth_manage_profile);
    ]

  let official_documents () : document list =
    List.map (fun (_id, body) -> of_string body) official_lexicons
end
