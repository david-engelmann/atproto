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
    {|{"lexicon":1,"id":"app.bsky.feed.defs","defs":{"knownLikers":{"type":"object","description":"The post's likers whom you also follow","required":["count","actors"],"properties":{"count":{"type":"integer"},"actors":{"type":"array","items":{"type":"ref","ref":"app.bsky.actor.defs#profileViewBasic"}}}},"viewerState":{"type":"object","properties":{"repost":{"type":"string"},"like":{"type":"string"},"bookmarked":{"type":"boolean"},"threadMuted":{"type":"boolean"},"replyDisabled":{"type":"boolean"},"embeddingDisabled":{"type":"boolean"},"pinned":{"type":"boolean"},"knownLikers":{"type":"ref","ref":"#knownLikers"}}}}}|}

  let official_embed_video =
    {|{"lexicon":1,"id":"app.bsky.embed.video","defs":{"main":{"type":"object","required":["video"],"properties":{"video":{"type":"blob"},"alt":{"type":"string"},"aspectRatio":{"type":"ref","ref":"app.bsky.embed.defs#aspectRatio"},"presentation":{"type":"string","knownValues":["default","gif"]}}},"view":{"type":"object","required":["cid","playlist"],"properties":{"cid":{"type":"string"},"playlist":{"type":"string"},"alt":{"type":"string"},"presentation":{"type":"string","knownValues":["default","gif"]}}}}}|}

  let official_lexicon_schema =
    {|{"lexicon":1,"id":"com.atproto.lexicon.schema","defs":{"main":{"type":"record","description":"Representation of Lexicon schemas themselves, when published as atproto records.","key":"nsid","record":{"type":"object","required":["lexicon"],"properties":{"lexicon":{"type":"integer"}}}}}}|}

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
    ]

  let official_documents () : document list =
    List.map (fun (_id, body) -> of_string body) official_lexicons
end
