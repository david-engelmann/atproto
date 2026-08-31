open Session
open Client

(** app.bsky.ageassurance — dedicated Age Assurance namespace (current lexicons). *)
module Ageassurance = struct
  type state = {
    status : string;
    access : string;
    last_initiated_at : string option;
  }

  type state_metadata = { account_created_at : string option }
  type state_bundle = { state : state; metadata : state_metadata }

  type region_rule =
    [ `Default of string
    | `Declared_over of int * string
    | `Declared_under of int * string
    | `Assured_over of int * string
    | `Assured_under of int * string
    | `Account_newer of string * string
    | `Account_older of string * string
    | `Unknown of Yojson.Safe.t ]

  type config_region = {
    platforms : string list;
    country_code : string;
    region_code : string option;
    min_access_age : int;
    additional_verification_methods : string list;
    rules : region_rule list;
  }

  type config = { regions : config_region list }

  type event = {
    created_at : string;
    attempt_id : string;
    status : string;
    access : string;
    country_code : string;
    region_code : string option;
    email : string option;
    original : Yojson.Safe.t;
  }

  let ends_with suffix s =
    let n = String.length s and m = String.length suffix in
    n >= m && String.sub s (n - m) m = suffix

  let string_list json field =
    List.filter_map
      (function `String s -> Some s | _ -> None)
      (Client.list_member json field)

  let parse_state json : state =
    {
      status = Client.string_member json "status";
      access = Client.string_member json "access";
      last_initiated_at = Client.string_opt json "lastInitiatedAt";
    }

  let parse_state_metadata json : state_metadata =
    { account_created_at = Client.string_opt json "accountCreatedAt" }

  let parse_state_bundle json : state_bundle =
    let state =
      match Yojson.Safe.Util.member "state" json with
      | `Assoc _ as s -> parse_state s
      | _ -> parse_state json
    in
    let metadata =
      match Yojson.Safe.Util.member "metadata" json with
      | `Assoc _ as m -> parse_state_metadata m
      | _ -> { account_created_at = None }
    in
    { state; metadata }

  let parse_region_rule json : region_rule =
    let ty = Client.string_opt json "$type" |> Option.value ~default:"" in
    let access = Client.string_member json "access" in
    if ends_with "configRegionRuleDefault" ty then `Default access
    else if ends_with "configRegionRuleIfDeclaredOverAge" ty then
      `Declared_over (Client.int_member json "age", access)
    else if ends_with "configRegionRuleIfDeclaredUnderAge" ty then
      `Declared_under (Client.int_member json "age", access)
    else if ends_with "configRegionRuleIfAssuredOverAge" ty then
      `Assured_over (Client.int_member json "age", access)
    else if ends_with "configRegionRuleIfAssuredUnderAge" ty then
      `Assured_under (Client.int_member json "age", access)
    else if ends_with "configRegionRuleIfAccountNewerThan" ty then
      `Account_newer (Client.string_member json "date", access)
    else if ends_with "configRegionRuleIfAccountOlderThan" ty then
      `Account_older (Client.string_member json "date", access)
    else `Unknown json

  let parse_config_region json : config_region =
    {
      platforms = string_list json "platforms";
      country_code = Client.string_member json "countryCode";
      region_code = Client.string_opt json "regionCode";
      min_access_age = Client.int_member json "minAccessAge";
      additional_verification_methods =
        string_list json "additionalVerificationMethods";
      rules = List.map parse_region_rule (Client.list_member json "rules");
    }

  let parse_config json : config =
    {
      regions = List.map parse_config_region (Client.list_member json "regions");
    }

  let parse_event json : event =
    {
      created_at = Client.string_member json "createdAt";
      attempt_id = Client.string_member json "attemptId";
      status = Client.string_member json "status";
      access = Client.string_member json "access";
      country_code = Client.string_member json "countryCode";
      region_code = Client.string_opt json "regionCode";
      email = Client.string_opt json "email";
      original = json;
    }

  let begin_body ~email ~language ~country_code ?region_code () : Yojson.Safe.t
      =
    let fields =
      [
        ("email", `String email);
        ("language", `String language);
        ("countryCode", `String country_code);
      ]
      @
      match region_code with
      | Some r -> [ ("regionCode", `String r) ]
      | None -> []
    in
    `Assoc fields

  let get_config ?session ?host () : config =
    Client.get_json ?session ?host "app.bsky.ageassurance.getConfig" []
    |> parse_config

  let get_state (s : Session.session) ~country_code ?region_code () :
      state_bundle =
    Client.get_json ~session:s "app.bsky.ageassurance.getState"
      ([ ("countryCode", country_code) ]
      @ Client.opt_pair "regionCode" region_code)
    |> parse_state_bundle

  let begin_assurance (s : Session.session) ~email ~language ~country_code
      ?region_code () : state =
    Client.post_json ~session:s "app.bsky.ageassurance.begin"
      (Yojson.Safe.to_string
         (begin_body ~email ~language ~country_code ?region_code ()))
    |> parse_state
end
