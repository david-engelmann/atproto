open Client

(** com.atproto.temp — temporary public helpers that are not yet stable. *)
module Temp = struct
  type handle_suggestion = { handle : string; method_ : string }

  type handle_availability =
    [ `Available
    | `Unavailable of handle_suggestion list
    | `Unknown of Yojson.Safe.t ]

  type handle_check = {
    handle : string;
    result : handle_availability;
    original : Yojson.Safe.t;
  }

  let parse_suggestion json : handle_suggestion =
    {
      handle = Client.string_member json "handle";
      method_ = Client.string_member json "method";
    }

  let parse_handle_availability json : handle_availability =
    let ty = Option.value ~default:"" (Client.string_opt json "$type") in
    let ends_with suffix =
      let n = String.length ty and m = String.length suffix in
      n >= m && String.sub ty (n - m) m = suffix
    in
    if ends_with "resultAvailable" then `Available
    else if ends_with "resultUnavailable" then
      `Unavailable
        (List.map parse_suggestion (Client.list_member json "suggestions"))
    else
      match Yojson.Safe.Util.member "suggestions" json with
      | `List _ ->
          `Unavailable
            (List.map parse_suggestion (Client.list_member json "suggestions"))
      | _ -> if ty = "" then `Available else `Unknown json

  let parse_handle_check json : handle_check =
    {
      handle = Client.string_member json "handle";
      result =
        (match Yojson.Safe.Util.member "result" json with
        | `Assoc _ as r -> parse_handle_availability r
        | _ -> parse_handle_availability json);
      original = json;
    }

  let check_handle_availability ?session ?host ~handle ?email ?birth_date () :
      handle_check =
    Client.get_json ?session ?host "com.atproto.temp.checkHandleAvailability"
      ((("handle", handle) :: Client.opt_pair "email" email)
      @ Client.opt_pair "birthDate" birth_date)
    |> parse_handle_check
end
