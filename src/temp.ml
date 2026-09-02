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

  (** Check whether [handle] is available via
      [com.atproto.temp.checkHandleAvailability]. Optional [email] /
      [birth_date] may yield suggestions when unavailable. *)
  let check_handle_availability ?session ?host ~handle ?email ?birth_date () :
      handle_check =
    Client.get_json ?session ?host "com.atproto.temp.checkHandleAvailability"
      ((("handle", handle) :: Client.opt_pair "email" email)
      @ Client.opt_pair "birthDate" birth_date)
    |> parse_handle_check

  type signup_queue = {
    activated : bool;
    place_in_queue : int option;
    estimated_time_ms : int option;
    original : Yojson.Safe.t;
  }

  type scope_deref = { scope : string; original : Yojson.Safe.t }

  let parse_signup_queue json : signup_queue =
    {
      activated = Client.bool_member json "activated";
      place_in_queue = Client.int_opt json "placeInQueue";
      estimated_time_ms = Client.int_opt json "estimatedTimeMs";
      original = json;
    }

  let parse_scope_deref json : scope_deref =
    { scope = Client.string_member json "scope"; original = json }

  let check_signup_queue ?session ?host () : signup_queue =
    Client.get_json ?session ?host "com.atproto.temp.checkSignupQueue" []
    |> parse_signup_queue

  let dereference_scope ?session ?host ~scope () : scope_deref =
    Client.get_json ?session ?host "com.atproto.temp.dereferenceScope"
      [ ("scope", scope) ]
    |> parse_scope_deref

  let add_reserved_handle_body ~handle () : Yojson.Safe.t =
    `Assoc [ ("handle", `String handle) ]

  let request_phone_verification_body ~phone_number () : Yojson.Safe.t =
    `Assoc [ ("phoneNumber", `String phone_number) ]

  let revoke_account_credentials_body ~account () : Yojson.Safe.t =
    `Assoc [ ("account", `String account) ]

  (* Privileged / hosted-PDS flows: clients only. Live calls need a real
     operator session and are not invented here. fetchLabels is deprecated
     in favor of Label.query_labels. *)

  let add_reserved_handle ?session ?host ~handle () : unit =
    ignore
      (Client.post_json ?session ?host "com.atproto.temp.addReservedHandle"
         (Yojson.Safe.to_string (add_reserved_handle_body ~handle ())))

  let request_phone_verification ?session ?host ~phone_number () : unit =
    ignore
      (Client.post_json ?session ?host
         "com.atproto.temp.requestPhoneVerification"
         (Yojson.Safe.to_string
            (request_phone_verification_body ~phone_number ())))

  let revoke_account_credentials ?session ?host ~account () : unit =
    ignore
      (Client.post_json ?session ?host
         "com.atproto.temp.revokeAccountCredentials"
         (Yojson.Safe.to_string (revoke_account_credentials_body ~account ())))
end
