open Session
open Cohttp_client
open App

(** Shared XRPC GET/POST helpers for AppView, chat, ozone, and admin clients. *)
module Client = struct
  let public_appview_host = "public.api.bsky.app"

  let string_member json field =
    match Yojson.Safe.Util.member field json with `String s -> s | _ -> ""

  let string_opt json field =
    match Yojson.Safe.Util.member field json with
    | `String s -> Some s
    | _ -> None

  let int_opt json field =
    match Yojson.Safe.Util.member field json with
    | `Int n -> Some n
    | `Intlit s -> ( try Some (int_of_string s) with _ -> None)
    | _ -> None

  let int_member json field = Option.value ~default:0 (int_opt json field)

  let bool_opt json field =
    match Yojson.Safe.Util.member field json with
    | `Bool b -> Some b
    | _ -> None

  let bool_member json field = Option.value ~default:false (bool_opt json field)

  let list_member json field =
    match Yojson.Safe.Util.member field json with `List xs -> xs | _ -> []

  let opt_pair k v = match v with Some s -> [ (k, s) ] | None -> []

  let opt_int k v =
    match v with Some n -> [ (k, string_of_int n) ] | None -> []

  let opt_bool k v =
    match v with Some b -> [ (k, string_of_bool b) ] | None -> []

  let repeat_param key values = List.map (fun v -> (key, v)) values

  let request_headers ?session ?(extra = []) () =
    let pairs =
      Cohttp_client.application_json_setting_tuple
      ::
      (match session with
      | Some s -> [ Session.bearer_token_from_session s ]
      | None -> [])
      @ extra
    in
    Cohttp_client.create_headers_from_pairs pairs

  let host_of ?session ?host () =
    match host with
    | Some h -> h
    | None -> (
        match session with
        | Some s -> s.Session.atp_host
        | None -> public_appview_host)

  let nsid_url ?session ?host nsid =
    App.create_endpoint_url
      (App.create_public_base_url ~host:(host_of ?session ?host ()) ())
      nsid

  let get_json ?session ?host ?(extra = []) nsid pairs =
    let headers = request_headers ?session ~extra () in
    let url = nsid_url ?session ?host nsid in
    let body = Cohttp_client.create_body_from_pairs pairs in
    let resp =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers url body headers)
    in
    Yojson.Safe.from_string resp

  let post_json ?session ?host ?(extra = []) nsid data =
    let headers = request_headers ?session ~extra () in
    let url = nsid_url ?session ?host nsid in
    let resp =
      Lwt_main.run (Cohttp_client.post_data_with_headers url data headers)
    in
    Yojson.Safe.from_string resp
end
