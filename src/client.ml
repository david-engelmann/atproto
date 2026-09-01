open Session
open Cohttp_client
open App
open Http_client
open Response

(** Shared XRPC GET/POST helpers for AppView, chat, ozone, and admin clients. *)
module Client = struct
  let public_appview_host = "public.api.bsky.app"

  (* Local official @atproto/dev-env AppView is localhost:2584. *)
  let appview_host_from_env : string =
    match Sys.getenv_opt "ATP_APPVIEW_HOST" with
    | Some h ->
        let h = String.trim h in
        if h = "" then public_appview_host else h
    | None -> public_appview_host

  (* Production AppView DID. Local @atproto/dev-env writes ATP_APPVIEW_DID. *)
  let appview_did_from_env : string =
    match Sys.getenv_opt "ATP_APPVIEW_DID" with
    | Some d ->
        let d = String.trim d in
        if d = "" then "did:web:api.bsky.app" else d
    | None -> "did:web:api.bsky.app"

  let bearer_jwt (token : string) : string * string =
    ("Authorization", "Bearer " ^ token)

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

  let request_headers ?session ?bearer ?(extra = []) () =
    let pairs =
      Cohttp_client.application_json_setting_tuple
      ::
      (match bearer with
      | Some token -> [ bearer_jwt token ]
      | None -> (
          match session with
          | Some s -> [ Session.bearer_token_from_session s ]
          | None -> []))
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

  (* XRPC procedures with no output lexicon return an empty body. *)
  let json_of_body (body : string) : Yojson.Safe.t =
    let body = String.trim body in
    if body = "" then `Assoc [] else Yojson.Safe.from_string body

  let get_json ?session ?host ?bearer ?(extra = []) nsid pairs =
    let headers = request_headers ?session ?bearer ~extra () in
    let url = nsid_url ?session ?host nsid in
    let body = Cohttp_client.create_body_from_pairs pairs in
    let resp =
      Lwt_main.run
        (Cohttp_client.get_request_with_body_and_headers url body headers)
    in
    json_of_body resp

  let get_text ?session ?host ?bearer ?(extra = []) nsid pairs =
    let headers = request_headers ?session ?bearer ~extra () in
    let url = nsid_url ?session ?host nsid in
    let body = Cohttp_client.create_body_from_pairs pairs in
    Lwt_main.run
      (Cohttp_client.get_request_with_body_and_headers url body headers)

  let post_json ?session ?host ?bearer ?(extra = []) nsid data =
    let headers = request_headers ?session ?bearer ~extra () in
    let url = nsid_url ?session ?host nsid in
    let resp =
      Lwt_main.run (Cohttp_client.post_data_with_headers url data headers)
    in
    json_of_body resp

  let header_pairs ?session ?bearer ?(extra = []) () =
    Cohttp_client.application_json_setting_tuple
    ::
    (match bearer with
    | Some token -> [ bearer_jwt token ]
    | None -> (
        match session with
        | Some s -> [ Session.bearer_token_from_session s ]
        | None -> []))
    @ extra

  (* HTTPS-only HTTP/2 GET that keeps status + response headers. Hosts with
     an explicit port (local stacks) stay on Cohttp. *)
  let get_json_h2 ?session ?host ?bearer ?(extra = []) nsid pairs =
    let host = host_of ?session ?host () in
    if String.contains host ':' then
      get_json ?session ~host ?bearer ~extra nsid pairs
    else
      let headers = header_pairs ?session ?bearer ~extra () in
      let url = Http_client.xrpc_url ~host nsid ~query:pairs () in
      let resp = Http_client.run (Http_client.get url ~headers ()) in
      json_of_body (Response.body_string resp)

  (* Symmetric HTTP/2 POST. Local :port hosts stay on Cohttp. *)
  let post_json_h2 ?session ?host ?bearer ?(extra = []) nsid data =
    let host = host_of ?session ?host () in
    if String.contains host ':' then
      post_json ?session ~host ?bearer ~extra nsid data
    else
      let headers = header_pairs ?session ?bearer ~extra () in
      let resp =
        Http_client.run
          (Http_client.xrpc_post ~host ~nsid ~headers ~body:data ())
      in
      json_of_body (Response.body_string resp)

  (* PDS accessJwt is at+jwt. AppView requires a service-auth JWT
     (com.atproto.server.getServiceAuth, aud=AppView DID, lxm=NSID).
     OAuth sessions mint the same JWT with Oauth.get_service_auth (DPoP)
     and pass it here as ~bearer — Client cannot depend on Oauth. *)
  let get_service_auth (s : Session.session) ~aud ~lxm () : string =
    let json =
      get_json ~session:s "com.atproto.server.getServiceAuth"
        [ ("aud", aud); ("lxm", lxm) ]
    in
    match Yojson.Safe.Util.member "token" json with
    | `String t when String.trim t <> "" -> t
    | _ -> failwith ("getServiceAuth failed: " ^ Yojson.Safe.to_string json)

  let get_json_appview ?session ?host ?aud ?(extra = []) nsid pairs =
    let host = match host with Some h -> h | None -> appview_host_from_env in
    match session with
    | None -> get_json ~host ~extra nsid pairs
    | Some s ->
        let aud = match aud with Some a -> a | None -> appview_did_from_env in
        let token = get_service_auth s ~aud ~lxm:nsid () in
        get_json ~host ~bearer:token ~extra nsid pairs

  let post_json_appview ?session ?host ?aud ?(extra = []) nsid data =
    let host = match host with Some h -> h | None -> appview_host_from_env in
    match session with
    | None -> post_json ~host ~extra nsid data
    | Some s ->
        let aud = match aud with Some a -> a | None -> appview_did_from_env in
        let token = get_service_auth s ~aud ~lxm:nsid () in
        post_json ~host ~bearer:token ~extra nsid data
end
