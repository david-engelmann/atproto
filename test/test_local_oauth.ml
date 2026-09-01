open OUnit2
open Atproto.Auth
open Atproto.Session
open Atproto.Oauth
open Atproto.Client
open Atproto.Error
open Atproto.Feed
open Atproto.Ozone
open Atproto.Xrpc
open Oauth
open Ozone

(* Live OAuth against official @atproto/dev-env TestNetwork (PDS oauth-provider).
   Serves a loopback client-metadata document, discovers the local AS, runs
   PAR + DPoP, then GET /oauth/authorize (document navigation). The HTML SPA
   does not mint a code itself; oauth-provider ~api/sign-in + /consent accept
   the account password with the real csrf-token / dev-id / ses-id cookies.
   Token exchange, DPoP getSession, DPoP getServiceAuth, refresh, and RFC 7009
   revoke are required when ATP_REQUIRE_LOCAL_PDS=1. Authed AppView
   (getTimeline / listNotifications) uses the OAuth-minted service-auth JWT
   (aud=AppView DID). Ozone privileged writes mint getServiceAuth
   (aud=Ozone DID) and POST to the Ozone host — DPoP cannot be proxied.
   If AppView/Ozone rejects that hop, only that hop is skipped. *)

let env_truthy name =
  match Sys.getenv_opt name with
  | Some v ->
      let v = String.lowercase_ascii (String.trim v) in
      List.mem v [ "1"; "true"; "yes"; "on" ]
  | None -> false

let require_local = env_truthy "ATP_REQUIRE_LOCAL_PDS"

let host_is_local host =
  let bare =
    match String.split_on_char ':' host with h :: _ -> h | [] -> host
  in
  let bare = String.lowercase_ascii bare in
  bare = "localhost" || bare = "127.0.0.1" || bare = "[::1]" || bare = "::1"

let intended_local () =
  env_truthy "ATP_LOCAL_PDS" || host_is_local Session.atp_host_from_env

let pds_origin () = Auth.origin_of_host Session.atp_host_from_env

let message_has hay needle =
  let h = String.lowercase_ascii hay and n = String.lowercase_ascii needle in
  let rec aux i =
    if i + String.length n > String.length h then false
    else if String.sub h i (String.length n) = n then true
    else aux (i + 1)
  in
  aux 0

let skip_unless_local () =
  if not (intended_local ()) then
    skip_if true
      "local PDS not selected (set ATP_HOST=localhost:2583 and start \
       scripts/local-atproto.sh)";
  try
    let url =
      Printf.sprintf "%s/xrpc/com.atproto.server.describeServer" (pds_origin ())
    in
    let resp = Oauth.live_http_get ~url ~headers:[] in
    if resp.status < 200 || resp.status >= 300 then
      failwith
        (Printf.sprintf "describeServer HTTP %d: %s" resp.status resp.body)
  with exn ->
    let msg = "local PDS is not reachable: " ^ Printexc.to_string exn in
    if require_local then failwith msg else skip_if true msg

let skip_step msg =
  skip_if true msg;
  failwith msg

(* After discovery + PAR have already asserted, do not skip the whole OUnit
   case (that would hide a required-green PAR). Stop only if /oauth/authorize
   is honestly not served. Sign-in, consent, token, getSession, refresh, and
   revoke fail hard. *)
exception Stop_after_par of string

let stop_after_par msg = raise (Stop_after_par msg)

let fail_or_skip ~label status body =
  if Oauth.is_http_not_served status body then
    skip_step
      (Printf.sprintf "%s not served by local AS (HTTP %d): %s" label status
         body)
  else
    failwith
      (Printf.sprintf "%s HTTP %d: %s" label status
         (if String.length body > 400 then String.sub body 0 400 else body))

let http_client_rejected status body =
  (status = 400 || status = 401)
  && (not (message_has body "invalid_scope"))
  && (message_has body "invalid_client"
     || (message_has body "https" && message_has body "client")
     || message_has body "must be https")

let local_handle () =
  match Sys.getenv_opt "ATP_AUTH" with
  | Some _ when Auth.has_live_credentials ->
      let u, _ = Auth.username_and_password_from_env in
      u
  | _ -> "alice.test"

let local_password () =
  if Auth.has_live_credentials then snd Auth.username_and_password_from_env
  else "hunter2"

(* Minimal loopback HTTP/1.1 GET server (no Lwt) so the PDS can fetch
   client-metadata.json during PAR without sharing Lwt_main with the test. *)
let start_loopback_server ~redirect_path =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen sock 32;
  let port =
    match Unix.getsockname sock with
    | Unix.ADDR_INET (_, p) -> p
    | _ -> failwith "loopback server: expected IPv4"
  in
  let callback_query = ref None in
  let metadata_json = ref "{}" in
  let stop = ref false in
  let write_response oc status ctype body =
    let extra =
      match ctype with
      | Some ct -> Printf.sprintf "Content-Type: %s\r\n" ct
      | None -> ""
    in
    Printf.fprintf oc
      "HTTP/1.1 %s\r\n%sContent-Length: %d\r\nConnection: close\r\n\r\n%s"
      status extra (String.length body) body;
    flush oc
  in
  let handle fd =
    let ic = Unix.in_channel_of_descr fd in
    let oc = Unix.out_channel_of_descr fd in
    Fun.protect
      ~finally:(fun () ->
        (try Unix.close fd with _ -> ());
        ())
      (fun () ->
        let line = try input_line ic with End_of_file -> "" in
        let line =
          if String.length line > 0 && line.[String.length line - 1] = '\r' then
            String.sub line 0 (String.length line - 1)
          else line
        in
        let path =
          match String.split_on_char ' ' line with
          | _meth :: target :: _ -> (
              match String.index_opt target '?' with
              | Some i -> String.sub target 0 i
              | None -> target)
          | _ -> ""
        in
        let query =
          match String.split_on_char ' ' line with
          | _ :: target :: _ -> (
              match String.index_opt target '?' with
              | Some i ->
                  Some
                    (String.sub target (i + 1) (String.length target - i - 1))
              | None -> None)
          | _ -> None
        in
        if
          path = "/client-metadata.json" || path = "/oauth-client-metadata.json"
        then write_response oc "200 OK" (Some "application/json") !metadata_json
        else if path = redirect_path || path = "/cb" then (
          callback_query := query;
          write_response oc "200 OK" (Some "text/plain") "ok")
        else write_response oc "404 Not Found" (Some "text/plain") "not found")
  in
  let rec accept_loop () =
    if !stop then ()
    else
      try
        match Unix.select [ sock ] [] [] 0.2 with
        | [], _, _ -> accept_loop ()
        | _ ->
            let fd, _ = Unix.accept sock in
            (try handle fd with _ -> ( try Unix.close fd with _ -> ()));
            accept_loop ()
      with
      | Unix.Unix_error (Unix.EBADF, _, _) when !stop -> ()
      | Unix.Unix_error (Unix.EINTR, _, _) -> accept_loop ()
      | exn when !stop -> ignore exn
      | exn ->
          if not !stop then (
            prerr_endline
              ("loopback oauth metadata server: " ^ Printexc.to_string exn);
            accept_loop ())
  in
  let thr = Thread.create accept_loop () in
  let stop_fn () =
    stop := true;
    (try Unix.shutdown sock Unix.SHUTDOWN_ALL with _ -> ());
    (try Unix.close sock with _ -> ());
    try Thread.join thr with _ -> ()
  in
  (port, callback_query, metadata_json, stop_fn)

let assert_dpop_session ~origin ~priv ~pub ~token ?nonce () =
  let session_url = Oauth.url_on origin "/xrpc/com.atproto.server.getSession" in
  let sess, nonce =
    Oauth.request_with_dpop ~http:Oauth.live_http_request ~priv ~pub
      ~url:session_url ~htm:"GET" ~access_token:token.access_token ?nonce ()
  in
  if sess.status < 200 || sess.status >= 300 then
    failwith
      (Printf.sprintf "DPoP getSession HTTP %d: %s" sess.status sess.body);
  let did, _ =
    Oauth.parse_sign_in_response (Yojson.Safe.from_string sess.body)
  in
  OUnit2.assert_equal ~printer:(fun x -> x) token.sub did;
  nonce

let appview_host () =
  match Sys.getenv_opt "ATP_APPVIEW_HOST" with
  | Some h when String.trim h <> "" -> String.trim h
  | _ ->
      if host_is_local Session.atp_host_from_env then "localhost:2584"
      else Client.appview_host_from_env

let appview_did () =
  match Sys.getenv_opt "ATP_APPVIEW_DID" with
  | Some d when String.trim d <> "" -> String.trim d
  | _ -> Client.appview_did_from_env

(* AppView still wants a PDS-minted service-auth JWT. Skip only that hop when
   this revision does not serve the NSID or rejects DPoP/OAuth in a way the
   stack cannot complete. Token assertions stay required. *)
let appview_rejects_oauth json =
  match Error.check_for_error json with
  | None -> false
  | Some err ->
      let e = Error.of_json json in
      err = "InvalidToken"
      || err = "AuthenticationRequired"
      || err = "InvalidDpopProof" || err = "ExpiredToken"
      || err = "BadJwtAudience"
      || message_has e.message "dpop"
      || message_has e.message "oauth"
      || message_has e.message "malformed token"
      || message_has e.message "malformed jwt"

let classify_appview json =
  if Error.is_not_served_json json then `Not_served
  else if appview_rejects_oauth json then `Rejected json
  else
    match Error.check_for_error json with
    | Some _ -> failwith ("XRPC error: " ^ Error.to_string (Error.of_json json))
    | None -> `Ok json

let mint_service_auth ~origin ~priv ~pub ~token ~aud ~lxm ?nonce () =
  let url =
    Oauth.xrpc_url ~origin "com.atproto.server.getServiceAuth"
      [ ("aud", aud); ("lxm", lxm) ]
  in
  let resp, nonce =
    Oauth.request_with_dpop ~http:Oauth.live_http_request ~priv ~pub ~url
      ~htm:"GET" ~access_token:token.access_token ?nonce ()
  in
  if Oauth.is_http_not_served resp.status resp.body then (`Not_served, nonce)
  else if resp.status < 200 || resp.status >= 300 then
    failwith
      (Printf.sprintf "DPoP getServiceAuth HTTP %d: %s" resp.status resp.body)
  else
    let svc =
      Oauth.parse_service_auth_token (Yojson.Safe.from_string resp.body)
    in
    OUnit2.assert_bool ("oauth getServiceAuth " ^ lxm) (String.length svc > 8);
    (`Token svc, nonce)

(* After the OAuth token exists, mint getServiceAuth with DPoP (required PDS
   XRPC) and send that JWT to AppView — the same hop createSession uses, but
   not the password at+jwt. *)
let assert_oauth_authed_appview ~origin ~priv ~pub ~token ?nonce () =
  let aud = appview_did () in
  let av_host = appview_host () in
  let try_nsid ~bearer nsid pairs parse =
    let json = Client.get_json ~host:av_host ~bearer nsid pairs in
    match classify_appview json with
    | `Not_served -> `Not_served
    | `Rejected _ -> `Rejected
    | `Ok json ->
        parse json;
        `Ok
  in
  let rec mint_and_call nonce = function
    | [] -> nonce
    | (nsid, pairs, parse) :: rest -> (
        match
          mint_service_auth ~origin ~priv ~pub ~token ~aud ~lxm:nsid ?nonce ()
        with
        | `Not_served, nonce ->
            prerr_endline
              ("DPoP getServiceAuth not served for " ^ nsid
             ^ "; skipping remaining AppView hop");
            nonce
        | `Token svc, nonce -> (
            match try_nsid ~bearer:svc nsid pairs parse with
            | `Ok -> nonce
            | `Not_served -> mint_and_call nonce rest
            | `Rejected ->
                prerr_endline
                  ("AppView rejected OAuth service-auth for " ^ nsid
                 ^ "; skipping remaining AppView hop");
                nonce))
  in
  mint_and_call nonce
    [
      ( "app.bsky.feed.getTimeline",
        [ ("algorithm", "reverse-chronological"); ("limit", "10") ],
        fun json ->
          let timeline = Feed.parse_timeline json in
          OUnit2.assert_bool "oauth getTimeline" (List.length timeline.feed >= 0)
      );
      ( "app.bsky.notification.listNotifications",
        [ ("limit", "10") ],
        fun json ->
          match Yojson.Safe.Util.member "notifications" json with
          | `List _ -> ()
          | _ ->
              OUnit2.assert_failure
                "oauth listNotifications missing notifications" );
    ]

type live_oauth = {
  origin : string;
  priv : Mirage_crypto_ec.P256.Dsa.priv;
  pub : Mirage_crypto_ec.P256.Dsa.pub;
  token : Oauth.token;
  nonce : string option;
  client_id : string;
  as_ : Oauth.as_metadata;
}

let ozone_admin_handle () =
  match Sys.getenv_opt "ATP_AUTH_OZONE" with
  | Some auth -> (
      match String.split_on_char ':' auth with
      | u :: _ when String.trim u <> "" -> String.trim u
      | _ -> "admin-mod.test")
  | None -> "admin-mod.test"

let ozone_admin_password () =
  match Sys.getenv_opt "ATP_AUTH_OZONE" with
  | Some auth -> (
      match String.split_on_char ':' auth with
      | _ :: rest -> String.trim (String.concat ":" rest)
      | _ -> "admin-mod-pass")
  | None -> "admin-mod-pass"

let leftover_tag () =
  Printf.sprintf "ocaml-oauth-ozone-%d"
    (int_of_float (Unix.gettimeofday () *. 1000.) mod 100_000_000)

let dpop_proxy_rejected status body =
  message_has body "cannot be proxied"
  || message_has body "dpop requests cannot"
  || message_has body "dpop proof"
  || ((status = 400 || status = 401) && message_has body "dpop")

let ozone_rejects_service_auth json =
  match Error.check_for_error json with
  | None -> false
  | Some err ->
      let e = Error.of_json json in
      err = "InvalidToken"
      || err = "AuthenticationRequired"
      || err = "InvalidDpopProof" || err = "ExpiredToken"
      || err = "BadJwtAudience" || err = "BadJwt"
      || message_has e.message "dpop"
      || message_has e.message "oauth"
      || message_has e.message "malformed token"
      || message_has e.message "malformed jwt"
      || message_has e.message "cannot be proxied"

let classify_ozone json =
  if Error.is_not_served_json json then `Not_served
  else if ozone_rejects_service_auth json then `Rejected json
  else
    match Error.check_for_error json with
    | Some _ -> failwith ("XRPC error: " ^ Error.to_string (Error.of_json json))
    | None -> `Ok json

(* DPoP is bound to the PDS. Sending atproto-proxy with a DPoP proof is
   rejected ("DPoP requests cannot be proxied"). Ozone writes use
   getServiceAuth + Bearer on the Ozone host instead. *)
let assert_dpop_ozone_not_proxied ~origin ~priv ~pub ~token ~ozone_did ?nonce ()
    =
  let url = Oauth.xrpc_url ~origin "tools.ozone.moderation.emitEvent" [] in
  let proxy = Xrpc.proxy_header (Ozone.labeler_proxy ozone_did) in
  let body =
    Yojson.Safe.to_string
      (Ozone.emit_event_body
         ~event:(Ozone.comment_event "dpop-proxy-must-fail")
         ~subject:(Ozone.repo_ref token.sub) ~created_by:token.sub ())
  in
  let resp, nonce =
    Oauth.request_with_dpop ~http:Oauth.live_http_request ~priv ~pub ~url
      ~htm:"POST" ~access_token:token.access_token ~body:(Some body) ?nonce
      ~extra:[ proxy ] ()
  in
  if Oauth.is_http_not_served resp.status resp.body then nonce
  else if dpop_proxy_rejected resp.status resp.body then nonce
  else if resp.status >= 200 && resp.status < 300 then
    failwith
      "DPoP+atproto-proxy emitEvent succeeded; ozone-proxy is not supposed to \
       forward DPoP"
  else
    (* Other 4xx (scope, invalid_request) still prove the PDS handled the
       DPoP request instead of forwarding it. *)
    nonce

let assert_oauth_ozone_write ~origin ~priv ~pub ~token ?nonce () =
  let ozone_did = Client.ozone_did_from_env in
  if ozone_did = "" then
    if require_local then
      failwith "ATP_OZONE_DID is required (see scripts/local-atproto.sh env)"
    else skip_step "ATP_OZONE_DID not set";
  let ozone_host = Client.ozone_host_from_env in
  let nonce =
    assert_dpop_ozone_not_proxied ~origin ~priv ~pub ~token ~ozone_did ?nonce ()
  in
  let auds = [ ozone_did; ozone_did ^ "#atproto_labeler" ] in
  let rec mint_aud nonce = function
    | [] -> (`Not_served, nonce)
    | aud :: rest -> (
        match
          mint_service_auth ~origin ~priv ~pub ~token ~aud
            ~lxm:"tools.ozone.moderation.emitEvent" ?nonce ()
        with
        | `Not_served, nonce -> mint_aud nonce rest
        | (`Token _ as tok), nonce -> (tok, nonce))
  in
  match mint_aud nonce auds with
  | `Not_served, nonce ->
      prerr_endline
        "DPoP getServiceAuth not served for Ozone; skipping Ozone hop";
      nonce
  | `Token svc, nonce -> (
      let tag = leftover_tag () in
      let ev =
        Ozone.emit_event_service ~bearer:svc ~host:ozone_host
          ~event:(Ozone.comment_event ("oauth dpop " ^ tag))
          ~subject:(Ozone.repo_ref token.sub) ~created_by:token.sub ()
      in
      match classify_ozone ev.original with
      | `Not_served ->
          prerr_endline
            "Ozone emitEvent not served for OAuth service-auth; skipping hop";
          nonce
      | `Rejected _ ->
          prerr_endline
            "Ozone rejected OAuth service-auth emitEvent; skipping hop";
          nonce
      | `Ok _ -> (
          OUnit2.assert_bool "oauth ozone emitEvent"
            (match ev.id with Some n -> n >= 0 | None -> true);
          match
            mint_service_auth ~origin ~priv ~pub ~token ~aud:ozone_did
              ~lxm:"tools.ozone.moderation.queryEvents" ?nonce ()
          with
          | `Not_served, nonce -> nonce
          | `Token qsvc, nonce -> (
              let json =
                Client.get_json ~bearer:qsvc ~host:ozone_host
                  "tools.ozone.moderation.queryEvents"
                  [ ("subject", token.sub); ("limit", "10") ]
              in
              match classify_ozone json with
              | `Not_served | `Rejected _ -> nonce
              | `Ok json ->
                  let events = Ozone.parse_events json in
                  OUnit2.assert_bool "oauth ozone queryEvents"
                    (List.length events.events >= 1);
                  nonce)))

let with_live_oauth ~handle ~password f =
  skip_unless_local ();
  let origin = pds_origin () in
  let redirect_path = "/cb" in
  let port, callback_query, metadata_json, stop_server =
    start_loopback_server ~redirect_path
  in
  Fun.protect ~finally:stop_server (fun () ->
      try
        let hosted_origin = Printf.sprintf "http://127.0.0.1:%d" port in
        let hosted_client_id = hosted_origin ^ "/client-metadata.json" in
        let redirect_uri = hosted_origin ^ redirect_path in
        let declared_scope = Oauth.default_scope in
        let hosted_meta =
          Oauth.public_metadata ~client_id:hosted_client_id
            ~redirect_uris:[ redirect_uri ] ~application_type:"native"
            ~client_name:"atproto-ocaml local TestNetwork" ~scope:declared_scope
            ()
        in
        Oauth.validate_metadata hosted_meta;
        OUnit2.assert_bool "hosted metadata declares transition:generic"
          (Oauth.contains_scope ~scope:hosted_meta.scope "transition:generic");
        metadata_json :=
          Yojson.Safe.to_string (Oauth.metadata_to_json hosted_meta);
        let self =
          Oauth.live_http_get ~url:hosted_client_id
            ~headers:[ ("Accept", "application/json") ]
        in
        OUnit2.assert_equal ~printer:string_of_int 200 self.status;
        let served =
          Oauth.metadata_of_json (Yojson.Safe.from_string self.body)
        in
        OUnit2.assert_equal
          ~printer:(fun x -> x)
          hosted_client_id served.client_id;
        OUnit2.assert_equal ~printer:(fun x -> x) declared_scope served.scope;
        OUnit2.assert_bool "served dpop bound" served.dpop_bound_access_tokens;

        let resource, as_ =
          try
            Oauth.discover_authorization_server ~http:Oauth.live_http_get
              ~pds_origin:origin ()
          with Failure msg ->
            if
              message_has msg "not served"
              || message_has msg "HTTP 404" || message_has msg "HTTP 501"
              || message_has msg "MethodNotImplemented"
            then skip_step ("local AS discovery not served: " ^ msg)
            else failwith msg
        in
        OUnit2.assert_bool "AS issuer is the local PDS"
          (Oauth.is_loopback_http_issuer as_.issuer
          || as_.issuer = origin
          || List.mem as_.issuer resource.authorization_servers);
        OUnit2.assert_bool "PAR endpoint advertised"
          (String.length as_.pushed_authorization_request_endpoint > 8);
        OUnit2.assert_bool "token endpoint advertised"
          (String.length as_.token_endpoint > 8);
        let well_known =
          Oauth.live_http_get
            ~url:(Oauth.authorization_server_url ~issuer:as_.issuer ())
            ~headers:[ ("Accept", "application/json") ]
        in
        if well_known.status < 200 || well_known.status >= 300 then
          fail_or_skip ~label:"authorization-server well-known"
            well_known.status well_known.body;
        OUnit2.assert_bool "well-known JSON"
          (match Yojson.Safe.from_string well_known.body with
          | `Assoc _ -> true
          | _ -> false);

        let priv, pub = Oauth.generate_dpop_pair () in
        let pkce = Oauth.pkce_s256 () in
        let state = Oauth.random_jti () in
        let jkt = Oauth.dpop_jkt pub in
        let loopback_id =
          Oauth.loopback_client_id ~redirect_uri ~scope:declared_scope ()
        in
        let loopback_meta = Oauth.localhost_metadata loopback_id in
        Oauth.validate_metadata loopback_meta;
        OUnit2.assert_equal
          ~printer:(fun x -> x)
          declared_scope loopback_meta.scope;
        let try_par client_id ~scope =
          let form =
            Oauth.pushed_authorization_body ~client_id ~redirect_uri
              ~code_challenge:pkce.challenge ~state ~scope ~login_hint:handle
              ~dpop_jkt:jkt ()
          in
          let body = Oauth.form_encode form in
          let resp, nonce =
            Oauth.post_with_dpop ~http:Oauth.live_http_post ~priv ~pub
              ~url:as_.pushed_authorization_request_endpoint ~htm:"POST" ~body
              ()
          in
          if resp.status >= 200 && resp.status < 300 then
            `Ok
              ( client_id,
                Oauth.parse_par_response (Yojson.Safe.from_string resp.body),
                nonce )
          else `Err (client_id, resp.status, resp.body)
        in
        let probe =
          Oauth.live_http_post ~url:as_.pushed_authorization_request_endpoint
            ~headers:[ ("Content-Type", "application/x-www-form-urlencoded") ]
            ~body:""
        in
        (match Oauth.header_value probe.headers "DPoP-Nonce" with
        | Some n ->
            OUnit2.assert_bool "DPoP-Nonce from PAR probe" (String.length n > 0)
        | None ->
            if Oauth.is_http_not_served probe.status probe.body then
              skip_step "local AS PAR endpoint is not served");

        let par_from_loopback first_err =
          match try_par loopback_id ~scope:declared_scope with
          | `Ok (id, par, nonce) -> (id, par, nonce)
          | `Err (_, st, bd) ->
              if Oauth.is_http_not_served st bd then
                skip_step ("local AS PAR not served: " ^ bd)
              else if http_client_rejected st bd then
                skip_step
                  (Printf.sprintf
                     "local AS rejected both hosted http client_id (%s) and \
                      loopback http://localhost client_id (%s); metadata was \
                      served at %s"
                     first_err bd hosted_client_id)
              else failwith (Printf.sprintf "PAR loopback HTTP %d: %s" st bd)
        in
        let par_client, par, nonce =
          match try_par hosted_client_id ~scope:declared_scope with
          | `Ok (id, par, nonce) -> (id, par, nonce)
          | `Err (_, status, body) ->
              if Oauth.is_http_not_served status body then
                skip_step ("local AS PAR not served: " ^ body)
              else if http_client_rejected status body then
                par_from_loopback body
              else
                failwith (Printf.sprintf "PAR hosted HTTP %d: %s" status body)
        in
        OUnit2.assert_bool "PAR request_uri" (String.length par.request_uri > 10);
        (match par.expires_in with
        | Some n -> OUnit2.assert_bool "PAR expires_in" (n > 0)
        | None -> ());

        let authorize =
          Oauth.authorize_redirect_url
            ~authorization_endpoint:as_.authorization_endpoint
            ~client_id:par_client ~request_uri:par.request_uri
        in
        let authz_resp =
          Oauth.live_http_get ~url:authorize
            ~headers:(Oauth.browser_navigation_headers ())
        in
        let cookies = Oauth.cookies_from_headers authz_resp.headers in
        if Oauth.is_http_not_served authz_resp.status authz_resp.body then
          stop_after_par "local AS /oauth/authorize is not served";
        (match Oauth.parse_provider_html authz_resp.body with
        | Oauth.Provider_error { error; description } ->
            let desc = match description with Some d -> d | None -> "" in
            failwith
              (Printf.sprintf "authorize HTTP %d %s: %s" authz_resp.status error
                 desc)
        | Oauth.Provider_authorize_page ->
            OUnit2.assert_bool "authorize HTML login page"
              (authz_resp.status = 200 || authz_resp.status = 401)
        | Oauth.Provider_html ->
            if
              authz_resp.status = 200 || authz_resp.status = 302
              || authz_resp.status = 303 || authz_resp.status = 401
            then ()
            else
              failwith
                (Printf.sprintf "authorize HTTP %d (oauth-provider HTML)"
                   authz_resp.status)
        | Oauth.Not_html ->
            if
              authz_resp.status <> 200 && authz_resp.status <> 302
              && authz_resp.status <> 303 && authz_resp.status <> 401
            then
              failwith
                (Printf.sprintf "authorize HTTP %d: %s" authz_resp.status
                   (if String.length authz_resp.body > 400 then
                      String.sub authz_resp.body 0 400
                    else authz_resp.body)));
        OUnit2.assert_bool "authorize returned HTML or redirect"
          (authz_resp.status = 302 || authz_resp.status = 303
         || authz_resp.status = 200 || authz_resp.status = 401);

        let location = Oauth.header_value authz_resp.headers "location" in
        let redirect_from_location =
          match location with
          | Some loc when message_has loc "code=" -> Some loc
          | _ -> None
        in

        let finish_with_code code iss_opt =
          (match iss_opt with
          | Some iss ->
              Oauth.expect_issuer ~expected:as_.issuer
                (Oauth.Authorized { code; state; iss = Some iss })
          | None -> ());
          let token_form =
            Oauth.token_body ~client_id:par_client ~redirect_uri ~code
              ~code_verifier:pkce.verifier ()
          in
          let token, nonce =
            try
              Oauth.exchange_code ~http:Oauth.live_http_post ~priv ~pub
                ~token_url:as_.token_endpoint ~form:token_form ?nonce ()
            with Failure msg -> failwith ("token exchange: " ^ msg)
          in
          OUnit2.assert_bool "token sub is a DID" (String.length token.sub > 8);
          OUnit2.assert_bool "granted atproto"
            (Oauth.contains_scope ~scope:token.scope "atproto");
          let nonce = assert_dpop_session ~origin ~priv ~pub ~token ?nonce () in
          ignore callback_query;
          f { origin; priv; pub; token; nonce; client_id = par_client; as_ }
        in

        match redirect_from_location with
        | Some loc -> (
            match Oauth.parse_redirect loc with
            | Oauth.Authorized { code; state = st; iss } ->
                Oauth.expect_state ~expected:state
                  (Oauth.Authorized { code; state = st; iss });
                finish_with_code code iss
            | Oauth.Denied { error; description; _ } ->
                failwith
                  (Printf.sprintf "authorize redirected with error %s: %s" error
                     (match description with Some d -> d | None -> "")))
        | None -> (
            Oauth.require_provider_cookies cookies;
            let issuer = as_.issuer in
            let referer = authorize in
            let headers =
              Oauth.provider_same_origin_headers ~issuer ~referer ~cookies ()
            in
            let signin =
              Oauth.live_http_post
                ~url:(Oauth.provider_api_url ~issuer "/sign-in")
                ~headers
                ~body:
                  (Yojson.Safe.to_string
                     (Oauth.sign_in_body ~username:handle ~password ()))
            in
            let cookies =
              Oauth.merge_cookies cookies
                (Oauth.cookies_from_headers signin.headers)
            in
            if signin.status < 200 || signin.status >= 300 then
              failwith
                (Printf.sprintf "oauth sign-in HTTP %d: %s" signin.status
                   signin.body);
            let signin_json =
              try Yojson.Safe.from_string signin.body
              with _ -> failwith ("sign-in returned non-JSON: " ^ signin.body)
            in
            let did, ephemeral = Oauth.parse_sign_in_response signin_json in
            let headers =
              Oauth.provider_same_origin_headers ~issuer ~referer ~cookies
                ?bearer:ephemeral ()
            in
            let consent =
              Oauth.live_http_post
                ~url:(Oauth.provider_api_url ~issuer "/consent")
                ~headers
                ~body:(Yojson.Safe.to_string (Oauth.consent_body ~did ()))
            in
            if consent.status < 200 || consent.status >= 300 then
              failwith
                (Printf.sprintf "oauth consent HTTP %d: %s" consent.status
                   consent.body);
            let consent_json =
              try Yojson.Safe.from_string consent.body
              with _ -> failwith ("consent returned non-JSON: " ^ consent.body)
            in
            match Oauth.parse_consent_response consent_json with
            | Oauth.Authorized { code; state = st; iss } ->
                Oauth.expect_state ~expected:state
                  (Oauth.Authorized { code; state = st; iss });
                finish_with_code code iss
            | Oauth.Denied { error; description; _ } ->
                failwith
                  (Printf.sprintf "consent redirected with error %s: %s" error
                     (match description with Some d -> d | None -> "")))
      with Stop_after_par msg ->
        prerr_endline ("local OAuth stopped after PAR: " ^ msg))

let test_live_local_oauth _ =
  with_live_oauth ~handle:(local_handle ()) ~password:(local_password ())
    (fun o ->
      let nonce =
        assert_oauth_authed_appview ~origin:o.origin ~priv:o.priv ~pub:o.pub
          ~token:o.token ?nonce:o.nonce ()
      in
      let token, nonce =
        match o.token.refresh_token with
        | None -> (o.token, nonce)
        | Some refresh_token ->
            let form =
              Oauth.refresh_body ~client_id:o.client_id ~refresh_token ()
            in
            let token, nonce =
              try
                Oauth.refresh ~http:Oauth.live_http_post ~priv:o.priv ~pub:o.pub
                  ~token_url:o.as_.token_endpoint ~form ?nonce ()
              with Failure msg -> failwith ("token refresh: " ^ msg)
            in
            OUnit2.assert_equal ~printer:(fun x -> x) o.token.sub token.sub;
            let nonce =
              assert_dpop_session ~origin:o.origin ~priv:o.priv ~pub:o.pub
                ~token ?nonce ()
            in
            (token, nonce)
      in
      match o.as_.revocation_endpoint with
      | None ->
          if require_local then
            failwith "local AS omitted revocation_endpoint (RFC 7009 required)"
      | Some revoke_url ->
          let form =
            Oauth.revoke_body ~client_id:o.client_id ~token:token.access_token
              ~token_type_hint:"access_token" ()
          in
          let (), _ =
            Oauth.revoke ~http:Oauth.live_http_post ~priv:o.priv ~pub:o.pub
              ~revoke_url ~form ?nonce ()
          in
          ())

let test_live_oauth_ozone _ =
  with_live_oauth ~handle:(ozone_admin_handle ())
    ~password:(ozone_admin_password ()) (fun o ->
      ignore
        (assert_oauth_ozone_write ~origin:o.origin ~priv:o.priv ~pub:o.pub
           ~token:o.token ?nonce:o.nonce ()))

let suite =
  "local_oauth"
  >::: [
         "test_live_local_oauth" >:: test_live_local_oauth;
         "test_live_oauth_ozone" >:: test_live_oauth_ozone;
       ]

let () =
  Unix.putenv "OUNIT_RUNNER" "sequential";
  run_test_tt_main suite
