open OUnit2
open Atproto.Auth
open Atproto.Session
open Atproto.Oauth
open Oauth

(* Live OAuth against official @atproto/dev-env TestNetwork (PDS oauth-provider).
   Serves a loopback client-metadata document, discovers the local AS, and
   exercises PAR + DPoP. Token exchange runs when the local AS can complete
   sign-in/consent without a browser (alice.test / hunter2). *)

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
  && (message_has body "invalid_client"
     || message_has body "https"
     || message_has body "client_id")

let undeclared_scope status body =
  status = 400
  && message_has body "invalid_scope"
  && message_has body "not declared"

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

let provider_headers ~issuer ~referer ~cookies ?csrf ?bearer () =
  let csrf =
    match csrf with
    | Some t -> t
    | None -> (
        match List.assoc_opt Oauth.csrf_cookie_name cookies with
        | Some t -> t
        | None -> Oauth.random_csrf_token ())
  in
  let cookies =
    Oauth.merge_cookies cookies [ (Oauth.csrf_cookie_name, csrf) ]
  in
  let headers =
    [
      ("Content-Type", "application/json");
      ("Accept", "application/json");
      ("Origin", issuer);
      ("Referer", referer);
      ("sec-fetch-mode", "same-origin");
      ("sec-fetch-site", "same-origin");
      (Oauth.csrf_header_name, csrf);
      ("Cookie", Oauth.cookie_header cookies);
    ]
  in
  let headers =
    match bearer with
    | Some t -> ("Authorization", "Bearer " ^ t) :: headers
    | None -> headers
  in
  (headers, cookies, csrf)

let json_string json field =
  match Yojson.Safe.Util.member field json with
  | `String s -> Some s
  | _ -> None

let json_did json =
  match json_string json "did" with
  | Some d -> Some d
  | None -> (
      match Yojson.Safe.Util.member "account" json with
      | `Assoc _ as acc -> (
          match json_string acc "did" with
          | Some d -> Some d
          | None -> json_string acc "sub")
      | _ -> json_string json "sub")

let test_live_local_oauth _ =
  skip_unless_local ();
  let origin = pds_origin () in
  let handle = local_handle () in
  let password = local_password () in
  let redirect_path = "/cb" in
  let port, callback_query, metadata_json, stop_server =
    start_loopback_server ~redirect_path
  in
  Fun.protect ~finally:stop_server (fun () ->
      let hosted_origin = Printf.sprintf "http://127.0.0.1:%d" port in
      let hosted_client_id = hosted_origin ^ "/client-metadata.json" in
      let redirect_uri = hosted_origin ^ redirect_path in
      let hosted_meta =
        Oauth.public_metadata ~client_id:hosted_client_id
          ~redirect_uris:[ redirect_uri ] ~application_type:"native"
          ~client_name:"atproto-ocaml local TestNetwork"
          ~scope:"atproto transition:generic" ()
      in
      Oauth.validate_metadata hosted_meta;
      metadata_json :=
        Yojson.Safe.to_string (Oauth.metadata_to_json hosted_meta);
      let self =
        Oauth.live_http_get ~url:hosted_client_id
          ~headers:[ ("Accept", "application/json") ]
      in
      OUnit2.assert_equal ~printer:string_of_int 200 self.status;
      let served = Oauth.metadata_of_json (Yojson.Safe.from_string self.body) in
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        hosted_client_id served.client_id;
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
        fail_or_skip ~label:"authorization-server well-known" well_known.status
          well_known.body;
      OUnit2.assert_bool "well-known JSON"
        (match Yojson.Safe.from_string well_known.body with
        | `Assoc _ -> true
        | _ -> false);

      let priv, pub = Oauth.generate_dpop_pair () in
      let pkce = Oauth.pkce_s256 () in
      let state = Oauth.random_jti () in
      let jkt = Oauth.dpop_jkt pub in
      let scope_full = "atproto transition:generic" in
      let scope_atproto = "atproto" in
      let loopback_id scope =
        Oauth.loopback_client_id ~redirect_uri ~scope ()
      in
      let try_par client_id ~scope =
        let form =
          Oauth.pushed_authorization_body ~client_id ~redirect_uri
            ~code_challenge:pkce.challenge ~state ~scope ~login_hint:handle
            ~dpop_jkt:jkt ()
        in
        let body = Oauth.form_encode form in
        let resp, nonce =
          Oauth.post_with_dpop ~http:Oauth.live_http_post ~priv ~pub
            ~url:as_.pushed_authorization_request_endpoint ~htm:"POST" ~body ()
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
        match try_par (loopback_id scope_full) ~scope:scope_full with
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
            else if undeclared_scope st bd then
              (* This oauth-provider derives loopback metadata from the
                 client_id query; if it still only declares [atproto], request
                 that subset instead of inventing extra scopes. *)
              match
                try_par (loopback_id scope_atproto) ~scope:scope_atproto
              with
              | `Ok (id, par, nonce) -> (id, par, nonce)
              | `Err (_, st2, bd2) ->
                  if Oauth.is_http_not_served st2 bd2 then
                    skip_step ("local AS PAR not served: " ^ bd2)
                  else if http_client_rejected st2 bd2 then
                    skip_step
                      (Printf.sprintf
                         "local AS rejected loopback client_id after \
                          atproto-only retry (%s); first loopback: %s"
                         bd2 bd)
                  else
                    failwith
                      (Printf.sprintf "PAR loopback atproto HTTP %d: %s" st2 bd2)
            else failwith (Printf.sprintf "PAR loopback HTTP %d: %s" st bd)
      in
      let par_client, par, nonce =
        match try_par hosted_client_id ~scope:scope_full with
        | `Ok (id, par, nonce) -> (id, par, nonce)
        | `Err (_, status, body) ->
            if Oauth.is_http_not_served status body then
              skip_step ("local AS PAR not served: " ^ body)
            else if http_client_rejected status body then par_from_loopback body
            else failwith (Printf.sprintf "PAR hosted HTTP %d: %s" status body)
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
      let authz_resp = Oauth.live_http_get ~url:authorize ~headers:[] in
      let cookies = Oauth.cookies_from_headers authz_resp.headers in
      if Oauth.is_http_not_served authz_resp.status authz_resp.body then
        skip_step "local AS /oauth/authorize is not served";
      if
        authz_resp.status <> 200 && authz_resp.status <> 302
        && authz_resp.status <> 303 && authz_resp.status <> 401
      then fail_or_skip ~label:"authorize" authz_resp.status authz_resp.body;
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
        let session_url =
          Oauth.url_on origin "/xrpc/com.atproto.server.getSession"
        in
        let sess, _ =
          Oauth.request_with_dpop ~http:Oauth.live_http_request ~priv ~pub
            ~url:session_url ~htm:"GET" ~access_token:token.access_token ?nonce
            ()
        in
        if sess.status >= 200 && sess.status < 300 then
          OUnit2.assert_bool "DPoP getSession"
            (match json_did (Yojson.Safe.from_string sess.body) with
            | Some d -> d = token.sub
            | None -> false)
        else if not (Oauth.is_http_not_served sess.status sess.body) then
          failwith
            (Printf.sprintf "DPoP getSession HTTP %d: %s" sess.status sess.body);
        (match as_.revocation_endpoint with
        | None -> ()
        | Some revoke_url ->
            let form =
              Oauth.revoke_body ~client_id:par_client ~token:token.access_token
                ~token_type_hint:"access_token" ()
            in
            let (), _ =
              Oauth.revoke ~http:Oauth.live_http_post ~priv ~pub ~revoke_url
                ~form ?nonce ()
            in
            ());
        ignore callback_query
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
          let issuer = as_.issuer in
          let referer = authorize in
          let api path =
            Oauth.url_on issuer (Oauth.provider_api_prefix ^ path)
          in
          let headers, cookies, _csrf =
            provider_headers ~issuer ~referer ~cookies ()
          in
          let signin_body =
            Yojson.Safe.to_string
              (`Assoc
                [
                  ("locale", `String "en");
                  ("username", `String handle);
                  ("password", `String password);
                  ("remember", `Bool true);
                ])
          in
          let signin =
            Oauth.live_http_post ~url:(api "/sign-in") ~headers
              ~body:signin_body
          in
          let cookies =
            Oauth.merge_cookies cookies
              (Oauth.cookies_from_headers signin.headers)
          in
          if Oauth.is_http_not_served signin.status signin.body then
            skip_step
              ("local AS sign-in API is not served; authorize HTML requires a \
                browser. PAR + discovery + DPoP nonce + hosted metadata \
                succeeded. " ^ signin.body);
          if signin.status = 400 && message_has signin.body "csrf" then
            skip_step
              ("local AS sign-in API rejected CSRF/device cookies from a \
                non-browser client: " ^ signin.body);
          if
            signin.status = 400
            && (message_has signin.body "sec-fetch"
               || message_has signin.body "referrer"
               || message_has signin.body "origin"
               || message_has signin.body "same-origin")
          then
            skip_step
              ("local AS sign-in API is browser-locked (same-origin fetch): "
             ^ signin.body);
          if signin.status < 200 || signin.status >= 300 then
            fail_or_skip ~label:"oauth sign-in" signin.status signin.body;
          let signin_json =
            try Yojson.Safe.from_string signin.body
            with _ -> failwith ("sign-in returned non-JSON: " ^ signin.body)
          in
          let did =
            match json_did signin_json with
            | Some d -> d
            | None ->
                if Auth.has_live_credentials then
                  (Session.create_session handle password).auth.did
                else failwith ("sign-in JSON missing did: " ^ signin.body)
          in
          let ephemeral = json_string signin_json "ephemeralToken" in
          let headers, _cookies, _ =
            provider_headers ~issuer ~referer ~cookies ?bearer:ephemeral ()
          in
          let consent_body did_field =
            Yojson.Safe.to_string (`Assoc [ (did_field, `String did) ])
          in
          let consent =
            let first =
              Oauth.live_http_post ~url:(api "/consent") ~headers
                ~body:(consent_body "did")
            in
            if
              first.status >= 400
              && (message_has first.body "did"
                 || message_has first.body "invalid")
            then
              Oauth.live_http_post ~url:(api "/consent") ~headers
                ~body:(consent_body "sub")
            else if Oauth.is_http_not_served first.status first.body then
              Oauth.live_http_post ~url:(api "/accept") ~headers
                ~body:(consent_body "sub")
            else first
          in
          if Oauth.is_http_not_served consent.status consent.body then
            skip_step
              ("local AS consent/accept API is not served; a browser consent \
                page is required after PAR. " ^ consent.body);
          if consent.status < 200 || consent.status >= 300 then
            fail_or_skip ~label:"oauth consent" consent.status consent.body;
          let consent_json =
            try Yojson.Safe.from_string consent.body
            with _ -> failwith ("consent returned non-JSON: " ^ consent.body)
          in
          match json_string consent_json "url" with
          | None -> failwith ("consent JSON missing url: " ^ consent.body)
          | Some url -> (
              match Oauth.parse_redirect url with
              | Oauth.Authorized { code; state = st; iss } ->
                  Oauth.expect_state ~expected:state
                    (Oauth.Authorized { code; state = st; iss });
                  finish_with_code code iss
              | Oauth.Denied { error; description; _ } ->
                  failwith
                    (Printf.sprintf "consent redirected with error %s: %s" error
                       (match description with Some d -> d | None -> "")))))

let suite =
  "local_oauth" >::: [ "test_live_local_oauth" >:: test_live_local_oauth ]

let () =
  Unix.putenv "OUNIT_RUNNER" "sequential";
  run_test_tt_main suite
