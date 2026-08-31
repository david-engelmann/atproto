open Http_method
open Request
open Response
open Lwt.Infix

(** HTTP/2 TLS client. Useful as an XRPC transport that keeps status
    and response headers (rate-limit, atproto-repo-rev) that the
    Cohttp helpers currently discard. Requires HTTPS + ALPN `h2`. *)
module Http_client = struct
  exception Error of string

  let default_timeout = 15.0
  let user_agent = Request.user_agent

  type parsed_url = {
    scheme : string;
    host : string;
    port : int;
    path : string;
  }

  let fail msg = raise (Error msg)

  let parse_url (url : string) : parsed_url =
    let uri = Uri.of_string url in
    let scheme =
      match Uri.scheme uri with
      | Some s -> String.lowercase_ascii s
      | None -> ""
    in
    if scheme <> "https" then
      fail "Http_client HTTP/2 transport requires an https URL";
    let host =
      match Uri.host uri with
      | Some h when h <> "" -> h
      | _ -> fail ("Http_client URL is missing a host: " ^ url)
    in
    let port = match Uri.port uri with Some p -> p | None -> 443 in
    let path =
      let p = Uri.path uri in
      let p = if p = "" then "/" else p in
      match Uri.verbatim_query uri with
      | Some q when q <> "" -> p ^ "?" ^ q
      | _ -> (
          match Uri.query uri with
          | [] -> p
          | qs -> p ^ "?" ^ Uri.encoded_of_query qs)
    in
    { scheme; host; port; path }

  let h2_meth = function
    | Http_method.Get -> `GET
    | Http_method.Post -> `POST
    | Http_method.Put -> `PUT
    | Http_method.Delete -> `DELETE
    | Http_method.Patch -> `Other "PATCH"

  let print_addr_info (addr_info : Unix.addr_info) : unit =
    match addr_info.Unix.ai_addr with
    | Unix.ADDR_INET (addr, port) ->
        Printf.printf "Address: %s, Port: %d\n"
          (Unix.string_of_inet_addr addr)
          port
    | _ -> Printf.printf "Unknown address format\n"

  let unpack_addr_info addr =
    match addr.Unix.ai_addr with
    | Unix.ADDR_UNIX _ -> None
    | ADDR_INET (addr, port) -> Some (addr, port)

  let get_addr_info (host : string) (port : int) : Unix.addr_info list Lwt.t =
    Lwt_unix.getaddrinfo host (string_of_int port) [ Unix.(AI_FAMILY PF_INET) ]

  let headers_to_list (h : H2.Headers.t) : (string * string) list =
    H2.Headers.to_list h
    |> List.filter (fun (k, _) ->
           match k with
           | ":status" | ":method" | ":path" | ":scheme" | ":authority" -> false
           | _ -> true)

  let request_headers ~host ~port (headers : (string * string) list) :
      H2.Headers.t =
    let authority =
      if port = 443 then host else Printf.sprintf "%s:%d" host port
    in
    let has_ua =
      List.exists
        (fun (k, _) -> String.lowercase_ascii k = "user-agent")
        headers
    in
    let pairs =
      (":authority", authority)
      :: (if has_ua then [] else [ ("user-agent", user_agent) ])
      @ List.filter
          (fun (k, _) ->
            let l = String.lowercase_ascii k in
            l <> ":authority" && l <> "host" && l <> "connection"
            && l <> "transfer-encoding")
          headers
    in
    H2.Headers.of_list pairs

  let error_message = function
    | `Malformed_response s -> "malformed HTTP/2 response: " ^ s
    | `Invalid_response_body_length _ -> "invalid HTTP/2 response body length"
    | `Exn exn -> Printexc.to_string exn
    | `Protocol_error (code, msg) ->
        Printf.sprintf "HTTP/2 protocol error %s: %s"
          (H2.Error_code.to_string code)
          msg

  let perform ~timeout (req : Request.request) : Response.response Lwt.t =
    let parsed = parse_url req.url in
    get_addr_info parsed.host parsed.port >>= fun addrs ->
    if addrs = [] then Lwt.fail (Error ("DNS lookup failed for " ^ parsed.host))
    else
      let rec try_addrs = function
        | [] -> Lwt.fail (Error ("HTTP/2 connect failed for " ^ parsed.host))
        | addr_info :: rest ->
            let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
            Lwt.catch
              (fun () ->
                Lwt.pick
                  [
                    ( Lwt_unix.sleep timeout >>= fun () ->
                      Lwt.fail (Error "HTTP/2 connect timed out") );
                    ( Lwt_unix.connect socket addr_info.Unix.ai_addr
                    >>= fun () ->
                      let error_p, error_w = Lwt.wait () in
                      let error_handler err =
                        if Lwt.is_sleeping error_p then
                          Lwt.wakeup_exn error_w (Error (error_message err))
                      in
                      H2_lwt_unix.Client.TLS.create_connection_with_default
                        ~error_handler socket
                      >>= fun connection ->
                      let response_p, response_w = Lwt.wait () in
                      let response_handler (response : H2.Response.t) body =
                        let buf = Buffer.create 4096 in
                        let rec read_response () =
                          H2.Body.Reader.schedule_read body
                            ~on_read:(fun bigstr ~off ~len ->
                              Buffer.add_string buf
                                (Bigstringaf.substring bigstr ~off ~len);
                              read_response ())
                            ~on_eof:(fun () ->
                              if Lwt.is_sleeping response_p then
                                let status =
                                  H2.Status.to_code response.H2.Response.status
                                in
                                Lwt.wakeup_later response_w
                                  (Response.of_string ~status_code:status
                                     ~headers:
                                       (headers_to_list
                                          response.H2.Response.headers)
                                     (Buffer.contents buf)))
                        in
                        read_response ()
                      in
                      let h2_req =
                        H2.Request.create (h2_meth req.method_) parsed.path
                          ~scheme:"https"
                          ~headers:
                            (request_headers ~host:parsed.host ~port:parsed.port
                               req.headers)
                      in
                      let writer =
                        H2_lwt_unix.Client.TLS.request connection h2_req
                          ~error_handler ~response_handler
                      in
                      (match req.body with
                      | Some data when data <> "" ->
                          H2.Body.Writer.write_string writer data
                      | _ -> ());
                      H2.Body.Writer.close writer;
                      Lwt.pick [ response_p; error_p ] );
                  ])
              (fun exn ->
                Lwt.catch
                  (fun () -> Lwt_unix.close socket)
                  (fun _ -> Lwt.return_unit)
                >>= fun () ->
                match rest with [] -> Lwt.fail exn | _ -> try_addrs rest)
      in
      try_addrs addrs

  let request ?(timeout = default_timeout) (req : Request.request) :
      Response.response Lwt.t =
    perform ~timeout req

  let get url ?(headers = []) ?timeout () : Response.response Lwt.t =
    request ?timeout (Request.get url ~headers ())

  let post url ?(headers = []) ?body ?timeout () : Response.response Lwt.t =
    request ?timeout (Request.post url ~headers ?body ())

  let get_host (host : string) (port : int) : Response.response Lwt.t =
    let url =
      if port = 443 then Printf.sprintf "https://%s/" host
      else Printf.sprintf "https://%s:%d/" host port
    in
    get url ()

  let xrpc_url ~host ?(port = 443) nsid ?(query = []) () : string =
    let base =
      if port = 443 then Printf.sprintf "https://%s/xrpc/%s" host nsid
      else Printf.sprintf "https://%s:%d/xrpc/%s" host port nsid
    in
    match query with
    | [] -> base
    | qs ->
        base ^ "?"
        ^ String.concat "&"
            (List.map
               (fun (k, v) -> Uri.pct_encode k ^ "=" ^ Uri.pct_encode v)
               qs)

  let xrpc_get ~host ?port ~nsid ?(query = []) ?(headers = []) ?timeout () :
      Response.response Lwt.t =
    get (xrpc_url ~host ?port nsid ~query ()) ~headers ?timeout ()

  let xrpc_post ~host ?port ~nsid ?(headers = []) ?body ?timeout () :
      Response.response Lwt.t =
    let hdrs =
      if
        List.exists
          (fun (k, _) -> String.lowercase_ascii k = "content-type")
          headers
      then headers
      else ("content-type", "application/json") :: headers
    in
    post (xrpc_url ~host ?port nsid ()) ~headers:hdrs ?body ?timeout ()

  let run (t : 'a Lwt.t) : 'a = Lwt_main.run t
end
