
module Http_client = struct
    open H2
    module Client = H2_lwt_unix.Client
    open Lwt.Infix
    let print_addr_info (addr_info : Unix.addr_info) : unit =
      match addr_info.Unix.ai_addr with
      | Unix.ADDR_INET (addr, port) ->
        Printf.printf "Address: %s, Port: %d\n" (Unix.string_of_inet_addr addr) port
      | _ ->
        Printf.printf "Unknown address format\n"

    let unpack_addr_info addr =
        match addr.Unix.ai_addr with
         | Unix.ADDR_UNIX _ -> None
         | ADDR_INET (addr, port) -> Some (addr, port)

    let print_converted_list (converted_list : Unix.addr_info list) : unit =
      List.iter print_addr_info converted_list

    let print_body (body_promise : H2.Body.Writer.t Lwt.t) : unit Lwt.t =
      body_promise >>= fun _ ->
      print_endline "The promise has resolved!";
      Lwt.return_unit

    let request_buffer = Buffer.create 4096
    let response_buffer = Buffer.create 4096

    let write_to_request_body body data =
      Buffer.add_string request_buffer data;
      H2.Body.Writer.write_string body data;
      H2.Body.Writer.flush body (fun () -> ())

    let get_addr_info (host : string) (port : int) : Unix.addr_info list Lwt.t =
        Lwt_unix.getaddrinfo host (string_of_int port) [ Unix.(AI_FAMILY PF_INET) ]

    let response_handler : Client_connection.response_handler = fun _response response_body ->
      let open H2.Body.Reader in
      let rec read_response () =
        schedule_read response_body
          ~on_read:(fun buffer ~off ~len ->
            let chunk = Bytes.create len in
            Bigstringaf.blit_to_bytes buffer ~src_off:off chunk ~dst_off:0 ~len;
            let chunk_string = Bytes.to_string chunk in
            Buffer.add_string response_buffer chunk_string;
            print_string chunk_string;
            read_response ()
          )
          ~on_eof:(fun () -> ())
      in
      read_response ()

    (*
    open H2.Response
    open H2.Body
    let response_handler notify_response_received response response_body =
      match response.status with
      | `OK ->
        let rec read_response () =
          Body.Reader.schedule_read response_body
          (*
          >>= function
          | `Data chunk ->
            print_string (Bytes.to_string chunk);
            read_response ()
          | `Eof ->
            Lwt.wakeup_later notify_response_received ()
          *)
            ~on_read:(fun buffer ~off ~len ->
              let chunk = Bytes.create len in
              Bigstringaf.blit_to_bytes buffer ~src_off:off chunk ~dst_off:0 ~len;
              print_string (Bytes.to_string chunk);
              read_response ()
            )
            ~on_eof:(fun () ->
              Lwt.wakeup_later notify_response_received ()
            )
        in
        read_response ()
    | _ ->
      Format.eprintf "Unsuccessful response: %a\n%!" Response.pp_hum response;
      exit 1
    *)

    (*
    let response_handler notify_response_received (response : Response.t) (response_body : Body.Reader.t) : unit =
      let rec read_response () =
        Body.Reader.read response_body
        >>= function
        | Some (chunk, _) ->
          print_string (Bytes.to_string chunk);
        | None ->
          Lwt.wakeup_later notify_response_received ()
      in
      match response.Response.status with
      | `OK ->
        read_response ()
      | _ ->
        Format.eprintf "Unsuccessful response: %a\n%!" Response.pp_hum response;
        exit 1
    *)

    (*
    match response.Response.status with
      | `OK ->
        let rec read_response () =
            Body.Reader.schedule_read
              response_body
              ~on_read:(fun bigstring ~off  ~len ->
                let response_fragment = Bytes.create len in
                Bigstringaf.blit_to_bytes
                bigstring
                ~src_off:off
                response_fragment
                ~dst_off:0
                ~len;
                print_string (Bytes.to_string response_fragment);
                read_response ())
              ~on_eof:(fun () ->
                Lwt.wakeup_later notify_response_received ())
        in
        read_response ()
      | _ ->
        Format.eprintf "Unsuccessful response: %a\n%!" Response.pp_hum response;
        exit 1
    *)

    (*let create_empty () = H2.Body.Writer.create ()*)
    let create_empty () = Lwt.fail_with "No Initial Value"

    let error_handler _error =
        Format.eprintf "Unsuccessful request!\n%!";
        exit 1

    let create_socket =
      Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0

    let connect_socket socket (addr_info : Unix.addr_info) =
      Lwt_unix.connect socket addr_info.Unix.ai_addr

    let create_socket_for_addr_info addr_info =
      let socket = create_socket in
      let%bind () = connect_socket socket addr_info in
      Lwt.return socket

    let create_get_request (host : string) : Request.t =
      Request.create
        `GET
        "/"
        ~scheme:"https"
        ~headers:Headers.(add_list empty [ ":authority", host ])

    let handle_response (response_received : unit Lwt.t) : unit Lwt.t =
      response_received >>= fun () ->
      Lwt.return_unit

    let perform_request connection (request : Request.t) : Body.Writer.t Lwt.t =
      let request_body =
          Client.TLS.request
              connection
              request
              ~error_handler:error_handler
              ~response_handler:response_handler
          in
          Lwt.return request_body
    (*
    (* CLOSE *)
    let get_host (host : string) (port : int) : Body.Writer.t Lwt.t =
      let open Lwt.Infix in
      get_addr_info host port
      >>= fun addrs ->
      let lwt_list =
        List.map Lwt.return addrs
      in
      Lwt_list.fold_left_s (fun acc addr ->
        addr >>= fun addr_info ->
        let%bind socket = create_socket_for_addr_info addr_info in
        let request = create_get_request host in
        let response_received, notify_response_received = Lwt.wait () in
        (*let response_handler = response_handler notify_response_received in*)
        Client.TLS.create_connection_with_default ~error_handler:error_handler socket
        >>= fun connection ->
        perform_request connection request >>= fun request_body ->
        Lwt.async (fun () ->
        handle_response notify_response_received >>= fun () ->
        Lwt.return_unit
      );
      Lwt.return request_body
      ) (Lwt.return (Body.Writer.create ())) lwt_list
    *)
    (* writer die
    let get_host (host : string) (port : int) : H2.Body.Writer.t Lwt.t =
      let open Lwt.Infix in
      get_addr_info host port >>= fun addrs ->
      let lwt_list = List.map Lwt.return addrs in
      Lwt_list.fold_left_s
        (fun acc addr ->
          addr >>= fun addr_info ->
          let%bind socket = create_socket_for_addr_info addr_info in
          let request = create_get_request host in
          let response_received, notify_response_received = Lwt.wait () in
          Client.TLS.create_connection_with_default ~error_handler:error_handler socket
          >>= fun connection ->
          perform_request connection request >>= fun request_body ->
          let writer = Body.Writer.create request_body in
          let handle_response' = handle_response response_received in
          Lwt.async (fun () -> handle_response' >>= fun () -> Lwt.return_unit);
          Lwt.return writer)
        (Lwt.return (Body.Writer.create ()))
        lwt_list
      >>= fun request_body ->
      Lwt.return request_body
    *)

    (* fuck writer 
    let get_host (host : string) (port : int) : H2.Body.Writer.t Lwt.t =
      let open Lwt.Infix in
      get_addr_info host port
      >>= fun addrs ->
      let lwt_list = List.map Lwt.return addrs in
      Lwt_list.fold_left_s
        (fun acc addr ->
          acc >>= fun _ ->
          addr >>= fun addr_info ->
          let%bind socket = create_socket_for_addr_info addr_info in
          let request = create_get_request host in
          let response_received, notify_response_received = Lwt.wait () in
          Client.TLS.create_connection_with_default ~error_handler:error_handler socket
          >>= fun connection ->
          perform_request connection request
          >>= fun request_body ->
          let handle_response' = handle_response response_received in
          Lwt.async (fun () -> handle_response' >>= fun () -> Lwt.return_unit);
          Lwt.return request_body)
        (create_empty ())
        lwt_list
      >>= fun request_body ->
      Lwt.return request_body
    *)
    (*
    let get_host (host : string) (port : int) : H2.Body.Writer.t Lwt.t =
      let open Lwt.Infix in
      get_addr_info host port
      >>= fun addrs ->
      let lwt_list = List.map Lwt.return addrs in
      Lwt_list.iter_s
        (fun addr ->
          addr >>= fun addr_info ->
          let%bind socket = create_socket_for_addr_info addr_info in
          let request = create_get_request host in
          let response_received, notify_response_received = Lwt.wait () in
          Client.TLS.create_connection_with_default ~error_handler:error_handler socket
          >>= fun connection ->
          perform_request connection request
          >>= fun request_body ->
          let handle_response' = handle_response response_received in
          Lwt.async (fun () -> handle_response' >>= fun () -> Lwt.return_unit);
          Lwt.return request_body)
        lwt_list
    *)
    (* fuck fold_map_s
    let get_host (host : string) (port : int) : H2.Body.Writer.t Lwt.t =
      let open Lwt.Infix in
      get_addr_info host port
      >>= fun addrs ->
      Lwt_list.fold_map_s
        (fun addr_info ->
          let%bind socket = create_socket_for_addr_info addr_info in
          let request = create_get_request host in
          Lwt.catch
            (fun () ->
              Client.TLS.create_connection_with_default ~error_handler:error_handler socket
              >>= fun connection ->
              let%bind request_body = perform_request connection request in
              Lwt.return (Some request_body))
            (fun _ -> Lwt.return None))  (* If an error occurs, return None and continue with the next address *)
        addrs
      >>= function
      | Some request_body -> Lwt.return request_body
      | None -> Lwt.fail_with "No successful request"
    *)

    (*
    let get_host (host : string) (port : int) : H2.Body.Writer.t Lwt.t =
      let open Lwt.Infix in
      get_addr_info host port >>= fun addrs ->
      let rec find_successful_connection = function
        | [] -> Lwt.fail_with "No successful request"
        | addr_info :: rest ->
            let%bind socket = create_socket_for_addr_info addr_info in
            let request = create_get_request host in
            Lwt.catch
              (fun () ->
                Client.TLS.create_connection_with_default ~error_handler:error_handler socket
                >>= fun connection ->
                perform_request connection request)
              (fun _ -> Lwt.return_none) (* If an error occurs, return None and continue with the next address *)
            >>= function
            | Some request_body -> Lwt.return request_body
            | None -> find_successful_connection rest
      in
      find_successful_connection addrs
    *)

    let get_host (host : string) (port : int) : H2.Body.Writer.t Lwt.t =
      let open Lwt.Infix in
      get_addr_info host port >>= fun addrs ->
      let rec find_successful_connection = function
        | [] -> Lwt.fail_with "No successful request"
        | addr_info :: rest ->
            let%bind socket = create_socket_for_addr_info addr_info in
            let request = create_get_request host in
            Lwt.catch
              (fun () ->
                Client.TLS.create_connection_with_default ~error_handler:error_handler socket
                >>= fun connection ->
                perform_request connection request >>= fun request_body ->
                Lwt.return_some request_body)
              (fun _ -> Lwt.return_none) (* If an error occurs, return None and continue with the next address *)
            >>= function
            | Some request_body -> Lwt.return request_body
            | None -> find_successful_connection rest
      in
      find_successful_connection addrs
    
    
    (*
    let start_client (host : string) (port : int) =
        Lwt_main.run
          (
          get_addr_info host port
          >>= fun addresses ->
              let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
              Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr >>= fun () ->
              let request =
                  Request.create
                   `GET
                   "/"
                   ~scheme:"https"
                   ~headers:
                       Headers.(add_list empty [ ":authority", host ])
              in
              let response_received, notify_response_received = Lwt.wait () in
              let response_handler = response_handler notify_response_received in
              Client.TLS.create_connection_with_default ~error_handler socket
              >>= fun connection ->

              let request_body =
                Client.TLS.request connection request ~error_handler ~response_handler
              in
              Body.Writer.close request_body;
              response_received )
     *)
end



