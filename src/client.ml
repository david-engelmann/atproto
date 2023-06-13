module Http_client = struct
    open H2
    module Client = H2_lwt_unix.Client
    let response_handler notify_response_received response response_body =

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
end



