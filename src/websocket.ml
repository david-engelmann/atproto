open Hash
open Base64url

(** Minimal RFC 6455 client (wss:// TLS and ws:// cleartext). *)
module Websocket = struct
  type transport = Tls of Ssl.socket | Tcp of Unix.file_descr
  type t = { transport : transport }

  type parsed_url = {
    secure : bool;
    host : string;
    port : int;
    path : string;
  }

  type message =
    | Text of string
    | Binary of string
    | Close of string
    | Ping of string
    | Pong of string

  let guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

  let accept_key (sec_websocket_key : string) : string =
    Base64url.encode_std ~pad:true (Hash.sha1 (sec_websocket_key ^ guid))

  let random_bytes n =
    let b = Bytes.create n in
    for i = 0 to n - 1 do
      Bytes.set b i (Char.chr (Random.int 256))
    done;
    Bytes.to_string b

  let mask_payload (key : string) (payload : string) : string =
    let out = Bytes.of_string payload in
    for i = 0 to Bytes.length out - 1 do
      Bytes.set out i
        (Char.chr (Char.code payload.[i] lxor Char.code key.[i mod 4]))
    done;
    Bytes.to_string out

  let encode_frame ?(fin = true) ?(mask = true) ~(opcode : int)
      (payload : string) : string =
    let buf = Buffer.create (String.length payload + 14) in
    let b0 = (if fin then 0x80 else 0) lor (opcode land 0x0f) in
    Buffer.add_char buf (Char.chr b0);
    let len = String.length payload in
    let mask_bit = if mask then 0x80 else 0 in
    if len < 126 then Buffer.add_char buf (Char.chr (mask_bit lor len))
    else if len < 65536 then (
      Buffer.add_char buf (Char.chr (mask_bit lor 126));
      Buffer.add_char buf (Char.chr ((len lsr 8) land 0xff));
      Buffer.add_char buf (Char.chr (len land 0xff)))
    else (
      Buffer.add_char buf (Char.chr (mask_bit lor 127));
      for shift = 7 downto 0 do
        Buffer.add_char buf (Char.chr ((len lsr (shift * 8)) land 0xff))
      done);
    let payload =
      if mask then (
        let key = random_bytes 4 in
        Buffer.add_string buf key;
        mask_payload key payload)
      else payload
    in
    Buffer.add_string buf payload;
    Buffer.contents buf

  type decoded_frame = { fin : bool; opcode : int; payload : string }

  let decode_frame_header (read_exact : int -> string) : decoded_frame =
    let h = read_exact 2 in
    let b0 = Char.code h.[0] in
    let b1 = Char.code h.[1] in
    let fin = b0 land 0x80 <> 0 in
    let opcode = b0 land 0x0f in
    let masked = b1 land 0x80 <> 0 in
    let len7 = b1 land 0x7f in
    let payload_len =
      if len7 < 126 then len7
      else if len7 = 126 then
        let ext = read_exact 2 in
        (Char.code ext.[0] lsl 8) lor Char.code ext.[1]
      else
        let ext = read_exact 8 in
        let rec acc i n =
          if i = 8 then n else acc (i + 1) ((n lsl 8) lor Char.code ext.[i])
        in
        acc 0 0
    in
    let mask_key = if masked then read_exact 4 else "" in
    let payload = read_exact payload_len in
    let payload = if masked then mask_payload mask_key payload else payload in
    { fin; opcode; payload }

  let decode_frame_bytes (bytes : string) : decoded_frame * int =
    if String.length bytes < 2 then failwith "Websocket.decode_frame: truncated";
    let off = ref 2 in
    let read n =
      if !off + n > String.length bytes then
        failwith "Websocket.decode_frame: truncated";
      let s = String.sub bytes !off n in
      off := !off + n;
      s
    in
    let b0 = Char.code bytes.[0] in
    let b1 = Char.code bytes.[1] in
    let fin = b0 land 0x80 <> 0 in
    let opcode = b0 land 0x0f in
    let masked = b1 land 0x80 <> 0 in
    let len7 = b1 land 0x7f in
    let payload_len =
      if len7 < 126 then len7
      else if len7 = 126 then
        let ext = read 2 in
        (Char.code ext.[0] lsl 8) lor Char.code ext.[1]
      else
        let ext = read 8 in
        let rec acc i n =
          if i = 8 then n else acc (i + 1) ((n lsl 8) lor Char.code ext.[i])
        in
        acc 0 0
    in
    let mask_key = if masked then read 4 else "" in
    let payload = read payload_len in
    let payload = if masked then mask_payload mask_key payload else payload in
    ({ fin; opcode; payload }, !off)

  let read_exact transport n =
    let buf = Bytes.create n in
    let rec loop off =
      if off = n then Bytes.to_string buf
      else
        let got =
          match transport with
          | Tls ssl -> Ssl.read ssl buf off (n - off)
          | Tcp fd -> Unix.read fd buf off (n - off)
        in
        if got = 0 then failwith "Websocket: connection closed";
        loop (off + got)
    in
    loop 0

  let write_all transport s =
    let buf = Bytes.of_string s in
    let rec loop off =
      if off >= Bytes.length buf then ()
      else
        let w =
          match transport with
          | Tls ssl -> Ssl.write ssl buf off (Bytes.length buf - off)
          | Tcp fd -> Unix.write fd buf off (Bytes.length buf - off)
        in
        if w = 0 then failwith "Websocket: write failed";
        loop (off + w)
    in
    loop 0

  let send (ws : t) ?(opcode = 2) (payload : string) : unit =
    write_all ws.transport (encode_frame ~opcode payload)

  let send_close (ws : t) : unit = send ws ~opcode:8 ""
  let send_pong (ws : t) payload = send ws ~opcode:10 payload

  let recv_frame (ws : t) : decoded_frame =
    decode_frame_header (fun n -> read_exact ws.transport n)

  let recv_message (ws : t) : message =
    let rec collect opcode acc =
      let frame = recv_frame ws in
      let opcode = if frame.opcode = 0 then opcode else frame.opcode in
      let acc = acc ^ frame.payload in
      if not frame.fin then collect opcode acc
      else
        match opcode with
        | 1 -> Text acc
        | 2 -> Binary acc
        | 8 -> Close acc
        | 9 ->
            send_pong ws acc;
            Ping acc
        | 10 -> Pong acc
        | n -> failwith (Printf.sprintf "Websocket: unknown opcode %d" n)
    in
    collect 0 ""

  let parse_url (url : string) : parsed_url =
    let secure, rest =
      if String.length url >= 6 && String.sub url 0 6 = "wss://" then
        (true, String.sub url 6 (String.length url - 6))
      else if String.length url >= 5 && String.sub url 0 5 = "ws://" then
        (false, String.sub url 5 (String.length url - 5))
      else (true, url)
    in
    let hostport, path =
      match String.index_opt rest '/' with
      | None -> (rest, "/")
      | Some i ->
          (String.sub rest 0 i, String.sub rest i (String.length rest - i))
    in
    let default_port = if secure then 443 else 80 in
    let host, port =
      match String.split_on_char ':' hostport with
      | [ h ] -> (h, default_port)
      | [ h; p ] -> (h, int_of_string p)
      | _ -> failwith "Websocket: invalid host"
    in
    { secure; host; port; path }

  let parse_wss_url (url : string) : string * int * string =
    let p = parse_url url in
    (p.host, p.port, p.path)

  let authority (p : parsed_url) : string =
    if (p.secure && p.port = 443) || ((not p.secure) && p.port = 80) then p.host
    else Printf.sprintf "%s:%d" p.host p.port

  let read_handshake_response transport =
    let buf = Buffer.create 256 in
    let rec loop () =
      if Buffer.length buf > 8192 then
        failwith "Websocket: handshake response too large";
      let chunk = read_exact transport 1 in
      Buffer.add_string buf chunk;
      let s = Buffer.contents buf in
      let n = String.length s in
      if n >= 4 && String.sub s (n - 4) 4 = "\r\n\r\n" then s else loop ()
    in
    loop ()

  let header_value headers name =
    let name = String.lowercase_ascii name in
    let rec find = function
      | [] -> None
      | line :: rest -> (
          match String.index_opt line ':' with
          | None -> find rest
          | Some i ->
              let k = String.lowercase_ascii (String.sub line 0 i) in
              if k = name then
                let v = String.sub line (i + 1) (String.length line - i - 1) in
                Some (String.trim v)
              else find rest)
    in
    find headers

  let extra_header_lines (extra : (string * string) list) : string =
    String.concat ""
      (List.map (fun (k, v) -> Printf.sprintf "%s: %s\r\n" k v) extra)

  let connect ?(tls_verify = false) ?(extra_headers = []) (url : string) : t =
    Random.self_init ();
    let p = parse_url url in
    if p.secure then Ssl.init ();
    let he = Unix.gethostbyname p.host in
    if Array.length he.Unix.h_addr_list = 0 then
      failwith ("Websocket: could not resolve " ^ p.host);
    let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.set_nonblock fd;
    Unix.clear_nonblock fd;
    Unix.connect fd (Unix.ADDR_INET (he.Unix.h_addr_list.(0), p.port));
    let transport =
      if p.secure then (
        let ctx = Ssl.create_context Ssl.TLSv1_2 Ssl.Client_context in
        if not tls_verify then Ssl.set_verify ctx [] None;
        let ssl = Ssl.embed_socket fd ctx in
        (try Ssl.set_client_SNI_hostname ssl p.host with _ -> ());
        Ssl.connect ssl;
        Tls ssl)
      else Tcp fd
    in
    let key = Base64url.encode_std ~pad:true (random_bytes 16) in
    let req =
      Printf.sprintf
        "GET %s HTTP/1.1\r\n\
         Host: %s\r\n\
         Upgrade: websocket\r\n\
         Connection: Upgrade\r\n\
         Sec-WebSocket-Key: %s\r\n\
         Sec-WebSocket-Version: 13\r\n\
         %s\r\n"
        p.path (authority p) key
        (extra_header_lines extra_headers)
    in
    write_all transport req;
    let resp = read_handshake_response transport in
    let lines = String.split_on_char '\n' resp in
    let status = String.trim (List.hd lines) in
    if not (String.length status >= 12 && String.sub status 9 3 = "101") then
      failwith ("Websocket: expected 101 Switching Protocols, got " ^ status);
    let headers = List.map String.trim (List.tl lines) in
    (match header_value headers "sec-websocket-accept" with
    | Some got ->
        let expected = accept_key key in
        if got <> expected then
          failwith "Websocket: Sec-WebSocket-Accept mismatch"
    | None -> failwith "Websocket: missing Sec-WebSocket-Accept");
    { transport }

  let close (ws : t) =
    (try send_close ws with _ -> ());
    match ws.transport with
    | Tls ssl -> (try Ssl.shutdown ssl with _ -> ())
    | Tcp fd -> (
        (try Unix.shutdown fd Unix.SHUTDOWN_ALL with _ -> ());
        try Unix.close fd with _ -> ())

  let with_connection ?tls_verify ?(extra_headers = []) url f =
    let ws = connect ?tls_verify ~extra_headers url in
    Fun.protect ~finally:(fun () -> close ws) (fun () -> f ws)
end
