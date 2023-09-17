open Cohttp

open Cohttp_lwt_unix

module Cohttp_client = struct
  let get_body url =
    let open Lwt.Infix in
    Client.get (Uri.of_string url) >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    Printf.printf "Response code: %d\n" code;
    Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    Printf.printf "Body of length: %d\n" (String.length body);
    body

  let get_host (host : string) (port : int) : string Lwt.t =
    let url = Printf.sprintf "http://%s:%d" host port in
    get_body url

  let add_pair_to_header h setting_pair =
    match setting_pair with
    | (setting, value) -> Header.add h setting value

  let rec add_pairs_to_header h (header_settings : (string * string) list) =
    match header_settings with
    | [] -> h
    | hd :: res ->
      let h = add_pair_to_header h hd in
      add_pairs_to_header h res

  let pairs_to_query_string params =
    let kv_to_string (k, v) = Uri.pct_encode k ^ "=" ^ Uri.pct_encode v in
    String.concat "&" (List.map kv_to_string params)

  (*
  let pairs_with_array_value_to_query_string params =
    let kv_to_string (k, vs) =
      let json_vs = `List (List.map (fun v -> `String v) vs) in
      Uri.pct_encode k ^ "=" ^ (Yojson.Safe.to_string json_vs)
    in
    String.concat "&" (List.map kv_to_string params)
  *)

  let create_body_from_pairs_with_array_value pairs =
    `Assoc (List.map (fun (key, values) ->
      (key, `List (List.map (fun value -> `String value) values))
    ) pairs)
    |> Yojson.to_string

  let create_headers_from_pairs (header_settings : (string * string) list) =
    match header_settings with
    | [] -> Header.init ()
    | hd :: res ->
      let headers = Header.init () in
      let headers = add_pair_to_header headers hd in
      add_pairs_to_header headers res

  let create_body_from_pairs (data : (string * string) list) =
    match data with
    | [] -> ""
    | _ -> pairs_to_query_string data

  let add_query_params_to_url url key values =
    List.fold_left (fun url value ->
      Uri.add_query_param' url (key, value)
    ) url values

    let add_query_params key values =
      List.map (fun value -> key ^ "=" ^ value) values |> String.concat "&"

  (*
  let create_body_from_pairs_with_array_value (data : (string * string list) list) =
    match data with
    | [] -> ""
    | _ -> pairs_with_array_value_to_query_string data
  *)

  let application_json_setting_tuple : (string * string) = ("Content-Type", "application/json")

  let post_data (url : string) data =
    let open Lwt.Infix in
    let headers = Header.init ()
    |> fun h -> Header.add h "Content-Type" "application/json" in
    let body = Cohttp_lwt.Body.of_string data in
    Client.post ~headers ~body (Uri.of_string url) >>= fun (_, body) ->
    (*let _ = resp |> Response.status |> Code.code_of_status in*)
    (*Printf.printf "Response Code: %d\n" code;*)
    (*Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);*)
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    (*Printf.printf "Body of length: %d\n" (String.length body);*)
    body

  let post_data_with_headers (url : string) data headers =
    let open Lwt.Infix in
    let body = Cohttp_lwt.Body.of_string data in
    Client.post ~headers ~body (Uri.of_string url) >>= fun (_, body) ->
    (*let _ = resp |> Response.status |> Code.code_of_status in*)
    (*Printf.printf "Response Code: %d\n" code;*)
    (*Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);*)
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    (*Printf.printf "Body of length: %d\n" (String.length body);*)
    body


  let get_request_with_body_and_headers (url : string) body headers =
    let open Lwt.Infix in
    let url_with_body = url ^ "?" ^ body in
    Client.get ~headers (Uri.of_string url_with_body) >>= fun (_, body) ->
    (*let _ = resp |> Response.status |> Code.code_of_status in*)
    (*Printf.printf "Response Code: %d\n" code;*)
    (*Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);*)
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    (*Printf.printf "Body of length: %d\n" (String.length body);*)
    body

  let temp_file_of_string s prefix suffix =
    let open Core in
    let temp_filename = Filename.temp_file prefix suffix in
    Out_channel.write_all temp_filename ~data:s;
    temp_filename

  let in_channel_of_temp_file f =
    let open Core in
    let ic = In_channel.create f in
    ic

  (*
  let in_channel_of_string s =
    let buffer = Lexing.from_string s in
    let input_function buf len =
      let available = String.length s - buffer.Lexing.lex_curr_pos in
      let amount_to_read = min available len in
      if amount_to_read > 0 then (
        String.blit s buffer.Lexing.lex_curr_pos buf 0 amount_to_read;
        buffer.Lexing.lex_curr_pos <- buffer.Lexing.lex_curr_pos + amount_to_read;
        amount_to_read
      ) else (
        0
      )
    in
    let seek_function = fun _ _ -> raise (Failure "seek not implemented") in
    let close_function _ = () in
    let channel_functions = {
      input = input_function;
      output = (fun _ _ _ -> raise (Failure "output not implemented"));
      flush = (fun _ -> ());
      seek = seek_function;
      close_in = close_function;
      close_out = (fun _ -> ());
      set_binary_mode_in = (fun _ _ -> ());
      set_binary_mode_out = (fun _ _ -> ());
    }
    in
    open_in_gen [Open_rdonly] 0 channel_functions

  let in_channel_of_string str =
    let buffer = Buffer.create (String.length str) in
    Buffer.add_string buffer str;
    let channel = Buffer.contents buffer |> Lexing.from_string in
    let input_fun buffer len =
      let read_bytes = min len (String.length channel.Lexing.lex_buffer - channel.Lexing.lex_curr_pos) in
      Bytes.blit_string channel.Lexing.lex_buffer channel.Lexing.lex_curr_pos buffer 0 read_bytes;
      channel.Lexing.lex_curr_pos <- channel.Lexing.lex_curr_pos + read_bytes;
      read_bytes
    in
    let close_fun () = () in
    let seek_fun = None in
    let pos_fun () = channel.Lexing.lex_curr_pos in
    let input_channel = Stdlib.create_in_channel ~input_fun ~close_fun ~seek_fun ~pos_fun () in
    input_channel
  *)

  let get_stream_request_with_body_and_headers (url : string) body headers =
    let open Lwt.Infix in
    let url_with_body = url ^ "?" ^ body in
    let partial_temp_file_of_string str = temp_file_of_string str "test_temp" "car" in
    Client.get ~headers (Uri.of_string url_with_body) >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_stream |> Lwt_stream.to_list >|= fun blob_list ->
    (String.concat "" blob_list) |> partial_temp_file_of_string |> in_channel_of_temp_file

  let get_bytes_request_with_body_and_headers (url : string) body headers =
    let open Lwt.Infix in
    let url_with_body = url ^ "?" ^ body in
    Client.get ~headers (Uri.of_string url_with_body) >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_stream |> Lwt_stream.to_list >|= fun blob_list ->
    String.concat "" blob_list

  let get_request_with_headers (url : string) headers =
    let open Lwt.Infix in
    Client.get ~headers (Uri.of_string url) >>= fun (_, body) ->
    (*let _ = resp |> Response.status |> Code.code_of_status in*)
    (*Printf.printf "Response Code: %d\n" code;*)
    (*Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);*)
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    (*Printf.printf "Body of length: %d\n" (String.length body);*)
    body

  let post_request_with_headers (url : string) headers =
    let open Lwt.Infix in
    Client.post ~headers (Uri.of_string url) >>= fun (_, body) ->
    (*let _ = resp |> Response.status |> Code.code_of_status in*)
    (*Printf.printf "Response Code: %d\n" code;*)
    (*Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);*)
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    (*Printf.printf "Body of length: %d\n" (String.length body);*)
    body

  let get_content_type_with_body_headers (url : string) body headers =
    let open Lwt.Infix in
    let url_with_body = url ^ "?" ^ body in
    Client.get ~headers (Uri.of_string url_with_body) >>= fun (resp, _) ->
    match Header.get (Response.headers resp) "content-type" with
    | Some ct -> Lwt.return ct
    | None -> Lwt.return "No content-type found"

end
