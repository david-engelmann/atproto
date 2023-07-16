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
      List.map (fun value -> key ^ "=" ^ value) values
      |> String.concat "&"

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
    Client.post ~headers ~body (Uri.of_string url) >>= fun (resp, body) ->
    let _ = resp |> Response.status |> Code.code_of_status in
    (*Printf.printf "Response Code: %d\n" code;*)
    (*Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);*)
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    (*Printf.printf "Body of length: %d\n" (String.length body);*)
    body

  let post_data_with_headers (url : string) data headers =
    let open Lwt.Infix in
    let body = Cohttp_lwt.Body.of_string data in
    Client.post ~headers ~body (Uri.of_string url) >>= fun (resp, body) ->
    let _ = resp |> Response.status |> Code.code_of_status in
    (*Printf.printf "Response Code: %d\n" code;*)
    (*Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);*)
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    (*Printf.printf "Body of length: %d\n" (String.length body);*)
    body


  let get_request_with_body_and_headers (url : string) body headers =
    let open Lwt.Infix in
    let url_with_body = url ^ "?" ^ body in
    Client.get ~headers (Uri.of_string url_with_body) >>= fun (resp, body) ->
    let _ = resp |> Response.status |> Code.code_of_status in
    (*Printf.printf "Response Code: %d\n" code;*)
    (*Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);*)
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    (*Printf.printf "Body of length: %d\n" (String.length body);*)
    body

  let get_bytes_request_with_body_and_headers (url : string) body headers =
    let open Lwt.Infix in
    let url_with_body = url ^ "?" ^ body in
    Client.get ~headers (Uri.of_string url_with_body) >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_stream |> Lwt_stream.to_list >|= fun blob_list ->
    String.concat "" blob_list

  let get_request_with_headers (url : string) headers =
    let open Lwt.Infix in
    Client.get ~headers (Uri.of_string url) >>= fun (resp, body) ->
    let _ = resp |> Response.status |> Code.code_of_status in
    (*Printf.printf "Response Code: %d\n" code;*)
    (*Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);*)
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    (*Printf.printf "Body of length: %d\n" (String.length body);*)
    body

  let post_request_with_headers (url : string) headers =
    let open Lwt.Infix in
    Client.post ~headers (Uri.of_string url) >>= fun (resp, body) ->
    let _ = resp |> Response.status |> Code.code_of_status in
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
