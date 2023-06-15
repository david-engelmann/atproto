open Cohttp
open Cohttp_lwt_unix

module Cohttp_client = struct
  let open Lwt.Infix in
  let get_body url =
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
end
