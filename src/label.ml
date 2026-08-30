open Session
open Cohttp_client
open App

module Label = struct
  type label = {
    src : string;
    uri : string;
    cid : string option;
    val_ : string;
    neg : bool option;
    cts : string option;
  }

  let parse_label json : label =
    let open Yojson.Safe.Util in
    {
      src = (match json |> member "src" with `String s -> s | _ -> "");
      uri = (match json |> member "uri" with `String s -> s | _ -> "");
      cid = (match json |> member "cid" with `String s -> Some s | _ -> None);
      val_ = (match json |> member "val" with `String s -> s | _ -> "");
      neg = (match json |> member "neg" with `Bool b -> Some b | _ -> None);
      cts = (match json |> member "cts" with `String s -> Some s | _ -> None);
    }

  let parse_label_values json : string list option =
    match json with
    | `Null -> None
    | `List items ->
        let vals =
          List.filter_map
            (function
              | `String s -> Some s
              | `Assoc _ as obj ->
                  (match Yojson.Safe.Util.member "val" obj with
                  | `String s -> Some s
                  | _ -> None)
              | _ -> None)
            items
        in
        if vals = [] then None else Some vals
    | _ -> None

  let create_label_endpoint (query_name : string) : string =
    "com.atproto.label" ^ "." ^ query_name

  (* List of AT URI patterns to match (boolean 'OR'). Each may
   * be a prefix (ending with '*'; will match inclusive of the string leading to
   * '*'), or a full URI *)
  let query_labels (s : Session.session) (uri_patterns : string list) : string =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let query_labels_url = App.create_endpoint_url base_url (create_label_endpoint "queryLabels") in
    let body = Cohttp_client.add_query_params "uriPatterns" uri_patterns in
    let labels = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers query_labels_url body headers) in
    labels
end
