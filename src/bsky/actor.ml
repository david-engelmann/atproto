open Session
open Cohttp_client
open App

module Actor = struct
  type viewer_status =
    {
      muted : bool;
      blocked_by : bool;
    }

  type profile =
    {
      did : string;
      handle : string;
      display_name : string;
      description : string;
      avatar : string;
      banner : string;
      follows_count : int;
      followers_count : int;
      posts_count : int;
      indexed_at : string;
      viewer : viewer_status;
      labels : (string list) option;
    }

  let parse_viewer_status json : viewer_status =
    let open Yojson.Safe.Util in
    let muted = json |> member "muted" |> to_bool in
    let blocked_by = json |> member "blockedBy" |> to_bool in
    { muted; blocked_by }

  let parse_profile json : profile =
    let open Yojson.Safe.Util in
    let did = json |> member "did" |> to_string in
    let handle = json |> member "handle" |> to_string in
    let display_name = json |> member "displayName" |> to_string in
    let description = json |> member "description" |> to_string in
    let avatar = json |> member "avatar" |> to_string in
    (*BANNER WASNT FOUND MIGHT HAVE SLIGHTLY DIFFERENT PROFILE FORMATS*)
    let banner = json |> member "banner" |> to_string in
    let follows_count = json |> member "followsCount" |> to_int in
    let followers_count = json |> member "followersCount" |> to_int in
    let posts_count = json |> member "postsCount" |> to_int in
    let indexed_at = json |> member "indexedAt" |> to_string in
    let viewer = json |> member "viewer" |> parse_viewer_status in
    let labels =
      match json |> member "labels" with
      | `Null -> None
      | labels_json -> Some (labels_json |> to_list |> List.map to_string)
    in
    { did; handle; display_name; description; avatar; banner; follows_count;
      followers_count; posts_count; indexed_at; viewer; labels }

  let parse_profiles json : profile list =
    let open Yojson.Safe.Util in
    let profile_dump = json |> to_list |> List.map to_string in
    match profile_dump with
    | [] -> []
    | hd :: _ ->
    Printf.printf "Profiles to parse: %s\n" hd;
    let open Yojson.Safe.Util in
    let profiles = json |> member "profiles" |> to_list in
    List.map parse_profile profiles

  let convert_body_to_json (body : string) : Yojson.Safe.t =
    let json = Yojson.Safe.from_string body in
    json

  let create_actor_endpoint (query_name : string) : string =
    "app.bsky.actor" ^ "." ^ query_name

  let get_profile (s : Session.session) (actor : string) : profile =
    let open Yojson.Safe.Util in
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_profile_url = App.create_endpoint_url base_url (create_actor_endpoint "getProfile") in
    let body = Cohttp_client.create_body_from_pairs [("actor", actor)] in
    let profile = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_profile_url body headers) in
    let profile_json = profile |> convert_body_to_json in
    Printf.printf "Checkout Profile on ingestion: %s\n" (to_string profile_json);
    profile_json |> parse_profile


  let get_profiles (s : Session.session) (actors : string list) : profile list =
    let open Yojson.Safe.Util in
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_profiles_url = App.create_endpoint_url base_url (create_actor_endpoint "getProfiles") in
    let body = Cohttp_client.add_query_params "actors" actors in
    let profiles = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_profiles_url body headers) in
    let profiles_json = profiles |> convert_body_to_json in
    Printf.printf "Checkout Profiles on ingestion: %s\n" (to_string profiles_json);
    profiles_json |> parse_profiles

  let get_suggestions (s : Session.session) (limit : int) : profile list =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let get_suggestions_url = App.create_endpoint_url base_url (create_actor_endpoint "getSuggestions") in
    let body = Cohttp_client.create_body_from_pairs [("limit", string_of_int limit)] in
    let suggestions = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers get_suggestions_url body headers) in
    suggestions |> convert_body_to_json |> parse_profiles

  let search_actors (s : Session.session) (term : string) (limit : int) : profile list =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let search_actors_url = App.create_endpoint_url base_url (create_actor_endpoint "searchActors") in
    let body = Cohttp_client.create_body_from_pairs [("term", term); ("limit", string_of_int limit)] in
    let profiles = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers search_actors_url body headers) in
    profiles |> convert_body_to_json |> parse_profiles

  let search_actors_typeahead (s : Session.session) (term : string) (limit : int) : profile list =
    let bearer_token = Session.bearer_token_from_session s in
    let application_json = Cohttp_client.application_json_setting_tuple in
    let headers = Cohttp_client.create_headers_from_pairs [application_json; bearer_token] in
    let base_url = App.create_base_url s in
    let search_actors_typeahead_url = App.create_endpoint_url base_url (create_actor_endpoint "searchActorsTypeahead") in
    let body = Cohttp_client.create_body_from_pairs [("term", term); ("limit", string_of_int limit)] in
    let profiles = Lwt_main.run (Cohttp_client.get_request_with_body_and_headers search_actors_typeahead_url body headers) in
    profiles |> convert_body_to_json |> parse_profiles

end
