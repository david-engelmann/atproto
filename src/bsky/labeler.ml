open Client

(** app.bsky.labeler.getServices — labeler views (public). *)
module Labeler = struct
  type policies = {
    label_values : string list;
    label_value_definitions : Label.Label.label_value_definition list;
  }

  type service = {
    uri : string;
    cid : string;
    creator_did : string option;
    like_count : int option;
    indexed_at : string;
    policies : policies option;
    labels : string list option;
    original : Yojson.Safe.t;
  }

  type services = { views : service list }

  let parse_policies json : policies =
    {
      label_values =
        List.filter_map
          (function `String s -> Some s | _ -> None)
          (Client.list_member json "labelValues");
      label_value_definitions =
        List.map Label.Label.parse_label_value_definition
          (Client.list_member json "labelValueDefinitions");
    }

  let parse_service json : service =
    let creator_did =
      match Yojson.Safe.Util.member "creator" json with
      | `Assoc _ as c -> Client.string_opt c "did"
      | _ -> None
    in
    {
      uri = Client.string_member json "uri";
      cid = Client.string_member json "cid";
      creator_did;
      like_count = Client.int_opt json "likeCount";
      indexed_at = Client.string_member json "indexedAt";
      policies =
        (match Yojson.Safe.Util.member "policies" json with
        | `Assoc _ as p -> Some (parse_policies p)
        | _ -> None);
      labels =
        Label.Label.parse_label_values (Yojson.Safe.Util.member "labels" json);
      original = json;
    }

  let parse_services json : services =
    { views = List.map parse_service (Client.list_member json "views") }

  (** Labeler views for [dids] via [app.bsky.labeler.getServices]. *)
  let get_services ?session ?host ~dids ?(detailed = false) () : services =
    Client.get_json ?session ?host "app.bsky.labeler.getServices"
      (Client.repeat_param "dids" dids
      @ [ ("detailed", string_of_bool detailed) ])
    |> parse_services
end
