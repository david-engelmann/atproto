open Label
open Base64url

(** Typed builder/parser for `com.germnetwork.declaration`. *)
module Germnetwork = struct
  (** NSID for [com.germnetwork.declaration]. *)
  let nsid_declaration = "com.germnetwork.declaration"

  (** [messageMe.showButtonTo] value: hide the Message Me button. *)
  let show_none = "none"

  (** [messageMe.showButtonTo] value: show the button to accounts the
      author follows. *)
  let show_users_i_follow = "usersIFollow"

  (** [messageMe.showButtonTo] value: show the button to everyone. *)
  let show_everyone = "everyone"

  type message_me = { show_button_to : string; message_me_url : string }

  type declaration = {
    version : string;
    current_key : string;
    message_me : message_me option;
    key_package : string option;
    continuity_proofs : string list;
  }

  (** Encode raw bytes as a lexicon [$bytes] object (standard
      base64url). *)
  let bytes_to_json (raw : string) : Yojson.Safe.t =
    `Assoc [ ("$bytes", `String (Base64url.encode_std raw)) ]

  (** Decode a lexicon bytes object (or raw base64url string) to octets. *)
  let bytes_of_json json : string option = Label.bytes_of_json json

  (** Build a [messageMe] object ([showButtonTo] / [messageMeUrl]). *)
  let message_me ~show_button_to ~message_me_url : message_me =
    { show_button_to; message_me_url }

  (** JSON object for [messageMe] ([showButtonTo], [messageMeUrl]). *)
  let message_me_to_json (m : message_me) : Yojson.Safe.t =
    `Assoc
      [
        ("showButtonTo", `String m.show_button_to);
        ("messageMeUrl", `String m.message_me_url);
      ]

  (** Parse a [messageMe] object. Missing [showButtonTo] defaults to
      [show_none]; missing [messageMeUrl] is empty. *)
  let parse_message_me json : message_me =
    {
      show_button_to =
        (match Yojson.Safe.Util.member "showButtonTo" json with
        | `String s -> s
        | _ -> show_none);
      message_me_url =
        (match Yojson.Safe.Util.member "messageMeUrl" json with
        | `String s -> s
        | _ -> "");
    }

  (** Build a [com.germnetwork.declaration] record. [current_key] is
      encoded as lexicon bytes; optional [message_me] / [key_package] /
      [continuity_proofs] map to the lexicon. *)
  let declaration ~version ~current_key ?message_me ?key_package
      ?(continuity_proofs = []) () : Yojson.Safe.t =
    let fields =
      [
        ("$type", `String nsid_declaration);
        ("version", `String version);
        ("currentKey", bytes_to_json current_key);
      ]
      @ (match message_me with
        | Some m -> [ ("messageMe", message_me_to_json m) ]
        | None -> [])
      @ (match key_package with
        | Some k -> [ ("keyPackage", bytes_to_json k) ]
        | None -> [])
      @
      match continuity_proofs with
      | [] -> []
      | xs -> [ ("continuityProofs", `List (List.map bytes_to_json xs)) ]
    in
    `Assoc fields

  (** Parse a [com.germnetwork.declaration] record. Missing fields
      become empty strings / [None] / []. *)
  let parse_declaration json : declaration =
    {
      version =
        (match Yojson.Safe.Util.member "version" json with
        | `String s -> s
        | _ -> "");
      current_key =
        Option.value ~default:""
          (bytes_of_json (Yojson.Safe.Util.member "currentKey" json));
      message_me =
        (match Yojson.Safe.Util.member "messageMe" json with
        | `Assoc _ as m -> Some (parse_message_me m)
        | _ -> None);
      key_package = bytes_of_json (Yojson.Safe.Util.member "keyPackage" json);
      continuity_proofs =
        (match Yojson.Safe.Util.member "continuityProofs" json with
        | `List xs -> List.filter_map bytes_of_json xs
        | _ -> []);
    }
end
