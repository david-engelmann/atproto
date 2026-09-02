open Session
open Client
open Feed

(** app.bsky.bookmark — private post bookmarks (auth required). *)
module Bookmark = struct
  type strong_ref = { uri : string; cid : string }
  type bookmark_item = Feed.reply_ref_item

  type bookmark_view = {
    uri : string;
    cid : string;
    created_at : string option;
    item : bookmark_item;
    original : Yojson.Safe.t;
  }

  type bookmarks = { cursor : string option; bookmarks : bookmark_view list }

  let parse_strong_ref json : strong_ref =
    {
      uri = Client.string_member json "uri";
      cid = Client.string_member json "cid";
    }

  let parse_bookmark_item json : bookmark_item =
    try Feed.parse_reply_ref_item json
    with Yojson.Safe.Util.Type_error _ ->
      `NotFound
        {
          uri = Client.string_member json "uri";
          not_found =
            (match Yojson.Safe.Util.member "notFound" json with
            | `Bool b -> b
            | _ -> true);
        }

  let parse_bookmark_view json : bookmark_view =
    let subject =
      match Yojson.Safe.Util.member "subject" json with
      | `Assoc _ as s -> parse_strong_ref s
      | _ -> { uri = ""; cid = "" }
    in
    let item_json = Yojson.Safe.Util.member "item" json in
    {
      uri = subject.uri;
      cid = subject.cid;
      created_at = Client.string_opt json "createdAt";
      item = parse_bookmark_item item_json;
      original = json;
    }

  let parse_bookmarks json : bookmarks =
    {
      cursor = Client.string_opt json "cursor";
      bookmarks =
        List.map parse_bookmark_view (Client.list_member json "bookmarks");
    }

  let create_bookmark_body ~uri ~cid : Yojson.Safe.t =
    `Assoc [ ("uri", `String uri); ("cid", `String cid) ]

  let delete_bookmark_body ~uri : Yojson.Safe.t =
    `Assoc [ ("uri", `String uri) ]

  (** Bookmark [uri] / [cid] via [app.bsky.bookmark.createBookmark] (auth
      required). *)
  let create_bookmark (s : Session.session) ~uri ~cid () : unit =
    ignore
      (Client.post_json ~session:s "app.bsky.bookmark.createBookmark"
         (Yojson.Safe.to_string (create_bookmark_body ~uri ~cid)))

  (** Remove the bookmark for [uri] via [app.bsky.bookmark.deleteBookmark]. *)
  let delete_bookmark (s : Session.session) ~uri () : unit =
    ignore
      (Client.post_json ~session:s "app.bsky.bookmark.deleteBookmark"
         (Yojson.Safe.to_string (delete_bookmark_body ~uri)))

  (** The session's bookmarks via [app.bsky.bookmark.getBookmarks]. *)
  let get_bookmarks (s : Session.session) ?limit ?cursor () : bookmarks =
    Client.get_json ~session:s "app.bsky.bookmark.getBookmarks"
      (Client.opt_int "limit" limit @ Client.opt_pair "cursor" cursor)
    |> parse_bookmarks
end
