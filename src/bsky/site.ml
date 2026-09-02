open Embed
open Label

(** Typed builders and parsers for official `site.standard.*` records. *)
module Site = struct
  (** NSID for [site.standard.document]. *)
  let nsid_document = "site.standard.document"

  (** NSID for [site.standard.publication]. *)
  let nsid_publication = "site.standard.publication"

  (** NSID for [site.standard.graph.recommend]. *)
  let nsid_recommend = "site.standard.graph.recommend"

  (** NSID for [site.standard.graph.subscription]. *)
  let nsid_subscription = "site.standard.graph.subscription"

  (** NSID for [site.standard.theme.basic]. *)
  let nsid_theme_basic = "site.standard.theme.basic"

  (** NSID for [site.standard.theme.color#rgb]. *)
  let nsid_theme_color_rgb = "site.standard.theme.color#rgb"

  (** NSID for [site.standard.theme.color#rgba]. *)
  let nsid_theme_color_rgba = "site.standard.theme.color#rgba"

  type contributor = {
    did : string;
    display_name : string option;
    role : string option;
  }

  type rgb = { r : int; g : int; b : int }
  type rgba = { r : int; g : int; b : int; a : int }
  type color = [ `Rgb of rgb | `Rgba of rgba | `Unknown of Yojson.Safe.t ]

  type theme = {
    background : color;
    foreground : color;
    accent : color;
    accent_foreground : color;
  }

  type preferences = { show_in_discover : bool option }

  type document = {
    site : string;
    title : string;
    published_at : string;
    path : string option;
    description : string option;
    text_content : string option;
    tags : string list;
    contributors : contributor list;
    updated_at : string option;
    bsky_post_ref : Embed.strong_ref option;
    self_labels : string list option;
    cover_image : Yojson.Safe.t option;
    content : Yojson.Safe.t option;
    links : Yojson.Safe.t option;
  }

  type publication = {
    url : string;
    name : string;
    description : string option;
    icon : Yojson.Safe.t option;
    self_labels : string list option;
    basic_theme : theme option;
    preferences : preferences option;
  }

  type recommend = { document : string; created_at : string }
  type subscription = { publication : string; created_at : string option }

  let string_member json field =
    match Yojson.Safe.Util.member field json with `String s -> s | _ -> ""

  let string_opt json field =
    match Yojson.Safe.Util.member field json with
    | `String s -> Some s
    | _ -> None

  let int_member json field =
    match Yojson.Safe.Util.member field json with
    | `Int n -> n
    | `Intlit s -> ( try int_of_string s with _ -> 0)
    | _ -> 0

  let bool_opt json field =
    match Yojson.Safe.Util.member field json with
    | `Bool b -> Some b
    | _ -> None

  let list_member json field =
    match Yojson.Safe.Util.member field json with `List xs -> xs | _ -> []

  (** Build a document [contributor] ([did] / optional [display_name] /
      [role]). *)
  let contributor ~did ?display_name ?role () : contributor =
    { did; display_name; role }

  (** JSON object for [contributor] ([did], [displayName], [role]). *)
  let contributor_to_json (c : contributor) : Yojson.Safe.t =
    let fields =
      ("did", `String c.did)
      ::
      (match c.display_name with
      | Some s -> [ ("displayName", `String s) ]
      | None -> [])
      @ match c.role with Some s -> [ ("role", `String s) ] | None -> []
    in
    `Assoc fields

  let parse_contributor json : contributor =
    {
      did = string_member json "did";
      display_name = string_opt json "displayName";
      role = string_opt json "role";
    }

  (** Build an RGB color ([r] / [g] / [b]) for
      [site.standard.theme.color#rgb]. *)
  let rgb ~r ~g ~b : rgb = { r; g; b }

  (** Build an RGBA color ([r] / [g] / [b] / [a]) for
      [site.standard.theme.color#rgba]. *)
  let rgba ~r ~g ~b ~a : rgba = { r; g; b; a }

  (** JSON object for a theme color ([site.standard.theme.color#rgb] or
      [#rgba]). *)
  let color_to_json (c : color) : Yojson.Safe.t =
    match c with
    | `Rgb c ->
        `Assoc
          [
            ("$type", `String nsid_theme_color_rgb);
            ("r", `Int c.r);
            ("g", `Int c.g);
            ("b", `Int c.b);
          ]
    | `Rgba c ->
        `Assoc
          [
            ("$type", `String nsid_theme_color_rgba);
            ("r", `Int c.r);
            ("g", `Int c.g);
            ("b", `Int c.b);
            ("a", `Int c.a);
          ]
    | `Unknown json -> json

  let parse_color (json : Yojson.Safe.t) : color =
    let ty = string_opt json "$type" in
    let has_a =
      match Yojson.Safe.Util.member "a" json with
      | `Int _ | `Intlit _ -> true
      | _ -> false
    in
    match ty with
    | Some t when t = nsid_theme_color_rgba || String.ends_with ~suffix:"rgba" t
      ->
        `Rgba
          {
            r = int_member json "r";
            g = int_member json "g";
            b = int_member json "b";
            a = int_member json "a";
          }
    | Some t when t = nsid_theme_color_rgb || String.ends_with ~suffix:"rgb" t
      ->
        `Rgb
          {
            r = int_member json "r";
            g = int_member json "g";
            b = int_member json "b";
          }
    | _ when has_a ->
        `Rgba
          {
            r = int_member json "r";
            g = int_member json "g";
            b = int_member json "b";
            a = int_member json "a";
          }
    | _ -> (
        match Yojson.Safe.Util.member "r" json with
        | `Int _ | `Intlit _ ->
            `Rgb
              {
                r = int_member json "r";
                g = int_member json "g";
                b = int_member json "b";
              }
        | _ -> `Unknown json)

  (** Build a [site.standard.theme.basic] color set ([background] /
      [foreground] / [accent] / [accent_foreground]). *)
  let theme ~background ~foreground ~accent ~accent_foreground : theme =
    { background; foreground; accent; accent_foreground }

  (** JSON object for [site.standard.theme.basic]. *)
  let theme_to_json (t : theme) : Yojson.Safe.t =
    `Assoc
      [
        ("$type", `String nsid_theme_basic);
        ("background", color_to_json t.background);
        ("foreground", color_to_json t.foreground);
        ("accent", color_to_json t.accent);
        ("accentForeground", color_to_json t.accent_foreground);
      ]

  let parse_theme json : theme =
    {
      background = parse_color (Yojson.Safe.Util.member "background" json);
      foreground = parse_color (Yojson.Safe.Util.member "foreground" json);
      accent = parse_color (Yojson.Safe.Util.member "accent" json);
      accent_foreground =
        parse_color (Yojson.Safe.Util.member "accentForeground" json);
    }

  (** Build a [site.standard.theme.basic] record. [background],
      [foreground], [accent], and [accent_foreground] map to the
      lexicon. *)
  let theme_basic ~background ~foreground ~accent ~accent_foreground () :
      Yojson.Safe.t =
    theme_to_json (theme ~background ~foreground ~accent ~accent_foreground)

  (** Build a [site.standard.document] record. [site], [title], and
      [published_at] are required; optional path, tags, contributors,
      and content map to the lexicon. *)
  let document ~site ~title ~published_at ?path ?description ?text_content
      ?(tags = []) ?(contributors = []) ?updated_at ?bsky_post_ref ?self_labels
      ?cover_image ?content ?links () : Yojson.Safe.t =
    let fields =
      [
        ("$type", `String nsid_document);
        ("site", `String site);
        ("title", `String title);
        ("publishedAt", `String published_at);
      ]
      @ (match path with Some s -> [ ("path", `String s) ] | None -> [])
      @ (match description with
        | Some s -> [ ("description", `String s) ]
        | None -> [])
      @ (match text_content with
        | Some s -> [ ("textContent", `String s) ]
        | None -> [])
      @ (match tags with
        | [] -> []
        | xs -> [ ("tags", `List (List.map (fun s -> `String s) xs)) ])
      @ (match contributors with
        | [] -> []
        | xs -> [ ("contributors", `List (List.map contributor_to_json xs)) ])
      @ (match updated_at with
        | Some s -> [ ("updatedAt", `String s) ]
        | None -> [])
      @ (match bsky_post_ref with
        | Some r -> [ ("bskyPostRef", Embed.strong_ref_to_json r) ]
        | None -> [])
      @ (match self_labels with
        | Some xs -> [ ("labels", Label.self_labels_to_json xs) ]
        | None -> [])
      @ (match cover_image with Some b -> [ ("coverImage", b) ] | None -> [])
      @ (match content with Some c -> [ ("content", c) ] | None -> [])
      @ match links with Some l -> [ ("links", l) ] | None -> []
    in
    `Assoc fields

  let parse_document json : document =
    {
      site = string_member json "site";
      title = string_member json "title";
      published_at = string_member json "publishedAt";
      path = string_opt json "path";
      description = string_opt json "description";
      text_content = string_opt json "textContent";
      tags =
        List.filter_map
          (function `String s -> Some s | _ -> None)
          (list_member json "tags");
      contributors =
        List.map parse_contributor (list_member json "contributors");
      updated_at = string_opt json "updatedAt";
      bsky_post_ref =
        (match Yojson.Safe.Util.member "bskyPostRef" json with
        | `Assoc _ as r -> Some (Embed.parse_strong_ref r)
        | _ -> None);
      self_labels =
        Label.parse_self_labels (Yojson.Safe.Util.member "labels" json);
      cover_image =
        (match Yojson.Safe.Util.member "coverImage" json with
        | `Null | `Assoc [] -> None
        | (`Assoc _ | `String _) as b -> Some b
        | _ -> None);
      content =
        (match Yojson.Safe.Util.member "content" json with
        | `Null -> None
        | j -> Some j);
      links =
        (match Yojson.Safe.Util.member "links" json with
        | `Null -> None
        | j -> Some j);
    }

  (** Build a [site.standard.publication] record. [url] and [name] are
      required; optional [description] / [icon] / [self_labels] /
      [basic_theme] / [show_in_discover] map to the lexicon. *)
  let publication ~url ~name ?description ?icon ?self_labels ?basic_theme
      ?show_in_discover () : Yojson.Safe.t =
    let fields =
      [
        ("$type", `String nsid_publication);
        ("url", `String url);
        ("name", `String name);
      ]
      @ (match description with
        | Some s -> [ ("description", `String s) ]
        | None -> [])
      @ (match icon with Some b -> [ ("icon", b) ] | None -> [])
      @ (match self_labels with
        | Some xs -> [ ("labels", Label.self_labels_to_json xs) ]
        | None -> [])
      @ (match basic_theme with
        | Some t -> [ ("basicTheme", theme_to_json t) ]
        | None -> [])
      @
      match show_in_discover with
      | Some b -> [ ("preferences", `Assoc [ ("showInDiscover", `Bool b) ]) ]
      | None -> []
    in
    `Assoc fields

  let parse_publication json : publication =
    {
      url = string_member json "url";
      name = string_member json "name";
      description = string_opt json "description";
      icon =
        (match Yojson.Safe.Util.member "icon" json with
        | `Null | `Assoc [] -> None
        | (`Assoc _ | `String _) as b -> Some b
        | _ -> None);
      self_labels =
        Label.parse_self_labels (Yojson.Safe.Util.member "labels" json);
      basic_theme =
        (match Yojson.Safe.Util.member "basicTheme" json with
        | `Assoc _ as t -> Some (parse_theme t)
        | _ -> None);
      preferences =
        (match Yojson.Safe.Util.member "preferences" json with
        | `Assoc _ as p ->
            Some { show_in_discover = bool_opt p "showInDiscover" }
        | _ -> None);
    }

  (** Build a [site.standard.graph.recommend] record for [document]
      (AT-URI). [created_at] is required. *)
  let recommend ~document ~created_at () : Yojson.Safe.t =
    `Assoc
      [
        ("$type", `String nsid_recommend);
        ("document", `String document);
        ("createdAt", `String created_at);
      ]

  let parse_recommend json : recommend =
    {
      document = string_member json "document";
      created_at = string_member json "createdAt";
    }

  (** Build a [site.standard.graph.subscription] record for
      [publication] (AT-URI). Optional [created_at] maps to the
      lexicon. *)
  let subscription ~publication ?created_at () : Yojson.Safe.t =
    let fields =
      [
        ("$type", `String nsid_subscription);
        ("publication", `String publication);
      ]
      @
      match created_at with
      | Some s -> [ ("createdAt", `String s) ]
      | None -> []
    in
    `Assoc fields

  let parse_subscription json : subscription =
    {
      publication = string_member json "publication";
      created_at = string_opt json "createdAt";
    }
end
