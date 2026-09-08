open OUnit2
open Atproto.Site
open Atproto.Embed

let test_document_roundtrip _ =
  let json =
    Site.document ~site:"https://standard.site" ~title:"Hello"
      ~published_at:"2026-01-01T00:00:00.000Z" ~path:"/hello"
      ~description:"intro" ~text_content:"plain hello" ~tags:[ "atproto" ]
      ~contributors:
        [
          Site.contributor ~did:"did:plc:abc123xyz0001112223333"
            ~display_name:"Ada" ~role:"editor" ();
        ]
      ~bsky_post_ref:
        {
          uri = "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/3k";
          cid = "bafyreihdummy000000000000000000000000000000000";
        }
      ~self_labels:[ "graphic-media" ] ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "site.standard.document"
    (json |> member "$type" |> to_string);
  let parsed = Site.parse_document json in
  OUnit2.assert_equal ~printer:(fun x -> x) "Hello" parsed.title;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "/hello"
    (Option.value parsed.path ~default:"");
  OUnit2.assert_equal [ "atproto" ] parsed.tags;
  OUnit2.assert_equal 1 (List.length parsed.contributors);
  OUnit2.assert_equal (Some [ "graphic-media" ]) parsed.self_labels;
  match parsed.bsky_post_ref with
  | Some r -> OUnit2.assert_bool "post ref uri" (String.length r.Embed.uri > 8)
  | None -> OUnit2.assert_failure "expected bskyPostRef"

let test_publication_and_theme _ =
  let theme =
    Site.theme
      ~background:(`Rgb (Site.rgb ~r:255 ~g:255 ~b:255))
      ~foreground:(`Rgb (Site.rgb ~r:0 ~g:0 ~b:0))
      ~accent:(`Rgb (Site.rgb ~r:0 ~g:80 ~b:200))
      ~accent_foreground:(`Rgba (Site.rgba ~r:255 ~g:255 ~b:255 ~a:100))
  in
  let json =
    Site.publication ~url:"https://standard.site" ~name:"Notes"
      ~description:"essays" ~basic_theme:theme ~show_in_discover:false ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "site.standard.publication"
    (json |> member "$type" |> to_string);
  let parsed = Site.parse_publication json in
  OUnit2.assert_equal ~printer:(fun x -> x) "Notes" parsed.name;
  (match parsed.preferences with
  | Some { show_in_discover = Some false } -> ()
  | _ -> OUnit2.assert_failure "expected showInDiscover=false");
  match parsed.basic_theme with
  | Some t -> (
      match t.accent with
      | `Rgb c -> OUnit2.assert_equal 200 c.b
      | _ -> OUnit2.assert_failure "expected rgb accent")
  | None -> OUnit2.assert_failure "expected basicTheme"

let test_graph_records _ =
  let rec_json =
    Site.recommend
      ~document:"at://did:plc:abc123xyz0001112223333/site.standard.document/3k"
      ~created_at:"2026-01-01T00:00:00.000Z" ()
  in
  let sub_json =
    Site.subscription
      ~publication:
        "at://did:plc:abc123xyz0001112223333/site.standard.publication/3k"
      ~created_at:"2026-01-01T00:00:00.000Z" ()
  in
  let rec_ = Site.parse_recommend rec_json in
  let sub = Site.parse_subscription sub_json in
  OUnit2.assert_bool "recommend uri" (String.length rec_.document > 10);
  OUnit2.assert_bool "subscription uri" (String.length sub.publication > 10);
  let theme_json =
    Site.theme_basic
      ~background:(`Rgb { r = 1; g = 2; b = 3 })
      ~foreground:(`Rgb { r = 4; g = 5; b = 6 })
      ~accent:(`Rgb { r = 7; g = 8; b = 9 })
      ~accent_foreground:(`Rgb { r = 10; g = 11; b = 12 })
      ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "site.standard.theme.basic"
    (theme_json |> member "$type" |> to_string)

let assoc_keys json =
  match json with `Assoc fields -> List.map fst fields | _ -> []

let assert_keys_subset ~allowed keys =
  List.iter
    (fun k ->
      OUnit2.assert_bool ("unexpected leftover key: " ^ k)
        (List.mem k allowed))
    keys

let test_typed_record_encodes _ =
  let open Yojson.Safe.Util in
  let cover =
    `Assoc
      [
        ("$type", `String "blob");
        ("ref", `Assoc [ ("$link", `String "bafyreihdummycover") ]);
        ("mimeType", `String "image/jpeg");
        ("size", `Int 123);
      ]
  in
  let content =
    `Assoc [ ("$type", `String "site.standard.document#placeholder") ]
  in
  let links =
    `Assoc [ ("$type", `String "site.standard.document#placeholder") ]
  in
  let doc_json =
    Site.document ~site:"https://standard.site" ~title:"Hello"
      ~published_at:"2026-01-01T00:00:00.000Z" ~path:"/hello"
      ~description:"intro" ~text_content:"plain hello" ~tags:[ "atproto" ]
      ~contributors:
        [
          Site.contributor ~did:"did:plc:abc123xyz0001112223333"
            ~display_name:"Ada" ~role:"editor" ();
        ]
      ~updated_at:"2026-01-02T00:00:00.000Z"
      ~bsky_post_ref:
        {
          uri = "at://did:plc:abc123xyz0001112223333/app.bsky.feed.post/3k";
          cid = "bafyreihdummy000000000000000000000000000000000";
        }
      ~self_labels:[ "graphic-media" ] ~cover_image:cover ~content ~links ()
  in
  let parsed_doc = Site.parse_document doc_json in
  let encoded_doc = Site.document_to_json parsed_doc in
  let again_doc = Site.parse_document encoded_doc in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "site.standard.document"
    (encoded_doc |> member "$type" |> to_string);
  OUnit2.assert_equal ~printer:(fun x -> x) "Hello" again_doc.title;
  OUnit2.assert_equal (Some "/hello") again_doc.path;
  OUnit2.assert_equal (Some "intro") again_doc.description;
  OUnit2.assert_equal (Some "plain hello") again_doc.text_content;
  OUnit2.assert_equal [ "atproto" ] again_doc.tags;
  OUnit2.assert_equal 1 (List.length again_doc.contributors);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "Ada"
    (match (List.hd again_doc.contributors).display_name with
    | Some s -> s
    | None -> "");
  OUnit2.assert_equal (Some "2026-01-02T00:00:00.000Z") again_doc.updated_at;
  OUnit2.assert_equal (Some [ "graphic-media" ]) again_doc.self_labels;
  (match again_doc.bsky_post_ref with
  | Some r ->
      OUnit2.assert_bool "post ref uri" (String.length r.Embed.uri > 8)
  | None -> OUnit2.assert_failure "expected bskyPostRef after encode");
  OUnit2.assert_bool "coverImage" (Option.is_some again_doc.cover_image);
  OUnit2.assert_bool "content" (Option.is_some again_doc.content);
  OUnit2.assert_bool "links" (Option.is_some again_doc.links);
  assert_keys_subset
    ~allowed:
      [
        "$type";
        "site";
        "title";
        "publishedAt";
        "path";
        "description";
        "textContent";
        "tags";
        "contributors";
        "updatedAt";
        "bskyPostRef";
        "labels";
        "coverImage";
        "content";
        "links";
      ]
    (assoc_keys encoded_doc);
  let minimal_doc =
    Site.document_to_json
      (Site.parse_document
         (Site.document ~site:"https://standard.site" ~title:"Hi"
            ~published_at:"2026-01-01T00:00:00.000Z" ()))
  in
  OUnit2.assert_equal
    [ "$type"; "site"; "title"; "publishedAt" ]
    (assoc_keys minimal_doc);
  let theme =
    Site.theme
      ~background:(`Rgb (Site.rgb ~r:255 ~g:255 ~b:255))
      ~foreground:(`Rgb (Site.rgb ~r:0 ~g:0 ~b:0))
      ~accent:(`Rgb (Site.rgb ~r:0 ~g:80 ~b:200))
      ~accent_foreground:(`Rgba (Site.rgba ~r:255 ~g:255 ~b:255 ~a:100))
  in
  let pub_json =
    Site.publication ~url:"https://standard.site" ~name:"Notes"
      ~description:"essays" ~basic_theme:theme ~show_in_discover:false ()
  in
  let parsed_pub = Site.parse_publication pub_json in
  let encoded_pub = Site.publication_to_json parsed_pub in
  let again_pub = Site.parse_publication encoded_pub in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "site.standard.publication"
    (encoded_pub |> member "$type" |> to_string);
  OUnit2.assert_equal ~printer:(fun x -> x) "Notes" again_pub.name;
  OUnit2.assert_equal (Some "essays") again_pub.description;
  (match again_pub.preferences with
  | Some { show_in_discover = Some false } -> ()
  | _ -> OUnit2.assert_failure "expected showInDiscover=false after encode");
  (match again_pub.basic_theme with
  | Some t -> (
      match t.accent with
      | `Rgb c -> OUnit2.assert_equal 200 c.b
      | _ -> OUnit2.assert_failure "expected rgb accent after encode")
  | None -> OUnit2.assert_failure "expected basicTheme after encode");
  assert_keys_subset
    ~allowed:
      [
        "$type";
        "url";
        "name";
        "description";
        "icon";
        "labels";
        "basicTheme";
        "preferences";
      ]
    (assoc_keys encoded_pub);
  let empty_prefs =
    Site.publication_to_json
      (Site.parse_publication
         (`Assoc
           [
             ("$type", `String Site.nsid_publication);
             ("url", `String "https://standard.site");
             ("name", `String "Empty");
             ("preferences", `Assoc []);
           ]))
  in
  OUnit2.assert_bool "omit empty preferences leftover"
    (not (List.mem "preferences" (assoc_keys empty_prefs)));
  let rec_json =
    Site.recommend
      ~document:"at://did:plc:abc123xyz0001112223333/site.standard.document/3k"
      ~created_at:"2026-01-01T00:00:00.000Z" ()
  in
  let encoded_rec = Site.recommend_to_json (Site.parse_recommend rec_json) in
  let again_rec = Site.parse_recommend encoded_rec in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "site.standard.graph.recommend"
    (encoded_rec |> member "$type" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "at://did:plc:abc123xyz0001112223333/site.standard.document/3k"
    again_rec.document;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "2026-01-01T00:00:00.000Z" again_rec.created_at;
  OUnit2.assert_equal
    [ "$type"; "document"; "createdAt" ]
    (assoc_keys encoded_rec);
  let sub_json =
    Site.subscription
      ~publication:
        "at://did:plc:abc123xyz0001112223333/site.standard.publication/3k"
      ~created_at:"2026-01-01T00:00:00.000Z" ()
  in
  let encoded_sub =
    Site.subscription_to_json (Site.parse_subscription sub_json)
  in
  let again_sub = Site.parse_subscription encoded_sub in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "site.standard.graph.subscription"
    (encoded_sub |> member "$type" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "at://did:plc:abc123xyz0001112223333/site.standard.publication/3k"
    again_sub.publication;
  OUnit2.assert_equal
    (Some "2026-01-01T00:00:00.000Z")
    again_sub.created_at;
  OUnit2.assert_equal
    [ "$type"; "publication"; "createdAt" ]
    (assoc_keys encoded_sub);
  let sub_required =
    Site.subscription_to_json
      (Site.parse_subscription
         (Site.subscription
            ~publication:
              "at://did:plc:abc123xyz0001112223333/site.standard.publication/3k"
            ()))
  in
  OUnit2.assert_equal [ "$type"; "publication" ] (assoc_keys sub_required)

let suite =
  "site"
  >::: [
         "test_document_roundtrip" >:: test_document_roundtrip;
         "test_publication_and_theme" >:: test_publication_and_theme;
         "test_graph_records" >:: test_graph_records;
         "test_typed_record_encodes" >:: test_typed_record_encodes;
       ]

let () = run_test_tt_main suite
