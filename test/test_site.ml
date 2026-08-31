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

let suite =
  "site"
  >::: [
         "test_document_roundtrip" >:: test_document_roundtrip;
         "test_publication_and_theme" >:: test_publication_and_theme;
         "test_graph_records" >:: test_graph_records;
       ]

let () = run_test_tt_main suite
