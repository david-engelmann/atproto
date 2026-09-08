open OUnit2
open Atproto.Labeler
open Atproto.Records
open Atproto.Label

let with_public_timeout ?(seconds = 20) f =
  let old =
    Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> failwith "timeout"))
  in
  ignore (Unix.alarm seconds);
  Fun.protect
    ~finally:(fun () ->
      ignore (Unix.alarm 0);
      Sys.set_signal Sys.sigalrm old)
    f

let official_labeler = "did:plc:ar7c4by46qjdydhdevvrndac"

let test_parse_services _ =
  let json =
    `Assoc
      [
        ( "views",
          `List
            [
              `Assoc
                [
                  ( "uri",
                    `String
                      "at://did:plc:ar7c4by46qjdydhdevvrndac/app.bsky.labeler.service/self"
                  );
                  ("cid", `String "bafyreiabc");
                  ( "creator",
                    `Assoc
                      [
                        ("did", `String official_labeler);
                        ("handle", `String "moderation.bsky.app");
                      ] );
                  ("indexedAt", `String "2024-01-01T00:00:00.000Z");
                  ( "policies",
                    `Assoc
                      [
                        ( "labelValues",
                          `List [ `String "spam"; `String "!hide" ] );
                        ( "labelValueDefinitions",
                          `List
                            [
                              `Assoc
                                [
                                  ("identifier", `String "spam");
                                  ("severity", `String "inform");
                                  ("blurs", `String "none");
                                  ("defaultSetting", `String "warn");
                                  ( "locales",
                                    `List
                                      [
                                        `Assoc
                                          [
                                            ("lang", `String "en");
                                            ("name", `String "Spam");
                                            ("description", `String "Spam");
                                          ];
                                      ] );
                                ];
                            ] );
                      ] );
                ];
            ] );
      ]
  in
  let svcs = Labeler.parse_services json in
  OUnit2.assert_equal 1 (List.length svcs.views);
  OUnit2.assert_equal (Some official_labeler) (List.hd svcs.views).creator_did;
  OUnit2.assert_bool "policies"
    (match (List.hd svcs.views).policies with
    | Some p -> (
        List.mem "spam" p.label_values
        &&
        match p.label_value_definitions with
        | def :: _ -> def.identifier = "spam" && def.severity = "inform"
        | [] -> false)
    | None -> false)

let test_policies_to_json _ =
  let locale : Label.label_value_definition_strings =
    { lang = "en"; name = "Spam"; description = "Spam" }
  in
  let def : Label.label_value_definition =
    {
      identifier = "spam";
      severity = "inform";
      blurs = "none";
      default_setting = Some "warn";
      adult_only = None;
      locales = [ locale ];
    }
  in
  let policies : Labeler.policies =
    { label_values = [ "spam"; "!hide" ]; label_value_definitions = [ def ] }
  in
  let encoded = Labeler.policies_to_json policies in
  let parsed = Labeler.parse_policies encoded in
  OUnit2.assert_bool "roundtrip values"
    (List.mem "spam" parsed.label_values
    && List.mem "!hide" parsed.label_values);
  (match parsed.label_value_definitions with
  | d :: _ ->
      OUnit2.assert_equal ~printer:(fun x -> x) "spam" d.identifier;
      OUnit2.assert_equal ~printer:(fun x -> x) "inform" d.severity;
      OUnit2.assert_equal (Some "warn") d.default_setting;
      OUnit2.assert_equal None d.adult_only
  | [] -> OUnit2.assert_failure "expected labelValueDefinitions");
  let values_only : Labeler.policies =
    { label_values = [ "!hide" ]; label_value_definitions = [] }
  in
  let values_json = Labeler.policies_to_json values_only in
  OUnit2.assert_equal `Null
    (Yojson.Safe.Util.member "labelValueDefinitions" values_json);
  let labeler =
    Records.labeler_service ~policies:values_json
      ~created_at:"2024-01-01T00:00:00.000Z"
      ~reason_types:[ "com.atproto.moderation.defs#reasonSpam" ]
      ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal ~printer:(fun x -> x) "app.bsky.labeler.service"
    (labeler |> member "$type" |> to_string);
  OUnit2.assert_equal ~printer:(fun x -> x) "!hide"
    (labeler |> member "policies" |> member "labelValues" |> to_list |> List.hd
    |> to_string)

let test_get_services_live _ =
  try
    with_public_timeout (fun () ->
        let svcs =
          Labeler.get_services ~dids:[ official_labeler ] ~detailed:true ()
        in
        OUnit2.assert_bool "labeler view"
          (List.length svcs.views > 0
          && String.length (List.hd svcs.views).uri > 8))
  with exn -> skip_if true ("getServices skipped: " ^ Printexc.to_string exn)

let suite =
  "labeler"
  >::: [
         "test_parse_services" >:: test_parse_services;
         "test_policies_to_json" >:: test_policies_to_json;
         "test_get_services_live" >:: test_get_services_live;
       ]

let () = run_test_tt_main suite
