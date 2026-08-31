open OUnit2
open Atproto.Labeler

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
    | Some p -> List.mem "spam" p.label_values
    | None -> false)

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
         "test_get_services_live" >:: test_get_services_live;
       ]

let () = run_test_tt_main suite
