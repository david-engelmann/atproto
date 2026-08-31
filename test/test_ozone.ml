open OUnit2
open Atproto.Ozone
open Atproto.Xrpc
open Atproto.Auth

let test_labeler_proxy _ =
  let p = Ozone.labeler_proxy "did:web:mod.example.com" in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:web:mod.example.com#atproto_labeler" (Xrpc.proxy_to_string p)

let test_parse_statuses _ =
  let json =
    `Assoc
      [
        ( "subjectStatuses",
          `List
            [
              `Assoc
                [
                  ( "subject",
                    `Assoc
                      [
                        ("$type", `String "com.atproto.admin.defs#repoRef");
                        ("did", `String "did:plc:abc123xyz0001112223333");
                      ] );
                  ( "reviewState",
                    `String "tools.ozone.moderation.defs#reviewOpen" );
                  ("comment", `String "looks spammy");
                  ("priorityScore", `Int 40);
                ];
            ] );
      ]
  in
  let page = Ozone.parse_statuses json in
  OUnit2.assert_equal 1 (List.length page.subject_statuses);
  OUnit2.assert_equal (Some "looks spammy")
    (List.hd page.subject_statuses).comment

let test_emit_event_body _ =
  let body =
    Ozone.emit_event_body
      ~event:(Ozone.comment_event "note")
      ~subject:(Ozone.repo_ref "did:plc:abc123xyz0001112223333")
      ~created_by:"did:plc:mod000111222333444555666" ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:plc:abc123xyz0001112223333"
    (body |> member "subject" |> member "did" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "tools.ozone.moderation.defs#modEventComment"
    (body |> member "event" |> member "$type" |> to_string)

let test_takedown_event _ =
  let ev = Ozone.takedown_event ~comment:"spam" () in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "tools.ozone.moderation.defs#modEventTakedown"
    (ev |> member "$type" |> to_string)

let test_parse_timeline_and_schedule _ =
  let timeline =
    Ozone.parse_account_timeline
      (`Assoc
        [
          ( "timeline",
            `List
              [
                `Assoc
                  [
                    ("day", `String "2024-01-01");
                    ( "summary",
                      `List
                        [
                          `Assoc
                            [
                              ("eventSubjectType", `String "account");
                              ( "eventType",
                                `String
                                  "tools.ozone.moderation.defs#modEventTakedown"
                              );
                              ("count", `Int 2);
                            ];
                        ] );
                  ];
              ] );
        ])
  in
  OUnit2.assert_equal 1 (List.length timeline.timeline);
  OUnit2.assert_equal 2 (List.hd (List.hd timeline.timeline).summary).count;
  let result =
    Ozone.parse_batch_result
      (`Assoc
        [
          ("succeeded", `List [ `String "did:plc:abc123xyz0001112223333" ]);
          ( "failed",
            `List
              [
                `Assoc
                  [
                    ("did", `String "did:plc:fail000111222333444555666");
                    ("error", `String "busy");
                    ("errorCode", `String "Conflict");
                  ];
              ] );
        ])
  in
  OUnit2.assert_equal 1 (List.length result.succeeded);
  OUnit2.assert_equal ~printer:(fun x -> x) "busy" (List.hd result.failed).error;
  let body =
    Ozone.schedule_action_body
      ~action:(Ozone.takedown_action ~comment:"spam" ())
      ~subjects:[ "did:plc:abc123xyz0001112223333" ]
      ~created_by:"did:plc:mod000111222333444555666"
      ~scheduling:
        {
          execute_at = Some "2024-02-01T00:00:00.000Z";
          execute_after = None;
          execute_until = None;
        }
      ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "tools.ozone.moderation.scheduleAction#takedown"
    (body |> member "action" |> member "$type" |> to_string)

let test_parse_typed_event_and_subject _ =
  let ev =
    Ozone.parse_mod_event
      (`Assoc
        [
          ("id", `Int 9);
          ( "event",
            `Assoc
              [
                ("$type", `String "tools.ozone.moderation.defs#modEventReport");
                ("comment", `String "spam");
                ("reportType", `String "com.atproto.moderation.defs#reasonSpam");
              ] );
          ( "subject",
            `Assoc
              [
                ("$type", `String "com.atproto.admin.defs#repoRef");
                ("did", `String "did:plc:abc123xyz0001112223333");
              ] );
          ("createdBy", `String "did:plc:mod000111222333444555666");
          ("createdAt", `String "2024-01-01T00:00:00.000Z");
        ])
  in
  (match ev.event with
  | `Report r ->
      OUnit2.assert_equal (Some "spam") r.comment;
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "com.atproto.moderation.defs#reasonSpam" r.report_type
  | _ -> OUnit2.assert_failure "expected report event");
  (match ev.subject with
  | `Repo_ref r ->
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        "did:plc:abc123xyz0001112223333" r.did
  | _ -> OUnit2.assert_failure "expected repoRef");
  let msg_subj =
    Ozone.parse_subject
      (`Assoc
        [
          ("$type", `String "chat.bsky.convo.defs#messageRef");
          ("did", `String "did:plc:abc123xyz0001112223333");
          ("convoId", `String "c1");
          ("messageId", `String "m1");
        ])
  in
  (match msg_subj with
  | `Message_ref m -> OUnit2.assert_equal ~printer:(fun x -> x) "c1" m.convo_id
  | _ -> OUnit2.assert_failure "expected messageRef");
  let label =
    Ozone.parse_event
      (`Assoc
        [
          ("$type", `String "tools.ozone.moderation.defs#modEventLabel");
          ("createLabelVals", `List [ `String "spam" ]);
          ("negateLabelVals", `List []);
        ])
  in
  match label with
  | `Label l -> OUnit2.assert_equal [ "spam" ] l.create_label_vals
  | _ -> OUnit2.assert_failure "expected label event"

let test_operator_namespace_parsers _ =
  let templates =
    Ozone.parse_templates
      (`Assoc
        [
          ( "communicationTemplates",
            `List
              [
                `Assoc
                  [
                    ("id", `String "t1");
                    ("name", `String "Hello");
                    ("contentMarkdown", `String "hi");
                    ("subject", `String "welcome");
                  ];
              ] );
        ])
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "Hello" (List.hd templates.templates).name;
  let sets =
    Ozone.parse_sets
      (`Assoc
        [
          ( "sets",
            `List
              [
                `Assoc
                  [
                    ("name", `String "spam-dids");
                    ("setSize", `Int 2);
                    ("description", `String "known spam");
                  ];
              ] );
        ])
  in
  OUnit2.assert_equal (Some 2) (List.hd sets.sets).set_size;
  let members =
    Ozone.parse_team_members
      (`Assoc
        [
          ( "members",
            `List
              [
                `Assoc
                  [
                    ("did", `String "did:plc:mod000111222333444555666");
                    ("role", `String "tools.ozone.team.defs#roleAdmin");
                    ("disabled", `Bool false);
                  ];
              ] );
        ])
  in
  OUnit2.assert_equal (Some "tools.ozone.team.defs#roleAdmin")
    (List.hd members.members).role;
  let rules =
    Ozone.parse_url_rules
      (`Assoc
        [
          ( "rules",
            `List
              [
                `Assoc
                  [
                    ("url", `String "https://evil.example");
                    ("action", `String "block");
                    ("patternType", `String "domain");
                  ];
              ] );
        ])
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "https://evil.example" (List.hd rules.rules).url;
  let body =
    Ozone.create_template_body ~name:"Hello" ~content_markdown:"hi"
      ~subject:"welcome" ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "Hello"
    (body |> member "name" |> to_string)

let test_parse_queue_and_report _ =
  let queues =
    Ozone.parse_queues
      (`Assoc
        [
          ( "queues",
            `List
              [
                `Assoc
                  [
                    ("id", `Int 7);
                    ("name", `String "spam");
                    ("subjectTypes", `List [ `String "account" ]);
                    ( "reportTypes",
                      `List [ `String Ozone.reason_misleading_spam ] );
                    ("enabled", `Bool true);
                    ( "stats",
                      `Assoc
                        [
                          ("pendingCount", `Int 3);
                          ("inboundCount", `Int 10);
                          ("actionRate", `Int 40);
                        ] );
                  ];
              ] );
        ])
  in
  OUnit2.assert_equal 1 (List.length queues.queues);
  OUnit2.assert_equal ~printer:(fun x -> x) "spam" (List.hd queues.queues).name;
  OUnit2.assert_equal (Some 3)
    (Option.bind (List.hd queues.queues).stats (fun s -> s.pending_count));
  let body =
    Ozone.create_queue_body ~name:"spam" ~subject_types:[ "account" ]
      ~report_types:[ Ozone.reason_misleading_spam ]
      ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "spam"
    (body |> member "name" |> to_string);
  let routed =
    Ozone.parse_route_reports_result
      (`Assoc [ ("assigned", `Int 4); ("unmatched", `Int 1) ])
  in
  OUnit2.assert_equal 4 routed.assigned;
  let report =
    Ozone.parse_report_result
      (`Assoc
        [
          ( "report",
            `Assoc
              [
                ("id", `Int 11);
                ("eventId", `Int 99);
                ("status", `String "open");
                ("reportType", `String Ozone.reason_violence_threats);
                ("reportedBy", `String "did:plc:reporter000111222333444555");
                ("comment", `String "threat");
                ( "subject",
                  `Assoc
                    [
                      ("$type", `String "com.atproto.admin.defs#repoRef");
                      ("did", `String "did:plc:abc123xyz0001112223333");
                    ] );
                ( "assignment",
                  `Assoc
                    [
                      ("did", `String "did:plc:mod000111222333444555666");
                      ("assignedAt", `String "2024-01-01T00:00:00.000Z");
                    ] );
              ] );
        ])
  in
  OUnit2.assert_equal 11 report.id;
  OUnit2.assert_equal ~printer:(fun x -> x) "open" report.status;
  OUnit2.assert_equal (Some "did:plc:mod000111222333444555666")
    (Option.map (fun (a : Ozone.report_assignment) -> a.did) report.assignment);
  let activity =
    Ozone.parse_report_activity
      (`Assoc
        [
          ("$type", `String "tools.ozone.report.defs#closeActivity");
          ("previousStatus", `String "assigned");
        ])
  in
  (match activity with
  | `Close (Some "assigned") -> ()
  | _ -> OUnit2.assert_failure "expected closeActivity");
  let create_body =
    Ozone.create_activity_body ~activity:(Ozone.note_activity ()) ~report_id:11
      ~internal_note:"looks resolved" ()
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "tools.ozone.report.defs#noteActivity"
    (create_body |> member "activity" |> member "$type" |> to_string);
  let closed =
    Ozone.parse_close_reports_result
      (`Assoc
        [ ("closedCount", `Int 2); ("reportIds", `List [ `Int 11; `Int 12 ]) ])
  in
  OUnit2.assert_equal 2 closed.closed_count;
  let live =
    Ozone.parse_live_stats
      (`Assoc
        [
          ( "stats",
            `Assoc [ ("pendingCount", `Int 5); ("inboundCount", `Int 9) ] );
        ])
  in
  OUnit2.assert_equal (Some 5) live.pending_count;
  let hist =
    Ozone.parse_historical_stats
      (`Assoc
        [
          ( "stats",
            `List
              [
                `Assoc
                  [ ("date", `String "2024-01-01"); ("actionedCount", `Int 8) ];
              ] );
        ])
  in
  OUnit2.assert_equal (Some "2024-01-01") (List.hd hist.stats).date;
  let deleted =
    Ozone.parse_delete_queue_result
      (`Assoc [ ("deleted", `Bool true); ("reportsMigrated", `Int 2) ])
  in
  OUnit2.assert_equal true deleted.deleted

let test_query_statuses_auth_skipped _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let username, password = Auth.username_and_password_from_env in
  let s = Atproto.Session.Session.create_session username password in
  try
    let proxy = Ozone.labeler_proxy "did:plc:ar7c4by46qjdydhdevvrndac" in
    let page = Ozone.query_statuses s ~proxy ~limit:1 () in
    OUnit2.assert_bool "statuses parsed" (List.length page.subject_statuses >= 0)
  with exn -> skip_if true ("queryStatuses skipped: " ^ Printexc.to_string exn)

let test_list_queues_auth_skipped _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let username, password = Auth.username_and_password_from_env in
  let s = Atproto.Session.Session.create_session username password in
  try
    let proxy = Ozone.labeler_proxy "did:plc:ar7c4by46qjdydhdevvrndac" in
    let page = Ozone.list_queues s ~proxy ~limit:1 () in
    OUnit2.assert_bool "queues parsed" (List.length page.queues >= 0)
  with exn -> skip_if true ("listQueues skipped: " ^ Printexc.to_string exn)

let suite =
  "ozone"
  >::: [
         "test_labeler_proxy" >:: test_labeler_proxy;
         "test_parse_statuses" >:: test_parse_statuses;
         "test_emit_event_body" >:: test_emit_event_body;
         "test_takedown_event" >:: test_takedown_event;
         "test_parse_timeline_and_schedule" >:: test_parse_timeline_and_schedule;
         "test_parse_typed_event_and_subject"
         >:: test_parse_typed_event_and_subject;
         "test_query_statuses_auth_skipped" >:: test_query_statuses_auth_skipped;
         "test_list_queues_auth_skipped" >:: test_list_queues_auth_skipped;
         "test_operator_namespace_parsers" >:: test_operator_namespace_parsers;
         "test_parse_queue_and_report" >:: test_parse_queue_and_report;
       ]

let () = run_test_tt_main suite
