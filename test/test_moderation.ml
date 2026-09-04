open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Moderation

let sample_strong_ref : Moderation.strong_ref =
  {
    uri =
      "at://did:plc:xov3uvxfd4to6ev3ak5g5uxk/app.bsky.feed.post/3jys3bxu3bt2m";
    cid = "bafyreihikeyzp2bd7k4zeywtcxbate7rhx4bkkcrzjlweisiejl5lypom4";
  }

let create_test_session _ =
  let username, password = Auth.username_and_password_from_env in
  Session.create_session username password

let test_report_bodies _ =
  let body =
    Moderation.create_report_body_from_strong_ref
      "com.atproto.moderation.defs#reasonSpam" ~reason:"bots" sample_strong_ref
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "com.atproto.moderation.defs#reasonSpam"
    (body |> member "reasonType" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "bots"
    (body |> member "reason" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "com.atproto.repo.strongRef"
    (body |> member "subject" |> member "$type" |> to_string);
  let data =
    Moderation.create_report_data_from_strong_ref
      "com.atproto.moderation.defs#reasonSpam" ~reason:"bots" sample_strong_ref
  in
  OUnit2.assert_equal ~printer:(fun x -> x) (Yojson.Safe.to_string body) data;
  let repo =
    Moderation.create_report_body_from_repo_ref
      "com.atproto.moderation.defs#reasonOther"
      { did = "did:plc:abc123xyz0001112223333" }
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "com.atproto.moderation.defs#reasonOther"
    (repo |> member "reasonType" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "com.atproto.admin.defs#repoRef"
    (repo |> member "subject" |> member "$type" |> to_string);
  let repo_data =
    Moderation.create_report_data_from_repo_ref
      "com.atproto.moderation.defs#reasonOther"
      { did = "did:plc:abc123xyz0001112223333" }
  in
  OUnit2.assert_equal ~printer:(fun x -> x) (Yojson.Safe.to_string repo)
    repo_data;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    Moderation.reason_spam "com.atproto.moderation.defs#reasonSpam";
  let with_tool =
    Moderation.create_report_body_from_strong_ref Moderation.reason_other
      ~reason:"context"
      ~mod_tool:{ name = "atproto-ocaml/test"; meta = None }
      sample_strong_ref
  in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "atproto-ocaml/test"
    (with_tool |> member "modTool" |> member "name" |> to_string)

let test_parse_report_response _ =
  let json =
    `Assoc
      [
        ("id", `Int 7);
        ("createdAt", `String "2024-01-01T00:00:00.000Z");
        ("reasonType", `String "com.atproto.moderation.defs#reasonSpam");
        ("reportedBy", `String "did:plc:abc123xyz0001112223333");
        ("subject", `Assoc [ ("did", `String "did:plc:abc123xyz0001112223333") ]);
      ]
  in
  let r = Moderation.parse_report_response json in
  OUnit2.assert_equal 7 r.id;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:plc:abc123xyz0001112223333" r.reported_by

let test_create_report_no_reason _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let created_report =
    Moderation.create_report_with_strong_ref test_session
      "com.atproto.moderation.defs#reasonOther" sample_strong_ref
  in
  match created_report with
  | { reported_by; _ } ->
      OUnit2.assert_equal "did:plc:xov3uvxfd4to6ev3ak5g5uxk" reported_by

let suite =
  "suite"
  >::: [
         "test_report_bodies" >:: test_report_bodies;
         "test_parse_report_response" >:: test_parse_report_response;
         "test_create_report_no_reason" >:: test_create_report_no_reason;
       ]

let () = run_test_tt_main suite
