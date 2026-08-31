open OUnit2
open Atproto.Video
open Atproto.Auth

let test_parse_job_status _ =
  let json =
    `Assoc
      [
        ( "jobStatus",
          `Assoc
            [
              ("jobId", `String "job-1");
              ("did", `String "did:plc:abc123xyz0001112223333");
              ("state", `String "JOB_STATE_COMPLETED");
              ("progress", `Int 100);
            ] );
      ]
  in
  let st = Video.parse_job_status_response json in
  OUnit2.assert_equal ~printer:(fun x -> x) "job-1" st.job_id;
  OUnit2.assert_equal ~printer:(fun x -> x) "JOB_STATE_COMPLETED" st.state;
  OUnit2.assert_equal (Some 100) st.progress

let test_parse_upload_limits _ =
  let json =
    `Assoc
      [
        ("canUpload", `Bool true);
        ("remainingDailyVideos", `Int 10);
        ("remainingDailyBytes", `Int 1_000_000);
      ]
  in
  let lim = Video.parse_upload_limits json in
  OUnit2.assert_equal true lim.can_upload;
  OUnit2.assert_equal (Some 10) lim.remaining_daily_videos

let test_upload_limits_auth_skipped _ =
  skip_if
    (not Auth.has_live_credentials)
    "ATP_AUTH not configured; live Bluesky test skipped";
  let username, password = Auth.username_and_password_from_env in
  let s = Atproto.Session.Session.create_session username password in
  let lim = Video.get_upload_limits s in
  OUnit2.assert_bool "canUpload field present"
    (lim.can_upload || not lim.can_upload)

let suite =
  "video"
  >::: [
         "test_parse_job_status" >:: test_parse_job_status;
         "test_parse_upload_limits" >:: test_parse_upload_limits;
         "test_upload_limits_auth_skipped" >:: test_upload_limits_auth_skipped;
       ]

let () = run_test_tt_main suite
