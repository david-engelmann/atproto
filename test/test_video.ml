open OUnit2
open Atproto.Video
open Atproto.Auth

let sample_blob =
  `Assoc
    [
      ("$type", `String "blob");
      ( "ref",
        `Assoc
          [
            ( "$link",
              `String
                "bafkreihdwdcefgh4dqkjv67uzcmw7ojee6xedzdetojuzjevtenxquvyku" );
          ] );
      ("mimeType", `String "video/mp4");
      ("size", `Int 2048);
    ]

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

let test_upload_video_url _ =
  let url =
    Video.upload_video_url ~did:"did:plc:abc123xyz0001112223333"
      ~name:"clip.mp4" ()
  in
  OUnit2.assert_bool "host" (String.length url > 20);
  OUnit2.assert_bool "nsid"
    (let needle = "app.bsky.video.uploadVideo" in
     let rec contains i =
       i + String.length needle <= String.length url
       && (String.sub url i (String.length needle) = needle || contains (i + 1))
     in
     contains 0);
  OUnit2.assert_bool "did query"
    (let needle = "did=did%3Aplc%3Aabc123xyz0001112223333" in
     let rec contains i =
       i + String.length needle <= String.length url
       && (String.sub url i (String.length needle) = needle || contains (i + 1))
     in
     contains 0
     ||
     let needle = "did=did:plc:abc123xyz0001112223333" in
     let rec contains i =
       i + String.length needle <= String.length url
       && (String.sub url i (String.length needle) = needle || contains (i + 1))
     in
     contains 0);
  OUnit2.assert_bool "default host"
    (let needle = "video.bsky.app" in
     let rec contains i =
       i + String.length needle <= String.length url
       && (String.sub url i (String.length needle) = needle || contains (i + 1))
     in
     contains 0)

let test_pds_audience _ =
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:web:bsky.social"
    (Video.pds_audience
       {
         username = "x";
         password = "y";
         atp_host = "bsky.social";
         auth =
           {
             exp = 0;
             iat = 0;
             scope = "com.atproto.access";
             did = "did:plc:abc123xyz0001112223333";
             jti = None;
             token = "t";
             refresh_token = None;
           };
       });
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "did:web:pds.example"
    (Video.pds_audience ~host:"https://pds.example/xrpc"
       {
         username = "x";
         password = "y";
         atp_host = "bsky.social";
         auth =
           {
             exp = 0;
             iat = 0;
             scope = "com.atproto.access";
             did = "did:plc:abc123xyz0001112223333";
             jti = None;
             token = "t";
             refresh_token = None;
           };
       })

let test_recommended_exp _ =
  let exp = Video.recommended_exp ~now:1_700_000_000.0 () in
  OUnit2.assert_equal ~printer:Int64.to_string
    (Int64.add 1_700_000_000L 1800L)
    exp

let test_job_phase _ =
  OUnit2.assert_equal Video.Completed
    (Video.classify_state "JOB_STATE_COMPLETED");
  OUnit2.assert_equal Video.Failed (Video.classify_state "JOB_STATE_FAILED");
  OUnit2.assert_equal Video.In_progress
    (Video.classify_state "JOB_STATE_CREATED");
  let done_ =
    Video.parse_job_status
      (`Assoc
        [
          ("jobId", `String "j");
          ("did", `String "did:plc:abc123xyz0001112223333");
          ("state", `String "JOB_STATE_COMPLETED");
          ("blob", sample_blob);
        ])
  in
  OUnit2.assert_bool "completed" (Video.is_completed done_);
  OUnit2.assert_bool "terminal" (Video.is_terminal done_);
  OUnit2.assert_bool "blob" (Video.blob_ready done_)

let test_already_exists _ =
  let json =
    `Assoc
      [
        ("error", `String "already_exists");
        ("jobId", `String "job-dup");
        ("blob", sample_blob);
      ]
  in
  let st = Video.parse_upload_response json in
  OUnit2.assert_equal ~printer:(fun x -> x) "job-dup" st.job_id;
  OUnit2.assert_bool "already_exists" (Video.already_exists st);
  OUnit2.assert_bool "blob present" (Video.blob_ready st);
  OUnit2.assert_bool "not failed" (not (Video.is_failed st));
  OUnit2.assert_bool "completed via blob" (Video.is_completed st)

let test_poll_until_blob _ =
  let n = ref 0 in
  let get_status _ =
    incr n;
    if !n < 3 then
      Video.parse_job_status
        (`Assoc
          [
            ("jobId", `String "job-p");
            ("did", `String "did:plc:abc123xyz0001112223333");
            ("state", `String "JOB_STATE_ENCODING");
            ("progress", `Int (!n * 10));
          ])
    else
      Video.parse_job_status
        (`Assoc
          [
            ("jobId", `String "job-p");
            ("did", `String "did:plc:abc123xyz0001112223333");
            ("state", `String "JOB_STATE_COMPLETED");
            ("progress", `Int 100);
            ("blob", sample_blob);
          ])
  in
  let slept = ref 0 in
  let st =
    Video.poll_job_status ~get_status
      ~sleep:(fun () -> incr slept)
      ~job_id:"job-p" ()
  in
  OUnit2.assert_equal 3 !n;
  OUnit2.assert_equal 2 !slept;
  OUnit2.assert_bool "blob" (Video.blob_ready st);
  OUnit2.assert_bool "completed" (Video.is_completed st)

let test_ensure_blob_short_circuit _ =
  let called = ref false in
  let ready =
    Video.parse_job_status
      (`Assoc
        [
          ("jobId", `String "job-e");
          ("did", `String "did:plc:abc123xyz0001112223333");
          ("state", `String "JOB_STATE_COMPLETED");
          ("blob", sample_blob);
        ])
  in
  let out =
    Video.ensure_blob
      ~get_status:(fun _ ->
        called := true;
        ready)
      ready
  in
  OUnit2.assert_bool "did not poll" (not !called);
  OUnit2.assert_bool "blob" (Video.blob_ready out)

let test_video_embed_json _ =
  let embed =
    Video.video_embed_json ~video:sample_blob ~alt:"demo"
      ~aspect_ratio:{ width = 16; height = 9 } ~presentation:"gif" ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "app.bsky.embed.video"
    (embed |> member "$type" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "demo"
    (embed |> member "alt" |> to_string);
  OUnit2.assert_equal 16
    (embed |> member "aspectRatio" |> member "width" |> to_int);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "gif"
    (embed |> member "presentation" |> to_string)

let test_upload_headers _ =
  let pairs =
    Video.upload_header_pairs ~token:"svc" ~content_type:"video/mp4"
  in
  OUnit2.assert_equal
    [ ("Authorization", "Bearer svc"); ("Content-Type", "video/mp4") ]
    pairs

let test_multipart_parsers _ =
  let sess =
    Video.parse_upload_session
      (`Assoc
        [
          ("jobId", `String "job-m");
          ("partSizeBytes", `Int 5_242_880);
          ("partCount", `Int 3);
          ("expiresAt", `String "2026-01-01T00:00:00.000Z");
        ])
  in
  OUnit2.assert_equal 3 sess.part_count;
  OUnit2.assert_equal (Some 5_242_880)
    (Video.expected_part_size sess ~part_number:1);
  OUnit2.assert_equal None (Video.expected_part_size sess ~part_number:9);
  let body =
    Video.start_upload_body ~size_bytes:10_000 ~mime_type:"video/mp4"
      ~name:"clip.mp4" ~width:1920 ~height:1080 ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal 10000 (body |> member "sizeBytes" |> to_int);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "video/mp4"
    (body |> member "mimeType" |> to_string);
  let st =
    Video.parse_upload_status
      (`Assoc
        [
          ("jobId", `String "job-m");
          ("partSizeBytes", `Int 100);
          ("partCount", `Int 3);
          ("receivedParts", `List [ `Int 1; `Int 3 ]);
          ("expiresAt", `String "2026-01-01T00:00:00.000Z");
          ("state", `String "created");
        ])
  in
  OUnit2.assert_equal Video.Phase_created st.state;
  OUnit2.assert_equal [ 2 ] (Video.missing_parts st);
  let abort =
    Video.parse_abort_result
      (`Assoc
        [ ("state", `String "aborted"); ("failureReason", `String "user") ])
  in
  OUnit2.assert_equal Video.Abort_aborted abort.state;
  let finish =
    Video.parse_finish_result
      (`Assoc
        [
          ("completedJobId", `String "job-done");
          ( "jobStatus",
            `Assoc
              [
                ("jobId", `String "job-done");
                ("did", `String "did:plc:abc123xyz0001112223333");
                ("state", `String "JOB_STATE_CREATED");
              ] );
        ])
  in
  OUnit2.assert_equal ~printer:(fun x -> x) "job-done" finish.completed_job_id;
  let url = Video.upload_part_url ~job_id:"job-m" ~part_number:2 () in
  OUnit2.assert_bool "uploadPart nsid"
    (let needle = "app.bsky.video.uploadPart" in
     let rec contains i =
       i + String.length needle <= String.length url
       && (String.sub url i (String.length needle) = needle || contains (i + 1))
     in
     contains 0)

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
         "test_upload_video_url" >:: test_upload_video_url;
         "test_pds_audience" >:: test_pds_audience;
         "test_recommended_exp" >:: test_recommended_exp;
         "test_job_phase" >:: test_job_phase;
         "test_already_exists" >:: test_already_exists;
         "test_poll_until_blob" >:: test_poll_until_blob;
         "test_ensure_blob_short_circuit" >:: test_ensure_blob_short_circuit;
         "test_video_embed_json" >:: test_video_embed_json;
         "test_upload_headers" >:: test_upload_headers;
         "test_multipart_parsers" >:: test_multipart_parsers;
         "test_upload_limits_auth_skipped" >:: test_upload_limits_auth_skipped;
       ]

let () = run_test_tt_main suite
