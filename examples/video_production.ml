(* Offline sketch: wire a production app.bsky.video client against
   hosted Bluesky video (video.bsky.app). This is not a transcoder.
   Official @atproto/dev-env 0.6.4 TestNetwork does not start one.
   No network is used here.

   After a password session or OAuth browser login:

     1. Audience is did:web:<pds-host> from the session DID document
        [#atproto_pds] (entryway ATP_HOST is often bsky.social).
        [lxm] is [Video.upload_blob_lxm] (com.atproto.repo.uploadBlob),
        not app.bsky.video.uploadVideo. ~30 min [recommended_exp].
     2. Password sessions: [Video.mint_upload_token].
        OAuth DPoP: [Oauth.get_service_auth] with the same aud / lxm / exp.
     3. Optionally [Video.get_upload_limits_service] on
        [Video.default_host] / [ATP_VIDEO_HOST] with that JWT.
     4. Single POST [Video.upload_video], or multipart
        [start_upload] / [upload_part] / [finish_upload] + [part_slice].
     5. Poll [Video.poll_job_status] / [ensure_blob] until a blob ref
        is present. [already_exists] is not a hard failure when a blob
        is present.
     6. Embed the blob ([Video.video_embed_json] / [embed_of_job]) on
        [Records.post]. The create embed is the blob ref, not the
        later app.bsky.embed.video#view playlist.
*)

open Atproto.Video
open Atproto.Records
open Atproto.Server

let example_did = "did:plc:abc123xyz0001112223333"

let dummy_session ?(atp_host = "bsky.social") ?(did_doc = None) () :
    Atproto.Session.Session.session =
  {
    username = "x";
    password = "y";
    atp_host;
    auth =
      {
        exp = 0;
        iat = 0;
        scope = "com.atproto.access";
        did = example_did;
        jti = None;
        token = "t";
        refresh_token = None;
      };
    did_doc;
  }

let () =
  assert (Video.default_host = "video.bsky.app");
  assert (Video.upload_blob_lxm = "com.atproto.repo.uploadBlob");
  assert (Video.upload_nsid = "app.bsky.video.uploadVideo");
  assert (Video.recommended_exp_seconds = 1800L);
  assert (Video.pds_audience (dummy_session ()) = "did:web:bsky.social");
  let did_doc =
    `Assoc
      [
        ("id", `String example_did);
        ( "service",
          `List
            [
              `Assoc
                [
                  ("id", `String "#atproto_pds");
                  ("type", `String "AtprotoPersonalDataServer");
                  ("serviceEndpoint", `String "https://pds.example.com");
                ];
            ] );
      ]
  in
  let s = dummy_session ~did_doc:(Some did_doc) () in
  assert (Video.pds_audience s = "did:web:pds.example.com");
  assert (
    Video.pds_audience ~host:"https://pds.override/xrpc" s
    = "did:web:pds.override");
  let exp = Video.recommended_exp ~now:1_700_000_000.0 () in
  let auth = Video.upload_service_auth_body ~exp s in
  assert (List.assoc "aud" auth = "did:web:pds.example.com");
  assert (List.assoc "lxm" auth = Video.upload_blob_lxm);
  assert (List.assoc "exp" auth = "1700001800");
  assert (
    Server.get_service_auth_body ~aud:(Video.pds_audience s)
      ~lxm:Video.upload_blob_lxm ~exp ()
    = auth);
  let upload = Video.upload_video_body ~did:example_did ~name:"clip.mp4" () in
  assert (List.assoc "did" upload = example_did);
  assert (List.assoc "name" upload = "clip.mp4");
  let url = Video.upload_video_url ~did:example_did ~name:"clip.mp4" () in
  assert (
    let needle = "video.bsky.app" in
    let rec contains i =
      i + String.length needle <= String.length url
      && (String.sub url i (String.length needle) = needle || contains (i + 1))
    in
    contains 0);
  assert (Video.get_job_status_body ~job_id:"job-1" () = [ ("jobId", "job-1") ]);
  assert (
    Video.get_upload_status_body ~job_id:"job-m" () = [ ("jobId", "job-m") ]);
  let blob =
    `Assoc
      [
        ("$type", `String "blob");
        ("ref", `Assoc [ ("$link", `String "bafyvideo") ]);
        ("mimeType", `String "video/mp4");
        ("size", `Int 2048);
      ]
  in
  let st =
    Video.parse_job_status
      (`Assoc
        [
          ("jobId", `String "job-1");
          ("did", `String example_did);
          ("state", `String "JOB_STATE_COMPLETED");
          ("blob", blob);
        ])
  in
  assert (Video.blob_ready st);
  assert (Video.is_completed st);
  assert (not (Video.is_failed st));
  let embed = Video.video_embed_json ~video:blob ~alt:"demo" () in
  assert (
    match Yojson.Safe.Util.member "$type" embed with
    | `String "app.bsky.embed.video" -> true
    | _ -> false);
  (match Video.embed_of_job ~alt:"demo" st with
  | Some (`Video _) -> ()
  | _ -> assert false);
  let post =
    Records.post ~text:"clip" ~created_at:"2024-01-01T00:00:00.000Z"
      ~embed:(Video.embed_of_blob ~alt:"demo" blob)
      ()
  in
  assert (
    match
      Yojson.Safe.Util.member "$type" (Yojson.Safe.Util.member "embed" post)
    with
    | `String "app.bsky.embed.video" -> true
    | _ -> false);
  let start =
    Video.start_upload_body ~size_bytes:10_485_761 ~mime_type:"video/mp4"
      ~name:"clip.mp4" ()
  in
  assert (
    match Yojson.Safe.Util.member "sizeBytes" start with
    | `Int 10485761 -> true
    | _ -> false);
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
  assert (
    Video.part_slice ~total_bytes:10_485_761 sess ~part_number:3
    = Some (10_485_760, 1));
  let dup =
    Video.parse_upload_response
      (`Assoc
        [
          ("error", `String "already_exists");
          ("jobId", `String "job-dup");
          ("blob", blob);
        ])
  in
  assert (Video.already_exists dup);
  assert (Video.blob_ready dup);
  print_endline
    "video_production: hosted video.bsky production wiring sketch ok"
