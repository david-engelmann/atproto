open Session
open Client

(** app.bsky.video — upload limits, raw-byte upload, and processing job status.

    This is a client for the hosted video service (default [video.bsky.app]).
    It does not run a transcoding service. The recommended flow is:

    1. mint a PDS-scoped service-auth token ([pds_audience] +
       [upload_blob_lxm], ~30 min [recommended_exp])
    2. POST the bytes to [upload_video]
    3. poll [get_job_status] until a blob ref is present
    4. embed the blob with [video_embed_json] *)
module Video = struct
  (** Hosted video service hostname (["video.bsky.app"]). *)
  let default_host = "video.bsky.app"

  (** NSID for raw-byte upload ([app.bsky.video.uploadVideo]). *)
  let upload_nsid = "app.bsky.video.uploadVideo"

  (** Service-auth [lxm] for video upload tokens
      ([com.atproto.repo.uploadBlob]). *)
  let upload_blob_lxm = "com.atproto.repo.uploadBlob"

  (** Recommended service-auth lifetime for video upload (1800s). *)
  let recommended_exp_seconds = 1800L

  type job_status = {
    job_id : string;
    did : string;
    state : string;
    progress : int option;
    error : string option;
    failure_code : string option;
    message : string option;
    blob : Yojson.Safe.t option;
  }

  type upload_limits = {
    can_upload : bool;
    remaining_daily_videos : int option;
    remaining_daily_bytes : int option;
    message : string option;
    error : string option;
  }

  type job_phase = Completed | Failed | In_progress
  type aspect_ratio = { width : int; height : int }

  let host_of_endpoint (url : string) : string =
    let strip prefix =
      let plen = String.length prefix in
      if String.length url >= plen && String.sub url 0 plen = prefix then
        String.sub url plen (String.length url - plen)
      else url
    in
    let rest =
      let after_https = strip "https://" in
      if after_https = url then strip "http://" else after_https
    in
    match String.index_opt rest '/' with
    | None -> rest
    | Some i -> String.sub rest 0 i

  (** PDS service-auth audience ([did:web:<pds-host>]). Optional [host]
      overrides [s.atp_host]. *)
  let pds_audience ?host (s : Session.session) : string =
    let raw = match host with Some h -> h | None -> s.Session.atp_host in
    "did:web:" ^ host_of_endpoint raw

  (** Unix expiry for a video upload token ([now] plus
      [recommended_exp_seconds]). *)
  let recommended_exp ?(now = Unix.gettimeofday ()) () : int64 =
    Int64.add (Int64.of_float now) recommended_exp_seconds

  (** Hosted video hostname ([host], or [default_host]). *)
  let video_host ?host () = Option.value host ~default:default_host

  let parse_job_status json : job_status =
    {
      job_id = Client.string_member json "jobId";
      did = Client.string_member json "did";
      state = Client.string_member json "state";
      progress = Client.int_opt json "progress";
      error = Client.string_opt json "error";
      failure_code = Client.string_opt json "failureCode";
      message = Client.string_opt json "message";
      blob =
        (match Yojson.Safe.Util.member "blob" json with
        | `Null -> None
        | other -> Some other);
    }

  let parse_job_status_response json : job_status =
    match Yojson.Safe.Util.member "jobStatus" json with
    | `Assoc _ as inner -> parse_job_status inner
    | _ -> parse_job_status json

  let parse_upload_limits json : upload_limits =
    {
      can_upload = Client.bool_member json "canUpload";
      remaining_daily_videos = Client.int_opt json "remainingDailyVideos";
      remaining_daily_bytes = Client.int_opt json "remainingDailyBytes";
      message = Client.string_opt json "message";
      error = Client.string_opt json "error";
    }

  (** Map a job [state] string to [Completed] / [Failed] / [In_progress]. *)
  let classify_state (state : string) : job_phase =
    let upper =
      String.map
        (function 'a' .. 'z' as c -> Char.chr (Char.code c - 32) | c -> c)
        state
    in
    if upper = "JOB_STATE_COMPLETED" || upper = "COMPLETED" then Completed
    else if upper = "JOB_STATE_FAILED" || upper = "FAILED" then Failed
    else In_progress

  (** True when [st] already carries a blob ref. *)
  let blob_ready (st : job_status) : bool = Option.is_some st.blob

  (* already_exists is not a hard failure — the tutorial notes the job
     may still carry a blob ref that can be embedded. *)

  (** True when [st] is [Completed] or already carries a blob ref. *)
  let is_completed (st : job_status) : bool =
    classify_state st.state = Completed || blob_ready st

  (** True when [st] is [Failed] and has no blob ref. *)
  let is_failed (st : job_status) : bool =
    classify_state st.state = Failed && not (blob_ready st)

  (** True when [st] is completed, failed, or already carries a blob
      ref. *)
  let is_terminal (st : job_status) : bool =
    blob_ready st
    ||
    match classify_state st.state with
    | Completed | Failed -> true
    | In_progress -> false

  (** True when [st.error] is [already_exists] (not a hard failure;
      the job may still carry a blob ref). *)
  let already_exists (st : job_status) : bool =
    match st.error with
    | Some e ->
        let upper =
          String.map
            (function 'a' .. 'z' as c -> Char.chr (Char.code c - 32) | c -> c)
            e
        in
        upper = "ALREADY_EXISTS"
    | None -> false

  let parse_upload_response json : job_status =
    let st = parse_job_status_response json in
    (* already_exists may put jobId / blob on the error object itself. *)
    if st.job_id = "" then
      match Yojson.Safe.Util.member "jobId" json with
      | `String id -> { st with job_id = id }
      | _ -> st
    else st

  (** Job status via [app.bsky.video.getJobStatus]. Hosted video
      service (default [video.bsky.app]); client poll only. *)
  let get_job_status ?session ?host ~job_id () : job_status =
    Client.get_json ?session ~host:(video_host ?host ())
      "app.bsky.video.getJobStatus"
      [ ("jobId", job_id) ]
    |> parse_job_status_response

  (** Daily upload limits via [app.bsky.video.getUploadLimits]. *)
  let get_upload_limits (s : Session.session) : upload_limits =
    Client.get_json ~session:s "app.bsky.video.getUploadLimits" []
    |> parse_upload_limits

  (** XRPC URL for [app.bsky.video.uploadVideo] on the hosted video
      service (default [video.bsky.app]). Query params are [did] and
      [name]. Client URL helper only — this is not a local transcoder. *)
  let upload_video_url ?host ~did ~name () =
    let base = Client.nsid_url ~host:(video_host ?host ()) upload_nsid in
    let qs =
      Cohttp_client.Cohttp_client.create_body_from_pairs
        [ ("did", did); ("name", name) ]
    in
    if qs = "" then base else base ^ "?" ^ qs

  let upload_header_pairs ~token ~content_type =
    [ ("Authorization", "Bearer " ^ token); ("Content-Type", content_type) ]

  (** POST bytes to [app.bsky.video.uploadVideo] with a service-auth
      [token]. Client upload only — this is not a local transcoder. *)
  let upload_video ?host ~token ~did ~name ?(content_type = "video/mp4")
      (bytes : string) : job_status =
    let url = upload_video_url ?host ~did ~name () in
    let headers =
      Cohttp_client.Cohttp_client.create_headers_from_pairs
        (upload_header_pairs ~token ~content_type)
    in
    let body =
      Lwt_main.run
        (Cohttp_client.Cohttp_client.post_data_with_headers url bytes headers)
    in
    parse_upload_response (Yojson.Safe.from_string body)

  (** Poll [get_job_status] until a blob ref or terminal state.
      Optional [get_status] / [sleep] are injectable. *)
  let poll_job_status ?(get_status : (string -> job_status) option)
      ?(sleep : (unit -> unit) option) ?session ?host ~job_id
      ?(max_attempts = 60) () : job_status =
    let get =
      match get_status with
      | Some f -> f
      | None -> fun id -> get_job_status ?session ?host ~job_id:id ()
    in
    let rest =
      match sleep with Some f -> f | None -> fun () -> Unix.sleepf 1.0
    in
    let rec loop attempt =
      let st = get job_id in
      if blob_ready st || is_terminal st || attempt >= max_attempts then st
      else (
        rest ();
        loop (attempt + 1))
    in
    loop 1

  (** Return [st] if it already has a blob ref; otherwise poll
      [st.job_id] via [poll_job_status]. *)
  let ensure_blob ?get_status ?sleep ?session ?host (st : job_status) :
      job_status =
    if blob_ready st then st
    else if st.job_id = "" then st
    else poll_job_status ?get_status ?sleep ?session ?host ~job_id:st.job_id ()

  (** Mint a PDS-scoped service-auth JWT ([pds_audience] +
      [upload_blob_lxm], [recommended_exp]). *)
  let mint_upload_token (s : Session.session) ?pds_host ?exp () : string =
    let aud = pds_audience ?host:pds_host s in
    let exp = match exp with Some e -> e | None -> recommended_exp () in
    (Server.Server.get_service_auth s ~aud ~lxm:upload_blob_lxm ~exp ()).token

  (** JSON [width] / [height] for [app.bsky.embed.video] aspectRatio. *)
  let aspect_ratio_json (ar : aspect_ratio) : Yojson.Safe.t =
    `Assoc [ ("width", `Int ar.width); ("height", `Int ar.height) ]

  (** [app.bsky.embed.video] JSON. [video] is a blob ref; optional
      [alt] / [aspect_ratio] / [presentation] map to the lexicon. *)
  let video_embed_json ~video ?alt ?aspect_ratio ?presentation () :
      Yojson.Safe.t =
    let fields =
      [ ("$type", `String "app.bsky.embed.video"); ("video", video) ]
      @ (match alt with Some a -> [ ("alt", `String a) ] | None -> [])
      @ (match aspect_ratio with
        | Some ar -> [ ("aspectRatio", aspect_ratio_json ar) ]
        | None -> [])
      @
      match presentation with
      | Some p -> [ ("presentation", `String p) ]
      | None -> []
    in
    `Assoc fields

  (* Multipart upload — start / part / finish / abort / status.
     Client only; the hosted transcoder still lives on video.bsky.app. *)

  type upload_session = {
    job_id : string;
    part_size_bytes : int;
    part_count : int;
    expires_at : string;
  }

  type part_ack = { part_number : int; size_bytes : int }
  type finish_result = { completed_job_id : string; job_status : job_status }

  type abort_state =
    | Abort_aborted
    | Abort_completed
    | Abort_failed
    | Abort_expired
    | Abort_other of string

  type abort_result = {
    state : abort_state;
    completed_job_id : string option;
    failure_reason : string option;
  }

  type upload_phase =
    | Phase_created
    | Phase_finishing
    | Phase_completed
    | Phase_failed
    | Phase_aborted
    | Phase_expired
    | Phase_other of string

  type upload_status = {
    job_id : string;
    part_size_bytes : int;
    part_count : int;
    received_parts : int list;
    expires_at : string;
    state : upload_phase;
    completed_job_id : string option;
    job_status : job_status option;
    failure_reason : string option;
  }

  let parse_abort_state (s : string) : abort_state =
    match String.lowercase_ascii s with
    | "aborted" -> Abort_aborted
    | "completed" -> Abort_completed
    | "failed" -> Abort_failed
    | "expired" -> Abort_expired
    | other -> Abort_other other

  let parse_upload_phase (s : string) : upload_phase =
    match String.lowercase_ascii s with
    | "created" -> Phase_created
    | "finishing" -> Phase_finishing
    | "completed" -> Phase_completed
    | "failed" -> Phase_failed
    | "aborted" -> Phase_aborted
    | "expired" -> Phase_expired
    | other -> Phase_other other

  let parse_upload_session json : upload_session =
    {
      job_id = Client.string_member json "jobId";
      part_size_bytes = Client.int_member json "partSizeBytes";
      part_count = Client.int_member json "partCount";
      expires_at = Client.string_member json "expiresAt";
    }

  let parse_part_ack json : part_ack =
    {
      part_number = Client.int_member json "partNumber";
      size_bytes = Client.int_member json "sizeBytes";
    }

  let parse_finish_result json : finish_result =
    {
      completed_job_id = Client.string_member json "completedJobId";
      job_status = parse_job_status_response json;
    }

  let parse_abort_result json : abort_result =
    {
      state = parse_abort_state (Client.string_member json "state");
      completed_job_id = Client.string_opt json "completedJobId";
      failure_reason = Client.string_opt json "failureReason";
    }

  let int_list json field =
    List.filter_map
      (function
        | `Int n -> Some n
        | `Intlit s -> ( try Some (int_of_string s) with _ -> None)
        | _ -> None)
      (Client.list_member json field)

  let parse_upload_status json : upload_status =
    {
      job_id = Client.string_member json "jobId";
      part_size_bytes = Client.int_member json "partSizeBytes";
      part_count = Client.int_member json "partCount";
      received_parts = int_list json "receivedParts";
      expires_at = Client.string_member json "expiresAt";
      state = parse_upload_phase (Client.string_member json "state");
      completed_job_id = Client.string_opt json "completedJobId";
      job_status =
        (match Yojson.Safe.Util.member "jobStatus" json with
        | `Assoc _ as st -> Some (parse_job_status st)
        | _ -> None);
      failure_reason = Client.string_opt json "failureReason";
    }

  (** JSON body for [app.bsky.video.startUpload] (multipart session).
      [size_bytes] and [mime_type] are required; optional [name] /
      [duration_ms] / [width] / [height] map to the lexicon. Client
      request body only. *)
  let start_upload_body ~size_bytes ~mime_type ?name ?duration_ms ?width ?height
      () : Yojson.Safe.t =
    let fields =
      [ ("sizeBytes", `Int size_bytes); ("mimeType", `String mime_type) ]
      @ (match name with Some n -> [ ("name", `String n) ] | None -> [])
      @ (match duration_ms with
        | Some n -> [ ("durationMs", `Int n) ]
        | None -> [])
      @ (match width with Some n -> [ ("width", `Int n) ] | None -> [])
      @ match height with Some n -> [ ("height", `Int n) ] | None -> []
    in
    `Assoc fields

  let job_id_body ~job_id : Yojson.Safe.t = `Assoc [ ("jobId", `String job_id) ]

  let bearer_extra ?token () =
    match token with
    | Some t -> [ ("Authorization", "Bearer " ^ t) ]
    | None -> []

  (** Start a multipart session via [app.bsky.video.startUpload].
      Client helper only — the hosted transcoder stays on
      [video.bsky.app]. *)
  let start_upload ?session ?host ?token ~size_bytes ~mime_type ?name
      ?duration_ms ?width ?height () : upload_session =
    Client.post_json ?session ~host:(video_host ?host ())
      ~extra:(bearer_extra ?token ()) "app.bsky.video.startUpload"
      (Yojson.Safe.to_string
         (start_upload_body ~size_bytes ~mime_type ?name ?duration_ms ?width
            ?height ()))
    |> parse_upload_session

  let upload_part_url ?host ~job_id ~part_number () =
    let base =
      Client.nsid_url ~host:(video_host ?host ()) "app.bsky.video.uploadPart"
    in
    let qs =
      Cohttp_client.Cohttp_client.create_body_from_pairs
        [ ("jobId", job_id); ("partNumber", string_of_int part_number) ]
    in
    if qs = "" then base else base ^ "?" ^ qs

  (** POST one part via [app.bsky.video.uploadPart]. Hosted video
      service; client part upload only. *)
  let upload_part ?host ~token ~job_id ~part_number
      ?(content_type = "application/octet-stream") (bytes : string) : part_ack =
    let url = upload_part_url ?host ~job_id ~part_number () in
    let headers =
      Cohttp_client.Cohttp_client.create_headers_from_pairs
        (upload_header_pairs ~token ~content_type)
    in
    let body =
      Lwt_main.run
        (Cohttp_client.Cohttp_client.post_data_with_headers url bytes headers)
    in
    parse_part_ack (Yojson.Safe.from_string body)

  (** Finish a multipart session via [app.bsky.video.finishUpload].
      Hosted video service; client helper only. *)
  let finish_upload ?session ?host ?token ~job_id () : finish_result =
    Client.post_json ?session ~host:(video_host ?host ())
      ~extra:(bearer_extra ?token ()) "app.bsky.video.finishUpload"
      (Yojson.Safe.to_string (job_id_body ~job_id))
    |> parse_finish_result

  (** Abort a multipart session via [app.bsky.video.abortUpload].
      Hosted video service; client helper only. *)
  let abort_upload ?session ?host ?token ~job_id () : abort_result =
    Client.post_json ?session ~host:(video_host ?host ())
      ~extra:(bearer_extra ?token ()) "app.bsky.video.abortUpload"
      (Yojson.Safe.to_string (job_id_body ~job_id))
    |> parse_abort_result

  (** Multipart session status via [app.bsky.video.getUploadStatus].
      Hosted video service; client poll only. *)
  let get_upload_status ?session ?host ?token ~job_id () : upload_status =
    Client.get_json ?session ~host:(video_host ?host ())
      ~extra:(bearer_extra ?token ()) "app.bsky.video.getUploadStatus"
      [ ("jobId", job_id) ]
    |> parse_upload_status

  (** Expected byte size for [part_number] in [sess], or [None] if out
      of range. The last part may be shorter. *)
  let expected_part_size (sess : upload_session) ~part_number : int option =
    if part_number < 1 || part_number > sess.part_count then None
    else if part_number < sess.part_count then Some sess.part_size_bytes
    else
      (* last part may be shorter; callers that know the total size can compute it *)
      Some sess.part_size_bytes

  (** 1-based part numbers not yet in [st.received_parts]. *)
  let missing_parts (st : upload_status) : int list =
    let rec loop i acc =
      if i > st.part_count then List.rev acc
      else if List.mem i st.received_parts then loop (i + 1) acc
      else loop (i + 1) (i :: acc)
    in
    loop 1 []
end
