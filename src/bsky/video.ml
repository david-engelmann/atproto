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
  let default_host = "video.bsky.app"
  let upload_nsid = "app.bsky.video.uploadVideo"
  let upload_blob_lxm = "com.atproto.repo.uploadBlob"
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

  let pds_audience ?host (s : Session.session) : string =
    let raw = match host with Some h -> h | None -> s.Session.atp_host in
    "did:web:" ^ host_of_endpoint raw

  let recommended_exp ?(now = Unix.gettimeofday ()) () : int64 =
    Int64.add (Int64.of_float now) recommended_exp_seconds

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

  let classify_state (state : string) : job_phase =
    let upper =
      String.map
        (function 'a' .. 'z' as c -> Char.chr (Char.code c - 32) | c -> c)
        state
    in
    if upper = "JOB_STATE_COMPLETED" || upper = "COMPLETED" then Completed
    else if
      upper = "JOB_STATE_FAILED" || upper = "FAILED" || upper = "ALREADY_EXISTS"
    then Failed
    else In_progress

  let is_completed (st : job_status) : bool =
    classify_state st.state = Completed

  let is_failed (st : job_status) : bool = classify_state st.state = Failed

  let is_terminal (st : job_status) : bool =
    match classify_state st.state with
    | Completed | Failed -> true
    | In_progress -> false

  let blob_ready (st : job_status) : bool = Option.is_some st.blob

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

  let get_job_status ?session ?host ~job_id () : job_status =
    Client.get_json ?session ~host:(video_host ?host ())
      "app.bsky.video.getJobStatus"
      [ ("jobId", job_id) ]
    |> parse_job_status_response

  let get_upload_limits (s : Session.session) : upload_limits =
    Client.get_json ~session:s "app.bsky.video.getUploadLimits" []
    |> parse_upload_limits

  let upload_video_url ?host ~did ~name () =
    let base = Client.nsid_url ~host:(video_host ?host ()) upload_nsid in
    let qs =
      Cohttp_client.Cohttp_client.create_body_from_pairs
        [ ("did", did); ("name", name) ]
    in
    if qs = "" then base else base ^ "?" ^ qs

  let upload_header_pairs ~token ~content_type =
    [ ("Authorization", "Bearer " ^ token); ("Content-Type", content_type) ]

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

  let ensure_blob ?get_status ?sleep ?session ?host (st : job_status) :
      job_status =
    if blob_ready st then st
    else if st.job_id = "" then st
    else poll_job_status ?get_status ?sleep ?session ?host ~job_id:st.job_id ()

  let mint_upload_token (s : Session.session) ?pds_host ?exp () : string =
    let aud = pds_audience ?host:pds_host s in
    let exp = match exp with Some e -> e | None -> recommended_exp () in
    (Server.Server.get_service_auth s ~aud ~lxm:upload_blob_lxm ~exp ()).token

  let aspect_ratio_json (ar : aspect_ratio) : Yojson.Safe.t =
    `Assoc [ ("width", `Int ar.width); ("height", `Int ar.height) ]

  let video_embed_json ~video ?alt ?aspect_ratio () : Yojson.Safe.t =
    let fields =
      [ ("$type", `String "app.bsky.embed.video"); ("video", video) ]
      @ (match alt with Some a -> [ ("alt", `String a) ] | None -> [])
      @
      match aspect_ratio with
      | Some ar -> [ ("aspectRatio", aspect_ratio_json ar) ]
      | None -> []
    in
    `Assoc fields
end
