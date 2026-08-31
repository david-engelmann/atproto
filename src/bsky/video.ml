open Session
open Client

(** app.bsky.video — upload limits and processing job status. *)
module Video = struct
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

  let get_job_status ?session ?host ~job_id () : job_status =
    Client.get_json ?session ?host "app.bsky.video.getJobStatus"
      [ ("jobId", job_id) ]
    |> parse_job_status_response

  let get_upload_limits (s : Session.session) : upload_limits =
    Client.get_json ~session:s "app.bsky.video.getUploadLimits" []
    |> parse_upload_limits

  let upload_video_url ?session ?host () =
    Client.nsid_url ?session ?host "app.bsky.video.uploadVideo"
end
