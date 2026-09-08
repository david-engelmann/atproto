open Auth

(** [com.atproto.server.createSession] / [getSession] and the live session record. *)
module Session : sig
  type session = {
    username : string;
    password : string;
    atp_host : string;
    auth : Auth.auth;
    did_doc : Yojson.Safe.t option;
  }

  (* com.atproto.server.getSession / createSession output extras. *)
  type session_request = {
    handle : string;
    did : string;
    email : string option;
    email_confirmed : bool option;
    email_auth_factor : bool option;
    active : bool option;
    status : string option;
    did_doc : Yojson.Safe.t option;
  }

  val parse_session_request : Yojson.Safe.t -> session_request

  (** PDS host from [ATP_HOST] (default [bsky.social]). *)
  val atp_host_from_env : string

  (** Create a password session ([com.atproto.server.createSession]) on
      [ATP_HOST] (default [bsky.social]). Optional [auth_factor_token] and
      [allow_takendown] map to the lexicon inputs. *)
  val create_session :
    ?auth_factor_token:string ->
    ?allow_takendown:bool ->
    string ->
    string ->
    session

  (** [Authorization: Bearer] header pair from the session access JWT. *)
  val bearer_token_from_session : session -> string * string

  (** [Authorization: Bearer] header pair from the session refresh JWT. *)
  val refresh_token_from_session : session -> string * string

  (** Raw JSON from [com.atproto.server.getSession] for [s]. *)
  val get_session_request : session -> string

  (** Current account info for [s] via [com.atproto.server.getSession]
      (handle, DID, email flags, active/status). *)
  val get_session : session -> session_request

  (** Rotate JWTs via [com.atproto.server.refreshSession] using
      [refreshJwt]. Fails if the session has no refresh token. *)
  val refresh_session : session -> session

  (** Refresh [s] when [Auth.is_token_expired]; otherwise return [s]. *)
  val refresh_session_auth : session -> session

  (** End the session via [com.atproto.server.deleteSession] (Bearer
      [refreshJwt]). Empty procedure output stays [""] for existing
      callers. *)
  val delete_session : session -> string
end
