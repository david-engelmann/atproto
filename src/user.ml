(** Minimal username/password pair (used by tests; prefer [Auth] + [Session]). *)
module User = struct
  type user = { username : string; password : string }
  (** Username/password pair used by tests. Prefer [Auth] + [Session]. *)
end
