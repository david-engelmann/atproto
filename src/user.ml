(** Minimal username/password pair (used by tests; prefer [Auth] + [Session]). *)
module User = struct
  (** Username/password pair used by tests. Prefer [Auth] + [Session]. *)
  type user = { username : string; password : string }
end
