open Session
open Auth

(** XRPC base-URL helpers for a PDS session or a public host. *)
module App = struct
  (** Join a base XRPC URL and an NSID (or path) with a single slash. *)
  let create_endpoint_url (url : string) (endpoint : string) : string =
    let url =
      if String.get url (String.length url - 1) = '/' then url else url ^ "/"
    in
    url ^ endpoint

  (** PDS XRPC base for [s] ([https://host/xrpc] or [ATP_SCHEME]). *)
  let create_base_url (s : Session.session) : string =
    let base_endpoint = Auth.get_base_endpoint in
    Auth.origin_of_host s.atp_host ^ "/" ^ base_endpoint

  (** XRPC base for a public [host] (default [ATP_HOST]). *)
  let create_public_base_url ?(host = Session.atp_host_from_env) () : string =
    let base_endpoint = Auth.get_base_endpoint in
    Auth.origin_of_host host ^ "/" ^ base_endpoint
end
