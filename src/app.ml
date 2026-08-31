open Session
open Auth

module App = struct
  let create_endpoint_url (url : string) (endpoint : string) : string =
    let url =
      if String.get url (String.length url - 1) = '/' then url else url ^ "/"
    in
    url ^ endpoint

  let create_base_url (s : Session.session) : string =
    let base_endpoint = Auth.get_base_endpoint in
    Auth.origin_of_host s.atp_host ^ "/" ^ base_endpoint

  let create_public_base_url ?(host = Session.atp_host_from_env) () : string =
    let base_endpoint = Auth.get_base_endpoint in
    Auth.origin_of_host host ^ "/" ^ base_endpoint
end
