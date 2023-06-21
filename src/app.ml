open Session
open Auth

module App = struct
  let create_endpoint_url (url : string) (endpoint : string) : string =
    let url = if String.get url (String.length url - 1) = '/' then url else url ^ "/" in
    url ^ endpoint

  let create_base_url (s : Session.session) : string =
    let base_endpoint = Auth.get_base_endpoint in
    "https://" ^ s.atp_host ^ "/" ^ base_endpoint
end
