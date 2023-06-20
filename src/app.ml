open Session

module App = struct
  let base_endpoint_from_env : string =
    let base_endpoint = try Sys.getenv "BASE_ENDPOINT" with Not_found -> "xrpc" in
    base_endpoint

  let get_base_endpoint : string =
    let base_endpoint = base_endpoint_from_env in
    let base_endpoint = if String.get base_endpoint (String.length base_endpoint - 1) = '/' then base_endpoint else base_endpoint ^ "/" in
    base_endpoint

  let create_endpoint_url (url : string) (endpoint : string) : string =
    let url = if String.get url (String.length url - 1) = '/' then url else url ^ "/" in
    url ^ endpoint

  let create_base_url (s : Session.session) : string =
    let base_endpoint = get_base_endpoint in
    "https://" ^ s.atp_host ^ "/" ^ base_endpoint
end
