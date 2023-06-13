open OUnit2
open Bluesky.Uri

let sample_uri_without_search_params : Uri.uri = {
      host = "david.host.com";
      path_name = "io.example.song";
      hash = "3yI5-c1z-cc2p-1a";
      search_params = None;
    }

let sample_uri_with_search_params : Uri.uri = {
      host = "david.host.com";
      path_name = "io.example.song";
      hash = "3yI5-c1z-cc2p-1a";
      search_params = Some [("july", "jackson")];
    }
