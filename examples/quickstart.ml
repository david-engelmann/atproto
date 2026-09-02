(* Copy-paste public AppView example. No ATP_AUTH. *)

open Atproto.Identity
open Atproto.Feed

let () =
  let did = (Identity.resolve_handle "jay.bsky.team").did in
  Printf.printf "jay.bsky.team -> %s\n%!" did;
  let page = Feed.search_posts ~q:"atproto" ~limit:5 () in
  Printf.printf "searchPosts %d\n%!" (List.length page.posts);
  List.iter (fun p -> Printf.printf "  %s\n%!" p.uri) page.posts
