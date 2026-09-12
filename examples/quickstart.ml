(* Copy-paste public-network example. Needs network. No ATP_AUTH.
   resolveHandle → entryway (ATP_HOST / bsky.social).
   searchPosts → public AppView. ATP_PUBLIC only gates live tests. *)

open Atproto.Identity
open Atproto.Feed

let () =
  let did = (Identity.resolve_handle "jay.bsky.team").did in
  Printf.printf "jay.bsky.team -> %s\n%!" did;
  let posts = Feed.search_posts ~q:"atproto" ~limit:5 () in
  Printf.printf "searchPosts %d\n%!" (List.length posts.posts)
