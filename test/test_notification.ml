open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Notification

let create_test_session _ =
    let (username, password) = Auth.username_and_password_from_env in
    Session.create_session username password

let test_get_unread_count _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let unread_count = Notification.get_unread_count test_session in
  match unread_count with
  | { count } ->
    OUnit2.assert_bool "Count is not present" (count >= 0)

let test_list_notifications _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let notifications = Notification.list_notifications test_session 10 in
  match notifications with
  | [] -> OUnit2.assert_equal "blah" ""
  | hd :: _ ->
    match hd with
    | { author; _ } ->
      match author with
      | {handle; _ } ->
        OUnit2.assert_bool "Handle is empty" (String.length handle > 0)

let test_update_seen _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let updated_seen = Notification.update_seen test_session "2023-07-15T12:34:56.789012Z" in
  Printf.printf "Updated Seen: %s\n" updated_seen;
  OUnit2.assert_bool "Updated Seen is not empty" (updated_seen = "")

let suite =
    "suite"
    >::: [

          "test_get_unread_count" >:: test_get_unread_count;
          "test_list_notifications" >:: test_list_notifications;
          "test_update_seen" >:: test_update_seen;

         ]

let () = run_test_tt_main suite
