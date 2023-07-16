open OUnit2
open Atproto.Session
open Atproto.Auth
open Atproto.Notification

let create_test_session _ =
    let (username, password) = Auth.username_and_password_from_env in
    Session.create_session username password

let test_get_unread_count _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let unread_count = Notification.get_unread_count test_session "2023-07-15T12:34:56.789012Z" in
  Printf.printf "Unread Count: %s\n" unread_count;
  OUnit2.assert_bool "Unread Count is not empty" (unread_count <> "")

let test_list_notifications _ =
  let test_session = create_test_session () |> Session.refresh_session_auth in
  let notification = Notification.list_notifications test_session 10 "2023-07-15T12:34:56.789012Z" in
  Printf.printf "List Notifications: %s\n" notification;
  OUnit2.assert_bool "List Notifications is not empty" (notification <> "")

let suite =
    "suite"
    >::: [

          "test_get_unread_count" >:: test_get_unread_count;
          "test_list_notifications" >:: test_list_notifications;

         ]

let () = run_test_tt_main suite
