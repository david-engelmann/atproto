open OUnit2
open Bluesky.User

let sample_user : User.user = {
      username = "your.name";
      password = "password";
    }

let test_sample_user_username _ =
  match sample_user with
   | { username; _ } ->
      OUnit2.assert_equal "your.name" username

let test_sample_user_password _ =
  match sample_user with
   | { password; _ } ->
      OUnit2.assert_equal "password" password

let suite =
  "suite"
  >::: [
         "test_sample_user_username" >:: test_sample_user_username;
         "test_sample_user_password" >:: test_sample_user_password;
       ]

let () = run_test_tt_main suite
