open OUnit2
open Atproto.Germnetwork

let test_declaration_roundtrip _ =
  let key = "\x00ed25519-public-key-bytes!!"
  and pkg = "mls-key-package"
  and proof = "continuity" in
  let json =
    Germnetwork.declaration ~version:"1.0.0" ~current_key:key
      ~message_me:
        (Germnetwork.message_me ~show_button_to:Germnetwork.show_everyone
           ~message_me_url:"https://germ.example/message")
      ~key_package:pkg ~continuity_proofs:[ proof ] ()
  in
  let open Yojson.Safe.Util in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "com.germnetwork.declaration"
    (json |> member "$type" |> to_string);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "1.0.0"
    (json |> member "version" |> to_string);
  let parsed = Germnetwork.parse_declaration json in
  OUnit2.assert_equal ~printer:(fun x -> x) key parsed.current_key;
  OUnit2.assert_equal (Some pkg) parsed.key_package;
  OUnit2.assert_equal [ proof ] parsed.continuity_proofs;
  match parsed.message_me with
  | Some m ->
      OUnit2.assert_equal
        ~printer:(fun x -> x)
        Germnetwork.show_everyone m.show_button_to
  | None -> OUnit2.assert_failure "expected messageMe"

let suite =
  "germnetwork"
  >::: [ "test_declaration_roundtrip" >:: test_declaration_roundtrip ]

let () = run_test_tt_main suite
