open OUnit2
open Atproto.Syntax

let assert_ok label pred =
  OUnit2.assert_bool ("expected valid: " ^ label) (pred label)

let assert_bad label pred =
  OUnit2.assert_bool ("expected invalid: " ^ label) (not (pred label))

let test_handle_spec_examples _ =
  List.iter
    (fun h -> assert_ok h Syntax.is_valid_handle)
    [
      "jay.bsky.social";
      "8.cn";
      "name.t--t";
      "XX.LCS.MIT.EDU";
      "a.co";
      "xn--notarealidn.com";
      "xn--fiqa61au8b7zsevnm8ak20mc4a87e.xn--fiqs8s";
      "xn--ls8h.test";
      "example.t";
    ];
  List.iter
    (fun h -> assert_bad h Syntax.is_valid_handle)
    [
      "jo@hn.test";
      "john..test";
      "xn--bcher-.tld";
      "john.0";
      "cn.8";
      "org";
      "name.org.";
      "";
    ]

let test_handle_normalize _ =
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "blueskyweb.xyz"
    (Syntax.normalize_and_ensure_handle "BlueskyWeb.xyz")

let test_handle_reserved_tld _ =
  OUnit2.assert_bool "syntax allows .onion"
    (Syntax.is_valid_handle
       "2gzyxa5ihm7nsggfxnu52rck2vv4rvmdlkiu3zzui5du4xyclen53wid.onion");
  OUnit2.assert_bool ".onion must fail resolution TLD check"
    (not
       (Syntax.is_valid_tld
          "2gzyxa5ihm7nsggfxnu52rck2vv4rvmdlkiu3zzui5du4xyclen53wid.onion"));
  OUnit2.assert_bool ".local must fail TLD check"
    (not (Syntax.is_valid_tld "laptop.local"));
  OUnit2.assert_bool ".arpa must fail TLD check"
    (not (Syntax.is_valid_tld "blah.arpa"));
  OUnit2.assert_bool ".test is allowed at TLD layer"
    (Syntax.is_valid_tld "xn--ls8h.test")

let test_did_spec_examples _ =
  List.iter
    (fun d -> assert_ok d Syntax.is_valid_did)
    [
      "did:plc:ewvi7nxzyoun6zhxrhs64oiz";
      "did:web:user.example.com";
      "did:method:val:two";
      "did:m:v";
      "did:method::::val";
      "did:method:-:_:.";
      "did:key:zQ3shZc2QzApp2oymGvQbzP8eKheVshBHbU4ZYjeXqwSKEn6N";
    ];
  List.iter
    (fun d -> assert_bad d Syntax.is_valid_did)
    [
      "did:METHOD:val";
      "did:m123:val";
      "DID:method:val";
      "did:method:";
      "did:method:val/two";
      "did:method:val?two";
      "did:method:val#two";
    ];
  OUnit2.assert_equal (Some "plc")
    (Syntax.did_method "did:plc:ewvi7nxzyoun6zhxrhs64oiz");
  OUnit2.assert_bool "plc is blessed"
    (Syntax.is_blessed_did "did:plc:ewvi7nxzyoun6zhxrhs64oiz");
  OUnit2.assert_bool "did:key is valid syntax but not blessed"
    (Syntax.is_valid_did
       "did:key:zQ3shZc2QzApp2oymGvQbzP8eKheVshBHbU4ZYjeXqwSKEn6N"
    && not
         (Syntax.is_blessed_did
            "did:key:zQ3shZc2QzApp2oymGvQbzP8eKheVshBHbU4ZYjeXqwSKEn6N"))

let test_nsid_spec_examples _ =
  List.iter
    (fun n -> assert_ok n Syntax.is_valid_nsid)
    [
      "com.example.fooBar";
      "net.users.bob.ping";
      "a-0.b-1.c";
      "a.b.c";
      "com.example.fooBarV2";
      "cn.8.lex.stuff";
      "com.atproto.sync.getRecord";
    ];
  List.iter
    (fun n -> assert_bad n Syntax.is_valid_nsid)
    [
      "com.example"; "com.example.3"; "example.com/foo"; "foo"; "8.example.foo";
    ];
  let nsid = Syntax.parse_nsid "com.example.fooBar" in
  OUnit2.assert_equal ~printer:(fun x -> x) "fooBar" (Syntax.nsid_name nsid);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "example.com"
    (Syntax.nsid_authority nsid);
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "com.example"
    (Syntax.nsid_authority_nsid nsid);
  let created = Syntax.create_nsid ~authority:"example.com" ~name:"fooBar" in
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "com.example.fooBar"
    (Syntax.nsid_to_string created)

let test_nsid_glob_and_ref _ =
  OUnit2.assert_bool "bare glob" (Syntax.is_valid_nsid_glob "*");
  OUnit2.assert_bool "authority glob"
    (Syntax.is_valid_nsid_glob "com.atproto.*");
  OUnit2.assert_bool "partial glob rejected"
    (not (Syntax.is_valid_nsid_glob "com.atpro*"));
  let r = Syntax.parse_nsid_ref "com.example.defs#userView" in
  OUnit2.assert_equal ~printer:(fun x -> x) "com.example.defs" r.nsid;
  OUnit2.assert_equal (Some "userView") r.fragment;
  OUnit2.assert_bool "#main rejected"
    (try
       ignore (Syntax.parse_nsid_ref "com.example.record#main");
       false
     with Syntax.Invalid _ -> true)

let test_record_key _ =
  List.iter
    (fun k -> assert_ok k Syntax.is_valid_record_key)
    [ "3jzfcijpj2z2a"; "self"; "literal:self"; "a"; String.make 512 'a' ];
  List.iter
    (fun k -> assert_bad k Syntax.is_valid_record_key)
    [
      ""; "."; ".."; "alpha/beta"; "#extra"; "@handle"; "any space"; "any+space";
    ]

let test_at_identifier _ =
  OUnit2.assert_bool "handle" (Syntax.is_valid_at_identifier "jay.bsky.team");
  OUnit2.assert_bool "did"
    (Syntax.is_valid_at_identifier "did:plc:ewvi7nxzyoun6zhxrhs64oiz");
  OUnit2.assert_bool "neither" (not (Syntax.is_valid_at_identifier "not an id"))

let test_datetime_spec_examples _ =
  List.iter
    (fun d -> assert_ok d Syntax.is_valid_datetime)
    [
      "1985-04-12T23:20:50.123Z";
      "1985-04-12T23:20:50.123456Z";
      "1985-04-12T23:20:50.120Z";
      "1985-04-12T23:20:50.120000Z";
      "0001-01-01T00:00:00.000Z";
      "0000-01-01T00:00:00.000Z";
      "1985-04-12T23:20:50.12345678912345Z";
      "1985-04-12T23:20:50Z";
      "1985-04-12T23:20:50.0Z";
      "1985-04-12T23:20:50.123+00:00";
      "1985-04-12T23:20:50.123-07:00";
    ];
  List.iter
    (fun d -> assert_bad d Syntax.is_valid_datetime)
    [
      "1985-04-12";
      "1985-04-12T23:20Z";
      "1985-04-12T23:20:5Z";
      "1985-04-12T23:20:50.123";
      "+001985-04-12T23:20:50.123Z";
      "23:20:50.123Z";
      "-1985-04-12T23:20:50.123Z";
      "1985-4-12T23:20:50.123Z";
      "01985-04-12T23:20:50.123Z";
      "1985-04-12T23:20:50.123+00";
      "1985-04-12T23:20:50.123+0000";
      "1985-04-12t23:20:50.123Z";
      "1985-04-12T23:20:50.123z";
      "1985-04-12T23:20:50.123-00:00";
      "1985-04-12 23:20:50.123Z";
      "1985-04-12T23:99:50.123Z";
      "1985-00-12T23:20:50.123Z";
      "0000-01-01T00:00:00+01:00";
      "1985-02-29T00:00:00Z";
    ];
  OUnit2.assert_bool "leap day 2024"
    (Syntax.is_valid_datetime "2024-02-29T00:00:00.000Z");
  OUnit2.assert_bool "now is valid"
    (Syntax.is_valid_datetime (Syntax.now_datetime ()))

let test_language _ =
  List.iter
    (fun t -> assert_ok t Syntax.is_valid_language)
    [
      "ja"; "ban"; "pt-BR"; "hy-Latn-IT-arevela"; "zh-Hans"; "de-CH-1996"; "en";
    ];
  List.iter
    (fun t -> assert_bad t Syntax.is_valid_language)
    [ ""; "1"; "toolonglanguage"; "en--US"; "en-" ]

let test_handle_resolution_helpers _ =
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "_atproto.user.example.com"
    (Syntax.handle_txt_name "user.example.com");
  OUnit2.assert_equal (Some "did:plc:ewvi7nxzyoun6zhxrhs64oiz")
    (Syntax.parse_txt_did "did=did:plc:ewvi7nxzyoun6zhxrhs64oiz");
  OUnit2.assert_equal None (Syntax.parse_txt_did "not-a-did");
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "https://user.example.app/.well-known/atproto-did"
    (Syntax.handle_well_known_url "user.example.app");
  OUnit2.assert_equal (Some "did:plc:ewvi7nxzyoun6zhxrhs64oiz")
    (Syntax.parse_well_known_did "  did:plc:ewvi7nxzyoun6zhxrhs64oiz \n")

let suite =
  "syntax"
  >::: [
         "test_handle_spec_examples" >:: test_handle_spec_examples;
         "test_handle_normalize" >:: test_handle_normalize;
         "test_handle_reserved_tld" >:: test_handle_reserved_tld;
         "test_did_spec_examples" >:: test_did_spec_examples;
         "test_nsid_spec_examples" >:: test_nsid_spec_examples;
         "test_nsid_glob_and_ref" >:: test_nsid_glob_and_ref;
         "test_record_key" >:: test_record_key;
         "test_at_identifier" >:: test_at_identifier;
         "test_datetime_spec_examples" >:: test_datetime_spec_examples;
         "test_language" >:: test_language;
         "test_handle_resolution_helpers" >:: test_handle_resolution_helpers;
       ]

let () = run_test_tt_main suite
