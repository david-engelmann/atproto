open OUnit2
open Atproto.Lexicon

let sample =
  {|
{
  "lexicon": 1,
  "id": "com.atproto.identity.resolveHandle",
  "defs": {
    "main": {
      "type": "query",
      "description": "Resolves a handle to a DID.",
      "parameters": {
        "type": "params",
        "required": ["handle"],
        "properties": {
          "handle": { "type": "string", "format": "handle" }
        }
      }
    }
  }
}
|}

let test_parse_document _ =
  let doc = Lexicon.of_string sample in
  OUnit2.assert_equal 1 doc.lexicon;
  OUnit2.assert_equal
    ~printer:(fun x -> x)
    "com.atproto.identity.resolveHandle" doc.id;
  match Lexicon.main doc with
  | None -> OUnit2.assert_failure "missing main def"
  | Some main ->
      OUnit2.assert_equal Lexicon.Query main.kind;
      OUnit2.assert_bool "missing description"
        (match main.description with Some _ -> true | None -> false)

let test_lookup_helpers _ =
  OUnit2.assert_equal Lexicon.Cid_link (Lexicon.lookup_primitive "cid-link");
  OUnit2.assert_equal Lexicon.Subscription
    (Lexicon.lookup_definition "subscription")

let test_nested_parameters_and_codegen _ =
  let doc = Lexicon.of_string sample in
  match Lexicon.main doc with
  | None -> OUnit2.assert_failure "missing main"
  | Some main -> (
      OUnit2.assert_equal [ "handle" ] main.required;
      OUnit2.assert_equal (Some Lexicon.String)
        (List.assoc_opt "handle" main.properties);
      let ocaml = Lexicon.to_ocaml doc in
      OUnit2.assert_bool "codegen must mention the lexicon id"
        (let needle = "com.atproto.identity.resolveHandle" in
         let rec contains i =
           i + String.length needle <= String.length ocaml
           && (String.sub ocaml i (String.length needle) = needle
              || contains (i + 1))
         in
         contains 0);
      OUnit2.assert_bool "codegen must emit handle"
        (let rec contains i =
           i + 6 <= String.length ocaml
           && (String.sub ocaml i 6 = "handle" || contains (i + 1))
         in
         contains 0);
      match
        Lexicon.validate main (`Assoc [ ("handle", `String "jay.bsky.team") ])
      with
      | Ok () -> ()
      | Error e -> OUnit2.assert_failure e)

let procedure_sample =
  {|
{
  "lexicon": 1,
  "id": "app.bsky.video.uploadVideo",
  "defs": {
    "main": {
      "type": "procedure",
      "description": "Upload a video as raw bytes.",
      "parameters": {
        "type": "params",
        "required": ["did"],
        "properties": {
          "did": { "type": "string", "format": "did" },
          "name": { "type": "string" }
        }
      },
      "input": {
        "encoding": "video/*"
      },
      "output": {
        "encoding": "application/json",
        "schema": {
          "type": "object",
          "required": ["jobId"],
          "properties": {
            "jobId": { "type": "string" },
            "blob": { "type": "blob" }
          }
        }
      }
    }
  }
}
|}

let test_procedure_input_output _ =
  let doc = Lexicon.of_string procedure_sample in
  match Lexicon.main doc with
  | None -> OUnit2.assert_failure "missing main"
  | Some main ->
      OUnit2.assert_equal Lexicon.Procedure main.kind;
      OUnit2.assert_equal [ "did" ] main.required;
      OUnit2.assert_equal (Some "video/*") main.input.encoding;
      OUnit2.assert_equal (Some "application/json") main.output.encoding;
      OUnit2.assert_equal [ "jobId" ] main.output.required;
      OUnit2.assert_equal (Some Lexicon.Bytes)
        (List.assoc_opt "blob" main.output.properties);
      let ocaml = Lexicon.to_ocaml doc in
      OUnit2.assert_bool "codegen mentions input encoding"
        (let needle = "video/*" in
         let rec contains i =
           i + String.length needle <= String.length ocaml
           && (String.sub ocaml i (String.length needle) = needle
              || contains (i + 1))
         in
         contains 0)

let test_validate_errors _ =
  let doc = Lexicon.of_string sample in
  match Lexicon.main doc with
  | None -> OUnit2.assert_failure "missing main"
  | Some main -> (
      (match Lexicon.validate main (`Assoc []) with
      | Error msg -> OUnit2.assert_bool "mentions handle" (String.length msg > 0)
      | Ok () -> OUnit2.assert_failure "empty object accepted");
      match Lexicon.validate main (`Assoc [ ("handle", `Int 1) ]) with
      | Error _ -> ()
      | Ok () -> OUnit2.assert_failure "wrong type accepted")

let test_union_codegen_and_official_bundle _ =
  let docs = Lexicon.official_documents () in
  OUnit2.assert_bool "bundled lexicons" (List.length docs >= 20);
  OUnit2.assert_bool "listQueues bundled"
    (List.exists
       (fun (d : Lexicon.document) -> d.id = "tools.ozone.queue.listQueues")
       docs);
  OUnit2.assert_bool "queryReports bundled"
    (List.exists
       (fun (d : Lexicon.document) -> d.id = "tools.ozone.report.queryReports")
       docs);
  OUnit2.assert_bool "checkSignupQueue bundled"
    (List.exists
       (fun (d : Lexicon.document) ->
         d.id = "com.atproto.temp.checkSignupQueue")
       docs);
  OUnit2.assert_bool "searchPostsV2 bundled"
    (List.exists
       (fun (d : Lexicon.document) -> d.id = "app.bsky.feed.searchPostsV2")
       docs);
  OUnit2.assert_bool "resolveLexicon bundled"
    (List.exists
       (fun (d : Lexicon.document) ->
         d.id = "com.atproto.lexicon.resolveLexicon")
       docs);
  OUnit2.assert_bool "getPostThreadV2 bundled"
    (List.exists
       (fun (d : Lexicon.document) ->
         d.id = "app.bsky.unspecced.getPostThreadV2")
       docs);
  OUnit2.assert_bool "subscribeModEvents bundled"
    (List.exists
       (fun (d : Lexicon.document) ->
         d.id = "chat.bsky.moderation.subscribeModEvents")
       docs);
  List.iter
    (fun (doc : Lexicon.document) ->
      OUnit2.assert_equal 1 doc.lexicon;
      OUnit2.assert_bool "id" (String.length doc.id > 3))
    docs;
  let thread =
    List.find
      (fun (d : Lexicon.document) -> d.id = "app.bsky.feed.getPostThread")
      docs
  in
  match Lexicon.main thread with
  | None -> OUnit2.assert_failure "missing main"
  | Some main -> (
      (match List.assoc_opt "thread" main.output.properties with
      | Some (Lexicon.Union refs) ->
          OUnit2.assert_bool "thread union refs"
            (List.exists
               (fun r ->
                 let n = String.length r in
                 n >= 14 && String.sub r (n - 14) 14 = "threadViewPost")
               refs);
          let ocaml = Lexicon.to_ocaml thread in
          OUnit2.assert_bool "codegen emits union variant"
            (let needle = "threadViewPost" in
             let rec contains i =
               i + String.length needle <= String.length ocaml
               && (String.sub ocaml i (String.length needle) = needle
                  || contains (i + 1))
             in
             contains 0)
      | Some _ -> OUnit2.assert_failure "thread is not a union"
      | None -> OUnit2.assert_failure "missing thread output property");
      let list_doc =
        List.find
          (fun (d : Lexicon.document) -> d.id = "app.bsky.graph.list")
          docs
      in
      match Lexicon.main list_doc with
      | None -> OUnit2.assert_failure "missing list main"
      | Some main -> (
          match List.assoc_opt "labels" main.properties with
          | Some (Lexicon.Union refs) ->
              OUnit2.assert_equal [ "com.atproto.label.defs#selfLabels" ] refs
          | _ -> OUnit2.assert_failure "list labels should be a union"))

let test_parse_resolved_lexicon _ =
  let json =
    `Assoc
      [
        ( "uri",
          `String
            "at://did:plc:abc123xyz0001112223333/com.atproto.lexicon.schema/app.bsky.feed.post"
        );
        ("cid", `String "bafyreischema");
        ( "schema",
          `Assoc
            [
              ("lexicon", `Int 1);
              ("id", `String "app.bsky.feed.post");
              ( "defs",
                `Assoc
                  [
                    ( "main",
                      `Assoc
                        [
                          ("type", `String "record");
                          ( "record",
                            `Assoc
                              [
                                ("type", `String "object");
                                ("required", `List [ `String "text" ]);
                                ( "properties",
                                  `Assoc
                                    [
                                      ( "text",
                                        `Assoc [ ("type", `String "string") ] );
                                    ] );
                              ] );
                        ] );
                  ] );
            ] );
      ]
  in
  let resolved = Lexicon.parse_resolved_lexicon json in
  OUnit2.assert_equal ~printer:(fun x -> x) "bafyreischema" resolved.cid;
  match resolved.document with
  | Some doc ->
      OUnit2.assert_equal ~printer:(fun x -> x) "app.bsky.feed.post" doc.id
  | None -> OUnit2.assert_failure "expected parsed schema document"

let suite =
  "lexicon"
  >::: [
         "test_parse_document" >:: test_parse_document;
         "test_lookup_helpers" >:: test_lookup_helpers;
         "test_nested_parameters_and_codegen"
         >:: test_nested_parameters_and_codegen;
         "test_procedure_input_output" >:: test_procedure_input_output;
         "test_validate_errors" >:: test_validate_errors;
         "test_union_codegen_and_official_bundle"
         >:: test_union_codegen_and_official_bundle;
         "test_parse_resolved_lexicon" >:: test_parse_resolved_lexicon;
       ]

let () = run_test_tt_main suite
