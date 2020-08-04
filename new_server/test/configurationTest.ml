(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Newserver
module Path = Pyre.Path

let ( ! ) = Path.create_absolute ~follow_symbolic_links:false

let test_json_parsing context =
  let assert_parsed ~expected json_string =
    let json = Yojson.Safe.from_string json_string in
    match ServerConfiguration.of_yojson json with
    | Result.Error message ->
        let message = Format.sprintf "Unexpected JSON parsing failure: %s" message in
        assert_failure message
    | Result.Ok actual ->
        let actual_json = ServerConfiguration.to_yojson actual in
        List.iter expected ~f:(fun (key, value) ->
            assert_equal
              ~ctxt:context
              ~cmp:Yojson.Safe.equal
              ~printer:Yojson.Safe.pretty_to_string
              value
              (Yojson.Safe.Util.member key actual_json))
  in
  let assert_not_parsed json_string =
    let json = Yojson.Safe.from_string json_string in
    match ServerConfiguration.of_yojson json with
    | Result.Ok _ -> assert_failure "Unexpected JSON parsing success"
    | Result.Error _ -> ()
  in

  (* Empty. *)
  assert_not_parsed "[]";
  assert_not_parsed "{}";

  (* Mandatory fields must exist with the right type. *)
  assert_not_parsed {| { "foo": 42 } |};
  assert_not_parsed {| { "log_path": "/foo/bar" } |};
  assert_not_parsed {| { "source_path": ["/foo/bar"] } |};
  assert_not_parsed {| { "global_root": "/foo/bar" } |};
  assert_not_parsed {| { "log_path": 42, "source_path": ["/foo/bar"], "global_root": "/foo/bar" } |};
  assert_not_parsed
    {| { "log_path": "/foo/bar", "source_path": "/foo/bar", "global_root": "/foo/bar" } |};
  assert_not_parsed {| { "log_path": "/foo/bar", "source_path": ["/foo/bar"], "global_root": [] } |};

  let mandatory_fileds =
    {|
    "log_path": "/log",
    "source_paths": ["/source"],
    "global_root": "/project"
  |}
  in

  (* Default values *)
  assert_parsed
    (Format.sprintf "{%s}" mandatory_fileds)
    ~expected:
      [
        "log_path", `String "/log";
        "source_paths", `List [`String "/source"];
        "global_root", `String "/project";
        "excludes", `List [];
        "checked_directory_allowlist", `List [];
        "checked_directory_blocklist", `List [];
        "extensions", `List [];
        "debug", `Bool false;
        "strict", `Bool false;
        "show_error_traces", `Bool false;
        "store_type_check_resolution", `Bool false;
        "critical_files", `List [];
        "parallel", `Bool false;
        "number_of_workers", `Int 1;
      ];

  (* Individual values *)
  assert_parsed
    (Format.sprintf
       {|
          {
            %s,
            "excludes": ["/excludes"],
            "checked_directory_allowlist": ["/allows"],
            "checked_directory_blocklist": ["/blocks"],
            "extensions": [".typsy"],
            "taint_model_paths": ["/taint/model"]
          }
       |}
       mandatory_fileds)
    ~expected:
      [
        "excludes", `List [`String "/excludes"];
        "checked_directory_allowlist", `List [`String "/allows"];
        "checked_directory_blocklist", `List [`String "/blocks"];
        "extensions", `List [`String ".typsy"];
        "taint_model_paths", `List [`String "/taint/model"];
      ];
  assert_parsed
    (Format.sprintf
       {|
          {
            %s,
            "debug": true,
            "strict": true,
            "show_error_traces": true,
            "store_type_check_resolution": true,
            "critical_files": [
              { "base_name": "foo.py" },
              { "full_path": "/home/bar.txt" }
            ]
          }
       |}
       mandatory_fileds)
    ~expected:
      [
        "debug", `Bool true;
        "strict", `Bool true;
        "show_error_traces", `Bool true;
        "store_type_check_resolution", `Bool true;
        ( "critical_files",
          `List
            [`Assoc ["base_name", `String "foo.py"]; `Assoc ["full_path", `String "/home/bar.txt"]]
        );
      ];
  assert_parsed
    (Format.sprintf
       {|
          {
            %s,
            "parallel": true,
            "number_of_workers": 20
          }
       |}
       mandatory_fileds)
    ~expected:["parallel", `Bool true; "number_of_workers", `Int 20];
  assert_parsed
    (Format.sprintf
       {|
          {
            %s,
            "local_root": "/project/local",
            "watchman_root": "/project"
          }
       |}
       mandatory_fileds)
    ~expected:["local_root", `String "/project/local"; "watchman_root", `String "/project"];
  assert_not_parsed
    (Format.sprintf
       {|
          {
            %s,
            "saved_state_action": "derp"
          }
       |}
       mandatory_fileds);
  assert_parsed
    (Format.sprintf
       {|
          {
            %s,
            "saved_state_action": [
              "load_from_file",
              {
                "shared_memory_path": "/some/path"
              }
            ]
          }
       |}
       mandatory_fileds)
    ~expected:
      [
        ( "saved_state_action",
          `List [`String "load_from_file"; `Assoc ["shared_memory_path", `String "/some/path"]] );
      ];
  assert_parsed
    (Format.sprintf
       {|
          {
            %s,
            "saved_state_action": [
              "load_from_file",
              {
                "shared_memory_path": "/some/path",
                "changed_files_path": "/some/other/path"
              }
            ]
          }
       |}
       mandatory_fileds)
    ~expected:
      [
        ( "saved_state_action",
          `List
            [
              `String "load_from_file";
              `Assoc
                [
                  "shared_memory_path", `String "/some/path";
                  "changed_files_path", `String "/some/other/path";
                ];
            ] );
      ];
  assert_parsed
    (Format.sprintf
       {|
          {
            %s,
            "saved_state_action": [
              "load_from_project",
              {
                "project_name": "my_project"
              }
            ]
          }
       |}
       mandatory_fileds)
    ~expected:
      [
        ( "saved_state_action",
          `List [`String "load_from_project"; `Assoc ["project_name", `String "my_project"]] );
      ];
  assert_parsed
    (Format.sprintf
       {|
          {
            %s,
            "saved_state_action": [
              "load_from_project",
              {
                "project_name": "my_project",
                "project_metadata": "my_metadata"
              }
            ]
          }
       |}
       mandatory_fileds)
    ~expected:
      [
        ( "saved_state_action",
          `List
            [
              `String "load_from_project";
              `Assoc
                ["project_name", `String "my_project"; "project_metadata", `String "my_metadata"];
            ] );
      ];
  ()


let test_critical_files context =
  let base_name name = ServerConfiguration.CriticalFile.BaseName name in
  let full_path path = ServerConfiguration.CriticalFile.FullPath !path in
  let assert_find ~expected ~critical_files paths =
    let actual = ServerConfiguration.CriticalFile.find critical_files ~within:paths in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: Path.t option]
      ~printer:(fun result -> [%sexp_of: Path.t option] result |> Sexp.to_string_hum)
      expected
      actual
  in
  assert_find ~critical_files:[] ~expected:None [];
  assert_find ~critical_files:[base_name "a.py"] ~expected:None [];
  assert_find ~critical_files:[base_name "a.py"] ~expected:None [!"b.py"];
  assert_find ~critical_files:[base_name "a.py"] ~expected:(Some !"a.py") [!"a.py"];
  assert_find ~critical_files:[base_name "a.py"] ~expected:(Some !"foo/a.py") [!"foo/a.py"];
  assert_find ~critical_files:[full_path "a.py"] ~expected:None [!"/foo/a.py"];
  assert_find ~critical_files:[full_path "/foo/a.py"] ~expected:(Some !"/foo/a.py") [!"/foo/a.py"];
  assert_find
    ~critical_files:[base_name "a.py"]
    ~expected:(Some !"/foo/bar/a.py")
    [!"/foo/bar/a.py"];
  assert_find ~critical_files:[full_path "/bar/a.py"] ~expected:None [!"/foo/bar/a.py"];
  assert_find
    ~critical_files:[full_path "/foo/bar/a.py"]
    ~expected:(Some !"/foo/bar/a.py")
    [!"/foo/bar/a.py"];
  assert_find
    ~critical_files:[base_name "b.py"]
    ~expected:(Some !"foo/b.py")
    [!"foo/a.py"; !"foo/b.py"];
  assert_find ~critical_files:[full_path "b.py"] ~expected:None [!"foo/a.py"; !"foo/b.py"];
  assert_find
    ~critical_files:[full_path "foo/b.py"]
    ~expected:(Some !"foo/b.py")
    [!"foo/a.py"; !"foo/b.py"];
  assert_find ~critical_files:[base_name "a.py"] ~expected:None [!"a/foo.py"; !"/b/a/foo.py"];
  assert_find ~critical_files:[base_name "a.py"] ~expected:(Some !"a.py") [!"a.py"; !"b.py"];
  assert_find ~critical_files:[base_name "a.py"] ~expected:(Some !"a.py") [!"b.py"; !"a.py"];
  assert_find ~critical_files:[base_name "a.py"; base_name "b.py"] ~expected:None [];
  assert_find ~critical_files:[base_name "a.py"; base_name "b.py"] ~expected:None [!"c.py"];
  assert_find
    ~critical_files:[base_name "a.py"; base_name "b.py"]
    ~expected:(Some !"b.py")
    [!"b.py"; !"c.py"];
  assert_find
    ~critical_files:[base_name "a.py"; base_name "b.py"]
    ~expected:(Some !"b.py")
    [!"c.py"; !"b.py"];
  assert_find
    ~critical_files:[base_name "a.py"; base_name "b.py"]
    ~expected:(Some !"foo/b.py")
    [!"foo/c.py"; !"d.py"; !"foo/b.py"];
  assert_find
    ~critical_files:[base_name "a.py"; full_path "b.py"]
    ~expected:None
    [!"foo/c.py"; !"d.py"; !"foo/b.py"];
  assert_find
    ~critical_files:[base_name "a.py"; full_path "foo/b.py"]
    ~expected:(Some !"foo/b.py")
    [!"foo/c.py"; !"d.py"; !"foo/b.py"];

  assert_find
    ~critical_files:[base_name ".pyre_configuration"]
    ~expected:(Some !"foo/.pyre_configuration")
    [!"foo/a.py"; !"foo/.pyre_configuration.local"; !"foo/.pyre_configuration"];
  assert_find
    ~critical_files:[base_name ".pyre_configuration.local"]
    ~expected:(Some !"foo/.pyre_configuration.local")
    [!"foo/a.py"; !"foo/.pyre_configuration"; !"foo/.pyre_configuration.local"];
  ()


let () =
  "configuration"
  >::: ["json_parsing" >:: test_json_parsing; "critical_files" >:: test_critical_files]
  |> Test.run
