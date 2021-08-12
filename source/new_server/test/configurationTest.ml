(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Newserver

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
  assert_not_parsed {| { "source_paths": {} } |};
  assert_not_parsed {| { "source_paths": { "kind": 42 } } |};
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
        "source_paths", `Assoc ["kind", `String "simple"; "paths", `List [`String "/source"]];
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
        ( "extensions",
          `List
            [`Assoc ["suffix", `String ".typsy"; "include_suffix_in_module_qualifier", `Bool false]]
        );
        "taint_model_paths", `List [`String "/taint/model"];
      ];
  assert_parsed
    (Format.sprintf
       {|
          {
            %s,
            "debug": true,
            "strict": true,
            "python_version": {
              "major": 3,
              "minor": 7,
              "micro": 4
            },
            "show_error_traces": true,
            "store_type_check_resolution": true,
            "critical_files": [
              { "base_name": "foo.py" },
              { "extension": "derp" },
              { "full_path": "/home/bar.txt" }
            ]
          }
       |}
       mandatory_fileds)
    ~expected:
      [
        "debug", `Bool true;
        "strict", `Bool true;
        "python_version", `Assoc ["major", `Int 3; "minor", `Int 7; "micro", `Int 4];
        "show_error_traces", `Bool true;
        "store_type_check_resolution", `Bool true;
        ( "critical_files",
          `List
            [
              `Assoc ["base_name", `String "foo.py"];
              `Assoc ["extension", `String "derp"];
              `Assoc ["full_path", `String "/home/bar.txt"];
            ] );
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
  assert_parsed
    (Format.sprintf
       {|
          {
            %s,
            "additional_logging_sections": ["foo", "bar"],
            "profiling_output": "/output0",
            "memory_profiling_output": "/output1"
          }
       |}
       mandatory_fileds)
    ~expected:
      [
        "additional_logging_sections", `List [`String "foo"; `String "bar"];
        "profiling_output", `String "/output0";
        "memory_profiling_output", `String "/output1";
      ];
  assert_not_parsed
    (Format.sprintf
       {|
          {
            %s,
            "additional_logging_sections": "derp"
          }
       |}
       mandatory_fileds);
  assert_parsed
    (Format.sprintf
       {|
          {
            %s,
            "remote_logging": { "logger": "/bin/logger", "identifier": "foo" }
          }
       |}
       mandatory_fileds)
    ~expected:
      ["remote_logging", `Assoc ["logger", `String "/bin/logger"; "identifier", `String "foo"]];
  assert_not_parsed
    (Format.sprintf
       {|
          {
            %s,
            "remote_logging": "derp"
          }
       |}
       mandatory_fileds);
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
              "save_to_file",
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
          `List [`String "save_to_file"; `Assoc ["shared_memory_path", `String "/some/path"]] );
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

  (* Specify source paths with `simple` *)
  let mandatory_fileds = {|
    "log_path": "/log",
    "global_root": "/project"
  |} in
  assert_parsed
    (Format.sprintf
       {|
          {
            %s,
            "source_paths": {
              "kind": "simple",
              "paths": ["/source/path0", "/source/path1"]
            }
          }
       |}
       mandatory_fileds)
    ~expected:
      [
        ( "source_paths",
          `Assoc
            [
              "kind", `String "simple";
              "paths", `List [`String "/source/path0"; `String "/source/path1"];
            ] );
      ];

  (* Specify source paths with `buck` *)
  assert_parsed
    (Format.sprintf
       {|
          {
            %s,
            "source_paths": {
              "kind": "buck",
              "source_root": "/buck/root",
              "artifact_root": "/build/root"
            }
          }
       |}
       mandatory_fileds)
    ~expected:
      [
        ( "source_paths",
          `Assoc
            [
              "kind", `String "buck";
              "targets", `List [];
              "source_root", `String "/buck/root";
              "artifact_root", `String "/build/root";
            ] );
      ];
  assert_parsed
    (Format.sprintf
       {|
          {
            %s,
            "source_paths": {
              "kind": "buck",
              "targets": ["//my:target"],
              "mode": "@mode/opt",
              "isolation_prefix": "prefix",
              "source_root": "/buck/root",
              "artifact_root": "/build/root"
            }
          }
       |}
       mandatory_fileds)
    ~expected:
      [
        ( "source_paths",
          `Assoc
            [
              "kind", `String "buck";
              "targets", `List [`String "//my:target"];
              "mode", `String "@mode/opt";
              "isolation_prefix", `String "prefix";
              "source_root", `String "/buck/root";
              "artifact_root", `String "/build/root";
            ] );
      ];
  ()


let () = "configuration" >::: ["json_parsing" >:: test_json_parsing] |> Test.run
