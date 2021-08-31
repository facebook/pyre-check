(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
module Path = Pyre.Path
module BaseConfiguration = Commands.BaseConfiguration

let dummy_base_json =
  [
    "log_path", `String "/log";
    "source_paths", `List [`String "/source"];
    "global_root", `String "/project";
  ]


let dummy_base_configuration =
  {
    BaseConfiguration.source_paths =
      Configuration.SourcePaths.Simple [SearchPath.Root (Path.create_absolute "/source")];
    search_paths = [];
    excludes = [];
    checked_directory_allowlist = [];
    checked_directory_blocklist = [];
    extensions = [];
    log_path = Path.create_absolute "/log";
    global_root = Path.create_absolute "/project";
    local_root = None;
    debug = false;
    python_version = Configuration.PythonVersion.default;
    parallel = false;
    number_of_workers = 1;
    shared_memory = Configuration.SharedMemory.default;
    remote_logging = None;
    profiling_output = None;
    memory_profiling_output = None;
  }


let test_json_parsing context =
  let assert_parsed ~expected json_string =
    let json = Yojson.Safe.from_string json_string in
    match BaseConfiguration.of_yojson json with
    | Result.Error message ->
        let message = Format.sprintf "Unexpected JSON parsing failure: %s" message in
        assert_failure message
    | Result.Ok actual ->
        assert_equal
          ~ctxt:context
          ~cmp:[%compare.equal: BaseConfiguration.t]
          ~printer:(fun result -> Sexp.to_string ([%sexp_of: BaseConfiguration.t] result))
          expected
          actual
  in
  let assert_not_parsed json_string =
    let json = Yojson.Safe.from_string json_string in
    match BaseConfiguration.of_yojson json with
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

  (* Test simple source path *)
  assert_parsed
    {|
      {
        "log_path": "/log",
        "source_paths": ["/source"],
        "global_root": "/project"
      }
    |}
    ~expected:
      {
        dummy_base_configuration with
        source_paths = Configuration.SourcePaths.Simple [SearchPath.create "/source"];
      };
  (* Test buck source path *)
  assert_parsed
    {|
      {
        "log_path": "/log",
        "source_paths": {
            "kind": "buck",
            "targets": ["//my:target"],
            "mode": "@mode/opt",
            "isolation_prefix": "prefix",
            "source_root": "/buck/root",
            "artifact_root": "/build/root"
        },
        "global_root": "/project"
      }
    |}
    ~expected:
      {
        dummy_base_configuration with
        source_paths =
          Configuration.SourcePaths.Buck
            {
              Configuration.Buck.targets = ["//my:target"];
              mode = Some "@mode/opt";
              isolation_prefix = Some "prefix";
              source_root = Path.create_absolute "/buck/root";
              artifact_root = Path.create_absolute "/build/root";
            };
      };

  assert_parsed
    {|
          {
            "log_path": "/log",
            "source_paths": ["/source"],
            "global_root": "/project",
            "local_root": "/project/local",
            "excludes": ["/excludes"],
            "checked_directory_allowlist": ["/allows"],
            "checked_directory_blocklist": ["/blocks"],
            "extensions": [".typsy"]
          }
       |}
    ~expected:
      {
        dummy_base_configuration with
        local_root = Some (Path.create_absolute "/project/local");
        excludes = ["/excludes"];
        checked_directory_allowlist = [Path.create_absolute "/allows"];
        checked_directory_blocklist = [Path.create_absolute "/blocks"];
        extensions =
          [
            { Configuration.Extension.suffix = ".typsy"; include_suffix_in_module_qualifier = false };
          ];
      };

  assert_parsed
    {|
          {
            "log_path": "/log",
            "source_paths": ["/source"],
            "global_root": "/project",
            "debug": true,
            "python_version": {
              "major": 3,
              "minor": 7,
              "micro": 4
            }
          }
       |}
    ~expected:
      {
        dummy_base_configuration with
        debug = true;
        python_version = { Configuration.PythonVersion.major = 3; minor = 7; micro = 4 };
      };

  assert_parsed
    {|
          {
            "log_path": "/log",
            "source_paths": ["/source"],
            "global_root": "/project",
            "parallel": true,
            "number_of_workers": 20
          }
       |}
    ~expected:{ dummy_base_configuration with parallel = true; number_of_workers = 20 };

  assert_parsed
    {|
          {
            "log_path": "/log",
            "source_paths": ["/source"],
            "global_root": "/project",
            "parallel": true,
            "number_of_workers": 20
          }
       |}
    ~expected:{ dummy_base_configuration with parallel = true; number_of_workers = 20 };

  assert_parsed
    {|
          {
            "log_path": "/log",
            "source_paths": ["/source"],
            "global_root": "/project",
            "profiling_output": "/output0",
            "memory_profiling_output": "/output1"
          }
       |}
    ~expected:
      {
        dummy_base_configuration with
        profiling_output = Some "/output0";
        memory_profiling_output = Some "/output1";
      };

  assert_parsed
    {|
          {
            "log_path": "/log",
            "source_paths": ["/source"],
            "global_root": "/project",
            "remote_logging": { "logger": "/bin/logger", "identifier": "foo" }
          }
       |}
    ~expected:
      {
        dummy_base_configuration with
        remote_logging =
          Some { Configuration.RemoteLogging.logger = "/bin/logger"; identifier = "foo" };
      };
  ()


let () = "base_configuration" >::: ["json_parsing" >:: test_json_parsing] |> Test.run
