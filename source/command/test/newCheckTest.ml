(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Commands.NewCheck
module Path = Pyre.Path

let test_json_parsing context =
  let assert_parsed ~expected json_string =
    let json = Yojson.Safe.from_string json_string in
    match CheckConfiguration.of_yojson json with
    | Result.Error message ->
        let message = Format.sprintf "Unexpected JSON parsing failure: %s" message in
        assert_failure message
    | Result.Ok actual ->
        assert_equal
          ~ctxt:context
          ~cmp:[%compare.equal: CheckConfiguration.t]
          ~printer:(fun result -> Sexp.to_string ([%sexp_of: CheckConfiguration.t] result))
          expected
          actual
  in
  let default_configuration =
    {
      CheckConfiguration.source_paths = Configuration.SourcePaths.Simple [];
      search_paths = [];
      excludes = [];
      checked_directory_allowlist = [];
      checked_directory_blocklist = [];
      extensions = [];
      log_path = Path.create_absolute "/log";
      global_root = Path.create_absolute "/project";
      local_root = None;
      debug = false;
      strict = false;
      python_version = Configuration.PythonVersion.default;
      show_error_traces = false;
      parallel = false;
      number_of_workers = 1;
      shared_memory = Configuration.SharedMemory.default;
      additional_logging_sections = [];
      remote_logging = None;
      profiling_output = None;
      memory_profiling_output = None;
    }
  in
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
        default_configuration with
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
        default_configuration with
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
  (* Test non-mandatory fields *)
  assert_parsed
    {|
      {
        "log_path": "/log",
        "source_paths": { "kind": "simple", "paths": ["/source0", "/source1"] },
        "search_paths": ["/search"],
        "global_root": "/project",
        "local_root": "/project/local",
        "excludes": ["/excludes"],
        "checked_directory_allowlist": ["/allows"],
        "checked_directory_blocklist": ["/blocks"],
        "extensions": [".typsy"],
        "debug": true,
        "strict": true,
        "python_version": {
            "major": 3,
            "minor": 7,
            "micro": 4
        },
        "shared_memory": {
            "heap_size": 42,
            "dependency_table_power": 10,
            "hash_table_power": 12
        },
        "show_error_traces": true,
        "parallel": true,
        "number_of_workers": 20,
        "additional_logging_sections": ["foo", "bar"],
        "profiling_output": "/output0",
        "memory_profiling_output": "/output1",
        "remote_logging": { "logger": "/bin/logger", "identifier": "foo" }
      }
    |}
    ~expected:
      {
        CheckConfiguration.source_paths =
          Configuration.SourcePaths.Simple
            [SearchPath.create "/source0"; SearchPath.create "/source1"];
        search_paths = [SearchPath.create "/search"];
        excludes = ["/excludes"];
        checked_directory_allowlist = [Path.create_absolute "/allows"];
        checked_directory_blocklist = [Path.create_absolute "/blocks"];
        extensions = [Configuration.Extension.create_extension ".typsy"];
        log_path = Path.create_absolute "/log";
        global_root = Path.create_absolute "/project";
        local_root = Some (Path.create_absolute "/project/local");
        debug = true;
        strict = true;
        python_version = { Configuration.PythonVersion.major = 3; minor = 7; micro = 4 };
        show_error_traces = true;
        parallel = true;
        number_of_workers = 20;
        shared_memory =
          {
            Configuration.SharedMemory.heap_size = 42;
            dependency_table_power = 10;
            hash_table_power = 12;
          };
        additional_logging_sections = ["foo"; "bar"];
        remote_logging =
          Some { Configuration.RemoteLogging.logger = "/bin/logger"; identifier = "foo" };
        profiling_output = Some "/output0";
        memory_profiling_output = Some "/output1";
      };
  ()


let () = "configuration" >::: ["json_parsing" >:: test_json_parsing] |> Test.run
