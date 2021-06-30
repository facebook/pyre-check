(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Commands.NewInfer
module Path = Pyre.Path

let test_json_parsing context =
  let assert_parsed ~expected json_string =
    let json = Yojson.Safe.from_string json_string in
    match InferConfiguration.of_yojson json with
    | Result.Error message ->
        let message = Format.sprintf "Unexpected JSON parsing failure: %s" message in
        assert_failure message
    | Result.Ok actual ->
        assert_equal
          ~ctxt:context
          ~cmp:[%compare.equal: InferConfiguration.t]
          ~printer:(fun result -> Sexp.to_string ([%sexp_of: InferConfiguration.t] result))
          expected
          actual
  in

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
        "ignore_infer": ["/ignored"],
        "extensions": [".typsy"],
        "infer_mode": ["Local"],
        "debug": true,
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
        "profiling_output": "/output0",
        "memory_profiling_output": "/output1",
        "remote_logging": { "logger": "/bin/logger", "identifier": "foo" }
      }
    |}
    ~expected:
      {
        InferConfiguration.source_paths =
          Configuration.SourcePaths.Simple
            [SearchPath.create "/source0"; SearchPath.create "/source1"];
        search_paths = [SearchPath.create "/search"];
        excludes = ["/excludes"];
        checked_directory_allowlist = [Path.create_absolute "/allows"];
        checked_directory_blocklist = [Path.create_absolute "/blocks"];
        ignore_infer = [Path.create_absolute "/ignored"];
        extensions = [Configuration.Extension.create_extension ".typsy"];
        log_path = Path.create_absolute "/log";
        global_root = Path.create_absolute "/project";
        local_root = Some (Path.create_absolute "/project/local");
        infer_mode = InferMode.Local;
        debug = true;
        python_version = { Configuration.PythonVersion.major = 3; minor = 7; micro = 4 };
        parallel = true;
        number_of_workers = 20;
        shared_memory =
          {
            Configuration.SharedMemory.heap_size = 42;
            dependency_table_power = 10;
            hash_table_power = 12;
          };
        remote_logging =
          Some { Configuration.RemoteLogging.logger = "/bin/logger"; identifier = "foo" };
        profiling_output = Some "/output0";
        memory_profiling_output = Some "/output1";
      };
  ()


let () = "configuration" >::: ["json_parsing" >:: test_json_parsing] |> Test.run
