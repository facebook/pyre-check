(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

let run_lint
    _lints
    _verbose
    _expected_version
    _sections
    _debug
    _strict
    _show_error_traces
    _infer
    _sequential
    _filter_directories
    _ignore_all_errors
    _number_of_workers
    _log_identifier
    _logger
    _profiling_output
    _memory_profiling_output
    _project_root
    _search_path
    _taint_models_directory
    _excludes
    _extensions
    _log_directory
    _local_root
    ()
  =
  print_endline "Coming soon..."


let command =
  Command.basic_spec
    ~summary:"Runs static analysis"
    Command.Spec.(
      empty
      +> flag
           "-lints"
           (optional_with_default [] (Arg_type.comma_separated string))
           ~doc:"The list of lints to run"
      ++ Specification.base_command_line_arguments)
    run_lint
