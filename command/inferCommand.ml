(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Analysis
open Pyre
open Service

let run_infer
    ignore_infer
    _verbose
    expected_version
    sections
    debug
    strict
    show_error_traces
    sequential
    filter_directories
    ignore_all_errors
    number_of_workers
    log_identifier
    logger
    profiling_output
    memory_profiling_output
    project_root
    search_path
    _taint_models_directory
    excludes
    extensions
    log_directory
    local_root
    ()
  =
  let local_root = Path.create_absolute local_root in
  Log.GlobalState.initialize ~debug ~sections;
  Statistics.GlobalState.initialize ~log_identifier ?logger ~project_name:(Path.last local_root) ();
  Profiling.initialize ~profiling_output ~memory_profiling_output ();
  let argument_to_paths argument =
    argument
    >>| String.split_on_chars ~on:[';']
    >>| List.map ~f:String.strip
    >>| List.map ~f:Path.create_absolute
  in
  let ignore_infer = argument_to_paths ignore_infer in
  let filter_directories = argument_to_paths filter_directories in
  let ignore_all_errors = argument_to_paths ignore_all_errors in
  let configuration =
    Configuration.Analysis.create
      ?expected_version
      ~debug
      ~strict
      ~show_error_traces
      ~infer:true
      ~project_root:(Path.create_absolute project_root)
      ~parallel:(not sequential)
      ?filter_directories
      ?ignore_all_errors
      ~number_of_workers
      ~search_path:(List.map search_path ~f:SearchPath.create)
      ~excludes
      ~extensions
      ?log_directory
      ?ignore_infer
      ~local_root
      ()
  in
  (fun () ->
    let scheduler = Scheduler.create ~configuration () in
    let errors, ast_environment =
      let { Infer.errors; ast_environment; _ } = Infer.infer ~configuration ~scheduler () in
      errors, ast_environment
    in
    if debug then
      Memory.report_statistics ();

    (* Print results. *)
    let errors =
      let ast_environment = AstEnvironment.read_only ast_environment in
      List.map
        errors
        ~f:
          (InferenceError.instantiate
             ~lookup:(AstEnvironment.ReadOnly.get_real_path_relative ~configuration ast_environment))
    in
    Yojson.Safe.to_string
      (`Assoc
        [
          ( "errors",
            `List
              (List.map
                 ~f:(fun error -> InferenceError.Instantiated.to_json ~show_error_traces error)
                 errors) );
        ])
    |> Log.print "%s")
  |> Scheduler.run_process


let infer_command =
  Command.basic_spec
    ~summary:"Runs type inference."
    Command.Spec.(
      empty
      +> flag "-ignore-infer" (optional string) ~doc:"Will not infer the listed files."
      ++ Specification.base_command_line_arguments)
    run_infer
