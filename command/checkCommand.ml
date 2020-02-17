(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Analysis
open Pyre
open Service

(* run_command prints out the errors, for a Check run *)
let run_check
    verbose
    expected_version
    sections
    debug
    strict
    show_error_traces
    _infer
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
  let argument_to_paths argument =
    argument
    >>| String.split_on_chars ~on:[';']
    >>| List.map ~f:String.strip
    >>| List.map ~f:Path.create_absolute
  in
  let filter_directories = argument_to_paths filter_directories in
  let ignore_all_errors = argument_to_paths ignore_all_errors in
  let configuration =
    Configuration.Analysis.create
      ~verbose
      ?expected_version
      ~sections
      ~debug
      ~strict
      ~show_error_traces
      ~log_identifier
      ?logger
      ?profiling_output
      ?memory_profiling_output
      ~infer:false
      ~project_root:(Path.create_absolute project_root)
      ~parallel:(not sequential)
      ?filter_directories
      ?ignore_all_errors
      ~number_of_workers
      ~search_path:(List.map search_path ~f:SearchPath.create)
      ~excludes
      ~extensions
      ?log_directory
      ~local_root:(Path.create_absolute local_root)
      ()
  in
  (fun () ->
    let scheduler = Scheduler.create ~configuration () in
    let errors, ast_environment =
      let timer = Timer.start () in
      let { Check.errors; ast_environment; _ } =
        Check.check
          ~scheduler
          ~configuration
          ~call_graph_builder:(module Analysis.Callgraph.DefaultBuilder)
      in
      let { Caml.Gc.minor_collections; major_collections; compactions; _ } = Caml.Gc.stat () in
      Statistics.performance
        ~name:"check"
        ~timer
        ~integers:
          [
            "gc_minor_collections", minor_collections;
            "gc_major_collections", major_collections;
            "gc_compactions", compactions;
          ]
        ~normals:["request kind", "FullCheck"]
        ();
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
          (AnalysisError.instantiate
             ~lookup:(AstEnvironment.ReadOnly.get_real_path_relative ~configuration ast_environment))
    in
    Yojson.Safe.to_string
      (`Assoc
        [
          ( "errors",
            `List
              (List.map
                 ~f:(fun error -> AnalysisError.Instantiated.to_json ~show_error_traces error)
                 errors) );
        ])
    |> Log.print "%s")
  |> Scheduler.run_process ~configuration


let check_command =
  Command.basic_spec
    ~summary:"Runs a full check without a server (default)"
    Command.Spec.(empty ++ Specification.base_command_line_arguments)
    run_check
