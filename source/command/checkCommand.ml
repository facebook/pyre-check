(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Analysis
open Pyre
open Service

(* run_command prints out the errors, for a Check run *)
let run_check
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
    source_path
    search_path
    _taint_models_directory
    excludes
    extensions
    log_directory
    python_major_version
    python_minor_version
    python_micro_version
    shared_memory_heap_size
    shared_memory_dependency_table_power
    shared_memory_hash_table_power
    local_root
    ()
  =
  try
    let source_path = Option.value source_path ~default:[local_root] in
    let local_root = SearchPath.create local_root |> SearchPath.get_root in
    Log.GlobalState.initialize ~debug ~sections;
    Statistics.GlobalState.initialize
      ~log_identifier
      ?logger
      ~project_root
      ~project_name:(Path.last local_root)
      ();
    Profiling.GlobalState.initialize ?profiling_output ?memory_profiling_output ();
    let argument_to_paths argument =
      argument
      >>| String.split_on_chars ~on:[';']
      >>| List.map ~f:String.strip
      >>| List.map ~f:(Path.create_absolute ~follow_symbolic_links:true)
    in
    let filter_directories = argument_to_paths filter_directories in
    let ignore_all_errors = argument_to_paths ignore_all_errors in
    let configuration =
      Configuration.Analysis.create
        ?expected_version
        ~debug
        ~strict
        ~show_error_traces
        ~project_root:(Path.create_absolute ~follow_symbolic_links:true project_root)
        ~parallel:(not sequential)
        ?filter_directories
        ?ignore_all_errors
        ~number_of_workers
        ~search_path:(List.map search_path ~f:SearchPath.create_normalized)
        ~excludes
        ~extensions:(List.map ~f:Configuration.Extension.create_extension extensions)
        ?log_directory
        ~local_root
        ?python_major_version
        ?python_minor_version
        ?python_micro_version
        ?shared_memory_heap_size
        ?shared_memory_dependency_table_power
        ?shared_memory_hash_table_power
        ~source_path:(List.map source_path ~f:SearchPath.create_normalized)
        ()
    in
    (fun () ->
      let errors, ast_environment =
        Scheduler.with_scheduler ~configuration ~f:(fun scheduler ->
            let timer = Timer.start () in
            let { Check.errors; environment } =
              Check.check
                ~scheduler
                ~configuration
                ~call_graph_builder:(module Analysis.Callgraph.DefaultBuilder)
            in
            let { Caml.Gc.minor_collections; major_collections; compactions; _ } =
              Caml.Gc.stat ()
            in
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
            errors, TypeEnvironment.ast_environment environment)
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
               ~show_error_traces
               ~lookup:
                 (AstEnvironment.ReadOnly.get_real_path_relative ~configuration ast_environment))
      in
      Yojson.Safe.to_string
        (`Assoc
          [
            ( "errors",
              `List (List.map ~f:(fun error -> AnalysisError.Instantiated.to_yojson error) errors) );
          ])
      |> Log.print "%s")
    |> Scheduler.run_process
  with
  | error ->
      Log.log_exception error;
      raise error


let check_command =
  Command.basic_spec
    ~summary:"Runs a full check without a server (default)"
    Command.Spec.(empty ++ Specification.base_command_line_arguments)
    run_check
