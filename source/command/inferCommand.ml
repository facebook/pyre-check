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

let argument_to_paths argument =
  argument
  >>| String.split_on_chars ~on:[';']
  >>| List.map ~f:String.strip
  >>| List.map ~f:(Path.create_absolute ~follow_symbolic_links:true)


let run_infer_interprocedural
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
      ~project_name:(Path.last local_root)
      ~project_root
      ();
    Profiling.GlobalState.initialize ?profiling_output ?memory_profiling_output ();
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
        ~project_root:(Path.create_absolute ~follow_symbolic_links:true project_root)
        ~parallel:(not sequential)
        ?filter_directories
        ?ignore_all_errors
        ~number_of_workers
        ~search_path:(List.map search_path ~f:SearchPath.create_normalized)
        ~excludes
        ~extensions:(List.map ~f:Configuration.Extension.create_extension extensions)
        ?log_directory
        ?ignore_infer
        ?python_major_version
        ?python_minor_version
        ?python_micro_version
        ?shared_memory_heap_size
        ?shared_memory_dependency_table_power
        ?shared_memory_hash_table_power
        ~local_root
        ~source_path:(List.map source_path ~f:SearchPath.create_normalized)
        ()
    in
    let static_analysis_configuration =
      {
        Configuration.StaticAnalysis.configuration;
        result_json_path = None;
        dump_call_graph = false;
        verify_models = false;
        rule_filter = None;
        find_missing_flows = None;
        dump_model_query_results = false;
        use_cache = false;
        maximum_trace_length = None;
        maximum_tito_depth = None;
      }
    in
    let analysis_kind = TypeInference.Analysis.abstract_kind in
    (fun () ->
      let timer = Timer.start () in
      Scheduler.with_scheduler ~configuration ~f:(fun scheduler ->
          Interprocedural.Analysis.initialize_configuration
            ~static_analysis_configuration
            analysis_kind;
          let environment = StaticAnalysis.type_check ~scheduler ~configuration ~use_cache:false in
          let qualifiers =
            Analysis.TypeEnvironment.module_tracker environment
            |> Analysis.ModuleTracker.tracked_explicit_modules
          in
          let environment = Analysis.TypeEnvironment.read_only environment in
          let ast_environment = Analysis.TypeEnvironment.ReadOnly.ast_environment environment in
          let initial_callables =
            StaticAnalysis.fetch_initial_callables
              ~scheduler
              ~configuration
              ~environment
              ~qualifiers
              ~use_cache:false
          in
          let filename_lookup path_reference =
            Analysis.AstEnvironment.ReadOnly.get_real_path_relative
              ~configuration
              ast_environment
              path_reference
          in
          StaticAnalysis.analyze
            ~scheduler
            ~analysis:analysis_kind
            ~static_analysis_configuration
            ~filename_lookup
            ~environment
            ~qualifiers
            ~initial_callables
            ~initial_models:Interprocedural.Callable.Map.empty
            ~skip_overrides:Ast.Reference.Set.empty
            ();
          let { Caml.Gc.minor_collections; major_collections; compactions; _ } = Caml.Gc.stat () in
          Statistics.performance
            ~name:"analyze"
            ~timer
            ~integers:
              [
                "gc_minor_collections", minor_collections;
                "gc_major_collections", major_collections;
                "gc_compactions", compactions;
              ]
            ()))
    |> Scheduler.run_process
  with
  | error ->
      Log.log_exception error;
      raise error


let run_infer_local
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
      ~project_name:(Path.last local_root)
      ~project_root
      ();
    Profiling.GlobalState.initialize ?profiling_output ?memory_profiling_output ();
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
        ~project_root:(Path.create_absolute ~follow_symbolic_links:true project_root)
        ~parallel:(not sequential)
        ?filter_directories
        ?ignore_all_errors
        ~number_of_workers
        ~search_path:(List.map search_path ~f:SearchPath.create_normalized)
        ~excludes
        ~extensions:(List.map ~f:Configuration.Extension.create_extension extensions)
        ?log_directory
        ?ignore_infer
        ?python_major_version
        ?python_minor_version
        ?python_micro_version
        ?shared_memory_heap_size
        ?shared_memory_dependency_table_power
        ?shared_memory_hash_table_power
        ~local_root
        ~source_path:(List.map source_path ~f:SearchPath.create_normalized)
        ()
    in
    (fun () ->
      let result =
        Scheduler.with_scheduler ~configuration ~f:(fun scheduler ->
            Infer.infer_v2 ~configuration ~scheduler ())
      in
      if debug then
        Memory.report_statistics ();

      (* Print results. *)
      Yojson.Safe.pretty_to_string (`List [TypeInference.Data.GlobalResult.to_yojson result])
      |> Log.print "%s")
    |> Scheduler.run_process
  with
  | error ->
      Log.log_exception error;
      raise error


let run_infer_legacy
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
      ~project_name:(Path.last local_root)
      ~project_root
      ();
    Profiling.GlobalState.initialize ?profiling_output ?memory_profiling_output ();
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
        ~project_root:(Path.create_absolute ~follow_symbolic_links:true project_root)
        ~parallel:(not sequential)
        ?filter_directories
        ?ignore_all_errors
        ~number_of_workers
        ~search_path:(List.map search_path ~f:SearchPath.create_normalized)
        ~excludes
        ~extensions:(List.map ~f:Configuration.Extension.create_extension extensions)
        ?log_directory
        ?ignore_infer
        ?python_major_version
        ?python_minor_version
        ?python_micro_version
        ?shared_memory_heap_size
        ?shared_memory_dependency_table_power
        ?shared_memory_hash_table_power
        ~local_root
        ~source_path:(List.map source_path ~f:SearchPath.create_normalized)
        ()
    in
    (fun () ->
      let errors, ast_environment =
        Scheduler.with_scheduler ~configuration ~f:(fun scheduler ->
            let { Infer.errors; ast_environment; _ } = Infer.infer ~configuration ~scheduler () in
            errors, ast_environment)
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
               ~show_error_traces
               ~lookup:
                 (AstEnvironment.ReadOnly.get_real_path_relative ~configuration ast_environment))
      in
      Yojson.Safe.to_string
        (`Assoc
          [
            ( "errors",
              `List (List.map ~f:(fun error -> InferenceError.Instantiated.to_yojson error) errors)
            );
          ])
      |> Log.print "%s")
    |> Scheduler.run_process
  with
  | error ->
      Log.log_exception error;
      raise error


let run_infer infer_mode =
  match infer_mode with
  | None -> run_infer_legacy
  | Some "interprocedural" -> run_infer_interprocedural
  | Some "local" -> run_infer_local
  | Some unknown_mode -> failwith (Format.asprintf "Unknown infer mode \"%s\"" unknown_mode)


let infer_command =
  Command.basic_spec
    ~summary:"Runs type inference."
    Command.Spec.(
      empty
      +> flag "-infer-mode" (optional string) ~doc:"Mode to use for type inference."
      +> flag "-ignore-infer" (optional string) ~doc:"Will not infer the listed files."
      ++ Specification.base_command_line_arguments)
    run_infer
