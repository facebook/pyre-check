(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre

let get_analysis_kind = function
  | "taint" -> Taint.Analysis.abstract_kind
  | "liveness" -> DeadStore.Analysis.abstract_kind
  | _ ->
      Log.error "Invalid analysis kind specified.";
      failwith "bad argument"


let run_analysis
    analysis
    result_json_path
    no_verify
    dump_call_graph
    repository_root
    rule_filter
    verbose
    expected_version
    sections
    debug
    strict
    show_error_traces
    infer
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
    taint_models_paths
    excludes
    extensions
    log_directory
    local_root
    ()
  =
  let filter_directories =
    filter_directories
    >>| String.split_on_chars ~on:[';']
    >>| List.map ~f:String.strip
    >>| List.map ~f:Path.create_absolute
  in
  let ignore_all_errors =
    ignore_all_errors
    >>| String.split_on_chars ~on:[';']
    >>| List.map ~f:String.strip
    >>| List.map ~f:Path.create_absolute
  in
  let repository_root = repository_root >>| Path.create_absolute in
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
      ~infer
      ~project_root:(Path.create_absolute project_root)
      ~parallel:(not sequential)
      ?filter_directories
      ?ignore_all_errors
      ~number_of_workers
      ~search_path:(List.map search_path ~f:SearchPath.create)
      ~taint_model_paths:(List.map taint_models_paths ~f:Path.create_absolute)
      ~excludes
      ~extensions
      ?log_directory
      ~local_root:(Path.create_absolute local_root)
      ()
  in
  let result_json_path = result_json_path >>| Path.create_absolute ~follow_symbolic_links:false in
  let () =
    match result_json_path with
    | Some path when not (Path.is_directory path) ->
        Log.error "--save-results-to path must be a directory.";
        failwith "bad argument"
    | _ -> ()
  in
  (fun () ->
    let timer = Timer.start () in
    let scheduler = Scheduler.create ~configuration () in
    (* In order to save time, sanity check models before starting the analysis. *)
    Log.info "Verifying model syntax.";
    Taint.Model.get_model_sources ~paths:configuration.Configuration.Analysis.taint_model_paths
    |> List.iter ~f:(fun (path, source) -> Taint.Model.verify_model_syntax ~path ~source);
    let environment, ast_environment, qualifiers =
      let configuration =
        (* In order to get an accurate call graph and type information, we need to ensure that we
           schedule a type check for external files. *)
        { configuration with analyze_external_sources = true }
      in
      Service.Check.check
        ~scheduler
        ~configuration
        ~call_graph_builder:(module Taint.CallGraphBuilder)
      |> fun { module_tracker; environment; ast_environment; _ } ->
      let qualifiers = Analysis.ModuleTracker.tracked_explicit_modules module_tracker in
      environment, Analysis.AstEnvironment.read_only ast_environment, qualifiers
    in
    let filename_lookup path_reference =
      match repository_root with
      | Some root ->
          Analysis.AstEnvironment.ReadOnly.get_real_path
            ~configuration
            ast_environment
            path_reference
          >>= Pyre.Path.follow_symbolic_link
          >>= fun path -> Pyre.Path.get_relative_to_root ~root ~path
      | None ->
          Analysis.AstEnvironment.ReadOnly.get_real_path_relative
            ~configuration
            ast_environment
            path_reference
    in
    let errors =
      Service.StaticAnalysis.analyze
        ~scheduler
        ~analysis_kind:(get_analysis_kind analysis)
        ~configuration:
          {
            Configuration.StaticAnalysis.configuration;
            result_json_path;
            dump_call_graph;
            verify_models = not no_verify;
            rule_filter;
          }
        ~filename_lookup
        ~environment:(Analysis.TypeEnvironment.read_only environment)
        ~qualifiers
        ()
    in
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
      ();

    (* Print results. *)
    List.map errors ~f:(fun error ->
        Interprocedural.Error.instantiate ~lookup:filename_lookup error
        |> Interprocedural.Error.Instantiated.to_json ~show_error_traces)
    |> (fun result -> Yojson.Safe.pretty_to_string (`List result))
    |> Log.print "%s")
  |> Scheduler.run_process ~configuration


let command =
  Command.basic_spec
    ~summary:"Runs a static analysis without a server (default)."
    Command.Spec.(
      empty
      +> flag "-analysis" (optional_with_default "taint" string) ~doc:"Type of analysis to run."
      +> flag
           "-save-results-to"
           (optional string)
           ~doc:"file A JSON file that Pyre Analyze will save its' results to."
      +> flag
           "-no-verify"
           no_arg
           ~doc:"Do not verify that all models passed into the analysis are valid."
      +> flag "-dump-call-graph" no_arg ~doc:"Store call graph in .pyre/call_graph.json"
      +> flag
           "-repository-root"
           (optional string)
           ~doc:"The repository root to use for path relativization (set to local root if missing)."
      +> flag
           "-rules"
           (optional (Arg_type.comma_separated int))
           ~doc:"If set, filter the analysis to only consider the provided rule numbers."
      ++ Specification.base_command_line_arguments)
    run_analysis
