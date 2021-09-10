(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
module Path = PyrePath

(* Infer command uses the same exit code scheme as check command. *)
module ExitStatus = NewCheckCommand.ExitStatus

module InferMode = struct
  type t =
    | Local
    | Interprocedural
  [@@deriving sexp, compare, hash, yojson]
end

module InferConfiguration = struct
  type file_list = string list option [@@deriving yojson, sexp, compare, hash]

  type t = {
    base: NewCommandStartup.BaseConfiguration.t;
    paths_to_modify: file_list;
    infer_mode: InferMode.t;
  }
  [@@deriving sexp, compare, hash]

  let of_yojson json =
    let open Yojson.Safe.Util in
    (* Parsing logic *)
    try
      match NewCommandStartup.BaseConfiguration.of_yojson json with
      | Result.Error _ as error -> error
      | Result.Ok base ->
          let paths_to_modify =
            json |> member "paths_to_modify" |> file_list_of_yojson |> Result.ok_or_failwith
          in
          let infer_mode =
            json |> member "infer_mode" |> InferMode.of_yojson |> Result.ok_or_failwith
          in
          Result.Ok { base; infer_mode; paths_to_modify }
    with
    | Type_error (message, _)
    | Undefined (message, _) ->
        Result.Error message
    | other_exception -> Result.Error (Exn.to_string other_exception)


  let analysis_configuration_of
      {
        base =
          {
            NewCommandStartup.BaseConfiguration.source_paths;
            search_paths;
            excludes;
            checked_directory_allowlist;
            checked_directory_blocklist;
            extensions;
            log_path;
            global_root;
            local_root;
            debug;
            python_version = { Configuration.PythonVersion.major; minor; micro };
            parallel;
            number_of_workers;
            shared_memory =
              { Configuration.SharedMemory.heap_size; dependency_table_power; hash_table_power };
            remote_logging = _;
            profiling_output = _;
            memory_profiling_output = _;
          };
        _;
      }
    =
    let source_path =
      match source_paths with
      | Configuration.SourcePaths.Simple source_paths -> source_paths
      | Buck { Configuration.Buck.artifact_root; _ } -> [SearchPath.Root artifact_root]
    in
    Configuration.Analysis.create
      ~parallel
      ~analyze_external_sources:false
      ~filter_directories:checked_directory_allowlist
      ~ignore_all_errors:checked_directory_blocklist
      ~number_of_workers
      ~local_root:(Option.value local_root ~default:global_root)
      ~project_root:global_root
      ~search_path:(List.map search_paths ~f:SearchPath.normalize)
      ~strict:false
      ~debug
      ~show_error_traces:false
      ~excludes
      ~extensions
      ~incremental_style:Configuration.Analysis.Shallow
      ~include_hints:false
      ~perform_autocompletion:false
      ~log_directory:(Path.absolute log_path)
      ~python_major_version:major
      ~python_minor_version:minor
      ~python_micro_version:micro
      ~shared_memory_heap_size:heap_size
      ~shared_memory_dependency_table_power:dependency_table_power
      ~shared_memory_hash_table_power:hash_table_power
      ~source_path
      ()
end

let run_infer_local ~configuration ~build_system ~paths_to_modify () =
  let result =
    Scheduler.with_scheduler ~configuration ~f:(fun scheduler ->
        let ({ Service.Infer.global_environment; _ } as environment_data) =
          Service.Infer.build_environment_data ~configuration ~scheduler ()
        in
        let filename_lookup qualifier =
          let ast_environment =
            Analysis.AnnotatedGlobalEnvironment.ReadOnly.ast_environment global_environment
          in
          Newserver.RequestHandler.instantiate_path
            ~build_system
            ~configuration
            ~ast_environment
            qualifier
        in
        Service.Infer.run_infer
          ~configuration
          ~scheduler
          ~filename_lookup
          ~paths_to_modify
          environment_data)
  in
  if configuration.debug then
    Memory.report_statistics ();
  Yojson.Safe.pretty_to_string (`List [TypeInference.Data.GlobalResult.to_yojson result])
  |> Log.print "%s";
  Lwt.return ExitStatus.Ok


let with_performance_tracking f =
  let timer = Timer.start () in
  let result = f () in
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
  result


let run_infer_interprocedural ~configuration ~build_system () =
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
  with_performance_tracking (fun () ->
      Scheduler.with_scheduler ~configuration ~f:(fun scheduler ->
          Interprocedural.FixpointAnalysis.initialize_configuration
            ~static_analysis_configuration
            analysis_kind;
          let environment =
            Service.StaticAnalysis.type_check ~scheduler ~configuration ~use_cache:false
          in
          let qualifiers =
            Analysis.TypeEnvironment.module_tracker environment
            |> Analysis.ModuleTracker.tracked_explicit_modules
          in
          let environment = Analysis.TypeEnvironment.read_only environment in
          let ast_environment = Analysis.TypeEnvironment.ReadOnly.ast_environment environment in
          let initial_callables =
            Service.StaticAnalysis.fetch_initial_callables
              ~scheduler
              ~configuration
              ~environment
              ~qualifiers
              ~use_cache:false
          in
          let filename_lookup path_reference =
            Newserver.RequestHandler.instantiate_path
              ~build_system
              ~configuration
              ~ast_environment
              path_reference
          in
          Service.StaticAnalysis.analyze
            ~scheduler
            ~analysis:analysis_kind
            ~static_analysis_configuration
            ~filename_lookup
            ~environment
            ~qualifiers
            ~initial_callables
            ~initial_models:Interprocedural.Target.Map.empty
            ~skip_overrides:Ast.Reference.Set.empty
            ()));
  Lwt.return ExitStatus.Ok


let run_infer infer_configuration =
  let {
    InferConfiguration.base = { NewCommandStartup.BaseConfiguration.source_paths; _ };
    paths_to_modify;
    infer_mode;
  }
    =
    infer_configuration
  in
  Newserver.BuildSystem.with_build_system source_paths ~f:(fun build_system ->
      let configuration = InferConfiguration.analysis_configuration_of infer_configuration in
      match infer_mode with
      | InferMode.Local -> run_infer_local ~configuration ~build_system ~paths_to_modify ()
      | InferMode.Interprocedural -> run_infer_interprocedural ~configuration ~build_system ())


let run_infer configuration_file =
  let exit_status =
    match
      NewCommandStartup.read_and_parse_json configuration_file ~f:InferConfiguration.of_yojson
    with
    | Result.Error message ->
        Log.error "%s" message;
        ExitStatus.PyreError
    | Result.Ok
        ({
           InferConfiguration.base =
             {
               NewCommandStartup.BaseConfiguration.global_root;
               local_root;
               debug;
               remote_logging;
               profiling_output;
               memory_profiling_output;
               _;
             };
           _;
         } as infer_configuration) ->
        NewCommandStartup.setup_global_states
          ~global_root
          ~local_root
          ~debug
          ~additional_logging_sections:[]
          ~remote_logging
          ~profiling_output
          ~memory_profiling_output
          ();

        Lwt_main.run
          (Lwt.catch (fun () -> run_infer infer_configuration) NewCheckCommand.on_exception)
  in
  Statistics.flush ();
  exit (ExitStatus.exit_code exit_status)


let command =
  let filename_argument = Command.Param.(anon ("filename" %: Filename.arg_type)) in
  Command.basic
    ~summary:"Runs type inference"
    (Command.Param.map filename_argument ~f:(fun filename () -> run_infer filename))
