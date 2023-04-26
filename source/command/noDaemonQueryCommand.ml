(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core

module ExitStatus = struct
  type t =
    | Ok
    | PyreError
  [@@deriving sexp, compare, hash]

  let exit_code = function
    | Ok -> 0
    | PyreError -> 1
end

module QueryConfiguration = struct
  type t = {
    base: CommandStartup.BaseConfiguration.t;
    no_validation_on_class_lookup_failure: bool;
    query: string;
  }
  [@@deriving sexp, compare, hash]

  let of_yojson json =
    let open Yojson.Safe.Util in
    let open JsonParsing in
    try
      match CommandStartup.BaseConfiguration.of_yojson json with
      | Result.Error _ as error -> error
      | Result.Ok base ->
          let query = json |> string_member "query" ~default:"" in
          let no_validation_on_class_lookup_failure =
            json |> bool_member "no_validation_on_class_lookup_failure" ~default:false
          in
          Result.Ok { base; no_validation_on_class_lookup_failure; query }
    with
    | Type_error (message, _)
    | Undefined (message, _) ->
        Result.Error message
    | other_exception -> Result.Error (Exn.to_string other_exception)


  let analysis_configuration_of
      {
        base =
          {
            CommandStartup.BaseConfiguration.source_paths;
            search_paths;
            excludes;
            checked_directory_allowlist;
            checked_directory_blocklist;
            extensions;
            log_path;
            global_root;
            local_root;
            debug;
            enable_type_comments;
            python_version = { Configuration.PythonVersion.major; minor; micro };
            parallel;
            number_of_workers;
            shared_memory =
              { Configuration.SharedMemory.heap_size; dependency_table_power; hash_table_power };
            enable_readonly_analysis;
            enable_unawaited_awaitable_analysis;
            include_suppressed_errors;
            remote_logging = _;
            profiling_output = _;
            memory_profiling_output = _;
            use_errpy_parser;
          };
        query = _;
        no_validation_on_class_lookup_failure = _;
      }
    =
    Configuration.Analysis.create
      ~parallel
      ~analyze_external_sources:false
      ~filter_directories:checked_directory_allowlist
      ~ignore_all_errors:checked_directory_blocklist
      ~number_of_workers
      ~local_root:(Option.value local_root ~default:global_root)
      ~project_root:global_root
      ~search_paths:(List.map search_paths ~f:SearchPath.normalize)
      ~debug
      ~excludes
      ~extensions
      ~track_dependencies:false
      ~log_directory:(PyrePath.absolute log_path)
      ~python_major_version:major
      ~python_minor_version:minor
      ~python_micro_version:micro
      ~shared_memory_heap_size:heap_size
      ~shared_memory_dependency_table_power:dependency_table_power
      ~shared_memory_hash_table_power:hash_table_power
      ~enable_type_comments
      ~source_paths:(Configuration.SourcePaths.to_search_paths source_paths)
      ~enable_readonly_analysis
      ~enable_unawaited_awaitable_analysis
      ~include_suppressed_errors
      ~use_errpy_parser
      ()
end

let with_performance_tracking ~debug f =
  let timer = Timer.start () in
  let result = f () in
  if debug then (
    let { Caml.Gc.minor_collections; major_collections; compactions; _ } = Caml.Gc.quick_stat () in
    Statistics.performance
      ~name:"no_daemon_query"
      ~timer
      ~integers:
        [
          "gc_minor_collections", minor_collections;
          "gc_major_collections", major_collections;
          "gc_compactions", compactions;
        ]
      ~normals:["request kind", "NoDaemonQuery"]
      ();
    Memory.report_statistics ())
  else
    Statistics.performance
      ~name:"no_daemon_query"
      ~timer
      ~normals:["request kind", "NoDaemonQuery"]
      ();
  result


let get_environment configuration no_validation_on_class_lookup_failure =
  Scheduler.with_scheduler
    ~configuration
    ~should_log_exception:(fun _ -> true)
    ~f:(fun _ ->
      with_performance_tracking ~debug:configuration.debug (fun () ->
          let read_write_environment =
            Analysis.EnvironmentControls.create
              ~populate_call_graph:false
              configuration
              ~no_validation_on_class_lookup_failure
            |> Analysis.ErrorsEnvironment.create
          in
          Analysis.OverlaidEnvironment.create read_write_environment))


let perform_query ~configuration ~build_system ~query ~no_validation_on_class_lookup_failure () =
  let overlaid_environment = get_environment configuration no_validation_on_class_lookup_failure in
  let query_response =
    (* This query does not support overlay logic *)
    Server.Query.parse_and_process_request ~overlaid_environment ~build_system query None
  in
  Server.Response.to_yojson (Server.Response.Query query_response)


let run_query_on_query_configuration query_configuration =
  let {
    QueryConfiguration.base = { CommandStartup.BaseConfiguration.source_paths; _ };
    query;
    no_validation_on_class_lookup_failure;
    _;
  }
    =
    query_configuration
  in
  Server.BuildSystem.with_build_system source_paths ~f:(fun build_system ->
      let query_response =
        perform_query
          ~configuration:(QueryConfiguration.analysis_configuration_of query_configuration)
          ~build_system
          ~query
          ~no_validation_on_class_lookup_failure
          ()
      in
      (* Prints the query response to stdout, which will be picked up by the client *)
      Printf.printf "%s" (Yojson.Safe.to_string query_response);
      Lwt.return ExitStatus.Ok)


let run_query configuration_file =
  let exit_status =
    match CommandStartup.read_and_parse_json configuration_file ~f:QueryConfiguration.of_yojson with
    | Result.Error message ->
        let () = Log.info "Encountered error with message: %s" message in
        ExitStatus.PyreError
    | Result.Ok
        ({
           QueryConfiguration.base =
             {
               CommandStartup.BaseConfiguration.global_root;
               local_root;
               debug;
               remote_logging;
               profiling_output;
               memory_profiling_output;
               _;
             };
           _;
         } as query_configuration) ->
        CommandStartup.setup_global_states
          ~global_root
          ~local_root
          ~debug
          ~additional_logging_sections:[]
          ~remote_logging
          ~profiling_output
          ~memory_profiling_output
          ();
        Lwt_main.run
          (Lwt.catch
             (fun () -> run_query_on_query_configuration query_configuration)
             (fun exn ->
               let () = Log.info "Got exception: %s" (Exn.to_string exn) in
               Lwt.return ExitStatus.PyreError))
  in
  exit (ExitStatus.exit_code exit_status)


let command =
  Printexc.record_backtrace true;
  let filename_argument = Command.Param.(anon ("filename" %: Filename_unix.arg_type)) in
  Command.basic
    ~summary:"Runs a full check without a server"
    (Command.Param.map filename_argument ~f:(fun filename () -> run_query filename))
