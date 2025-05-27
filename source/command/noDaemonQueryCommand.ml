(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The no-daemon query module contains logic for the ability to query pyre without spinning up a
   long-lasting pyre daemon. This is for use cases that only require a single query: it is less
   crash-prone and does not leave a process around afterwards.

   These queries do not output to the normal daemon logs, instead redirect all output to stdout and
   terminate when the job is done. *)

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
    let open JsonParsing.YojsonUtils in
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
    | other_exception -> Result.Error (Exception.exn_to_string other_exception)


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
            python_version;
            system_platform;
            parallel;
            number_of_workers;
            long_lived_workers;
            shared_memory =
              { Configuration.SharedMemory.heap_size; dependency_table_power; hash_table_power };
            enable_readonly_analysis;
            enable_strict_override_check;
            enable_strict_any_check;
            enable_unawaited_awaitable_analysis;
            include_suppressed_errors;
            remote_logging = _;
            profiling_output = _;
            memory_profiling_output = _;
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
      ?long_lived_workers
      ~local_root:(Option.value local_root ~default:global_root)
      ~project_root:global_root
      ~search_paths
      ~debug
      ~excludes
      ~extensions
      ~track_dependencies:false
      ~log_directory:(PyrePath.absolute log_path)
      ~python_version
      ~system_platform
      ~shared_memory_heap_size:heap_size
      ~shared_memory_dependency_table_power_from_configuration:dependency_table_power
      ~shared_memory_hash_table_power:hash_table_power
      ~enable_type_comments
      ~source_paths:(Configuration.SourcePaths.to_search_paths source_paths)
      ~enable_readonly_analysis
      ~enable_strict_override_check
      ~enable_strict_any_check
      ~enable_unawaited_awaitable_analysis
      ~include_suppressed_errors
      ()
end

let with_performance_tracking ~debug f =
  let timer = Timer.start () in
  let result = f () in
  if debug then (
    let { Stdlib.Gc.minor_collections; major_collections; compactions; _ } =
      Stdlib.Gc.quick_stat ()
    in
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
  with_performance_tracking ~debug:configuration.Configuration.Analysis.debug (fun () ->
      let read_write_environment =
        Analysis.EnvironmentControls.create
          ~populate_call_graph:false
          configuration
          ~no_validation_on_class_lookup_failure
        |> Analysis.ErrorsEnvironment.create_with_ast_environment
      in
      Analysis.OverlaidEnvironment.create read_write_environment)


let perform_query
    ~configuration
    ~scheduler
    ~build_system
    ~query
    ~no_validation_on_class_lookup_failure
    ()
  =
  let overlaid_environment = get_environment configuration no_validation_on_class_lookup_failure in
  let query_response =
    (* This query does not support overlay logic *)
    Server.Query.parse_and_process_request
      ~overlaid_environment
      ~scheduler
      ~build_system
      ~query_cache:(Server.Query.Cache.create ())
      query
      None
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
  let configuration = QueryConfiguration.analysis_configuration_of query_configuration in
  Scheduler.with_scheduler
    ~configuration
    ~should_log_exception:(fun _ -> true)
    ~f:(fun scheduler ->
      Server.BuildSystem.with_build_system source_paths ~f:(fun build_system ->
          let query_response =
            perform_query
              ~configuration
              ~scheduler
              ~build_system
              ~query
              ~no_validation_on_class_lookup_failure
              ()
          in
          (* Prints the query response to stdout, which will be picked up by the client *)
          Printf.printf "%s" (Yojson.Safe.to_string query_response);
          Lwt.return ExitStatus.Ok))


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
               let () = Log.info "Got exception: %s" (Exception.exn_to_string exn) in
               Lwt.return ExitStatus.PyreError))
  in
  exit (ExitStatus.exit_code exit_status)


let doc = "Runs a full check without a server"

let command () =
  let open Cmdliner in
  let filename = Arg.(required & pos 0 (some string) None & info [] ~docv:"filename") in
  let term = Term.(const run_query $ filename) in
  let info = Cmd.info "no-daemon-query" ~doc in
  Cmd.v info term
