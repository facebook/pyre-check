(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre

let run_analysis
    _taint
    result_json_path
    dump_call_graph
    verbose
    expected_version
    sections
    debug
    strict
    declare
    show_error_traces
    infer
    additional_checks
    sequential
    filter_directories
    ignore_all_errors
    number_of_workers
    log_identifier
    logger
    profiling_output
    project_root
    search_path
    taint_models_directories
    excludes
    extensions
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
  let configuration =
    Configuration.Analysis.create
      ~verbose
      ?expected_version
      ~sections
      ~debug
      ~strict
      ~declare
      ~show_error_traces
      ~log_identifier
      ?logger
      ?profiling_output
      ~infer
      ~additional_checks
      ~project_root:(Path.create_absolute project_root)
      ~parallel:(not sequential)
      ?filter_directories
      ?ignore_all_errors
      ~number_of_workers
      ~search_path:(List.map search_path ~f:SearchPath.create)
      ~taint_models_directories:(List.map taint_models_directories ~f:Path.create_absolute)
      ~excludes
      ~extensions
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
    let bucket_multiplier =
      try
        Int.of_string (Sys.getenv "BUCKET_MULTIPLIER" |> fun value -> Option.value_exn value)
      with
      | _ -> 10
    in
    let scheduler = Scheduler.create ~configuration ~bucket_multiplier () in
    let errors =
      Service.Check.check ~scheduler:(Some scheduler) ~configuration
      |> fun { module_tracker; environment; _ } ->
      let qualifiers =
        Analysis.ModuleTracker.source_paths module_tracker
        |> List.map ~f:(fun { Ast.SourcePath.qualifier; _ } -> qualifier)
      in
      Service.StaticAnalysis.analyze
        ~scheduler
        ~configuration:
          { Configuration.StaticAnalysis.configuration; result_json_path; dump_call_graph }
        ~environment
        ~qualifiers
        ()
    in
    let { Caml.Gc.minor_collections; major_collections; compactions; _ } = Caml.Gc.stat () in
    Statistics.performance
      ~name:"analyze"
      ~timer
      ~integers:
        [ "gc_minor_collections", minor_collections;
          "gc_major_collections", major_collections;
          "gc_compactions", compactions ]
      ();

    (* Print results. *)
    List.map errors ~f:(fun error -> Interprocedural.Error.to_json ~show_error_traces error)
    |> (fun result -> Yojson.Safe.pretty_to_string (`List result))
    |> Log.print "%s";
    Scheduler.destroy scheduler)
  |> Scheduler.run_process ~configuration


let command =
  Command.basic_spec
    ~summary:"Runs a static analysis without a server (default)."
    Command.Spec.(
      empty
      +> flag "-taint" no_arg ~doc:"Run the taint analysis."
      +> flag
           "-save-results-to"
           (optional string)
           ~doc:"file A JSON file that Pyre Analyze will save its' results to."
      +> flag "-dump-call-graph" no_arg ~doc:"Store call graph in .pyre/call_graph.json"
      ++ Specification.base_command_line_arguments)
    run_analysis
