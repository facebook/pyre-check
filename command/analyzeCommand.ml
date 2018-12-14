(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre


let run_analysis
    _taint
    taint_models_directory
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
    recursive_infer
    run_additional_checks
    sequential
    filter_directories
    number_of_workers
    log_identifier
    logger
    project_root
    search_path
    typeshed
    excludes
    local_root
    () =
  let filter_directories =
    filter_directories
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
      ~infer
      ~recursive_infer
      ~run_additional_checks
      ~project_root:(Path.create_absolute project_root)
      ~parallel:(not sequential)
      ?filter_directories
      ~number_of_workers
      ~search_path:(List.map search_path ~f:Path.SearchPath.create)
      ?typeshed:(typeshed >>| Path.create_absolute)
      ~excludes
      ~local_root:(Path.create_absolute local_root)
      ()
  in
  let result_json_path =
    result_json_path
    >>| Path.create_absolute ~follow_symbolic_links:false
  in
  let () =
    match result_json_path with
    | Some path when not (Path.is_directory path) ->
        begin
          Log.error "--save-results-to path must be a directory.";
          failwith "bad argument"
        end
    | _ ->
        ()
  in
  (fun () ->
     let timer = Timer.start () in
     let bucket_multiplier =
       try Int.of_string (Sys.getenv "BUCKET_MULTIPLIER" |> (fun value -> Option.value_exn value))
       with _ -> 10
     in
     let scheduler = Scheduler.create ~configuration ~bucket_multiplier () in
     let errors =
       Service.Check.check ~scheduler:(Some scheduler) ~configuration
       |> fun { handles; environment; _ } ->
       Service.StaticAnalysis.analyze
         ?taint_models_directory
         ~scheduler
         ~configuration:{
           Configuration.StaticAnalysis.configuration;
           result_json_path;
           dump_call_graph;
         }
         ~environment
         ~handles
         ()
     in
     let { Caml.Gc.minor_collections; major_collections; compactions; _ } = Caml.Gc.stat () in
     Statistics.performance
       ~name:"analyze"
       ~timer
       ~integers:[
         "gc_minor_collections", minor_collections;
         "gc_major_collections", major_collections;
         "gc_compactions", compactions;
       ]
       ();
     (* Print results. *)
     List.map
       errors
       ~f:(fun error -> Interprocedural.Error.to_json ~detailed:show_error_traces error)
     |> (fun result -> Yojson.Safe.pretty_to_string (`List result))
     |> Log.print "%s";
     Scheduler.destroy scheduler)
  |> Scheduler.run_process ~configuration


let command =
  Command.basic_spec
    ~summary:"Runs a static analysis without a server (default)."
    Command.Spec.(
      empty
      +> flag
        "-taint"
        no_arg
        ~doc:"Run the taint analysis."
      +> flag
        "-taint-models"
        (optional file)
        ~doc:"directory A directory containing models to introduce taint."
      +> flag
        "-save-results-to"
        (optional file)
        ~doc:"file A JSON file that Pyre Analyze will save its' results to."
      +> flag
        "-dump-call-graph"
        no_arg
        ~doc:"Store call graph in .pyre/call_graph.json"
      ++ Specification.base_command_line_arguments)
    run_analysis
