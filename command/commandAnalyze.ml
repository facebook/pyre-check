(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre

module Scheduler = Service.Scheduler


let run_analysis
    taint
    taint_models
    verbose
    expected_version
    sections
    debug
    strict
    declare
    show_error_traces
    infer
    recursive_infer
    sequential
    filter_directories
    filter_directories_semicolon
    number_of_workers
    log_identifier
    logger
    project_root
    search_path
    typeshed
    source_root
    () =
  let filter_directories =
    let deprecated_directories =
      filter_directories
      >>| List.map ~f:Path.create_absolute
    in
    filter_directories_semicolon
    >>| String.split_on_chars ~on:[';']
    >>| List.map ~f:String.strip
    >>| List.map ~f:Path.create_absolute
    |> (fun directories ->
        if Option.is_some directories then directories else deprecated_directories)
  in
  let configuration =
    Configuration.create
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
      ~project_root:(Path.create_absolute project_root)
      ~parallel:(not sequential)
      ?filter_directories
      ~number_of_workers
      ~search_path:(List.map ~f:Path.create_absolute search_path)
      ?typeshed:(typeshed >>| Path.create_absolute)
      ~source_root:(Path.create_absolute source_root)
      ()
  in
  let timer = Timer.start () in
  let bucket_multiplier =
    try Int.of_string (Sys.getenv "BUCKET_MULTIPLIER" |> (fun value -> Option.value_exn value))
    with _ -> 10
  in
  let scheduler = Scheduler.create ~configuration ~bucket_multiplier () in
  let () =
    CommandCheck.check configuration (Some scheduler) ()
    |> fun { handles; environment; _ } ->
    Service.Analysis.analyze ~scheduler ~configuration ~environment ~handles
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
    ~normals:["request kind", "FullCheck"]
    ();
  Statistics.flush ();
  Scheduler.destroy scheduler


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
      ++ CommandSpec.base_command_line_arguments)
    run_analysis
