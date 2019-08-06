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
    _taint_models_directory
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
      ~excludes
      ~extensions
      ~local_root:(Path.create_absolute local_root)
      ()
  in
  (fun () ->
    let timer = Timer.start () in
    let { Check.errors; environment; _ } = Check.check ~scheduler:None ~configuration in
    let { Caml.Gc.minor_collections; major_collections; compactions; _ } = Caml.Gc.stat () in
    Statistics.performance
      ~name:"check"
      ~timer
      ~integers:
        [ "gc_minor_collections", minor_collections;
          "gc_major_collections", major_collections;
          "gc_compactions", compactions ]
      ~normals:["request kind", "FullCheck"]
      ();
    if debug then
      Memory.report_statistics ();

    (* Print results. *)
    let errors =
      let ast_environment = Analysis.Environment.ast_environment environment in
      List.map
        errors
        ~f:(Error.instantiate ~lookup:(AstEnvironment.ReadOnly.get_relative ast_environment))
    in
    Yojson.Safe.to_string
      (`Assoc
        [ ( "errors",
            `List
              (List.map
                 ~f:(fun error -> Error.Instantiated.to_json ~show_error_traces error)
                 errors) ) ])
    |> Log.print "%s")
  |> Scheduler.run_process ~configuration


let check_command =
  Command.basic_spec
    ~summary:"Runs a full check without a server (default)"
    Specification.base_command_line_arguments
    run_check
