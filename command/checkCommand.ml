(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

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
    local_root
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
      ~local_root:(Path.create_absolute local_root)
      ()
  in

  let timer = Timer.start () in
  let { TypeCheck.errors; _ } = TypeCheck.check configuration None () in
  let { Caml.Gc.minor_collections; major_collections; compactions; _ } = Caml.Gc.stat () in
  Statistics.performance
    ~name:"check"
    ~timer
    ~integers:[
      "gc_minor_collections", minor_collections;
      "gc_major_collections", major_collections;
      "gc_compactions", compactions;
    ]
    ~normals:["request kind", "FullCheck"]
    ();
  (* Print results. *)
  Yojson.Safe.to_string
    (`List
       (List.map ~f:(fun error -> Error.to_json ~detailed:show_error_traces error) errors))
  |> Log.print "%s";
  Statistics.flush ()


let check_command =
  Command.basic_spec
    ~summary:"Runs a full check without a server (default)"
    Specification.base_command_line_arguments
    run_check
