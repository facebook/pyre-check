(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis
open Configuration
open Pyre

module Scheduler = Service.Scheduler


let server_not_running = 2


type result = {
  handles: File.Handle.t list;
  environment: (module Environment.Handler);
  errors: Error.t list
}


let check
    {
      start_time = _;
      verbose;
      version = _;
      sections;
      debug;
      infer;
      recursive_infer;
      strict;
      declare;
      show_error_traces;
      log_identifier;
      parallel;
      project_root;
      stub_roots;
      source_root;
    }
    original_scheduler
    () =
  Log.initialize ~verbose ~sections;

  if not (Path.is_directory source_root) then
    raise (Invalid_argument (Format.asprintf "`%a` is not a directory" Path.pp source_root));
  if not (Path.is_directory project_root) then
    raise (Invalid_argument (Format.asprintf "`%a` is not a directory" Path.pp project_root));
  List.iter
    ~f:(fun stub_root ->
        if not (Path.is_directory stub_root) then
          raise (Invalid_argument (Format.asprintf "`%a` is not a directory" Path.pp stub_root)))
    stub_roots;

  let bucket_multiplier =
    try Int.of_string (Sys.getenv "BUCKET_MULTIPLIER" |> (fun value -> Option.value_exn value))
    with _ -> 10
  in
  let scheduler =
    match original_scheduler with
    | None -> Scheduler.create ~is_parallel:parallel ~bucket_multiplier ()
    | Some scheduler -> scheduler
  in
  let configuration =
    Configuration.create
      ~verbose
      ~sections
      ~source_root
      ~debug
      ~strict
      ~declare
      ~show_error_traces
      ~log_identifier
      ~project_root
      ~stub_roots
      ~infer
      ~recursive_infer
      ()
  in

  (* Parsing. *)
  let stubs = Service.Parser.parse_stubs scheduler ~configuration in
  let sources = Service.Parser.parse_sources scheduler ~configuration in

  (* Build environment. *)
  ServiceIgnore.register ~configuration scheduler sources;
  let environment =
    let handler =
      if Scheduler.is_parallel scheduler then
        Service.Environment.shared_memory_handler
      else
        Service.Environment.in_process_handler
    in
    handler scheduler ~configuration ~stubs ~sources
  in
  (* Run type checker. *)
  let errors, _, { TypeCheck.Coverage.full; partial; untyped; ignore; crashes } =
    Service.TypeCheck.analyze_sources scheduler configuration environment sources
  in
  (* Log coverage results *)
  let path_to_files =
    Path.get_relative_to_root ~root:project_root ~path:source_root
    |> Option.value ~default:(Path.absolute source_root)
  in
  Statistics.coverage
    ~coverage:[
      "full_type_coverage", full;
      "partial_type_coverage", partial;
      "no_type_coverage", untyped;
      "ignore_coverage", ignore;
      "total_errors", List.length errors;
      "crashes", crashes;
    ]
    ~configuration
    ~normals:[
      "file_name", path_to_files;
    ]
    ();

  (* Only destroy the scheduler if the check command created it. *)
  begin
    match original_scheduler with
    | None -> Scheduler.destroy scheduler
    | Some _ -> ()
  end;
  { handles = stubs @ sources; environment; errors }


(** run_command prints out the errors, for a Check run *)
let run_check
    verbose
    version
    sections
    debug
    strict
    declare
    show_error_traces
    infer
    recursive_infer
    sequential
    log_identifier
    project_root
    stub_roots
    source_root
    () =
  let configuration =
    Configuration.create
      ~verbose
      ?version
      ~sections
      ~debug
      ~strict
      ~declare
      ~show_error_traces
      ~log_identifier
      ~infer
      ~recursive_infer
      ~project_root:(Path.create_absolute project_root)
      ~parallel:(not sequential)
      ~stub_roots:(List.map ~f:Path.create_absolute stub_roots)
      ~source_root:(Path.create_absolute source_root)
      ()
  in

  let timer = Timer.start () in
  let { errors; _ } = check configuration None () in
  let { Caml.Gc.minor_collections; major_collections; compactions; _ } = Caml.Gc.stat () in
  Statistics.performance
    ~name:"check"
    ~timer
    ~configuration
    ~integers:[
      "gc_minor_collections", minor_collections;
      "gc_major_collections", major_collections;
      "gc_compactions", compactions;
    ]
    ~normals:["request kind", "FullCheck"]
    ();
  (* Print results. *)
  Log.print
    "%s"
    (Yojson.Safe.to_string (
        `List (List.map
                 ~f:(fun error -> Error.to_json ~detailed:show_error_traces error)
                 errors)));
  Statistics.flush ()


let check_command =
  Command.basic_spec
    ~summary:"Runs a full check without a server (default)"
    CommandSpec.base_command_line_arguments
    run_check
