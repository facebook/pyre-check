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
  errors: Error.t list;
  lookups: Lookup.t String.Map.t;
}


let check
    {
      start_time = _;
      verbose;
      expected_version = _;
      sections;
      debug;
      infer;
      recursive_infer;
      analyze;
      strict;
      declare;
      show_error_traces;
      log_identifier;
      parallel;
      number_of_workers;
      project_root;
      search_path;
      typeshed;
      source_root;
    }
    original_scheduler
    () =
  Log.initialize ~verbose ~sections;

  let check_directory_exists directory =
    if not (Path.is_directory directory) then
      raise (Invalid_argument (Format.asprintf "`%a` is not a directory" Path.pp directory));
  in
  check_directory_exists source_root;
  check_directory_exists project_root;
  List.iter ~f:check_directory_exists search_path;
  Option.iter typeshed ~f:check_directory_exists;

  let bucket_multiplier =
    try Int.of_string (Sys.getenv "BUCKET_MULTIPLIER" |> (fun value -> Option.value_exn value))
    with _ -> 10
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
      ~parallel
      ~number_of_workers
      ~search_path
      ?typeshed
      ~infer
      ~recursive_infer
      ~analyze
      ()
  in
  let scheduler =
    match original_scheduler with
    | None -> Scheduler.create ~configuration ~bucket_multiplier ()
    | Some scheduler -> scheduler
  in
  (* Parsing. *)
  let stubs, sources = Service.Parser.parse_all scheduler ~configuration in
  (* Coverage. *)
  let () =
    let number_of_files = List.length sources in
    let { Service.Coverage.strict_coverage; declare_coverage; default_coverage; source_files } =
      Service.Coverage.coverage ~sources ~number_of_files
    in
    let path_to_files =
      Path.get_relative_to_root ~root:project_root ~path:source_root
      |> Option.value ~default:(Path.absolute source_root)
    in

    Statistics.coverage
      ~coverage:[
        "strict_coverage", strict_coverage;
        "declare_coverage", declare_coverage;
        "default_coverage", default_coverage;
        "source_files", source_files;
      ]
      ~normals:[
        "file_name", path_to_files;
      ]
      ~configuration
      ()
  in

  (* Build environment. *)
  Service.Ignore.register ~configuration scheduler sources;
  let environment =
    let handler =
      if Scheduler.is_parallel scheduler then
        Service.Environment.shared_memory_handler
      else
        Service.Environment.in_process_handler
    in
    handler ~configuration ~stubs ~sources
  in

  let errors, lookups, { TypeCheck.Coverage.full; partial; untyped; ignore; crashes } =
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

  if analyze then
    Service.Analysis.analyze
      ~scheduler
      ~configuration
      ~environment
      ~handles:sources;

  (* Only destroy the scheduler if the check command created it. *)
  begin
    match original_scheduler with
    | None -> Scheduler.destroy scheduler
    | Some _ -> ()
  end;
  { handles = stubs @ sources; environment; errors; lookups }


(** run_command prints out the errors, for a Check run *)
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
    analyze
    sequential
    number_of_workers
    log_identifier
    project_root
    search_path
    typeshed
    source_root
    () =
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
      ~infer
      ~recursive_infer
      ~analyze
      ~project_root:(Path.create_absolute project_root)
      ~parallel:(not sequential)
      ~number_of_workers
      ~search_path:(List.map ~f:Path.create_absolute search_path)
      ?typeshed:(typeshed >>| Path.create_absolute)
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
  if not analyze then
    Yojson.Safe.to_string
      (`List
         (List.map ~f:(fun error -> Error.to_json ~detailed:show_error_traces error) errors))
    |> Log.print "%s";
  Statistics.flush ()


let check_command =
  Command.basic_spec
    ~summary:"Runs a full check without a server (default)"
    CommandSpec.base_command_line_arguments
    run_check
