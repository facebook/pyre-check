(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
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
      version = _;
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
      ~stub_roots
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
  let stubs = Service.Parser.parse_stubs scheduler ~configuration in
  let known_stubs =
    List.fold
      stubs
      ~init:Expression.Access.Set.empty
      ~f:(fun known_stubs handle ->
          match Service.AstSharedMemory.get_source handle with
          | Some { Ast.Source.qualifier; _ } ->
              Set.add known_stubs qualifier
          | _ ->
              known_stubs)
  in
  let sources =
    let filter path =
      match Path.get_relative_to_root ~root:source_root ~path:(Path.create_absolute path) with
      | Some path ->
          not (Set.mem known_stubs (Source.qualifier ~path))
      | _ ->
          true
    in
    let sources = Service.Parser.parse_sources ~filter scheduler ~configuration in
    let number_of_files = List.length (Service.Parser.find_sources ~filter configuration) in
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
      ();

    sources
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
    handler scheduler ~configuration ~stubs ~sources
  in

  let call_graph =
    if Scheduler.is_parallel scheduler then
      Service.CallGraph.shared_memory_handler ()
    else
      Service.CallGraph.in_process_handler ()
  in

  let errors, lookups, { TypeCheck.Coverage.full; partial; untyped; ignore; crashes } =
    Service.TypeCheck.analyze_sources scheduler configuration environment call_graph sources
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
      ~call_graph
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
    version
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
      ~analyze
      ~project_root:(Path.create_absolute project_root)
      ~parallel:(not sequential)
      ~number_of_workers
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
