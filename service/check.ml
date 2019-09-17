(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre

type result = {
  module_tracker: Analysis.ModuleTracker.t;
  ast_environment: Analysis.AstEnvironment.t;
  environment: Analysis.Environment.t;
  errors: Analysis.Error.t list;
}

type analyze_source_results = {
  errors: Analysis.Error.t list;
  number_files: int;
}
(** Internal result type; not exposed. *)

let run_check
    ?open_documents
    ~scheduler
    ~configuration
    ~environment
    checked_sources
    (module Check : Analysis.Check.Signature)
  =
  let empty_result = { errors = []; number_files = 0 } in
  let number_of_sources = List.length checked_sources in
  Log.info "Running check `%s`..." Check.name;
  let timer = Timer.start () in
  let map _ sources =
    Analysis.Annotated.Class.AttributeCache.clear ();
    Analysis.AstEnvironment.FromEmptyStubCache.clear ();
    let analyze_source
        { errors; number_files }
        ({ Source.source_path = { SourcePath.qualifier; _ }; _ } as source)
      =
      let configuration =
        match open_documents with
        | Some predicate when predicate qualifier ->
            { configuration with Configuration.Analysis.store_type_check_resolution = true }
        | _ -> configuration
      in
      let new_errors = Check.run ~configuration ~environment ~source in
      { errors = List.append new_errors errors; number_files = number_files + 1 }
    in
    List.fold sources ~init:empty_result ~f:analyze_source
  in
  let reduce left right =
    let number_files = left.number_files + right.number_files in
    Log.log ~section:`Progress "Processed %d of %d sources" number_files number_of_sources;
    { errors = List.append left.errors right.errors; number_files }
  in
  let { errors; _ } =
    Scheduler.map_reduce
      scheduler
      ~configuration
      ~bucket_size:75
      ~initial:empty_result
      ~map
      ~reduce
      ~inputs:checked_sources
      ()
  in
  Statistics.performance ~name:(Format.asprintf "check_%s" Check.name) ~timer ();
  Statistics.event
    ~section:`Memory
    ~name:"shared memory size post-typecheck"
    ~integers:["size", Memory.heap_size ()]
    ();
  errors


let analyze_sources
    ?open_documents
    ?(filter_external_sources = true)
    ~scheduler
    ~configuration
    ~environment
    sources
  =
  let open Analysis in
  Annotated.Class.AttributeCache.clear ();
  let checked_sources =
    if filter_external_sources then
      List.filter sources ~f:(fun { Source.source_path = { SourcePath.is_external; _ }; _ } ->
          not is_external)
    else
      sources
  in
  let number_of_sources = List.length checked_sources in
  Log.info "Checking %d sources..." number_of_sources;
  let timer = Timer.start () in
  let run = run_check ?open_documents ~scheduler ~configuration ~environment checked_sources in
  let errors = List.map (Analysis.Check.checks ~configuration) ~f:run |> List.concat in
  Statistics.performance ~name:"analyzed sources" ~timer ();
  errors


let check
    ~scheduler:original_scheduler
    ~configuration:( { Configuration.Analysis.project_root; local_root; search_path; debug; _ } as
                   configuration )
    ~build_legacy_dependency_graph
  =
  (* Sanity check environment. *)
  let check_directory_exists directory =
    if not (Path.is_directory directory) then
      raise (Invalid_argument (Format.asprintf "`%a` is not a directory" Path.pp directory))
  in
  check_directory_exists local_root;
  check_directory_exists project_root;
  search_path |> List.map ~f:SearchPath.to_path |> List.iter ~f:check_directory_exists;
  let scheduler =
    let bucket_multiplier =
      try
        Int.of_string (Sys.getenv "BUCKET_MULTIPLIER" |> fun value -> Option.value_exn value)
      with
      | _ -> 10
    in
    match original_scheduler with
    | None -> Scheduler.create ~configuration ~bucket_multiplier ()
    | Some scheduler -> scheduler
  in
  (* Find sources to parse *)
  let module_tracker = Analysis.ModuleTracker.create configuration in
  (* Parse sources. *)
  let ast_environment = Analysis.AstEnvironment.create module_tracker in
  let ast_environment_update_result =
    Analysis.AstEnvironment.update ~scheduler ~configuration ast_environment ColdStart
  in
  let sources =
    let ast_environment = Analysis.AstEnvironment.read_only ast_environment in
    Analysis.AstEnvironment.UpdateResult.reparsed ast_environment_update_result
    |> List.filter_map ~f:(Analysis.AstEnvironment.ReadOnly.get_source ast_environment)
  in
  ( if build_legacy_dependency_graph then
      let legacy_dependency_tracker =
        Analysis.Dependencies.create (Analysis.AstEnvironment.read_only ast_environment)
      in
      Analysis.Dependencies.register_all_dependencies legacy_dependency_tracker sources );
  let environment =
    let populate = Environment.populate in
    let open Analysis in
    let ast_environment = AstEnvironment.read_only ast_environment in
    Log.info "Adding built-in environment information to shared memory...";
    let timer = Timer.start () in
    let unannotated_global_environment = UnannotatedGlobalEnvironment.create ast_environment in
    let alias_environment =
      AliasEnvironment.create
        (UnannotatedGlobalEnvironment.read_only unannotated_global_environment)
    in
    let class_hierarchy_environment =
      ClassHierarchyEnvironment.create (AliasEnvironment.read_only alias_environment)
    in
    let environment =
      Environment.shared_memory_handler
        (ClassHierarchyEnvironment.read_only class_hierarchy_environment)
    in
    Environment.add_special_globals environment;
    Statistics.performance ~name:"added environment to shared memory" ~timer ();
    Log.info "Building type environment...";

    (* This grabs all sources from shared memory. It is unavoidable: Environment must be built
       sequentially until we find a way to build the environment in parallel. *)
    let timer = Timer.start () in
    let qualifiers =
      List.map sources ~f:(fun { Ast.Source.source_path = { SourcePath.qualifier; _ }; _ } ->
          qualifier)
    in
    let update_result =
      UnannotatedGlobalEnvironment.update
        unannotated_global_environment
        ~scheduler
        ~configuration
        ~ast_environment_update_result
        (Ast.Reference.Set.of_list qualifiers)
      |> AliasEnvironment.update alias_environment ~scheduler ~configuration
      |> ClassHierarchyEnvironment.update class_hierarchy_environment ~scheduler ~configuration
    in
    populate ~configuration ~scheduler ~update_result environment sources;
    Statistics.performance ~name:"full environment built" ~timer ();
    if Log.is_enabled `Dotty then (
      let type_order_file =
        Path.create_relative
          ~root:(Configuration.Analysis.pyre_root configuration)
          ~relative:"type_order.dot"
      in
      Log.info "Emitting type order dotty file to %s" (Path.absolute type_order_file);
      File.create ~content:(Environment.class_hierarchy_dot environment) type_order_file
      |> File.write );
    if debug then (
      Analysis.Environment.check_class_hierarchy_integrity environment;
      Statistics.event
        ~section:`Memory
        ~name:"shared memory size"
        ~integers:["size", Memory.heap_size ()]
        () );
    environment
  in
  let errors = analyze_sources ~scheduler ~configuration ~environment sources in
  (* Log coverage results *)
  let path_to_files =
    Path.get_relative_to_root ~root:project_root ~path:local_root
    |> Option.value ~default:(Path.absolute local_root)
  in
  let open Analysis in
  let { Coverage.strict_coverage; declare_coverage; default_coverage; source_files } =
    Coverage.coverage ~configuration sources
  in
  let { Coverage.full; partial; untyped; ignore; crashes } =
    let aggregate sofar { Source.source_path = { SourcePath.qualifier; _ }; _ } =
      match Coverage.get ~qualifier with
      | Some coverage -> Coverage.sum sofar coverage
      | _ -> sofar
    in
    List.fold sources ~init:(Coverage.create ()) ~f:aggregate
  in
  Statistics.coverage
    ~randomly_log_every:20
    ~path:path_to_files
    ~coverage:
      [
        "crashes", crashes;
        "declare_coverage", declare_coverage;
        "default_coverage", default_coverage;
        "full_type_coverage", full;
        "ignore_coverage", ignore;
        "no_type_coverage", untyped;
        "partial_type_coverage", partial;
        "source_files", source_files;
        "strict_coverage", strict_coverage;
        "total_errors", List.length errors;
      ]
    ();

  (* Only destroy the scheduler if the check command created it. *)
  ( match original_scheduler with
  | None -> Scheduler.destroy scheduler
  | Some _ -> () );
  { module_tracker; ast_environment; environment; errors }
