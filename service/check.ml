(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre

type result = {
  module_tracker: Analysis.ModuleTracker.t;
  ast_environment: Analysis.AstEnvironment.t;
  environment: Analysis.TypeEnvironment.t;
  errors: Analysis.AnalysisError.t list;
}

let check
    ~scheduler
    ~configuration:
      ({ Configuration.Analysis.project_root; local_root; search_path; debug; _ } as configuration)
    ~build_legacy_dependency_graph
    ~call_graph_builder
  =
  (* Sanity check environment. *)
  let check_directory_exists directory =
    if not (Path.is_directory directory) then
      raise (Invalid_argument (Format.asprintf "`%a` is not a directory" Path.pp directory))
  in
  check_directory_exists local_root;
  check_directory_exists project_root;
  search_path |> List.map ~f:SearchPath.to_path |> List.iter ~f:check_directory_exists;
  (* Profiling helper *)
  Profiling.track_shared_memory_usage ~name:"Before module tracking" ();

  (* Find sources to parse *)
  let module_tracker = Analysis.ModuleTracker.create configuration in
  (* Parse sources. *)
  let ast_environment = Analysis.AstEnvironment.create module_tracker in
  let ast_environment_update_result =
    Analysis.AstEnvironment.update ~scheduler ~configuration ast_environment ColdStart
  in
  let qualifiers = Analysis.AstEnvironment.UpdateResult.reparsed ast_environment_update_result in
  let build_legacy_dependencies () =
    let sources =
      qualifiers
      |> List.filter_map
           ~f:
             (Analysis.AstEnvironment.ReadOnly.get_source
                (Analysis.AstEnvironment.read_only ast_environment))
    in
    let legacy_dependency_tracker =
      Analysis.Dependencies.create (Analysis.AstEnvironment.read_only ast_environment)
    in
    Analysis.Dependencies.register_all_dependencies legacy_dependency_tracker sources
  in
  if build_legacy_dependency_graph then (
    Log.info "Building legacy dependency graph...";
    Profiling.track_duration_and_shared_memory "Build legacy dependency graph" ~f:(fun _ ->
        build_legacy_dependencies ()) );
  let environment =
    let open Analysis in
    let ast_environment = AstEnvironment.read_only ast_environment in
    Log.info "Building type environment...";

    let timer = Timer.start () in
    let update_result =
      AnnotatedGlobalEnvironment.update_this_and_all_preceding_environments
        ast_environment
        ~scheduler
        ~configuration
        ~ast_environment_update_result
        (Ast.Reference.Set.of_list qualifiers)
    in
    let global_environment = AnnotatedGlobalEnvironment.UpdateResult.read_only update_result in
    let environment = TypeEnvironment.create global_environment in
    let resolution = GlobalResolution.create global_environment in
    Statistics.performance ~name:"full environment built" ~timer ();
    let indices () =
      GlobalResolution.unannotated_global_environment resolution
      |> UnannotatedGlobalEnvironment.ReadOnly.all_indices
    in
    if Log.is_enabled `Dotty then (
      let type_order_file =
        Path.create_relative
          ~root:(Configuration.Analysis.log_directory configuration)
          ~relative:"type_order.dot"
      in
      Log.info "Emitting type order dotty file to %s" (Path.absolute type_order_file);
      let class_hierarchy_dot =
        ClassHierarchy.to_dot (GlobalResolution.class_hierarchy resolution) ~indices:(indices ())
      in
      File.create ~content:class_hierarchy_dot type_order_file |> File.write );
    if debug then (
      GlobalResolution.class_hierarchy resolution
      |> ClassHierarchy.check_integrity ~indices:(indices ());
      Statistics.event
        ~section:`Memory
        ~name:"shared memory size"
        ~integers:["size", Memory.heap_size ()]
        () );
    environment
  in
  let errors =
    Analysis.TypeCheck.legacy_run_on_modules
      ~scheduler
      ~configuration
      ~environment
      ~call_graph_builder
      qualifiers;
    Analysis.Postprocessing.run
      ~scheduler
      ~configuration
      ~environment:(Analysis.TypeEnvironment.read_only environment)
      qualifiers
  in
  Profiling.track_shared_memory_usage ();

  (* Log coverage results *)
  let path_to_files =
    Path.get_relative_to_root ~root:project_root ~path:local_root
    |> Option.value ~default:(Path.absolute local_root)
  in
  let open Analysis in
  let { Coverage.strict_coverage; declare_coverage; default_coverage; source_files } =
    Coverage.coverage
      ~configuration
      ~ast_environment:(AstEnvironment.read_only ast_environment)
      qualifiers
  in
  Statistics.coverage
    ~randomly_log_every:20
    ~path:path_to_files
    ~coverage:
      [
        "declare_coverage", declare_coverage;
        "default_coverage", default_coverage;
        "source_files", source_files;
        "strict_coverage", strict_coverage;
        "total_errors", List.length errors;
      ]
    ();

  (* Only destroy the scheduler if the check command created it. *)
  { module_tracker; ast_environment; environment; errors }
