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
  let environment, qualifiers =
    let open Analysis in
    Log.info "Building type environment...";

    let timer = Timer.start () in
    let update_result =
      let annotated_global_environment = AnnotatedGlobalEnvironment.create ast_environment in
      AnnotatedGlobalEnvironment.update_this_and_all_preceding_environments
        annotated_global_environment
        ~scheduler
        ~configuration
        ColdStart
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
    ( environment,
      AnnotatedGlobalEnvironment.UpdateResult.ast_environment_update_result update_result
      |> AstEnvironment.UpdateResult.invalidated_modules )
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

  (* Only destroy the scheduler if the check command created it. *)
  { module_tracker; ast_environment; environment; errors }
