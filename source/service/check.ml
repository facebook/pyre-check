(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre

type result = {
  environment: Analysis.TypeEnvironment.t;
  errors: Analysis.AnalysisError.t list;
}

let check
    ~scheduler
    ~configuration:
      ({ Configuration.Analysis.project_root; source_paths; search_paths; debug; _ } as
      configuration)
    ~call_graph_builder
  =
  (* Sanity check environment. *)
  let check_directory_exists directory =
    if not (PyrePath.is_directory directory) then
      raise (Invalid_argument (Format.asprintf "`%a` is not a directory" PyrePath.pp directory))
  in
  let check_path_exists path =
    if not (PyrePath.file_exists path) then
      raise (Invalid_argument (Format.asprintf "`%a` is not a valid path" PyrePath.pp path))
  in
  let check_search_path_exists search_path =
    match search_path with
    | SearchPath.Root _
    | SearchPath.Subdirectory _ ->
        check_directory_exists (SearchPath.to_path search_path)
    | SearchPath.Submodule _ -> check_path_exists (SearchPath.to_path search_path)
  in
  source_paths |> List.map ~f:SearchPath.to_path |> List.iter ~f:check_directory_exists;
  check_directory_exists project_root;
  search_paths |> List.iter ~f:check_search_path_exists;
  (* Profiling helper *)
  Profiling.track_shared_memory_usage ~name:"Before module tracking" ();
  let environment, qualifiers =
    let open Analysis in
    Log.info "Building type environment...";

    let timer = Timer.start () in
    let type_environment = TypeEnvironment.create configuration in
    let global_environment =
      TypeEnvironment.global_environment type_environment |> AnnotatedGlobalEnvironment.read_only
    in

    Statistics.performance ~name:"full environment built" ~timer ();

    let global_resolution = GlobalResolution.create global_environment in
    let indices () =
      global_environment
      |> AnnotatedGlobalEnvironment.ReadOnly.unannotated_global_environment
      |> UnannotatedGlobalEnvironment.ReadOnly.all_indices
    in
    if Log.is_enabled `Dotty then (
      let type_order_file =
        PyrePath.create_relative
          ~root:(Configuration.Analysis.log_directory configuration)
          ~relative:"type_order.dot"
      in
      Log.info "Emitting type order dotty file to %s" (PyrePath.absolute type_order_file);
      let class_hierarchy_dot =
        ClassHierarchy.to_dot
          (GlobalResolution.class_hierarchy global_resolution)
          ~indices:(indices ())
      in
      File.create ~content:class_hierarchy_dot type_order_file |> File.write);
    if debug then
      Statistics.event
        ~section:`Memory
        ~name:"shared memory size"
        ~integers:["size", Memory.heap_size ()]
        ();

    let project_qualifiers =
      AnnotatedGlobalEnvironment.ReadOnly.project_qualifiers global_environment
    in
    type_environment, project_qualifiers
  in
  let errors =
    Analysis.TypeEnvironment.populate_for_modules
      ~scheduler
      ~configuration
      ~call_graph_builder
      environment
      qualifiers;
    Analysis.Postprocessing.run
      ~scheduler
      ~configuration
      ~environment:(Analysis.TypeEnvironment.read_only environment)
      qualifiers
  in
  Profiling.track_shared_memory_usage ();

  (* Only destroy the scheduler if the check command created it. *)
  { environment; errors }
