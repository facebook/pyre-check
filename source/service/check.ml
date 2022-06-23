(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre

let check
    ~scheduler
    ~configuration:
      ({ Configuration.Analysis.project_root; source_paths; search_paths; debug; _ } as
      configuration)
    ~populate_call_graph
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
  let environment =
    let open Analysis in
    Log.info "Building type environment...";

    let timer = Timer.start () in
    let errors_environment =
      EnvironmentControls.create ~populate_call_graph configuration |> ErrorsEnvironment.create
    in
    let type_environment =
      ErrorsEnvironment.read_only errors_environment |> ErrorsEnvironment.ReadOnly.type_environment
    in

    let global_environment = TypeEnvironment.ReadOnly.global_environment type_environment in
    Statistics.performance ~name:"full environment built" ~timer ();

    if Log.is_enabled `Dotty then (
      let type_order_file =
        PyrePath.create_relative
          ~root:(Configuration.Analysis.log_directory configuration)
          ~relative:"type_order.dot"
      in
      Log.info "Emitting type order dotty file to %s" (PyrePath.absolute type_order_file);
      let indices =
        AnnotatedGlobalEnvironment.ReadOnly.unannotated_global_environment global_environment
        |> UnannotatedGlobalEnvironment.ReadOnly.all_indices
      in
      let global_resolution = GlobalResolution.create global_environment in
      let class_hierarchy_dot =
        ClassHierarchy.to_dot (GlobalResolution.class_hierarchy global_resolution) ~indices
      in
      File.create ~content:class_hierarchy_dot type_order_file |> File.write);
    if debug then
      Statistics.event
        ~section:`Memory
        ~name:"shared memory size"
        ~integers:["size", Memory.heap_size ()]
        ();

    errors_environment
  in
  let () = Analysis.ErrorsEnvironment.populate_all_errors ~scheduler environment in
  Profiling.track_shared_memory_usage ();

  (* Only destroy the scheduler if the check command created it. *)
  environment
