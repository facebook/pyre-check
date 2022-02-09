(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

exception LinkTreeConstructionError of string

module IncrementalBuildResult = struct
  type t = {
    build_map: BuildMap.t;
    targets: Target.t list;
    changed_artifacts: PyrePath.t list;
  }
end

type t = {
  interface: Interface.t;
  source_root: PyrePath.t;
  artifact_root: PyrePath.t;
}

let create ~source_root ~artifact_root interface = { interface; source_root; artifact_root }

let build ~targets { interface; source_root; artifact_root } =
  let open Lwt.Infix in
  Interface.normalize_targets interface targets
  >>= fun normalized_targets ->
  Interface.construct_build_map interface normalized_targets
  >>= fun ({ Interface.BuildResult.build_map; _ } as build_result) ->
  Log.info "Constructing Python link-tree for type checking...";
  Artifacts.populate ~source_root ~artifact_root build_map
  >>= function
  | Result.Error message -> raise (LinkTreeConstructionError message)
  | Result.Ok () -> Lwt.return build_result


let restore ~build_map { source_root; artifact_root; _ } =
  let open Lwt.Infix in
  Artifacts.populate ~source_root ~artifact_root build_map
  >>= function
  | Result.Error message -> raise (LinkTreeConstructionError message)
  | Result.Ok () -> Lwt.return_unit


let update_artifacts ~source_root ~artifact_root difference =
  let open Lwt.Infix in
  Log.info "Incrementally updating Python link-tree for type checking...";
  Artifacts.update ~source_root ~artifact_root difference
  >>= function
  | Result.Error message -> raise (LinkTreeConstructionError message)
  | Result.Ok () ->
      let to_artifact_path (relative, _) = PyrePath.create_relative ~root:artifact_root ~relative in
      BuildMap.Difference.to_alist difference |> List.map ~f:to_artifact_path |> Lwt.return


let do_incremental_build ~source_root ~artifact_root ~old_build_map ~new_build_map () =
  let difference =
    Log.info "Calculating the scope of the re-build...";
    BuildMap.difference ~original:old_build_map new_build_map
  in
  update_artifacts ~source_root ~artifact_root difference


let full_incremental_build ~old_build_map ~targets { interface; source_root; artifact_root } =
  let open Lwt.Infix in
  Interface.normalize_targets interface targets
  >>= fun normalized_targets ->
  Interface.construct_build_map interface normalized_targets
  >>= fun { Interface.BuildResult.targets; build_map } ->
  do_incremental_build ~source_root ~artifact_root ~old_build_map ~new_build_map:build_map ()
  >>= fun changed_artifacts ->
  Lwt.return { IncrementalBuildResult.targets; build_map; changed_artifacts }


let incremental_build_with_normalized_targets
    ~old_build_map
    ~targets
    { interface; source_root; artifact_root }
  =
  let open Lwt.Infix in
  Interface.construct_build_map interface targets
  >>= fun { Interface.BuildResult.targets; build_map } ->
  do_incremental_build ~source_root ~artifact_root ~old_build_map ~new_build_map:build_map ()
  >>= fun changed_artifacts ->
  Lwt.return { IncrementalBuildResult.targets; build_map; changed_artifacts }


let to_relative_path ~root path = PyrePath.get_relative_to_root ~root ~path

let to_relative_paths ~root paths = List.filter_map paths ~f:(to_relative_path ~root)

let compute_difference_from_removed_relative_paths ~build_map_index removed_paths =
  List.concat_map removed_paths ~f:(BuildMap.Indexed.lookup_artifact build_map_index)
  |> List.map ~f:(fun artifact -> artifact, BuildMap.Difference.Kind.Deleted)
  (* This `of_alist_exn` won't raise because build map never hold duplicated artifact paths. *)
  |> BuildMap.Difference.of_alist_exn


let compute_difference_from_removed_paths ~source_root ~build_map_index removed_paths =
  to_relative_paths ~root:source_root removed_paths
  |> compute_difference_from_removed_relative_paths ~build_map_index


let compute_difference_from_changed_relative_paths ~build_map_index changed_paths =
  List.concat_map changed_paths ~f:(fun source_path ->
      BuildMap.Indexed.lookup_artifact build_map_index source_path
      |> List.map ~f:(fun artifact_path ->
             artifact_path, BuildMap.Difference.Kind.Changed source_path))
  (* This `of_alist_exn` won't raise because build map never hold duplicated artifact paths. *)
  |> BuildMap.Difference.of_alist_exn


let compute_difference_from_changed_paths ~source_root ~interface ~targets changed_paths =
  let open Lwt.Infix in
  try
    Interface.query_owner_targets interface ~targets changed_paths
    >>= fun query_output ->
    Log.info "Constructing local build map for changed files...";
    match Interface.BuckChangedTargetsQueryOutput.to_build_map_batch query_output with
    | Result.Error _ as error -> Lwt.return error
    | Result.Ok build_map ->
        to_relative_paths ~root:source_root changed_paths
        |> compute_difference_from_changed_relative_paths
             ~build_map_index:(BuildMap.index build_map)
        |> Lwt.return_ok
  with
  | Interface.JsonError message -> Lwt.return_error message
  | Raw.BuckError { description; _ } ->
      let message = Format.sprintf "Buck query failed: %s" description in
      Lwt.return_error message


let build_map_and_difference_from_paths
    ~old_build_map
    ~old_build_map_index
    ~targets
    ~changed_paths
    ~removed_paths
    { interface; source_root; _ }
  =
  let open Lwt.Infix in
  Log.info "Computing build map deltas from changed paths...";
  compute_difference_from_changed_paths ~source_root ~interface ~targets changed_paths
  >>= function
  | Result.Error _ as error -> Lwt.return error
  | Result.Ok difference_from_changed_paths -> (
      Log.info "Computing build map deltas from removed paths...";
      let difference_from_removed_paths =
        compute_difference_from_removed_paths
          ~source_root
          ~build_map_index:old_build_map_index
          removed_paths
      in
      Log.info "Merging build map deltas...";
      match
        BuildMap.Difference.merge difference_from_changed_paths difference_from_removed_paths
      with
      | Result.Error artifact_path ->
          Format.sprintf "Conflicting source updates on artifact `%s`" artifact_path
          |> Lwt.return_error
      | Result.Ok difference -> (
          Log.info "Updating old build map...";
          match BuildMap.strict_apply_difference ~difference old_build_map with
          | Result.Ok build_map -> Lwt.return_ok (build_map, difference)
          | Result.Error artifact_path ->
              Format.sprintf "Cannot determine source path for artifact `%s`" artifact_path
              |> Lwt.return_error))


let fast_incremental_build_with_normalized_targets
    ~old_build_map
    ~old_build_map_index
    ~targets
    ~changed_paths
    ~removed_paths
    ({ source_root; artifact_root; _ } as builder)
  =
  let open Lwt.Infix in
  Log.info "Attempting to perform fast incremental rebuild...";
  build_map_and_difference_from_paths
    ~old_build_map
    ~old_build_map_index
    ~targets
    ~changed_paths
    ~removed_paths
    builder
  >>= function
  | Result.Error message ->
      Log.info "Fast incremental rebuild failed: %s. Falling back to the slow path..." message;
      incremental_build_with_normalized_targets ~old_build_map ~targets builder
  | Result.Ok (build_map, difference) ->
      let open Lwt.Infix in
      update_artifacts ~source_root ~artifact_root difference
      >>= fun changed_artifacts ->
      Lwt.return { IncrementalBuildResult.targets; build_map; changed_artifacts }


let incremental_build_with_unchanged_build_map
    ~build_map
    ~build_map_index
    ~targets
    ~changed_sources
    { source_root; artifact_root; _ }
  =
  let changed_artifacts =
    to_relative_paths ~root:source_root changed_sources
    |> List.concat_map ~f:(BuildMap.Indexed.lookup_artifact build_map_index)
    |> List.map ~f:(fun relative -> PyrePath.create_relative ~root:artifact_root ~relative)
  in
  Lwt.return { IncrementalBuildResult.targets; build_map; changed_artifacts }


let do_lookup_source ~index ~source_root ~artifact_root path =
  match to_relative_path ~root:artifact_root path with
  | None -> None
  | Some relative_artifact_path ->
      BuildMap.Indexed.lookup_source index relative_artifact_path
      |> Option.map ~f:(fun relative -> PyrePath.create_relative ~root:source_root ~relative)


let lookup_source ~index ~builder:{ source_root; artifact_root; _ } path =
  do_lookup_source ~index ~source_root ~artifact_root path


let do_lookup_artifact ~index ~source_root ~artifact_root path =
  match to_relative_path ~root:source_root path with
  | None -> []
  | Some relative_source_path ->
      BuildMap.Indexed.lookup_artifact index relative_source_path
      |> List.map ~f:(fun relative -> PyrePath.create_relative ~root:artifact_root ~relative)


let lookup_artifact ~index ~builder:{ source_root; artifact_root; _ } path =
  do_lookup_artifact ~index ~source_root ~artifact_root path
