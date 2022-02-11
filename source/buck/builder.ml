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

let build ~interface ~source_root ~artifact_root targets =
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


let restore ~source_root ~artifact_root build_map =
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


let full_incremental_build ~interface ~source_root ~artifact_root ~old_build_map targets =
  let open Lwt.Infix in
  Interface.normalize_targets interface targets
  >>= fun normalized_targets ->
  Interface.construct_build_map interface normalized_targets
  >>= fun { Interface.BuildResult.targets; build_map } ->
  do_incremental_build ~source_root ~artifact_root ~old_build_map ~new_build_map:build_map ()
  >>= fun changed_artifacts ->
  Lwt.return { IncrementalBuildResult.targets; build_map; changed_artifacts }


let incremental_build_with_normalized_targets
    ~interface
    ~source_root
    ~artifact_root
    ~old_build_map
    targets
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
    ~interface
    ~source_root
    ~old_build_map
    ~old_build_map_index
    ~changed_paths
    ~removed_paths
    targets
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
    ~interface
    ~source_root
    ~artifact_root
    ~old_build_map
    ~old_build_map_index
    ~changed_paths
    ~removed_paths
    targets
  =
  let open Lwt.Infix in
  Log.info "Attempting to perform fast incremental rebuild...";
  build_map_and_difference_from_paths
    ~interface
    ~source_root
    ~old_build_map
    ~old_build_map_index
    ~changed_paths
    ~removed_paths
    targets
  >>= function
  | Result.Error message ->
      Log.info "Fast incremental rebuild failed: %s. Falling back to the slow path..." message;
      incremental_build_with_normalized_targets
        ~interface
        ~source_root
        ~artifact_root
        ~old_build_map
        targets
  | Result.Ok (build_map, difference) ->
      let open Lwt.Infix in
      update_artifacts ~source_root ~artifact_root difference
      >>= fun changed_artifacts ->
      Lwt.return { IncrementalBuildResult.targets; build_map; changed_artifacts }


let incremental_build_with_unchanged_build_map
    ~source_root
    ~artifact_root
    ~build_map
    ~build_map_index
    ~changed_sources
    targets
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


let lookup_source ~source_root ~artifact_root ~index path =
  do_lookup_source ~index ~source_root ~artifact_root path


let do_lookup_artifact ~index ~source_root ~artifact_root path =
  match to_relative_path ~root:source_root path with
  | None -> []
  | Some relative_source_path ->
      BuildMap.Indexed.lookup_artifact index relative_source_path
      |> List.map ~f:(fun relative -> PyrePath.create_relative ~root:artifact_root ~relative)


let lookup_artifact ~source_root ~artifact_root ~index path =
  do_lookup_artifact ~index ~source_root ~artifact_root path


type t = {
  build: string list -> Interface.BuildResult.t Lwt.t;
  restore: BuildMap.t -> unit Lwt.t;
  full_incremental_build: old_build_map:BuildMap.t -> string list -> IncrementalBuildResult.t Lwt.t;
  incremental_build_with_normalized_targets:
    old_build_map:BuildMap.t -> Target.t list -> IncrementalBuildResult.t Lwt.t;
  fast_incremental_build_with_normalized_targets:
    old_build_map:BuildMap.t ->
    old_build_map_index:BuildMap.Indexed.t ->
    changed_paths:PyrePath.t list ->
    removed_paths:PyrePath.t list ->
    Target.t list ->
    IncrementalBuildResult.t Lwt.t;
  incremental_build_with_unchanged_build_map:
    build_map:BuildMap.t ->
    build_map_index:BuildMap.Indexed.t ->
    changed_sources:PyrePath.t list ->
    Target.t list ->
    IncrementalBuildResult.t Lwt.t;
  lookup_source: index:BuildMap.Indexed.t -> PyrePath.t -> PyrePath.t option;
  lookup_artifact: index:BuildMap.Indexed.t -> PyrePath.t -> PyrePath.t list;
  identifier: string;
}

let create ~source_root ~artifact_root interface =
  {
    build = build ~interface ~source_root ~artifact_root;
    restore = restore ~source_root ~artifact_root;
    full_incremental_build = full_incremental_build ~interface ~source_root ~artifact_root;
    incremental_build_with_normalized_targets =
      incremental_build_with_normalized_targets ~interface ~source_root ~artifact_root;
    fast_incremental_build_with_normalized_targets =
      fast_incremental_build_with_normalized_targets ~interface ~source_root ~artifact_root;
    incremental_build_with_unchanged_build_map =
      incremental_build_with_unchanged_build_map ~source_root ~artifact_root;
    lookup_source = lookup_source ~source_root ~artifact_root;
    lookup_artifact = lookup_artifact ~source_root ~artifact_root;
    identifier = "new_server";
  }


let create_v2 ~source_root ~artifact_root interface =
  let fast_incremental_build_with_normalized_targets
      ~old_build_map
      ~old_build_map_index:_
      ~changed_paths:_
      ~removed_paths:_
      targets
    =
    (* TODO: The same query we relied on to optimize incremental build in Buck1 does not exist in
       Buck2. For now, fallback to a less optimized rebuild approach. *)
    incremental_build_with_normalized_targets
      ~interface
      ~source_root
      ~artifact_root
      ~old_build_map
      targets
  in
  {
    build = build ~interface ~source_root ~artifact_root;
    restore = restore ~source_root ~artifact_root;
    full_incremental_build = full_incremental_build ~interface ~source_root ~artifact_root;
    incremental_build_with_normalized_targets =
      incremental_build_with_normalized_targets ~interface ~source_root ~artifact_root;
    fast_incremental_build_with_normalized_targets;
    incremental_build_with_unchanged_build_map =
      incremental_build_with_unchanged_build_map ~source_root ~artifact_root;
    lookup_source = lookup_source ~source_root ~artifact_root;
    lookup_artifact = lookup_artifact ~source_root ~artifact_root;
    identifier = "new_server_buck2";
  }


let build ~targets { build; _ } = build targets

let restore ~build_map { restore; _ } = restore build_map

let full_incremental_build ~old_build_map ~targets { full_incremental_build; _ } =
  full_incremental_build ~old_build_map targets


let incremental_build_with_normalized_targets
    ~old_build_map
    ~targets
    { incremental_build_with_normalized_targets; _ }
  =
  incremental_build_with_normalized_targets ~old_build_map targets


let fast_incremental_build_with_normalized_targets
    ~old_build_map
    ~old_build_map_index
    ~targets
    ~changed_paths
    ~removed_paths
    { fast_incremental_build_with_normalized_targets; _ }
  =
  fast_incremental_build_with_normalized_targets
    ~old_build_map
    ~old_build_map_index
    ~changed_paths
    ~removed_paths
    targets


let incremental_build_with_unchanged_build_map
    ~build_map
    ~build_map_index
    ~targets
    ~changed_sources
    { incremental_build_with_unchanged_build_map; _ }
  =
  incremental_build_with_unchanged_build_map ~build_map ~build_map_index ~changed_sources targets


let lookup_source ~index ~builder:{ lookup_source; _ } path = lookup_source ~index path

let lookup_artifact ~index ~builder:{ lookup_artifact; _ } path = lookup_artifact ~index path

let identifier_of { identifier; _ } = identifier
