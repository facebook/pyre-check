(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module exposes the top-level build functions (full build and incremental build) relied on in
   the buck BuildSystem implementation *)

open Base

exception LinkTreeConstructionError of string

let update_artifacts ~source_root ~artifact_root difference =
  let open Lwt.Infix in
  Log.info "Incrementally updating Python link-tree for type checking...";
  Artifacts.update ~source_root ~artifact_root difference
  >>= function
  | Result.Error message -> raise (LinkTreeConstructionError message)
  | Result.Ok () ->
      let to_artifact_path (relative, difference_kind) =
        let kind =
          match difference_kind with
          | BuildMap.Difference.Kind.New _
          | BuildMap.Difference.Kind.Changed _ ->
              ArtifactPath.Event.Kind.CreatedOrChanged
          | BuildMap.Difference.Kind.Deleted -> ArtifactPath.Event.Kind.Deleted
        in
        PyrePath.create_relative ~root:artifact_root ~relative
        |> ArtifactPath.create
        |> ArtifactPath.Event.create ~kind
      in
      BuildMap.Difference.to_alist difference |> List.map ~f:to_artifact_path |> Lwt.return


let do_incremental_build ~source_root ~artifact_root ~old_build_map ~new_build_map () =
  let open Lwt.Infix in
  let difference =
    Log.info "Calculating the scope of the re-build...";
    BuildMap.difference ~original:old_build_map new_build_map
  in
  update_artifacts ~source_root ~artifact_root difference
  >>= fun result ->
  Log.info "Rebuild completed. %d artifacts were changed" (List.length result);
  Lwt.return result


let to_relative_path ~root path = PyrePath.get_relative_to_root ~root ~path

let to_relative_paths ~root paths = List.filter_map paths ~f:(to_relative_path ~root)

let do_lookup_source ~index ~source_root ~artifact_root path =
  match to_relative_path ~root:artifact_root path with
  | None -> None
  | Some relative_artifact_path ->
      BuildMap.Indexed.lookup_source index relative_artifact_path
      |> Option.map ~f:(fun relative -> PyrePath.create_relative ~root:source_root ~relative)


let do_lookup_artifact ~index ~source_root ~artifact_root path =
  match to_relative_path ~root:source_root path with
  | None -> []
  | Some relative_source_path ->
      BuildMap.Indexed.lookup_artifact index relative_source_path
      |> List.map ~f:(fun relative -> PyrePath.create_relative ~root:artifact_root ~relative)


module Eager = struct
  module IncrementalBuildResult = struct
    type t = {
      build_map: BuildMap.t;
      targets: BuckTarget.t list;
      changed_artifacts: ArtifactPath.Event.t list;
    }
  end

  type build_result_t = (Interface.BuildResult.t, string) Interface.WithMetadata.t

  type incremental_build_result_t = (IncrementalBuildResult.t, string) Interface.WithMetadata.t

  let restore ~source_root ~artifact_root build_map =
    let open Lwt.Infix in
    Artifacts.populate ~source_root ~artifact_root build_map
    >>= function
    | Result.Error message -> raise (LinkTreeConstructionError message)
    | Result.Ok () -> Lwt.return_unit


  let lookup_source ~source_root ~artifact_root ~index path =
    do_lookup_source ~index ~source_root ~artifact_root path


  let lookup_artifact ~source_root ~artifact_root ~index path =
    do_lookup_artifact ~index ~source_root ~artifact_root path


  type t = {
    build: string list -> build_result_t Lwt.t;
    restore: BuildMap.t -> unit Lwt.t;
    full_incremental_build:
      old_build_map:BuildMap.t -> string list -> incremental_build_result_t Lwt.t;
    incremental_build_with_normalized_targets:
      old_build_map:BuildMap.t -> BuckTarget.t list -> incremental_build_result_t Lwt.t;
    fast_incremental_build_with_normalized_targets:
      old_build_map:BuildMap.t ->
      old_build_map_index:BuildMap.Indexed.t ->
      changed_paths:PyrePath.t list ->
      removed_paths:PyrePath.t list ->
      BuckTarget.t list ->
      incremental_build_result_t Lwt.t;
    lookup_source: index:BuildMap.Indexed.t -> PyrePath.t -> PyrePath.t option;
    lookup_artifact: index:BuildMap.Indexed.t -> PyrePath.t -> PyrePath.t list;
    identifier: string;
  }

  let build ~interface ~source_root ~artifact_root targets =
    let open Lwt.Infix in
    Interface.Eager.construct_build_map interface targets
    >>= fun { Interface.WithMetadata.data = build_map; metadata } ->
    Log.info "Constructing Python link-tree for type checking...";
    Artifacts.populate ~source_root ~artifact_root build_map
    >>= function
    | Result.Error message -> raise (LinkTreeConstructionError message)
    | Result.Ok () ->
        Lwt.return Interface.(WithMetadata.create { BuildResult.targets; build_map } ?metadata)


  let full_incremental_build ~interface ~source_root ~artifact_root ~old_build_map targets =
    let open Lwt.Infix in
    Interface.Eager.construct_build_map interface targets
    >>= fun { Interface.WithMetadata.data = build_map; metadata } ->
    do_incremental_build ~source_root ~artifact_root ~old_build_map ~new_build_map:build_map ()
    >>= fun changed_artifacts ->
    Lwt.return
      (Interface.WithMetadata.create
         { IncrementalBuildResult.targets; build_map; changed_artifacts }
         ?metadata)


  let create ~source_root ~artifact_root interface =
    let fast_incremental_build_with_normalized_targets
        ~old_build_map
        ~old_build_map_index:_
        ~changed_paths:_
        ~removed_paths:_
        targets
      =
      (* NOTE: The same query we relied on to optimize incremental build in Buck1 does not exist in
         Buck2. For now, fallback to a less optimized rebuild approach. *)
      full_incremental_build ~interface ~source_root ~artifact_root ~old_build_map targets
    in
    {
      build = build ~interface ~source_root ~artifact_root;
      restore = restore ~source_root ~artifact_root;
      full_incremental_build = full_incremental_build ~interface ~source_root ~artifact_root;
      incremental_build_with_normalized_targets =
        full_incremental_build ~interface ~source_root ~artifact_root;
      fast_incremental_build_with_normalized_targets;
      lookup_source = lookup_source ~source_root ~artifact_root;
      lookup_artifact = lookup_artifact ~source_root ~artifact_root;
      identifier = "new_server_buck2_bxl";
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


  let lookup_source ~index ~builder:{ lookup_source; _ } path = lookup_source ~index path

  let lookup_artifact ~index ~builder:{ lookup_artifact; _ } path = lookup_artifact ~index path

  let identifier_of { identifier; _ } = identifier
end

module Lazy = struct
  module IncrementalBuildResult = struct
    type t = {
      build_map: BuildMap.t;
      changed_artifacts: ArtifactPath.Event.t list;
    }
  end

  type t = {
    incremental_build:
      old_build_map:BuildMap.t -> SourcePath.t list -> IncrementalBuildResult.t Lwt.t;
    lookup_source: index:BuildMap.Indexed.t -> ArtifactPath.t -> SourcePath.t option;
    lookup_artifact: index:BuildMap.Indexed.t -> SourcePath.t -> ArtifactPath.t list;
  }

  let incremental_build ~interface ~source_root ~artifact_root ~old_build_map source_paths =
    let open Lwt.Infix in
    List.map source_paths ~f:SourcePath.raw
    |> to_relative_paths ~root:source_root
    |> Interface.Lazy.construct_build_map interface
    >>= fun build_map ->
    do_incremental_build ~source_root ~artifact_root ~old_build_map ~new_build_map:build_map ()
    >>= fun changed_artifacts -> Lwt.return { IncrementalBuildResult.build_map; changed_artifacts }


  let lookup_source ~source_root ~artifact_root ~index artifact_path =
    do_lookup_source ~source_root ~artifact_root ~index (ArtifactPath.raw artifact_path)
    |> Option.map ~f:SourcePath.create


  let lookup_artifact ~source_root ~artifact_root ~index source_path =
    do_lookup_artifact ~source_root ~artifact_root ~index (SourcePath.raw source_path)
    |> List.map ~f:ArtifactPath.create


  let create ~source_root ~artifact_root interface =
    {
      incremental_build = incremental_build ~interface ~source_root ~artifact_root;
      lookup_source = lookup_source ~source_root ~artifact_root;
      lookup_artifact = lookup_artifact ~source_root ~artifact_root;
    }


  let incremental_build ~old_build_map ~source_paths { incremental_build; _ } =
    incremental_build ~old_build_map source_paths


  let lookup_source ~index ~builder:{ lookup_source; _ } path = lookup_source ~index path

  let lookup_artifact ~index ~builder:{ lookup_artifact; _ } path = lookup_artifact ~index path
end
