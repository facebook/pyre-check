(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

type t = {
  update_working_set: SourcePath.t list -> ArtifactPath.Event.t list Lwt.t;
  update_sources:
    working_set:SourcePath.t list -> SourcePath.Event.t list -> ArtifactPath.Event.t list Lwt.t;
  lookup_source: ArtifactPath.t -> SourcePath.t option;
  lookup_artifact: SourcePath.t -> ArtifactPath.t list;
}

let default_lookup_source analysis_path = Some (ArtifactPath.raw analysis_path |> SourcePath.create)

let default_lookup_artifact source_path = [SourcePath.raw source_path |> ArtifactPath.create]

let create_for_testing
    ?(update_working_set = fun _ -> Lwt.return [])
    ?(update_sources = fun ~working_set:_ _ -> Lwt.return [])
    ?(lookup_source = default_lookup_source)
    ?(lookup_artifact = default_lookup_artifact)
    ()
  =
  { update_working_set; update_sources; lookup_source; lookup_artifact }


let update_working_set { update_working_set; _ } = update_working_set

let update_sources { update_sources; _ } = update_sources

let lookup_source { lookup_source; _ } = lookup_source

let lookup_artifact { lookup_artifact; _ } = lookup_artifact

module LazyBuckBuilder = Buck.Builder.Lazy

module BuckBuildSystem = struct
  module State = struct
    type t = {
      builder: LazyBuckBuilder.t;
      mutable build_map: Buck.BuildMap.t;
      (* Derived field of `build_map`. Do not update manually. *)
      mutable build_map_index: Buck.BuildMap.Indexed.t;
    }

    let create_empty builder =
      let empty_build_map = Buck.BuildMap.(Partial.empty |> create) in
      {
        builder;
        build_map = empty_build_map;
        build_map_index = Buck.BuildMap.index empty_build_map;
      }


    let update ~build_map state =
      let () =
        state.build_map <- build_map;
        state.build_map_index <- Buck.BuildMap.index build_map
      in
      ()
  end

  let create (state : State.t) =
    let update_working_set source_paths =
      let%lwt { LazyBuckBuilder.IncrementalBuildResult.build_map; changed_artifacts } =
        LazyBuckBuilder.incremental_build state.builder ~old_build_map:state.build_map ~source_paths
      in
      if not (List.is_empty changed_artifacts) then
        State.update ~build_map state;
      Lwt.return changed_artifacts
    in
    let lookup_source =
      LazyBuckBuilder.lookup_source ~index:state.build_map_index ~builder:state.builder
    in
    let lookup_artifact =
      LazyBuckBuilder.lookup_artifact ~index:state.build_map_index ~builder:state.builder
    in
    let build_map_may_change source_path_events =
      (* NOTE: This is a very conservative heuristic. We only skip rebuild when all edits are
         changes in files already existed in the link tree. *)
      let not_changed_in_map { SourcePath.Event.kind; path } =
        match kind with
        | SourcePath.Event.Kind.Deleted -> true
        | SourcePath.Event.Kind.CreatedOrChanged -> List.is_empty (lookup_artifact path)
      in
      List.exists source_path_events ~f:not_changed_in_map
    in
    let update_sources ~working_set source_path_events =
      if not (build_map_may_change source_path_events) then
        Lwt.return []
      else
        update_working_set working_set
    in
    { update_working_set; update_sources; lookup_source; lookup_artifact }
end

module Initializer = struct
  type build_system = t

  type t = {
    initialize: unit -> build_system;
    cleanup: unit -> unit;
  }

  let null = { initialize = (fun () -> create_for_testing ()); cleanup = (fun () -> ()) }

  let buck ~artifact_root builder =
    let ensure_directory_exist_and_clean path =
      let result =
        let open Result in
        PyrePath.create_directory_recursively path
        >>= fun () -> PyrePath.remove_contents_of_directory path
      in
      match result with
      | Result.Error message -> raise (Buck.Builder.LinkTreeConstructionError message)
      | Result.Ok () -> ()
    in
    let initialize () =
      ensure_directory_exist_and_clean artifact_root;
      BuckBuildSystem.State.create_empty builder |> BuckBuildSystem.create
    in
    let cleanup () =
      match PyrePath.remove_contents_of_directory artifact_root with
      | Result.Error message ->
          Log.warning "Encountered error during buck builder cleanup: %s" message;
          ()
      | Result.Ok () -> ()
    in
    { initialize; cleanup }


  let create_for_testing ~initialize ~cleanup () = { initialize; cleanup }

  let initialize { initialize; _ } = initialize ()

  let cleanup { cleanup; _ } = cleanup ()
end

let get_initializer = function
  | Configuration.SourcePaths.Simple _
  | Configuration.SourcePaths.WithUnwatchedDependency _ ->
      (* TODO: Implement support for unwatched dependency in codenav *)
      Initializer.null
  | Configuration.SourcePaths.Buck
      {
        Configuration.Buck.mode;
        isolation_prefix;
        bxl_builder;
        use_buck2;
        source_root;
        artifact_root;
        _;
      } -> (
      match use_buck2 with
      | false -> failwith "Code navigation server only supports Buck2"
      | true -> (
          match bxl_builder with
          | None -> failwith "Code navigation server requires a BXL builder to function"
          | Some bxl_builder ->
              Buck.Raw.V2.create ~additional_log_size:10 ()
              |> Buck.Interface.Lazy.create ?mode ?isolation_prefix ~bxl_builder
              |> Buck.Builder.Lazy.create ~source_root ~artifact_root
              |> Initializer.buck ~artifact_root))
