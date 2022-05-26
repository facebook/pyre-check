(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

type t = {
  update: SourcePath.t list -> ArtifactPath.t list Lwt.t;
  lookup_source: ArtifactPath.t -> SourcePath.t option;
  lookup_artifact: SourcePath.t -> ArtifactPath.t list;
  store: unit -> unit;
}

let update { update; _ } = update

let lookup_source { lookup_source; _ } = lookup_source

let lookup_artifact { lookup_artifact; _ } = lookup_artifact

let store { store; _ } = store ()

let default_lookup_source analysis_path = Some (ArtifactPath.raw analysis_path |> SourcePath.create)

let default_lookup_artifact source_path = [SourcePath.raw source_path |> ArtifactPath.create]

let create_for_testing
    ?(update = fun _ -> Lwt.return [])
    ?(lookup_source = default_lookup_source)
    ?(lookup_artifact = default_lookup_artifact)
    ?(store = fun () -> ())
    ()
  =
  { update; lookup_source; lookup_artifact; store }


module BuckBuildSystem = struct
  module State = struct
    type t = {
      builder: Buck.Builder.t;
      targets: string list;
      mutable normalized_targets: Buck.Target.t list;
      mutable build_map: Buck.BuildMap.t;
      (* Derived field of `build_map`. Do not update manually. *)
      mutable build_map_index: Buck.BuildMap.Indexed.t;
    }

    let create ~builder ~targets ~normalized_targets ~build_map () =
      {
        builder;
        targets;
        normalized_targets;
        build_map;
        build_map_index = Buck.BuildMap.index build_map;
      }


    let update ~normalized_targets ~build_map state =
      state.normalized_targets <- normalized_targets;
      state.build_map <- build_map;
      state.build_map_index <- Buck.BuildMap.index build_map;
      ()


    let create_from_scratch ~builder ~targets () =
      let open Lwt.Infix in
      Buck.Builder.build builder ~targets
      >>= fun { Buck.Interface.BuildResult.targets = normalized_targets; build_map } ->
      Lwt.return (create ~targets ~builder ~normalized_targets ~build_map ())


    let create_from_saved_state ~builder ~targets ~normalized_targets ~build_map () =
      let open Lwt.Infix in
      (* NOTE (grievejia): This may not be a 100% faithful restore, since there is no guarantee that
         the source directory contains exactly the same set of files when saved state gets stored
         and saved state gets loaded. It is possible that we might be creating some dead symlinks by
         calling `restore` here.

         But that should be fine -- it is guaranteed that after saved state loading, the server will
         process another incremental update request to bring everything up-to-date again. If that
         incremental update is correctly handled, the dead links will be properly cleaned up. *)
      Buck.Builder.restore builder ~build_map
      >>= fun () -> Lwt.return (create ~targets ~builder ~normalized_targets ~build_map ())
  end

  (* This module defines how `State.t` will be preserved in the saved state. *)
  module SerializableState = struct
    type t = {
      targets: string list;
      normalized_targets: Buck.Target.t list;
      serialized_build_map: (string * string) list;
    }

    module Serialized = struct
      type nonrec t = t

      let prefix = Prefix.make ()

      let description = "Buck Builder States"
    end

    let serialize = Fn.id

    let deserialize = Fn.id
  end

  module SavedState = Memory.Serializer (SerializableState)

  (* Both `integers` and `normals` are functions that return a list instead of a list directly,
     since some of the logging may depend on the return value of `f`. *)
  let with_logging ?(integers = fun _ -> []) ?(normals = fun () -> []) f =
    let open Lwt.Infix in
    let timer = Timer.start () in
    Lwt.catch
      (fun () ->
        f ()
        >>= fun result ->
        let millisecond = Timer.stop_in_ms timer in
        let normals = ("version", Version.version ()) :: normals () in
        let integers = ("runtime", millisecond) :: integers result in
        Statistics.buck_event ~normals ~integers ();
        Lwt.return result)
      (fun exn ->
        let millisecond = Timer.stop_in_ms timer in
        let normals =
          ("version", Version.version ()) :: ("exception", Exn.to_string exn) :: normals ()
        in
        let integers = ["runtime", millisecond] in
        Statistics.buck_event ~normals ~integers ();
        Lwt.fail exn)


  module IncrementalBuilder = struct
    type t = {
      name: string;
      run: Buck.Builder.t -> Buck.Builder.IncrementalBuildResult.t Lwt.t;
    }
  end

  let initialize_from_state (state : State.t) =
    let update source_paths =
      let raw_paths = List.map source_paths ~f:SourcePath.raw in
      let incremental_builder =
        let should_renormalize paths =
          let f path =
            let file_name = PyrePath.last path in
            String.equal file_name "TARGETS" || String.equal file_name "BUCK"
          in
          List.exists paths ~f
        in
        let should_reconstruct_build_map paths =
          let f path =
            List.is_empty
              (Buck.Builder.lookup_artifact
                 ~index:state.build_map_index
                 ~builder:state.builder
                 path)
          in
          List.exists paths ~f
        in
        if should_renormalize raw_paths then
          {
            IncrementalBuilder.name = "full";
            run =
              Buck.Builder.full_incremental_build
                ~old_build_map:state.build_map
                ~targets:state.targets;
          }
        else
          let changed_paths, removed_paths =
            (* TODO (T90174546): This check may lead to temporary inconsistent view of the
               filesystem with `ModuleTracker`. *)
            List.partition_tf raw_paths ~f:PyrePath.file_exists
          in
          if List.is_empty removed_paths && not (should_reconstruct_build_map changed_paths) then
            {
              IncrementalBuilder.name = "skip_rebuild";
              run =
                Buck.Builder.incremental_build_with_unchanged_build_map
                  ~build_map:state.build_map
                  ~build_map_index:state.build_map_index
                  ~targets:state.normalized_targets
                  ~changed_sources:raw_paths;
            }
          else
            {
              IncrementalBuilder.name = "skip_renormalize_optimized";
              run =
                Buck.Builder.fast_incremental_build_with_normalized_targets
                  ~old_build_map:state.build_map
                  ~old_build_map_index:state.build_map_index
                  ~targets:state.normalized_targets
                  ~changed_paths
                  ~removed_paths;
            }
      in
      let open Lwt.Infix in
      with_logging
        ~integers:(fun changed_analysis_paths ->
          [
            "number_of_user_changed_files", List.length source_paths;
            "number_of_updated_files", List.length changed_analysis_paths;
          ])
        ~normals:(fun _ ->
          [
            "buck_builder_type", Buck.Builder.identifier_of state.builder;
            "event_type", "rebuild";
            "event_subtype", incremental_builder.name;
          ])
        (fun () ->
          incremental_builder.run state.builder
          >>= fun {
                    Buck.Builder.IncrementalBuildResult.targets = normalized_targets;
                    build_map;
                    changed_artifacts;
                  } ->
          State.update ~normalized_targets ~build_map state;
          let changed_analysis_paths = List.map changed_artifacts ~f:ArtifactPath.create in
          Lwt.return changed_analysis_paths)
    in
    let lookup_source path =
      ArtifactPath.raw path
      |> Buck.Builder.lookup_source ~index:state.build_map_index ~builder:state.builder
      |> Option.map ~f:SourcePath.create
    in
    let lookup_artifact path =
      SourcePath.raw path
      |> Buck.Builder.lookup_artifact ~index:state.build_map_index ~builder:state.builder
      |> List.map ~f:ArtifactPath.create
    in
    let store () =
      {
        SerializableState.targets = state.targets;
        normalized_targets = state.normalized_targets;
        serialized_build_map = Buck.BuildMap.to_alist state.build_map;
      }
      |> SavedState.store
    in
    { update; lookup_source; lookup_artifact; store }


  let initialize_from_options ~builder targets =
    let open Lwt.Infix in
    with_logging
      ~integers:(fun { State.build_map; _ } ->
        [
          "number_of_user_changed_files", 0;
          "number_of_updated_files", Buck.BuildMap.artifact_count build_map;
        ])
      ~normals:(fun _ ->
        [
          "buck_builder_type", Buck.Builder.identifier_of builder;
          "event_type", "build";
          "event_subtype", "cold_start";
        ])
      (fun () -> State.create_from_scratch ~builder ~targets ())
    >>= fun initial_state -> Lwt.return (initialize_from_state initial_state)


  let initialize_from_saved_state ~builder () =
    let open Lwt.Infix in
    (* NOTE (grievejia): For saved state loading, are still using the passed-in `mode`,
       `isolation_prefix`, `source_root`, and `artifact_root`, instead of preserving these options
       in saved state itself. For `source_root` and `artifact_root`, this is actually mandatory
       since these roots may legitimately change when loading states on a different machine. But for
       `mode` and `isolation_prefix`, an argument can be made that in the future we should indeed
       store them into saved state and check for potential changes when loading the state. *)
    let { SerializableState.targets; normalized_targets; serialized_build_map } =
      SavedState.load ()
    in
    with_logging
      ~integers:(fun { State.build_map; _ } ->
        [
          "number_of_user_changed_files", 0;
          "number_of_updated_files", Buck.BuildMap.artifact_count build_map;
        ])
      ~normals:(fun _ ->
        [
          "buck_builder_type", Buck.Builder.identifier_of builder;
          "event_type", "build";
          "event_subtype", "saved_state";
        ])
      (fun () ->
        let build_map =
          Buck.BuildMap.Partial.of_alist_exn serialized_build_map |> Buck.BuildMap.create
        in
        State.create_from_saved_state ~builder ~targets ~normalized_targets ~build_map ())
    >>= fun initial_state -> Lwt.return (initialize_from_state initial_state)
end

module TrackUnwatchedDependencyBuildSystem = struct
  module State = struct
    type t = {
      change_indicator_path: PyrePath.t;
      unwatched_files: Configuration.UnwatchedFiles.t;
      mutable checksum_map: ChecksumMap.t;
    }
  end

  let unwatched_files_may_change ~change_indicator_path paths =
    List.exists paths ~f:(PyrePath.equal change_indicator_path)


  let initialize_from_state (state : State.t) =
    let update source_paths =
      let raw_paths = List.map source_paths ~f:SourcePath.raw in
      let paths =
        if unwatched_files_may_change ~change_indicator_path:state.change_indicator_path raw_paths
        then (
          Log.info "Detecting potential changes in unwatched files...";
          (* NOTE(grievejia): If checksum map loading fails, there will be no way for us to figure
             out what has changed in the unwatched directory. Bring down the server immediately to
             avoid incremental inconsistency. *)
          let new_checksum_map = ChecksumMap.load_exn state.unwatched_files in
          let differences = ChecksumMap.difference ~original:state.checksum_map new_checksum_map in
          state.checksum_map <- new_checksum_map;
          List.map differences ~f:(fun { ChecksumMap.Difference.path; _ } ->
              (* TODO (T90174546): Utilizes the info provided by `ChecksumMap.Difference.kind`. *)
              PyrePath.create_relative ~root:state.unwatched_files.root ~relative:path))
        else
          []
      in
      List.map paths ~f:ArtifactPath.create |> Lwt.return
    in
    let lookup_source = default_lookup_source in
    let lookup_artifact = default_lookup_artifact in
    let store () = () in
    { update; lookup_source; lookup_artifact; store }


  let initialize_from_options
      { Configuration.UnwatchedDependency.change_indicator; files = unwatched_files }
    =
    let change_indicator_path = Configuration.ChangeIndicator.to_path change_indicator in
    let checksum_map =
      match ChecksumMap.load unwatched_files with
      | Result.Ok checksum_map -> checksum_map
      | Result.Error message ->
          (* NOTE(grievejia): We do not want a hard crash here, as the initialization may be invoked
             from a non-server command where incremental check is not needed and therefore the
             content of the checksum map does not matter. *)
          Log.warning "Initial checksum map loading failed: %s. Assuming an empty map." message;
          ChecksumMap.empty
    in
    Lwt.return
      (initialize_from_state { State.change_indicator_path; unwatched_files; checksum_map })
end

module Initializer = struct
  type build_system = t

  type t = {
    initialize: unit -> build_system Lwt.t;
    load: unit -> build_system Lwt.t;
    cleanup: unit -> unit Lwt.t;
  }

  let run { initialize; _ } = initialize ()

  let load { load; _ } = load ()

  let cleanup { cleanup; _ } = cleanup ()

  let null =
    {
      initialize = (fun () -> Lwt.return (create_for_testing ()));
      load = (fun () -> Lwt.return (create_for_testing ()));
      cleanup = (fun () -> Lwt.return_unit);
    }


  let buck ~builder ~artifact_root ~targets () =
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
      BuckBuildSystem.initialize_from_options ~builder targets
    in
    let load () =
      ensure_directory_exist_and_clean artifact_root;
      BuckBuildSystem.initialize_from_saved_state ~builder ()
    in
    let cleanup () =
      match PyrePath.remove_contents_of_directory artifact_root with
      | Result.Error message ->
          Log.warning "Encountered error during buck builder cleanup: %s" message;
          Lwt.return_unit
      | Result.Ok () -> Lwt.return_unit
    in
    { initialize; load; cleanup }


  let track_unwatched_dependency unwatched_dependency =
    {
      initialize =
        (fun () -> TrackUnwatchedDependencyBuildSystem.initialize_from_options unwatched_dependency);
      load =
        (fun () ->
          (* NOTE(grievejia): The only state used in this build system is the checksum map. Given
             that checksum map loading seems to be a fairly cheap thing to do, I think it makes
             sense to avoid putting it into saved state, and simply re-build the map on each saved
             state loading. *)
          TrackUnwatchedDependencyBuildSystem.initialize_from_options unwatched_dependency);
      cleanup = (fun () -> Lwt.return_unit);
    }


  let create_for_testing ~initialize ~load ~cleanup () = { initialize; load; cleanup }
end

let get_initializer source_paths =
  match source_paths with
  | Configuration.SourcePaths.Simple _ -> Initializer.null
  | Configuration.SourcePaths.WithUnwatchedDependency { unwatched_dependency; _ } ->
      Initializer.track_unwatched_dependency unwatched_dependency
  | Configuration.SourcePaths.Buck
      { Configuration.Buck.mode; isolation_prefix; use_buck2; source_root; artifact_root; targets }
    ->
      let builder =
        match use_buck2 with
        | false ->
            let raw = Buck.Raw.create ~additional_log_size:10 () in
            let interface = Buck.Interface.create ?mode ?isolation_prefix raw in
            Buck.Builder.create ~source_root ~artifact_root interface
        | true ->
            let raw = Buck.Raw.create_v2 ~additional_log_size:10 () in
            let interface = Buck.Interface.create_v2 ?mode ?isolation_prefix raw in
            Buck.Builder.create_v2 ~source_root ~artifact_root interface
      in
      Initializer.buck ~builder ~artifact_root ~targets ()


let with_build_system ~f source_paths =
  let open Lwt.Infix in
  let build_system_initializer = get_initializer source_paths in
  Lwt.finalize
    (fun () -> Initializer.run build_system_initializer >>= fun build_system -> f build_system)
    (fun () -> Initializer.cleanup build_system_initializer)
