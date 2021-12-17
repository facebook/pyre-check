(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

type t = {
  update: PyrePath.t list -> PyrePath.t list Lwt.t;
  lookup_source: PyrePath.t -> PyrePath.t option;
  lookup_artifact: PyrePath.t -> PyrePath.t list;
  store: unit -> unit;
}

let update { update; _ } = update

let lookup_source { lookup_source; _ } = lookup_source

let lookup_artifact { lookup_artifact; _ } = lookup_artifact

let store { store; _ } = store ()

let create_for_testing
    ?(update = fun _ -> Lwt.return [])
    ?(lookup_source = fun path -> Some path)
    ?(lookup_artifact = fun path -> [path])
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
      >>= fun { Buck.Builder.BuildResult.targets = normalized_targets; build_map } ->
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

      let unmarshall value = Marshal.from_string value 0
    end

    let serialize = Fn.id

    let deserialize = Fn.id
  end

  module SavedState = Memory.Serializer (SerializableState)

  let ensure_directory_exist_and_clean path =
    let result =
      let open Result in
      PyrePath.create_directory_recursively path
      >>= fun () -> PyrePath.remove_contents_of_directory path
    in
    match result with
    | Result.Error message -> raise (Buck.Builder.LinkTreeConstructionError message)
    | Result.Ok () -> ()


  (* Both `integers` and `normals` are functions that return a list instead of a list directly,
     since some of the logging may depend on the return value of `f`. *)
  let with_logging ?(integers = fun _ -> []) ?(normals = fun _ -> []) f =
    let open Lwt.Infix in
    let timer = Timer.start () in
    f ()
    >>= fun result ->
    let millisecond = Timer.stop_in_ms timer in
    let normals = ("version", Version.version ()) :: normals result in
    let integers = ("runtime", millisecond) :: integers result in
    Statistics.buck_event ~normals ~integers ();
    Lwt.return result


  module IncrementalBuilder = struct
    type t = {
      name: string;
      run: Buck.Builder.t -> Buck.Builder.IncrementalBuildResult.t Lwt.t;
    }
  end

  let initialize_from_state (state : State.t) =
    let update paths =
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
        if should_renormalize paths then
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
            List.partition_tf paths ~f:PyrePath.file_exists
          in
          if List.is_empty removed_paths && not (should_reconstruct_build_map changed_paths) then
            {
              IncrementalBuilder.name = "skip_rebuild";
              run =
                Buck.Builder.incremental_build_with_unchanged_build_map
                  ~build_map:state.build_map
                  ~build_map_index:state.build_map_index
                  ~targets:state.normalized_targets
                  ~changed_sources:paths;
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
        ~integers:(fun changed_artifacts ->
          [
            "number_of_user_changed_files", List.length paths;
            "number_of_updated_files", List.length changed_artifacts;
          ])
        ~normals:(fun _ -> ["event_type", "rebuild"; "event_subtype", incremental_builder.name])
        (fun () ->
          incremental_builder.run state.builder
          >>= fun {
                    Buck.Builder.IncrementalBuildResult.targets = normalized_targets;
                    build_map;
                    changed_artifacts;
                  } ->
          State.update ~normalized_targets ~build_map state;
          Lwt.return changed_artifacts)
    in
    let lookup_source path =
      Buck.Builder.lookup_source ~index:state.build_map_index ~builder:state.builder path
    in
    let lookup_artifact path =
      Buck.Builder.lookup_artifact ~index:state.build_map_index ~builder:state.builder path
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


  let initialize_from_options
      ~raw
      ~buck_options:
        { Configuration.Buck.mode; isolation_prefix; targets; source_root; artifact_root }
      ()
    =
    let open Lwt.Infix in
    ensure_directory_exist_and_clean artifact_root;
    let builder = Buck.Builder.create ?mode ?isolation_prefix ~source_root ~artifact_root raw in
    with_logging
      ~integers:(fun { State.build_map; _ } ->
        [
          "number_of_user_changed_files", 0;
          "number_of_updated_files", Buck.BuildMap.artifact_count build_map;
        ])
      ~normals:(fun _ -> ["event_type", "build"; "event_subtype", "cold_start"])
      (fun () -> State.create_from_scratch ~builder ~targets ())
    >>= fun initial_state -> Lwt.return (initialize_from_state initial_state)


  let initialize_from_saved_state
      ~raw
      ~buck_options:{ Configuration.Buck.mode; isolation_prefix; source_root; artifact_root; _ }
      ()
    =
    let open Lwt.Infix in
    ensure_directory_exist_and_clean artifact_root;
    (* NOTE (grievejia): For saved state loading, are still using the passed-in `mode`,
       `isolation_prefix`, `source_root`, and `artifact_root`, instead of preserving these options
       in saved state itself. For `source_root` and `artifact_root`, this is actually mandatory
       since these roots may legitimately change when loading states on a different machine. But for
       `mode` and `isolation_prefix`, an argument can be made that in the future we should indeed
       store them into saved state and check for potential changes when loading the state. *)
    let { SerializableState.targets; normalized_targets; serialized_build_map } =
      SavedState.load ()
    in
    let builder = Buck.Builder.create ?mode ?isolation_prefix ~source_root ~artifact_root raw in
    with_logging
      ~integers:(fun { State.build_map; _ } ->
        [
          "number_of_user_changed_files", 0;
          "number_of_updated_files", Buck.BuildMap.artifact_count build_map;
        ])
      ~normals:(fun _ -> ["event_type", "build"; "event_subtype", "saved_state"])
      (fun () ->
        let build_map =
          Buck.BuildMap.Partial.of_alist_exn serialized_build_map |> Buck.BuildMap.create
        in
        State.create_from_saved_state ~builder ~targets ~normalized_targets ~build_map ())
    >>= fun initial_state -> Lwt.return (initialize_from_state initial_state)


  let cleanup artifact_root =
    match PyrePath.remove_contents_of_directory artifact_root with
    | Result.Error message ->
        Log.warning "Encountered error during buck builder cleanup: %s" message
    | Result.Ok () -> ()
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


  let buck ~raw ({ Configuration.Buck.artifact_root; _ } as buck_options) =
    {
      initialize = BuckBuildSystem.initialize_from_options ~raw ~buck_options;
      load = BuckBuildSystem.initialize_from_saved_state ~raw ~buck_options;
      cleanup =
        (fun () ->
          BuckBuildSystem.cleanup artifact_root;
          Lwt.return_unit);
    }


  let create_for_testing ~initialize ~load ~cleanup () = { initialize; load; cleanup }
end

let get_initializer source_paths =
  match source_paths with
  | Configuration.SourcePaths.Simple _ -> Initializer.null
  | Configuration.SourcePaths.Buck buck_options ->
      let raw = Buck.Raw.create ~additional_log_size:10 () in
      Initializer.buck ~raw buck_options


let with_build_system ~f source_paths =
  let open Lwt.Infix in
  let build_system_initializer = get_initializer source_paths in
  Lwt.finalize
    (fun () -> Initializer.run build_system_initializer >>= fun build_system -> f build_system)
    (fun () -> Initializer.cleanup build_system_initializer)
