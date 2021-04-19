(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
module Path = Pyre.Path

type t = {
  update: Path.t list -> Path.t list Lwt.t;
  cleanup: unit -> unit Lwt.t;
  lookup_source: Path.t -> Path.t option;
  lookup_artifact: Path.t -> Path.t list;
}

let update { update; _ } = update

let cleanup { cleanup; _ } = cleanup ()

let lookup_source { lookup_source; _ } = lookup_source

let lookup_artifact { lookup_artifact; _ } = lookup_artifact

let create_for_testing
    ?(update = fun _ -> Lwt.return [])
    ?(cleanup = fun () -> Lwt.return_unit)
    ?(lookup_source = fun path -> Some path)
    ?(lookup_artifact = fun path -> [path])
    ()
  =
  { update; cleanup; lookup_source; lookup_artifact }


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
  end

  let ensure_directory_exist_and_clean path =
    let result =
      let open Result in
      Path.create_directory_recursively path >>= fun () -> Path.remove_contents_of_directory path
    in
    match result with
    | Result.Error message -> raise (Buck.Builder.LinkTreeConstructionError message)
    | Result.Ok () -> ()


  let initialize_from_state (state : State.t) =
    let update paths =
      let incremental_build =
        let should_renormalize paths =
          let f path =
            let file_name = Path.last path in
            String.equal file_name "TARGETS" || String.equal file_name "BUCK"
          in
          List.exists paths ~f
        in
        if should_renormalize paths then
          Buck.Builder.full_incremental_build ~old_build_map:state.build_map ~targets:state.targets
        else
          Buck.Builder.incremental_build_with_normalized_targets
            ~old_build_map:state.build_map
            ~targets:state.normalized_targets
      in
      let open Lwt.Infix in
      incremental_build state.builder
      >>= fun {
                Buck.Builder.IncrementalBuildResult.targets = normalized_targets;
                build_map;
                changed_artifacts;
              } ->
      State.update ~normalized_targets ~build_map state;
      Lwt.return changed_artifacts
    in
    let cleanup () =
      Buck.Builder.cleanup state.builder;
      Lwt.return_unit
    in
    let lookup_source path =
      Buck.Builder.lookup_source ~index:state.build_map_index ~builder:state.builder path
    in
    let lookup_artifact path =
      Buck.Builder.lookup_artifact ~index:state.build_map_index ~builder:state.builder path
    in
    Lwt.return { update; cleanup; lookup_source; lookup_artifact }


  let initialize_from_options
      ~raw
      ~buck_options:
        { ServerConfiguration.Buck.mode; isolation_prefix; targets; source_root; artifact_root }
      ()
    =
    let open Lwt.Infix in
    ensure_directory_exist_and_clean artifact_root;
    let builder = Buck.Builder.create ?mode ?isolation_prefix ~source_root ~artifact_root raw in
    State.create_from_scratch ~builder ~targets ()
    >>= fun initial_state -> initialize_from_state initial_state
end

module Initializer = struct
  type build_system = t

  type t = { initialize: unit -> build_system Lwt.t }

  let run { initialize } = initialize ()

  let null = { initialize = (fun () -> Lwt.return (create_for_testing ())) }

  let buck ~raw buck_options =
    { initialize = BuckBuildSystem.initialize_from_options ~raw ~buck_options }


  let create_for_testing ~initialize () = { initialize }
end
