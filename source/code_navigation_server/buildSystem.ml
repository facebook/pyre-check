(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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

module Initializer = struct
  type build_system = t

  type t = {
    initialize: unit -> build_system;
    cleanup: unit -> unit;
  }

  let null = { initialize = (fun () -> create_for_testing ()); cleanup = (fun () -> ()) }

  let create_for_testing ~initialize ~cleanup () = { initialize; cleanup }

  let initialize { initialize; _ } = initialize ()

  let cleanup { cleanup; _ } = cleanup ()
end
