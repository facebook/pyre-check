(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The SourceCodeIncrementalApi provides and updatable, dependency-tracked
 * abstract interface around the simple read-only SourceCodeApi.
 *
 * The abstract dependency-tracked API, much like the legacy environment
 * layers, has four components:
 * - A dependency-aware ReadOnly, whose only job is to provide tracked
 *   or untracked versions of the SourceCodeApi
 * - An UpdateResult representing the results of incremental updates.
 * - An updatable Base representing a read-write incremental root source tree.
 * - An updatable Overlay, which includes qualifier ownership hooks.
 *)
open Base

module ReadOnly = struct
  type t = {
    get_tracked_api: dependency:SharedMemoryKeys.DependencyKey.registered -> SourceCodeApi.t;
    get_untracked_api: unit -> SourceCodeApi.t;
  }

  let create ~get_tracked_api ~get_untracked_api = { get_tracked_api; get_untracked_api }

  let controls { get_untracked_api; _ } = SourceCodeApi.controls (get_untracked_api ())

  let get_untracked_api { get_untracked_api; _ } = get_untracked_api ()

  let get_tracked_api { get_tracked_api; _ } = get_tracked_api
end

module UpdateResult = struct
  module ModuleUpdate = struct
    type t =
      | NewExplicit of Ast.ModulePath.t
      | NewImplicit of Ast.Reference.t
      | Delete of Ast.Reference.t
    [@@deriving show, sexp, compare, equal]
  end

  type t = {
    triggered_dependencies: SharedMemoryKeys.DependencyKey.RegisteredSet.t;
    invalidated_modules: Ast.Reference.t list;
    module_updates: ModuleUpdate.t list;
  }

  let create ~triggered_dependencies ~invalidated_modules ~module_updates =
    { triggered_dependencies; invalidated_modules; module_updates }


  let triggered_dependencies { triggered_dependencies; _ } = triggered_dependencies

  let invalidated_modules { invalidated_modules; _ } = invalidated_modules

  let module_updates { module_updates; _ } = module_updates
end

module Overlay = struct
  module CodeUpdate = struct
    type t =
      | NewCode of string
      | ResetCode
    [@@deriving show]
  end

  module CodeUpdates = struct
    type t = (ArtifactPath.t * CodeUpdate.t) list
  end

  type t = {
    read_only: ReadOnly.t;
    owns_qualifier: Ast.Reference.t -> bool;
    update_overlaid_code: code_updates:CodeUpdates.t -> UpdateResult.t;
  }

  let create ~read_only ~owns_qualifier ~update_overlaid_code =
    { read_only; owns_qualifier; update_overlaid_code }


  let read_only { read_only; _ } = read_only

  let owns_qualifier { owns_qualifier; _ } = owns_qualifier

  let owns_reference api reference =
    Ast.Reference.possible_qualifiers_after_delocalize reference
    |> List.exists ~f:(owns_qualifier api)


  let owns_identifier environment name = Ast.Reference.create name |> owns_reference environment

  let update_overlaid_code { update_overlaid_code; _ } = update_overlaid_code
end

module Base = struct
  type t = {
    read_only: ReadOnly.t;
    overlay: unit -> Overlay.t;
    update: scheduler:Scheduler.t -> ArtifactPath.Event.t list -> UpdateResult.t;
    global_module_paths_api: GlobalModulePathsApi.t;
  }

  let create ~read_only ~overlay ~update ~global_module_paths_api =
    { read_only; overlay; update; global_module_paths_api }


  let read_only { read_only; _ } = read_only

  let overlay { overlay; _ } = overlay ()

  let update { update; _ } = update

  module AssumeGlobalModuleListing = struct
    let global_module_paths_api { global_module_paths_api; _ } = global_module_paths_api
  end
end
