(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

module ReadOnly : sig
  type t

  val as_source_code_incremental_read_only : t -> SourceCodeIncrementalApi.ReadOnly.t
end

module Overlay : sig
  type t

  val module_tracker : t -> ModuleTracker.Overlay.t

  val update_overlaid_code
    :  t ->
    code_updates:SourceCodeIncrementalApi.Overlay.CodeUpdates.t ->
    SourceCodeIncrementalApi.UpdateResult.t

  val read_only : t -> ReadOnly.t
end

type t

val global_module_paths_api : t -> GlobalModulePathsApi.t

val module_tracker : t -> ModuleTracker.t

val controls : t -> EnvironmentControls.t

val create : EnvironmentControls.t -> t

(* Load and store the environment to and from saved-state *)

val load : EnvironmentControls.t -> t

val store : t -> unit

val update
  :  scheduler:Scheduler.t ->
  t ->
  ArtifactPath.Event.t list ->
  SourceCodeIncrementalApi.UpdateResult.t

val clear_memory_for_tests : scheduler:Scheduler.t -> t -> unit

val remove_sources : t -> Reference.t list -> unit

val read_only : t -> ReadOnly.t

val overlay : t -> Overlay.t
