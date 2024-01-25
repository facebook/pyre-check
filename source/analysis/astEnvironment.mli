(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

module ReadOnly : sig
  type t

  val processed_source_of_qualifier
    :  t ->
    ?dependency:SharedMemoryKeys.DependencyKey.registered ->
    Reference.t ->
    Source.t option

  val controls : t -> EnvironmentControls.t

  val raw_source_of_qualifier
    :  t ->
    ?dependency:SharedMemoryKeys.DependencyKey.registered ->
    Reference.t ->
    Parsing.ParseResult.t option

  val module_tracker : t -> ModuleTracker.ReadOnly.t

  val get_tracked_source_code_api
    :  t ->
    dependency:SharedMemoryKeys.DependencyKey.registered ->
    SourceCodeApi.t

  val get_untracked_source_code_api : t -> SourceCodeApi.t
end

module UpdateResult : sig
  type t = {
    triggered_dependencies: SharedMemoryKeys.DependencyKey.RegisteredSet.t;
    invalidated_modules: Reference.t list;
    module_updates: ModuleTracker.IncrementalUpdate.t list;
  }

  val triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.RegisteredSet.t

  val invalidated_modules : t -> Reference.t list

  val module_updates : t -> ModuleTracker.IncrementalUpdate.t list
end

type t

val module_tracker : t -> ModuleTracker.t

val controls : t -> EnvironmentControls.t

val create : EnvironmentControls.t -> t

(* Load and store the environment to and from saved-state *)

val load : EnvironmentControls.t -> t

val store : t -> unit

val update : scheduler:Scheduler.t -> t -> ArtifactPath.Event.t list -> UpdateResult.t

val clear_memory_for_tests : scheduler:Scheduler.t -> t -> unit

val remove_sources : t -> Reference.t list -> unit

val read_only : t -> ReadOnly.t

module Overlay : sig
  type t

  val create : ReadOnly.t -> t

  val module_tracker : t -> ModuleTracker.Overlay.t

  val update_overlaid_code
    :  t ->
    code_updates:(ArtifactPath.t * ModuleTracker.Overlay.CodeUpdate.t) list ->
    UpdateResult.t

  val read_only : t -> ReadOnly.t
end
