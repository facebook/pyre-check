(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Core

module ParserError : sig
  type t = {
    module_path: ModulePath.t;
    location: Location.t;
    is_suppressed: bool;
    message: string;
  }
  [@@deriving sexp, compare, hash]
end

module ReadOnly : sig
  type t

  val get_processed_source : t -> ?track_dependency:bool -> Reference.t -> Source.t option

  val controls : t -> EnvironmentControls.t

  val get_raw_source
    :  t ->
    ?dependency:SharedMemoryKeys.DependencyKey.registered ->
    Reference.t ->
    (Source.t, ParserError.t) Result.t option

  val module_tracker : t -> ModuleTracker.ReadOnly.t
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

val update : scheduler:Scheduler.t -> t -> ArtifactPath.t list -> UpdateResult.t

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
