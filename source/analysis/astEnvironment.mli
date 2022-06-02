(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Core

type t

module ParserError : sig
  type t = {
    source_path: ModulePath.t;
    location: Location.t;
    is_suppressed: bool;
    message: string;
  }
  [@@deriving sexp, compare, hash]
end

module ReadOnly : sig
  type t

  val get_processed_source : t -> ?track_dependency:bool -> Reference.t -> Source.t option

  val get_raw_source : t -> Reference.t -> (Source.t, ParserError.t) Result.t option

  val module_tracker : t -> ModuleTracker.ReadOnly.t

  val get_source_path : t -> Reference.t -> ModulePath.t option

  val get_relative : t -> Reference.t -> string option

  val get_real_path : t -> Reference.t -> ArtifactPath.t option

  val get_real_path_relative : t -> Reference.t -> string option

  val all_explicit_modules : t -> Reference.t list

  val is_module_tracked : t -> Reference.t -> bool

  val project_qualifiers : t -> Ast.Reference.t list
end

val module_tracker : t -> ModuleTracker.t

val configuration : t -> Configuration.Analysis.t

(* Store the environment to saved-state *)
val store : t -> unit

(* Load the environment from saved-state. Taking a `ModuleTracker` parameter just to signal that
   loading an `AstEnvironment` must be done after loading a `ModuleTracker` *)
val load : ModuleTracker.t -> t

val create : ModuleTracker.t -> t

module UpdateResult : sig
  type t

  val invalidated_modules : t -> Reference.t list

  val module_updates : t -> ModuleTracker.IncrementalUpdate.t list
end

val update : scheduler:Scheduler.t -> t -> ArtifactPath.t list -> UpdateResult.t

val clear_memory_for_tests : scheduler:Scheduler.t -> t -> unit

val remove_sources : t -> Reference.t list -> unit

val read_only : t -> ReadOnly.t
