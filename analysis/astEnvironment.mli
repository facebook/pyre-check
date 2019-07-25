(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast

type t

val create : ModuleTracker.t -> t

val get_source : t -> Reference.t -> Source.t option

val add_source : t -> Source.t -> unit

val remove_sources : t -> Reference.t list -> unit

val get_source_path : t -> Reference.t -> SourcePath.t option

(* Store the environment to saved-state *)
val store : t -> unit

(* Load the environment from saved-state. Taking a `ModuleTracker` parameter just to signal that
   loading an `AstEnvironment` must be done after loading a `ModuleTracker` *)
val load : ModuleTracker.t -> t

type environment_t = t

module ReadOnly : sig
  type t

  val create : environment_t -> t

  val get_source : t -> Reference.t -> Source.t option

  val get_source_path : t -> Reference.t -> SourcePath.t option
end
