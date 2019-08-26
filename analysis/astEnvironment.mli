(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast

type t

val create : ModuleTracker.t -> t

val get_source : t -> ?dependency:Reference.t -> Reference.t -> Source.t option

val get_wildcard_exports : t -> ?dependency:Reference.t -> Reference.t -> Reference.t list option

val add_source : t -> Source.t -> unit

val remove_sources : t -> Reference.t list -> unit

val update_and_compute_dependencies
  :  t ->
  update:(unit -> unit) ->
  Reference.t list ->
  Reference.t list

val get_source_path : t -> Reference.t -> SourcePath.t option

module Raw : sig
  val get_source : t -> ?dependency:Reference.t -> Reference.t -> Source.t option

  val get_wildcard_exports : t -> ?dependency:Reference.t -> Reference.t -> Reference.t list option

  val add_source : t -> Source.t -> unit

  val update_and_compute_dependencies
    :  t ->
    update:(unit -> unit) ->
    Reference.t list ->
    Reference.t list
end

(* Store the environment to saved-state *)
val store : t -> unit

(* Load the environment from saved-state. Taking a `ModuleTracker` parameter just to signal that
   loading an `AstEnvironment` must be done after loading a `ModuleTracker` *)
val load : ModuleTracker.t -> t

val shared_memory_hash_to_key_map : Reference.t list -> string Core.String.Map.t

val serialize_decoded : Memory.decodable -> (string * string * string option) option

val decoded_equal : Memory.decodable -> Memory.decodable -> bool option

type environment_t = t

module ReadOnly : sig
  type t

  val create
    :  ?get_source:(Reference.t -> Source.t option) ->
    ?get_wildcard_exports:(Reference.t -> Reference.t list option) ->
    ?get_source_path:(Reference.t -> SourcePath.t option) ->
    unit ->
    t

  val get_source : t -> Reference.t -> Source.t option

  val get_wildcard_exports : t -> Reference.t -> Reference.t list option

  val get_source_path : t -> Reference.t -> SourcePath.t option

  val get_relative : t -> Reference.t -> string option
end

val read_only : t -> ReadOnly.t
