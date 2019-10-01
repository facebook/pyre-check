(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)
open Ast
open Core
open SharedMemoryKeys

type t

module ReadOnly : sig
  type t

  val get_global : t -> ?dependency:dependency -> Reference.t -> GlobalResolution.global option

  val class_metadata_environment : t -> ClassMetadataEnvironment.ReadOnly.t

  val hash_to_key_map : t -> string String.Map.t

  val serialize_decoded : t -> Memory.decodable -> (string * string * string option) option

  val decoded_equal : t -> Memory.decodable -> Memory.decodable -> bool option

  val resolution : t -> GlobalResolution.t

  val dependency_tracked_resolution : t -> dependency:dependency -> GlobalResolution.t

  (* Shortcut for walking through all of the environments *)
  val ast_environment : t -> AstEnvironment.ReadOnly.t
end

val create : ClassMetadataEnvironment.ReadOnly.t -> t

module UpdateResult : sig
  type t

  val triggered_dependencies : t -> DependencyKey.KeySet.t

  val upstream : t -> ClassMetadataEnvironment.UpdateResult.t

  val all_triggered_dependencies : t -> DependencyKey.KeySet.t list
end

val update
  :  t ->
  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  ClassMetadataEnvironment.UpdateResult.t ->
  UpdateResult.t

val read_only : t -> ReadOnly.t
