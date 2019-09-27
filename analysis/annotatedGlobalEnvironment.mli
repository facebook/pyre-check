(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)
open Ast
open Core

type t

type dependency = TypeCheckSource of Reference.t [@@deriving show, compare, sexp]

module DependencyKey : Memory.DependencyKey.S with type t = dependency

module ReadOnly : sig
  type t

  val get_global : t -> ?dependency:dependency -> Reference.t -> GlobalResolution.global option

  val class_metadata_environment : t -> ClassMetadataEnvironment.ReadOnly.t

  val hash_to_key_map : t -> string String.Map.t

  val serialize_decoded : t -> Memory.decodable -> (string * string * string option) option

  val decoded_equal : t -> Memory.decodable -> Memory.decodable -> bool option
end

val create : ClassMetadataEnvironment.ReadOnly.t -> t

module UpdateResult : sig
  type t

  val triggered_dependencies : t -> DependencyKey.KeySet.t

  val upstream : t -> ClassMetadataEnvironment.UpdateResult.t
end

val update
  :  t ->
  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  ClassMetadataEnvironment.UpdateResult.t ->
  UpdateResult.t

val read_only : t -> ReadOnly.t
