(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)
open Ast

type t

module ReadOnly : sig
  type t

  val get_alias : t -> ?dependency:Reference.t -> Type.Primitive.t -> Type.alias option

  val unannotated_global_environment : t -> UnannotatedGlobalEnvironment.ReadOnly.t
end

val create : UnannotatedGlobalEnvironment.ReadOnly.t -> t

module UpdateResult : sig
  type t

  val triggered_dependencies : t -> SharedMemoryKeys.ReferenceDependencyKey.KeySet.t

  val upstream : t -> UnannotatedGlobalEnvironment.UpdateResult.t
end

val update
  :  t ->
  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  UnannotatedGlobalEnvironment.UpdateResult.t ->
  UpdateResult.t

val read_only : t -> ReadOnly.t
