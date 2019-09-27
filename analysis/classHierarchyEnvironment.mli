(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)
open Ast

type t

type dependency =
  | TypeCheckSource of Reference.t
  | RegisterClassMetadata of Type.Primitive.t
  | AnnotateGlobal of Reference.t
[@@deriving show, compare, sexp]

module DependencyKey : Memory.DependencyKey.S with type t = dependency

module ReadOnly : sig
  type t

  val get_edges
    :  t ->
    ?dependency:dependency ->
    IndexTracker.t ->
    ClassHierarchy.Target.t list option

  val get_backedges : t -> IndexTracker.t -> ClassHierarchy.Target.Set.Tree.t option

  val get_undecorated_function
    :  t ->
    ?dependency:dependency ->
    Reference.t ->
    Type.t Type.Callable.overload option

  val alias_environment : t -> AliasEnvironment.ReadOnly.t
end

val create : AliasEnvironment.ReadOnly.t -> t

module UpdateResult : sig
  type t

  val triggered_dependencies : t -> DependencyKey.KeySet.t

  val upstream : t -> AliasEnvironment.UpdateResult.t
end

val update
  :  t ->
  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  AliasEnvironment.UpdateResult.t ->
  UpdateResult.t

val read_only : t -> ReadOnly.t
