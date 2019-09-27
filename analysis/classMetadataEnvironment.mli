(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)
open Ast

type t

type dependency =
  | TypeCheckSource of Reference.t
  | AnnotateGlobal of Reference.t
[@@deriving show, compare, sexp]

module DependencyKey : Memory.DependencyKey.S with type t = dependency

type class_metadata = {
  successors: Type.Primitive.t list;
  is_test: bool;
  is_final: bool;
  extends_placeholder_stub_class: bool;
}
[@@deriving eq, compare, show]

module ReadOnly : sig
  type t

  val get_class_metadata : t -> ?dependency:dependency -> Type.Primitive.t -> class_metadata option

  val class_hierarchy_environment : t -> ClassHierarchyEnvironment.ReadOnly.t
end

val create : ClassHierarchyEnvironment.ReadOnly.t -> t

module UpdateResult : sig
  type t

  val triggered_dependencies : t -> DependencyKey.KeySet.t

  val upstream : t -> ClassHierarchyEnvironment.UpdateResult.t
end

val update
  :  t ->
  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  ClassHierarchyEnvironment.UpdateResult.t ->
  UpdateResult.t

val read_only : t -> ReadOnly.t
