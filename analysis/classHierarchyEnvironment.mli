(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)
open Ast
open SharedMemoryKeys

module HierarchyReadOnly : sig
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

include
  Environment.S
    with module ReadOnly = HierarchyReadOnly
     and module PreviousEnvironment = AliasEnvironment
