(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)
open SharedMemoryKeys

module HierarchyReadOnly : sig
  include Environment.ReadOnly

  val get_edges
    :  t ->
    ?dependency:dependency ->
    IndexTracker.t ->
    ClassHierarchy.Target.t list option

  val extends_placeholder_stub : t -> ?dependency:dependency -> IndexTracker.t -> bool

  val alias_environment : t -> AliasEnvironment.ReadOnly.t

  val class_hierarchy : ?dependency:dependency -> t -> (module ClassHierarchy.Handler)

  val variables
    :  ?default:ClassHierarchy.Variable.t list option ->
    t ->
    ?dependency:dependency ->
    Type.Primitive.t ->
    ClassHierarchy.Variable.t list option
end

include
  Environment.S
    with module ReadOnly = HierarchyReadOnly
     and module PreviousEnvironment = AliasEnvironment
