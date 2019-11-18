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

  val alias_environment : t -> AliasEnvironment.ReadOnly.t
end

include
  Environment.S
    with module ReadOnly = HierarchyReadOnly
     and module PreviousEnvironment = AliasEnvironment
