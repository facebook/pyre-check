(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)
open Ast
open SharedMemoryKeys

module UndecoratedReadOnly : sig
  include Environment.ReadOnly

  val get_undecorated_function
    :  t ->
    ?dependency:DependencyKey.registered ->
    Reference.t ->
    Type.Callable.t option

  val class_hierarchy_environment : t -> ClassHierarchyEnvironment.ReadOnly.t
end

include
  Environment.S
    with module ReadOnly = UndecoratedReadOnly
     and module PreviousEnvironment = ClassHierarchyEnvironment
