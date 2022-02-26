(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open SharedMemoryKeys

type class_metadata = {
  successors: Type.Primitive.t list;
  is_test: bool;
  is_final: bool;
  extends_placeholder_stub_class: bool;
  is_abstract: bool;
  is_protocol: bool;
  is_typed_dictionary: bool;
}
[@@deriving compare, show]

module MetadataReadOnly : sig
  include Environment.ReadOnly

  val get_class_metadata
    :  t ->
    ?dependency:DependencyKey.registered ->
    Type.Primitive.t ->
    class_metadata option

  val is_typed_dictionary : t -> ?dependency:DependencyKey.registered -> Type.Primitive.t -> bool

  val class_hierarchy_environment : t -> ClassHierarchyEnvironment.ReadOnly.t

  val successors : t -> ?dependency:DependencyKey.registered -> Type.Primitive.t -> string list
end

include
  Environment.S
    with module ReadOnly = MetadataReadOnly
     and module PreviousEnvironment = ClassHierarchyEnvironment
