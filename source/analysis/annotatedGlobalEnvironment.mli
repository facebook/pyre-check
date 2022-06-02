(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open SharedMemoryKeys

module AnnotatedReadOnly : sig
  include Environment.ReadOnly

  val get_global_location
    :  t ->
    ?dependency:DependencyKey.registered ->
    Reference.t ->
    Location.WithModule.t option

  val attribute_resolution : t -> AttributeResolution.ReadOnly.t

  val class_metadata_environment : t -> ClassMetadataEnvironment.ReadOnly.t

  (* Shortcut for walking through all of the environments *)
  val ast_environment : t -> AstEnvironment.ReadOnly.t

  val project_qualifiers : t -> Ast.Reference.t list
end

include
  Environment.S
    with module ReadOnly = AnnotatedReadOnly
     and module PreviousEnvironment = AttributeResolution

module Testing : sig
  module ReadOnly : sig
    val annotated_global_environment : ReadOnly.t -> ReadOnly.t

    val attribute_resolution : ReadOnly.t -> AttributeResolution.ReadOnly.t

    val class_metadata_environment : ReadOnly.t -> ClassMetadataEnvironment.ReadOnly.t

    val class_hierarchy_environment : ReadOnly.t -> ClassHierarchyEnvironment.ReadOnly.t

    val alias_environment : ReadOnly.t -> AliasEnvironment.ReadOnly.t

    val empty_stub_environment : ReadOnly.t -> EmptyStubEnvironment.ReadOnly.t

    val unannotated_global_environment : ReadOnly.t -> UnannotatedGlobalEnvironment.ReadOnly.t
  end

  module UpdateResult : sig
    val annotated_global_environment : UpdateResult.t -> UpdateResult.t

    val attribute_resolution : UpdateResult.t -> AttributeResolution.UpdateResult.t

    val class_metadata_environment : UpdateResult.t -> ClassMetadataEnvironment.UpdateResult.t

    val class_hierarchy_environment : UpdateResult.t -> ClassHierarchyEnvironment.UpdateResult.t

    val alias_environment : UpdateResult.t -> AliasEnvironment.UpdateResult.t

    val empty_stub_environment : UpdateResult.t -> EmptyStubEnvironment.UpdateResult.t

    val unannotated_global_environment
      :  UpdateResult.t ->
      UnannotatedGlobalEnvironment.UpdateResult.t
  end
end
