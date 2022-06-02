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
  val annotated_global_environment : t -> t

  val attribute_resolution : t -> AttributeResolution.t

  val class_metadata_environment : t -> ClassMetadataEnvironment.t

  val class_hierarchy_environment : t -> ClassHierarchyEnvironment.t

  val alias_environment : t -> AliasEnvironment.t

  val empty_stub_environment : t -> EmptyStubEnvironment.t

  val unannotated_global_environment : t -> UnannotatedGlobalEnvironment.t

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
