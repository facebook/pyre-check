(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module FunctionDefinitionReadOnly : sig
  include Environment.ReadOnly

  val function_definitions_of_qualifier
    :  t ->
    ?dependency:SharedMemoryKeys.DependencyKey.registered ->
    Ast.Reference.t ->
    FunctionDefinition.t Ast.Reference.Map.Tree.t option

  val define_names_of_qualifier
    :  t ->
    ?dependency:SharedMemoryKeys.DependencyKey.registered ->
    Ast.Reference.t ->
    Ast.Reference.t list

  val function_definition
    :  t ->
    ?dependency:SharedMemoryKeys.DependencyKey.registered ->
    Ast.Reference.t ->
    FunctionDefinition.t option

  val attribute_resolution : t -> AttributeResolution.ReadOnly.t
end

include
  Environment.S
    with module ReadOnly = FunctionDefinitionReadOnly
     and module PreviousEnvironment = AttributeResolution
