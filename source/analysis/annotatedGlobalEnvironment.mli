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

  val location_of_global
    :  t ->
    ?dependency:DependencyKey.registered ->
    Reference.t ->
    Location.WithModule.t option

  val attribute_resolution : t -> AttributeResolution.ReadOnly.t

  val class_metadata_environment : t -> ClassSuccessorMetadataEnvironment.ReadOnly.t

  val get_tracked_source_code_api
    :  t ->
    dependency:SharedMemoryKeys.DependencyKey.registered ->
    SourceCodeApi.t

  val get_untracked_source_code_api : t -> SourceCodeApi.t
end

include
  Environment.S
    with module ReadOnly = AnnotatedReadOnly
     and module PreviousEnvironment = AttributeResolution
