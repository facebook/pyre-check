(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)
open Ast
open SharedMemoryKeys

type global = Annotation.t [@@deriving eq, show, compare, sexp]

module AnnotatedReadOnly : sig
  include Environment.ReadOnly

  val get_global : t -> ?dependency:DependencyKey.registered -> Reference.t -> global option

  val get_global_location
    :  t ->
    ?dependency:DependencyKey.registered ->
    Reference.t ->
    Location.WithModule.t option

  val attribute_resolution : t -> AttributeResolution.ReadOnly.t

  val class_metadata_environment : t -> ClassMetadataEnvironment.ReadOnly.t

  (* Shortcut for walking through all of the environments *)
  val ast_environment : t -> AstEnvironment.ReadOnly.t
end

include Environment.S with module ReadOnly = AnnotatedReadOnly

module PreviousEnvironment = AttributeResolution
