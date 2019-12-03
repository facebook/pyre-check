(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)
open Ast
open Core
open SharedMemoryKeys

type global = Annotation.t Node.t [@@deriving eq, show, compare, sexp]

module AnnotatedReadOnly : sig
  type t

  val get_global : t -> ?dependency:dependency -> Reference.t -> global option

  val attribute_resolution : t -> AttributeResolution.ReadOnly.t

  val class_metadata_environment : t -> ClassMetadataEnvironment.ReadOnly.t

  val hash_to_key_map : t -> string String.Map.t

  val serialize_decoded : t -> Memory.decodable -> (string * string * string option) option

  val decoded_equal : t -> Memory.decodable -> Memory.decodable -> bool option

  (* Shortcut for walking through all of the environments *)
  val ast_environment : t -> AstEnvironment.ReadOnly.t
end

include
  Environment.S
    with module ReadOnly = AnnotatedReadOnly
     and module PreviousEnvironment = AttributeResolution
