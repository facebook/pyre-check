(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast

type annotation_map = {
  precondition: Annotation.t Reference.Map.Tree.t;
  postcondition: Annotation.t Reference.Map.Tree.t
}
[@@deriving eq]
(** Maps a key, unique to each statement for a function CFG, to type annotations. They key is
    computed from a tuple CFG node ID and and statement index (see Fixpoint.forward) *)

type annotations = annotation_map Int.Map.Tree.t [@@deriving eq, show]

module TypeAnnotationsValue : sig
  type t = annotations

  val prefix : Prefix.t

  val description : string
end

include module type of Memory.WithCache (Ast.SharedMemory.ReferenceKey) (TypeAnnotationsValue)

module AnnotationsKeyValue : sig
  type t = Reference.t list

  val prefix : Prefix.t

  val description : string
end

module Keys : module type of Memory.NoCache (Ast.SharedMemory.HandleKey) (AnnotationsKeyValue)

val add : handle:File.Handle.t -> Reference.t -> annotations -> unit

val remove : File.Handle.t list -> unit

val get : Reference.t -> annotations option

val get_keys : handles:File.Handle.t list -> Reference.t list
