(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast


(** Maps a key, unique to each statement for a function CFG, to type
    annotations.  They key is computed from a tuple CFG node ID and and statement
    index (see Fixpoint.forward) *)
type annotation_map = {
  precondition: Annotation.t Reference.Map.Tree.t;
  postcondition: Annotation.t Reference.Map.Tree.t;
}

type annotations = annotation_map Int.Map.Tree.t

val add: Reference.t -> annotations -> unit
val remove: Reference.t list -> unit

val get: Reference.t -> annotations option


module TypeAnnotationsValue: sig
  type t = annotations
  val prefix: Prefix.t
  val description: string
end

include module type of Memory.WithCache (Ast.SharedMemory.ReferenceKey) (TypeAnnotationsValue)
