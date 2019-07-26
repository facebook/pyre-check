(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast

type annotation_map = {
  precondition: Annotation.t Reference.Map.Tree.t;
  postcondition: Annotation.t Reference.Map.Tree.t;
}
[@@deriving eq]
(** Maps a key, unique to each statement for a function CFG, to type annotations. They key is
    computed from a tuple CFG node ID and and statement index (see Fixpoint.forward) *)

type annotations = annotation_map Int.Map.Tree.t [@@deriving eq, show]

module TypeAnnotationsValue : sig
  type t = annotations

  val prefix : Prefix.t

  val description : string

  val unmarshall : string -> t
end

include module type of Memory.WithCache (Reference.Key) (TypeAnnotationsValue)

module AnnotationsKeyValue : sig
  type t = Reference.t list

  val prefix : Prefix.t

  val description : string

  val unmarshall : string -> t
end

module Keys : module type of Memory.NoCache (Reference.Key) (AnnotationsKeyValue)

val add : qualifier:Reference.t -> Reference.t -> annotations -> unit

val remove : Reference.t list -> unit

val get : Reference.t -> annotations option

val get_keys : qualifiers:Reference.t list -> Reference.t list
