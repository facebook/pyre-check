(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression


(** Maps a key, unique to each statement for a function CFG, to type
    annotations.  They key is computed from a tuple CFG node ID and and statement
    index (see Fixpoint.forward) *)
type annotation_map = {
  precondition: Annotation.t Access.Map.Tree.t;
  postcondition: Annotation.t Access.Map.Tree.t;
}

type annotations = annotation_map Int.Map.Tree.t

val add: Access.t -> annotations -> unit
val remove: Access.t list -> unit

val get: Access.t -> annotations option
