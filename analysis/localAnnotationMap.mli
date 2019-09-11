(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast

type t

val empty : t

val equal : t -> t -> bool

val pp : Format.formatter -> t -> unit

val show : t -> string

val set
  :  ?precondition:Annotation.t Reference.Map.t ->
  ?postcondition:Annotation.t Reference.Map.t ->
  key:int ->
  t ->
  t

val merge : t -> t -> t

val get_precondition : t -> int -> Annotation.t Reference.Map.t option

val get_postcondition : t -> int -> Annotation.t Reference.Map.t option
