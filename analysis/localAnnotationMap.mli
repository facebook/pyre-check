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

val merge : t -> t -> t

val set_statement
  :  ?precondition:RefinementUnit.t Reference.Map.t ->
  ?postcondition:RefinementUnit.t Reference.Map.t ->
  key:int ->
  t ->
  t

val set_expression
  :  ?precondition:RefinementUnit.t Reference.Map.t ->
  ?postcondition:RefinementUnit.t Reference.Map.t ->
  key:Location.t ->
  t ->
  t

val get_statement_precondition : t -> int -> RefinementUnit.t Reference.Map.t option

val get_statement_postcondition : t -> int -> RefinementUnit.t Reference.Map.t option

val get_expression_precondition : t -> Location.t -> RefinementUnit.t Reference.Map.t option

val get_expression_postcondition : t -> Location.t -> RefinementUnit.t Reference.Map.t option
