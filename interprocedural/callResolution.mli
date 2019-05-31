(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Analysis
open Expression

val is_local : Identifier.t -> bool

val get_global_targets
  :  resolution:Resolution.t ->
  global:Reference.t ->
  (Callable.t * Type.Callable.implicit option) list

val get_indirect_targets
  :  resolution:Resolution.t ->
  receiver:Access.general_access ->
  method_name:Identifier.t ->
  (Callable.t * Type.Callable.implicit option) list

(* Returns a normalized path and optional addition parameter prefix, e.g. for constructor calls *)
val normalize_global
  :  resolution:Resolution.t ->
  Reference.t ->
  Reference.t * Expression.t Argument.record list

(* Returns all call targets from Call expressions in the given access *)
val resolve_call_targets
  :  resolution:Resolution.t ->
  Expression.t Expression.Call.t ->
  (Callable.t * Type.Callable.implicit option) list
