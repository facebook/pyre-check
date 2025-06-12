(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

type resolved_stringify =
  | Str
  | Repr

(* Redirect a call to `str(e)` to the proper method for any expression `e` *)
val resolve_stringify_call
  :  resolve_expression_to_type:(Expression.t -> Type.t) ->
  Expression.t ->
  resolved_stringify

val redirect_special_calls
  :  resolve_expression_to_type:(Expression.t -> Type.t) ->
  location:Location.t ->
  Expression.Call.t ->
  Expression.Call.t
