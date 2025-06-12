(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

val redirect
  :  resolve_expression_to_type:(Expression.t -> Type.t) ->
  location:Location.t ->
  Expression.Call.t ->
  Expression.Call.t option

val recognized_callable_target_types : Type.Set.t
