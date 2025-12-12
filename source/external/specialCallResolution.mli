(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

val shim_calls
  :  resolve_expression_to_type:(Expression.t -> Type.t) ->
  Expression.Call.t ->
  Shims.ShimArgumentMapping.t option
