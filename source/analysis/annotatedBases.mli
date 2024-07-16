(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

val base_is_from_placeholder_stub
  :  Expression.t ->
  variables:(string -> Type.Variable.Declaration.t option) ->
  aliases:(?replace_unbound_parameters_with_any:bool -> string -> Type.t option) ->
  is_from_empty_stub:(Reference.t -> bool) ->
  bool
