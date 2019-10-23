(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast

val find_propagated_type_variables
  :  Expression.Call.Argument.t list ->
  parse_annotation:(Expression.t -> Type.t) ->
  Type.t Type.OrderedTypes.record

(* Find free variables in the parametric type. E.g. for generic class `class A(typing.Generic[_T],
   typing.Generic[_S]): ...` and instantiated type `A[int, Bottom]` we consider `_S` to be free. *)

val inferred_generic_base
  :  ClassSummary.t Node.t ->
  parse_annotation:(Expression.t -> Type.t) ->
  Expression.Call.Argument.t list

val extends_placeholder_stub_class
  :  ClassSummary.t Node.t ->
  aliases:(Type.Primitive.t -> Type.alias option) ->
  from_empty_stub:(Reference.t -> bool) ->
  bool
