(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

type typed_dictionary_mismatch =
  | MissingRequiredField of {
      field_name: Identifier.t;
      class_name: Identifier.t;
    }
  | FieldTypeMismatch of {
      field_name: Identifier.t;
      expected_type: Type.t;
      actual_type: Type.t;
      class_name: Identifier.t;
    }
  | UndefinedField of {
      field_name: Identifier.t;
      class_name: Identifier.t;
    }
[@@deriving compare, show, sexp]

type weakened_type = {
  resolved: Type.t;
  typed_dictionary_errors: typed_dictionary_mismatch Node.t list;
}
[@@deriving compare, show]

val make_weakened_type
  :  ?typed_dictionary_errors:typed_dictionary_mismatch Node.t list ->
  Type.t ->
  weakened_type

val distribute_union_over_parametric
  :  parametric_name:Identifier.t ->
  number_of_parameters:int ->
  Type.t ->
  Type.t option

val weaken_mutable_literals
  :  (Expression.expression Node.t -> Type.t) ->
  get_typed_dictionary:(Type.t -> Type.t Type.Record.TypedDictionary.record option) ->
  expression:Expression.expression Node.t option ->
  resolved:Type.t ->
  expected:Type.t ->
  comparator:
    (get_typed_dictionary_override:(Type.t -> Type.t Type.Record.TypedDictionary.record option) ->
    left:Type.t ->
    right:Type.t ->
    bool) ->
  weakened_type

val resolved_type : weakened_type -> Type.t
