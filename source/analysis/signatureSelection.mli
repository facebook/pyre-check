(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

module Argument : sig
  type 'argument_type t = {
    expression: Expression.t option;
    kind: Ast.Expression.Call.Argument.kind;
    resolved: 'argument_type;
  }
  [@@deriving show]

  module WithPosition : sig
    type 'argument_type t = {
      position: int;
      expression: Expression.t option;
      kind: Ast.Expression.Call.Argument.kind;
      resolved: 'argument_type;
    }
    [@@deriving compare, show]
  end
end

type 'argument_type matched_argument =
  | MatchedArgument of {
      argument: 'argument_type Argument.WithPosition.t;
      index_into_starred_tuple: int option;
    }
  | Default
[@@deriving compare, show]

val make_matched_argument
  :  ?index_into_starred_tuple:int ->
  'argument_type Argument.WithPosition.t ->
  'argument_type matched_argument

type reasons = {
  arity: SignatureSelectionTypes.reason list;
  annotation: SignatureSelectionTypes.reason list;
}
[@@deriving compare, show]

val empty_reasons : reasons

val location_insensitive_compare_reasons : reasons -> reasons -> int

module ParameterArgumentMapping : sig
  type 'argument_type t = {
    parameter_argument_mapping:
      'argument_type matched_argument list Type.Callable.CallableParamType.Map.t;
    reasons: reasons;
  }

  val empty : Type.t t

  val equal_mapping_with_resolved_type : Type.t t -> Type.t t -> bool

  val pp_with_resolved_type : Format.formatter -> Type.t t -> unit
end

type ranks = {
  arity: int;
  annotation: int;
  position: int;
}

type signature_match = {
  callable: Type.Callable.t;
  parameter_argument_mapping: Type.t matched_argument list Type.Callable.CallableParamType.Map.t;
  constraints_set: TypeConstraints.t list;
  ranks: ranks;
  reasons: reasons;
}
[@@deriving compare, show]

val reserved_position_for_self_argument : int

val prepare_arguments_for_signature_selection
  :  self_argument:'argument_type option ->
  'argument_type Argument.t list ->
  'argument_type Argument.WithPosition.t list

val get_parameter_argument_mapping
  :  all_parameters:Type.t Type.Callable.record_parameters ->
  parameters:Type.t Type.Callable.CallableParamType.t list ->
  self_argument:Type.t option ->
  order:ConstraintsSet.order ->
  location:Location.t ->
  resolve:(Expression.t -> Type.t) ->
  get_typed_dictionary:(Type.t -> Type.TypedDictionary.t option) ->
  Type.t Argument.WithPosition.t list ->
  Type.t ParameterArgumentMapping.t

val check_arguments_against_parameters
  :  order:ConstraintsSet.order ->
  resolve_mutable_literals:
    (resolve:(Expression.t -> Type.t) ->
    expression:Expression.t option ->
    resolved:Type.t ->
    expected:Type.t ->
    WeakenMutableLiterals.weakened_type) ->
  resolve_with_locals:(locals:(Reference.t * TypeInfo.Unit.t) list -> Expression.t -> Type.t) ->
  get_typed_dictionary:(Type.t -> Type.TypedDictionary.t option) ->
  location:Location.t ->
  callable:Type.Callable.t ->
  Type.t ParameterArgumentMapping.t ->
  signature_match

val find_closest_signature : signature_match list -> signature_match option

val default_instantiated_return_annotation
  :  Type.Callable.t ->
  SignatureSelectionTypes.instantiated_return_annotation

val most_important_error_reason
  :  arity_mismatch_reasons:SignatureSelectionTypes.reason list ->
  SignatureSelectionTypes.reason list ->
  SignatureSelectionTypes.reason option

val instantiate_return_annotation
  :  ?skip_marking_escapees:bool ->
  order:ConstraintsSet.order ->
  signature_match ->
  SignatureSelectionTypes.instantiated_return_annotation

val select_closest_signature_for_function_call
  :  order:ConstraintsSet.order ->
  resolve_with_locals:
    (locals:(Reference.t * TypeInfo.Unit.t) list -> Expression.expression Node.t -> Type.t) ->
  resolve_mutable_literals:
    (resolve:(Expression.t -> Type.t) ->
    expression:Expression.t option ->
    resolved:Type.t ->
    expected:Type.t ->
    WeakenMutableLiterals.weakened_type) ->
  get_typed_dictionary:(Type.t -> Type.TypedDictionary.t option) ->
  arguments:Type.t Argument.t list ->
  location:Location.t ->
  callable:Type.Callable.t ->
  self_argument:Type.t option ->
  signature_match option
