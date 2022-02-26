(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open SharedMemoryKeys
open Statement
open Core

module Global : sig
  type t = {
    annotation: Annotation.t;
    undecorated_signature: Type.Callable.t option;
    problem: AnnotatedAttribute.problem option;
  }
  [@@deriving show, compare, sexp]
end

type resolved_define = {
  undecorated_signature: Type.Callable.t;
  decorated: (Type.t, AnnotatedAttribute.problem) Result.t;
}

type generic_type_problems =
  | IncorrectNumberOfParameters of {
      actual: int;
      expected: int;
      can_accept_more_parameters: bool;
    }
  | ViolateConstraints of {
      actual: Type.t;
      expected: Type.Variable.Unary.t;
    }
  | UnexpectedKind of {
      actual: Type.Parameter.t;
      expected: Type.Variable.t;
    }
[@@deriving compare, sexp, show, hash]

type type_parameters_mismatch = {
  name: string;
  kind: generic_type_problems;
}
[@@deriving compare, sexp, show, hash]

module Argument : sig
  type t = {
    expression: Expression.t option;
    kind: Ast.Expression.Call.Argument.kind;
    resolved: Type.t;
  }

  module WithPosition : sig
    type t = {
      position: int;
      expression: Expression.t option;
      kind: Ast.Expression.Call.Argument.kind;
      resolved: Type.t;
    }
    [@@deriving compare, show]
  end
end

type matched_argument =
  | MatchedArgument of {
      argument: Argument.WithPosition.t;
      index_into_starred_tuple: int option;
    }
  | Default
[@@deriving compare, show]

val make_matched_argument
  :  ?index_into_starred_tuple:int ->
  Argument.WithPosition.t ->
  matched_argument

type reasons = {
  arity: SignatureSelectionTypes.reason list;
  annotation: SignatureSelectionTypes.reason list;
}
[@@deriving compare, show]

val empty_reasons : reasons

val location_insensitive_compare_reasons : reasons -> reasons -> int

module ParameterArgumentMapping : sig
  type t = {
    parameter_argument_mapping: matched_argument list Type.Callable.Parameter.Map.t;
    reasons: reasons;
  }
  [@@deriving compare]

  val pp : Format.formatter -> t -> unit
end

type ranks = {
  arity: int;
  annotation: int;
  position: int;
}

type signature_match = {
  callable: Type.Callable.t;
  parameter_argument_mapping: matched_argument list Type.Callable.Parameter.Map.t;
  constraints_set: TypeConstraints.t list;
  ranks: ranks;
  reasons: reasons;
}
[@@deriving compare, show]

module SignatureSelection : sig
  val get_parameter_argument_mapping
    :  all_parameters:Type.t Type.Callable.record_parameters ->
    parameters:Type.t Type.Callable.RecordParameter.t list ->
    self_argument:Type.t option ->
    Argument.WithPosition.t list ->
    ParameterArgumentMapping.t

  val check_arguments_against_parameters
    :  order:ConstraintsSet.order ->
    resolve_mutable_literals:
      (resolve:(Expression.t -> Type.t) ->
      expression:Expression.t option ->
      resolved:Type.t ->
      expected:Type.t ->
      WeakenMutableLiterals.weakened_type) ->
    resolve_with_locals:(locals:(Reference.t * Annotation.t) list -> Expression.t -> Type.t) ->
    callable:Type.Callable.t ->
    ParameterArgumentMapping.t ->
    signature_match

  val prepare_arguments_for_signature_selection
    :  self_argument:Type.t option ->
    Argument.t list ->
    Argument.WithPosition.t list

  val find_closest_signature : signature_match list -> signature_match option

  val default_signature : Type.Callable.t -> SignatureSelectionTypes.instantiated_return_annotation

  val instantiate_return_annotation
    :  ?skip_marking_escapees:bool ->
    order:ConstraintsSet.order ->
    signature_match ->
    SignatureSelectionTypes.instantiated_return_annotation
end

type uninstantiated

type uninstantiated_attribute = uninstantiated AnnotatedAttribute.t

module AttributeReadOnly : sig
  include Environment.ReadOnly

  val class_metadata_environment : t -> ClassMetadataEnvironment.ReadOnly.t

  val get_typed_dictionary
    :  t ->
    ?dependency:DependencyKey.registered ->
    Type.t ->
    Type.t Type.Record.TypedDictionary.record option

  val full_order : ?dependency:DependencyKey.registered -> t -> TypeOrder.order

  val check_invalid_type_parameters
    :  t ->
    ?dependency:DependencyKey.registered ->
    Type.t ->
    type_parameters_mismatch list * Type.t

  val parse_annotation
    :  t ->
    ?dependency:DependencyKey.registered ->
    ?validation:SharedMemoryKeys.ParseAnnotationKey.type_validation_policy ->
    Expression.expression Node.t ->
    Type.t

  val attribute
    :  t ->
    ?dependency:DependencyKey.registered ->
    transitive:bool ->
    accessed_through_class:bool ->
    include_generated_attributes:bool ->
    ?special_method:bool ->
    ?instantiated:Type.t ->
    attribute_name:Identifier.t ->
    string ->
    AnnotatedAttribute.instantiated option

  val attribute_names
    :  t ->
    ?dependency:DependencyKey.registered ->
    transitive:bool ->
    accessed_through_class:bool ->
    include_generated_attributes:bool ->
    ?special_method:bool ->
    string ->
    Identifier.t list option

  val all_attributes
    :  t ->
    ?dependency:DependencyKey.registered ->
    transitive:bool ->
    accessed_through_class:bool ->
    include_generated_attributes:bool ->
    ?special_method:bool ->
    string ->
    uninstantiated_attribute list option

  val metaclass : t -> ?dependency:DependencyKey.registered -> Type.Primitive.t -> Type.t option

  val constraints
    :  t ->
    ?dependency:DependencyKey.registered ->
    target:Type.Primitive.t ->
    ?parameters:Type.Parameter.t list ->
    instantiated:Type.t ->
    unit ->
    ConstraintsSet.Solution.t

  val resolve_literal
    :  t ->
    ?dependency:DependencyKey.registered ->
    Expression.expression Node.t ->
    Type.t

  val resolve_define
    :  t ->
    ?dependency:DependencyKey.registered ->
    implementation:Define.Signature.t option ->
    overloads:Define.Signature.t list ->
    resolved_define

  val signature_select
    :  t ->
    ?dependency:DependencyKey.registered ->
    resolve_with_locals:
      (locals:(Reference.t * Annotation.t) list -> Expression.expression Node.t -> Type.t) ->
    arguments:Argument.t list ->
    callable:Type.Callable.t ->
    self_argument:Type.t option ->
    SignatureSelectionTypes.instantiated_return_annotation

  val resolve_mutable_literals
    :  t ->
    ?dependency:DependencyKey.registered ->
    resolve:(Expression.expression Node.t -> Type.t) ->
    expression:Expression.expression Node.t option ->
    resolved:Type.t ->
    expected:Type.t ->
    WeakenMutableLiterals.weakened_type

  val constraints_solution_exists
    :  t ->
    ?dependency:DependencyKey.registered ->
    get_typed_dictionary_override:(Type.t -> Type.t Type.Record.TypedDictionary.record option) ->
    left:Type.t ->
    right:Type.t ->
    bool

  val instantiate_attribute
    :  t ->
    ?dependency:DependencyKey.registered ->
    accessed_through_class:bool ->
    ?instantiated:Type.t ->
    uninstantiated_attribute ->
    AnnotatedAttribute.instantiated

  val get_global : t -> ?dependency:DependencyKey.registered -> Reference.t -> Global.t option
end

include
  Environment.S
    with module ReadOnly = AttributeReadOnly
     and module PreviousEnvironment = ClassMetadataEnvironment
