(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open SharedMemoryKeys
open Statement

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
[@@deriving compare, eq, show]

type weakened_type = {
  resolved: Type.t;
  typed_dictionary_errors: typed_dictionary_mismatch Node.t list;
}
[@@deriving eq, show]

val resolved_type : weakened_type -> Type.t

val typed_dictionary_errors : weakened_type -> typed_dictionary_mismatch Node.t list

val make_weakened_type
  :  ?typed_dictionary_errors:typed_dictionary_mismatch Node.t list ->
  Type.t ->
  weakened_type

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

type generic_type_problems =
  | IncorrectNumberOfParameters of {
      actual: int;
      expected: int;
    }
  | ViolateConstraints of {
      actual: Type.t;
      expected: Type.Variable.Unary.t;
    }
  | UnexpectedKind of {
      actual: Type.Parameter.t;
      expected: Type.Variable.t;
    }
[@@deriving compare, eq, sexp, show, hash]

type type_parameters_mismatch = {
  name: string;
  kind: generic_type_problems;
}
[@@deriving compare, eq, sexp, show, hash]

type mismatch = {
  actual: Type.t;
  expected: Type.t;
  name: Identifier.t option;
  position: int;
}
[@@deriving eq, show, compare]

type invalid_argument = {
  expression: Expression.t option;
  annotation: Type.t;
}
[@@deriving compare, eq, show, sexp, hash]

type missing_argument =
  | Named of Identifier.t
  | PositionalOnly of int
[@@deriving eq, show, compare, sexp, hash]

type mismatch_with_list_variadic_type_variable =
  | NotDefiniteTuple of invalid_argument
  | CantConcatenate of Type.OrderedTypes.t list
  | ConstraintFailure of Type.OrderedTypes.t
[@@deriving compare, eq, show, sexp, hash]

type reason =
  | AbstractClassInstantiation of {
      class_name: Reference.t;
      abstract_methods: string list;
    }
  | CallingParameterVariadicTypeVariable
  | InvalidKeywordArgument of invalid_argument Node.t
  | InvalidVariableArgument of invalid_argument Node.t
  | Mismatch of mismatch Node.t
  | MismatchWithListVariadicTypeVariable of {
      variable: Type.OrderedTypes.t;
      mismatch: mismatch_with_list_variadic_type_variable;
    }
  | MissingArgument of missing_argument
  | MutuallyRecursiveTypeVariables
  | ProtocolInstantiation of Reference.t
  | TooManyArguments of {
      expected: int;
      provided: int;
    }
  | TypedDictionaryInitializationError of typed_dictionary_mismatch Node.t list
  | UnexpectedKeyword of Identifier.t
[@@deriving eq, show, compare]

type closest = {
  closest_return_annotation: Type.t;
  reason: reason option;
}
[@@deriving show]

module Argument : sig
  type t = {
    expression: Expression.t option;
    kind: Ast.Expression.Call.Argument.kind;
    resolved: Type.t;
  }
end

type arguments =
  | Resolved of Argument.t list
  | Unresolved of Ast.Expression.Call.Argument.t list

type sig_t =
  | Found of { selected_return_annotation: Type.t }
  | NotFound of closest
[@@deriving eq, show, sexp]

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

  val metaclass : t -> ?dependency:DependencyKey.registered -> ClassSummary.t Node.t -> Type.t

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

  val create_overload
    :  t ->
    ?dependency:DependencyKey.registered ->
    Define.Signature.t ->
    Type.t Type.Callable.overload

  val signature_select
    :  t ->
    ?dependency:DependencyKey.registered ->
    resolve_with_locals:
      (locals:(Reference.t * Annotation.t) list -> Expression.expression Node.t -> Type.t) ->
    arguments:arguments ->
    callable:Type.Callable.t ->
    self_argument:Type.t option ->
    sig_t

  val resolve_mutable_literals
    :  t ->
    ?dependency:DependencyKey.registered ->
    resolve:(Expression.expression Node.t -> Type.t) ->
    expression:Expression.expression Node.t option ->
    resolved:Type.t ->
    expected:Type.t ->
    weakened_type

  val constraints_solution_exists
    :  t ->
    ?dependency:DependencyKey.registered ->
    get_typed_dictionary_override:(Type.t -> Type.t Type.Record.TypedDictionary.record option) ->
    left:Type.t ->
    right:Type.t ->
    bool

  val constructor
    :  t ->
    ?dependency:DependencyKey.registered ->
    Type.Primitive.t ->
    instantiated:Type.t ->
    Type.t

  val instantiate_attribute
    :  t ->
    ?dependency:DependencyKey.registered ->
    accessed_through_class:bool ->
    ?instantiated:Type.t ->
    uninstantiated_attribute ->
    AnnotatedAttribute.instantiated
end

include Environment.S with module ReadOnly = AttributeReadOnly

module PreviousEnvironment = ClassMetadataEnvironment
