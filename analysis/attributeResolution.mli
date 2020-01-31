(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open SharedMemoryKeys
open Statement

type generic_type_problems =
  | IncorrectNumberOfParameters of {
      actual: int;
      expected: int;
    }
  | ViolateConstraints of {
      actual: Type.t;
      expected: Type.Variable.Unary.t;
    }
  | UnexpectedGroup of {
      actual: Type.OrderedTypes.t;
      expected: Type.Variable.Unary.t;
    }
  | UnexpectedSingle of {
      actual: Type.t;
      expected: Type.Variable.Variadic.List.t;
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
  expression: Expression.t;
  annotation: Type.t;
}
[@@deriving compare, eq, show, sexp, hash]

type missing_argument =
  | Named of Identifier.t
  | Anonymous of int
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
  | UnexpectedKeyword of Identifier.t
[@@deriving eq, show, compare]

type closest = {
  callable: Type.Callable.t;
  reason: reason option;
}
[@@deriving show]

type sig_t =
  | Found of Type.Callable.t
  | NotFound of closest
[@@deriving eq, show]

module Argument : sig
  type kind =
    | SingleStar
    | DoubleStar
    | Named of string Node.t
    | Positional

  type t = {
    expression: Expression.t;
    full_expression: Expression.t;
    position: int;
    kind: kind;
    resolved: Type.t;
  }
end

type argument =
  | Argument of Argument.t
  | Default

type ranks = {
  arity: int;
  annotation: int;
  position: int;
}

type reasons = {
  arity: reason list;
  annotation: reason list;
}

type signature_match = {
  callable: Type.Callable.t;
  argument_mapping: argument list Type.Callable.Parameter.Map.t;
  constraints_set: TypeConstraints.t list;
  ranks: ranks;
  reasons: reasons;
}

val weaken_mutable_literals
  :  (Expression.expression Node.t -> Type.t) ->
  expression:Expression.expression Node.t option ->
  resolved:Type.t ->
  expected:Type.t ->
  comparator:(left:Type.t -> right:Type.t -> bool) ->
  Type.t

module AttributeReadOnly : sig
  include Environment.ReadOnly

  val class_metadata_environment : t -> ClassMetadataEnvironment.ReadOnly.t

  val full_order : ?dependency:dependency -> t -> TypeOrder.order

  val check_invalid_type_parameters
    :  t ->
    ?dependency:SharedMemoryKeys.dependency ->
    Type.t ->
    type_parameters_mismatch list * Type.t

  val parse_annotation
    :  t ->
    ?allow_untracked:bool ->
    ?allow_invalid_type_parameters:bool ->
    ?allow_primitives_from_empty_stubs:bool ->
    ?dependency:SharedMemoryKeys.dependency ->
    Expression.expression Node.t ->
    Type.t

  val attribute_table
    :  t ->
    transitive:bool ->
    class_attributes:bool ->
    include_generated_attributes:bool ->
    ?special_method:bool ->
    ?instantiated:Type.t ->
    ?dependency:SharedMemoryKeys.dependency ->
    string ->
    AnnotatedAttribute.Table.t option

  val metaclass : t -> ?dependency:SharedMemoryKeys.dependency -> ClassSummary.t Node.t -> Type.t

  val constraints
    :  t ->
    target:Type.Primitive.t ->
    ?parameters:Type.Parameter.t list ->
    ?dependency:SharedMemoryKeys.dependency ->
    instantiated:Type.t ->
    unit ->
    TypeConstraints.Solution.t

  val resolve_literal
    :  t ->
    ?dependency:SharedMemoryKeys.dependency ->
    Expression.expression Node.t ->
    Type.t

  val create_overload
    :  t ->
    ?dependency:SharedMemoryKeys.dependency ->
    Define.Signature.t Node.t ->
    Type.t Type.Callable.overload

  val signature_select
    :  t ->
    ?dependency:SharedMemoryKeys.dependency ->
    resolve:(Expression.expression Node.t -> Type.t) ->
    arguments:Expression.Call.Argument.t list ->
    callable:Type.t Type.Callable.record ->
    sig_t

  val resolve_mutable_literals
    :  t ->
    ?dependency:SharedMemoryKeys.dependency ->
    resolve:(Expression.expression Node.t -> Type.t) ->
    expression:Expression.expression Node.t option ->
    resolved:Type.t ->
    expected:Type.t ->
    Type.t

  val constraints_solution_exists
    :  t ->
    ?dependency:SharedMemoryKeys.dependency ->
    left:Type.t ->
    right:Type.t ->
    bool

  val constructor
    :  t ->
    ?dependency:SharedMemoryKeys.dependency ->
    ClassSummary.t Node.t ->
    instantiated:Type.t ->
    Type.t
end

include Environment.S with module ReadOnly = AttributeReadOnly

module PreviousEnvironment = ClassMetadataEnvironment
