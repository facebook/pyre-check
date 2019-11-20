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
  | UnexpectedVariadic of {
      actual: Type.OrderedTypes.t;
      expected: Type.Variable.Unary.t list;
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
  | MismatchWithListVariadicTypeVariable of
      Type.OrderedTypes.t * mismatch_with_list_variadic_type_variable
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

type assumptions = {
  protocol_assumptions: TypeOrder.ProtocolAssumptions.t;
  callable_assumptions: TypeOrder.CallableAssumptions.t;
}

module AttributeCache : sig
  val clear : unit -> unit
end

module AnnotationCache : sig
  val clear : scheduler:Scheduler.t -> configuration:Configuration.Analysis.t -> unit
end

val full_order : ?dependency:dependency -> ClassMetadataEnvironment.ReadOnly.t -> TypeOrder.order

val check_invalid_type_parameters
  :  ClassMetadataEnvironment.ReadOnly.t ->
  ?dependency:SharedMemoryKeys.dependency ->
  Type.t ->
  type_parameters_mismatch list * Type.t

val parse_annotation
  :  ?allow_untracked:bool ->
  ?allow_invalid_type_parameters:bool ->
  ?allow_primitives_from_empty_stubs:bool ->
  ?dependency:SharedMemoryKeys.dependency ->
  class_metadata_environment:ClassMetadataEnvironment.MetadataReadOnly.t ->
  Expression.expression Node.t ->
  Type.t

val attribute_table
  :  transitive:bool ->
  class_attributes:bool ->
  include_generated_attributes:bool ->
  ?special_method:bool ->
  ?instantiated:Type.t ->
  ?dependency:SharedMemoryKeys.dependency ->
  ClassSummary.t Node.t ->
  class_metadata_environment:ClassMetadataEnvironment.MetadataReadOnly.t ->
  AnnotatedAttribute.Table.t

val attributes
  :  ?transitive:bool ->
  ?class_attributes:bool ->
  ?include_generated_attributes:bool ->
  ?instantiated:Type.t ->
  ClassSummary.t Node.t ->
  ?dependency:SharedMemoryKeys.dependency ->
  class_metadata_environment:ClassMetadataEnvironment.MetadataReadOnly.t ->
  AnnotatedAttribute.attribute Node.t list

val create_attribute
  :  ?dependency:SharedMemoryKeys.dependency ->
  class_metadata_environment:ClassMetadataEnvironment.MetadataReadOnly.t ->
  parent:ClassSummary.t Node.t ->
  ?instantiated:Type.t ->
  ?defined:bool ->
  ?inherited:bool ->
  ?default_class_attribute:bool ->
  Attribute.attribute Node.t ->
  AnnotatedAttribute.attribute Node.t

val metaclass
  :  ?dependency:SharedMemoryKeys.dependency ->
  ClassSummary.t Node.t ->
  class_metadata_environment:ClassMetadataEnvironment.MetadataReadOnly.t ->
  Type.t

val constraints
  :  ?target:ClassSummary.t Node.t ->
  ?parameters:Type.t Type.OrderedTypes.record ->
  ClassSummary.t Node.t ->
  ?dependency:SharedMemoryKeys.dependency ->
  instantiated:Type.t ->
  class_metadata_environment:ClassMetadataEnvironment.MetadataReadOnly.t ->
  TypeConstraints.Solution.t

val generics
  :  ?dependency:SharedMemoryKeys.dependency ->
  ClassSummary.t Node.t ->
  class_metadata_environment:ClassMetadataEnvironment.MetadataReadOnly.t ->
  Type.t Type.OrderedTypes.record

val resolve_literal
  :  ?dependency:SharedMemoryKeys.dependency ->
  class_metadata_environment:ClassMetadataEnvironment.MetadataReadOnly.t ->
  Expression.expression Node.t ->
  Type.t

val apply_decorators
  :  ?dependency:SharedMemoryKeys.dependency ->
  class_metadata_environment:ClassMetadataEnvironment.MetadataReadOnly.t ->
  Define.Signature.t Node.t ->
  Type.t Type.Callable.overload

val create_callable
  :  ?dependency:SharedMemoryKeys.dependency ->
  class_metadata_environment:ClassMetadataEnvironment.MetadataReadOnly.t ->
  parent:Type.t option ->
  name:string ->
  (bool * Type.t Type.Callable.overload) list ->
  Type.t Type.Callable.record

val signature_select
  :  ?dependency:SharedMemoryKeys.dependency ->
  class_metadata_environment:ClassMetadataEnvironment.MetadataReadOnly.t ->
  resolve:(Expression.expression Node.t -> Type.t) ->
  arguments:Expression.Call.Argument.t list ->
  callable:Type.t Type.Callable.record ->
  sig_t

val weaken_mutable_literals
  :  (Expression.expression Node.t -> Type.t) ->
  expression:Expression.expression Node.t option ->
  resolved:Type.t ->
  expected:Type.t ->
  comparator:(left:Type.t -> right:Type.t -> bool) ->
  Type.t

val resolve_mutable_literals
  :  ?dependency:SharedMemoryKeys.dependency ->
  class_metadata_environment:ClassMetadataEnvironment.MetadataReadOnly.t ->
  resolve:(Expression.expression Node.t -> Type.t) ->
  expression:Expression.expression Node.t option ->
  resolved:Type.t ->
  expected:Type.t ->
  Type.t

val constraints_solution_exists
  :  ?dependency:SharedMemoryKeys.dependency ->
  class_metadata_environment:ClassMetadataEnvironment.MetadataReadOnly.t ->
  left:Type.t ->
  right:Type.t ->
  bool

val constructor
  :  ?dependency:SharedMemoryKeys.dependency ->
  ClassSummary.t Node.t ->
  instantiated:Type.t ->
  class_metadata_environment:ClassMetadataEnvironment.MetadataReadOnly.t ->
  Type.t

val attribute
  :  ?transitive:bool ->
  ?class_attributes:bool ->
  ?special_method:bool ->
  ClassSummary.t Node.t ->
  ?dependency:SharedMemoryKeys.dependency ->
  class_metadata_environment:ClassMetadataEnvironment.MetadataReadOnly.t ->
  name:string ->
  instantiated:Type.t ->
  AnnotatedAttribute.attribute Node.t
