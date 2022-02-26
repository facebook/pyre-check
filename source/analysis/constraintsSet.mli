(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Assumptions

type class_hierarchy = {
  instantiate_successors_parameters:
    source:Type.t -> target:Type.Primitive.t -> Type.Parameter.t list option;
  is_transitive_successor: source:Type.Primitive.t -> target:Type.Primitive.t -> bool;
  variables: Type.Primitive.t -> Type.Variable.t list option;
  least_upper_bound: Type.Primitive.t -> Type.Primitive.t -> Type.Primitive.t list;
}

type order = {
  class_hierarchy: class_hierarchy;
  all_attributes:
    Type.t -> assumptions:Assumptions.t -> AnnotatedAttribute.instantiated list option;
  attribute:
    Type.t ->
    assumptions:Assumptions.t ->
    name:Ast.Identifier.t ->
    AnnotatedAttribute.instantiated option;
  is_protocol: Type.t -> protocol_assumptions:ProtocolAssumptions.t -> bool;
  get_typed_dictionary: Type.t -> Type.t Type.Record.TypedDictionary.record option;
  metaclass: Type.Primitive.t -> assumptions:Assumptions.t -> Type.t option;
  assumptions: Assumptions.t;
}

val resolve_callable_protocol : assumption:Type.t -> order:order -> Type.t -> Type.t option

type t = TypeConstraints.t list [@@deriving show]

val empty : t

val impossible : t

val potentially_satisfiable : t -> bool

module Solution : sig
  type t [@@deriving eq]

  val empty : t

  val instantiate : t -> Type.t -> Type.t

  val instantiate_single_variable : t -> Type.Variable.Unary.t -> Type.t option

  (* For testing *)
  val create : Type.Variable.pair list -> t

  val show : t -> string
end

type kind =
  | LessOrEqual of {
      left: Type.t;
      right: Type.t;
    }
  | OrderedTypesLessOrEqual of {
      left: Type.OrderedTypes.t;
      right: Type.OrderedTypes.t;
    }
  | VariableIsExactly of Type.Variable.pair

module type OrderedConstraintsSetType = sig
  val add : t -> new_constraint:kind -> order:order -> t

  val solve : ?only_solve_for:Type.Variable.t list -> t -> order:order -> Solution.t option

  val get_parameter_specification_possibilities
    :  t ->
    order:order ->
    parameter_specification:Type.Variable.Variadic.Parameters.t ->
    Type.Callable.parameters list

  (* Only exposed for testing *)
  val instantiate_protocol_parameters
    :  order ->
    candidate:Type.t ->
    protocol:Ast.Identifier.t ->
    Type.Parameter.t list option
end

module type OrderedConstraintsType = TypeConstraints.OrderedConstraintsType with type order = order

module Make (OrderedConstraints : OrderedConstraintsType) : OrderedConstraintsSetType
