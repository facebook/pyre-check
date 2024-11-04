(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type class_hierarchy = {
  instantiate_successors_parameters:
    source:Type.t -> target:Type.Primitive.t -> Type.Argument.t list option;
  has_transitive_successor: successor:Type.Primitive.t -> Type.Primitive.t -> bool;
  generic_parameters: Type.Primitive.t -> Type.GenericParameter.t list option;
  least_upper_bound: Type.Primitive.t -> Type.Primitive.t -> Type.Primitive.t option;
}

type order = {
  class_hierarchy: class_hierarchy;
  instantiated_attributes:
    Type.t -> cycle_detections:CycleDetection.t -> AnnotatedAttribute.instantiated list option;
  attribute:
    Type.t ->
    cycle_detections:CycleDetection.t ->
    name:Ast.Identifier.t ->
    AnnotatedAttribute.instantiated option;
  is_protocol: Type.t -> bool;
  get_typed_dictionary: Type.t -> Type.TypedDictionary.t option;
  get_named_tuple_fields: Type.t -> Type.t list option;
  metaclass: Type.Primitive.t -> cycle_detections:CycleDetection.t -> Type.t option;
  cycle_detections: CycleDetection.t;
  variance_map:
    class_name:string ->
    parameters:Type.GenericParameter.t list ->
    Type.Record.Variance.t Ast.Identifier.Map.t;
}

val resolve_callable_protocol : assumption:Type.t -> order:order -> Type.t -> Type.t option

type t = TypeConstraints.t list [@@deriving show]

val empty : t

val impossible : t

val potentially_satisfiable : t -> bool

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
  val add_and_simplify : t -> new_constraint:kind -> order:order -> t

  val solve
    :  ?only_solve_for:Type.Variable.t list ->
    t ->
    order:order ->
    TypeConstraints.Solution.t option

  val get_parameter_specification_possibilities
    :  t ->
    order:order ->
    parameter_specification:Type.Variable.ParamSpec.t ->
    Type.Callable.parameters list

  module Testing : sig
    val instantiate_protocol_parameters
      :  order ->
      protocol:Ast.Identifier.t ->
      ?protocol_arguments:Type.Argument.t list ->
      Type.t ->
      Type.Argument.t list option
  end
end

module type OrderedConstraintsType = TypeConstraints.OrderedConstraintsType with type order = order

module Make (_ : OrderedConstraintsType) : OrderedConstraintsSetType
