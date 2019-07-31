(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
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
[@@deriving compare, eq, sexp, show, hash]

type type_parameters_mismatch = {
  name: string;
  kind: generic_type_problems;
}
[@@deriving compare, eq, sexp, show, hash]

type class_metadata = {
  successors: Type.Primitive.t list;
  is_test: bool;
  is_final: bool;
  extends_placeholder_stub_class: bool;
}
[@@deriving eq]

type global = Annotation.t Node.t [@@deriving eq, show]

type t

val create
  :  class_hierarchy:(module ClassHierarchy.Handler) ->
  aliases:(Type.Primitive.t -> Type.alias option) ->
  module_definition:(Reference.t -> Module.t option) ->
  class_definition:(Type.Primitive.t -> Class.t Node.t option) ->
  class_metadata:(Type.Primitive.t -> class_metadata option) ->
  constructor:(resolution:t -> Type.Primitive.t -> Type.t option) ->
  undecorated_signature:(Reference.t -> Type.t Type.Callable.overload option) ->
  attributes:(resolution:t -> Type.t -> AnnotatedAttribute.t list option) ->
  is_protocol:(Type.t -> bool) ->
  global:(Reference.t -> global option) ->
  unit ->
  t

val resolve_literal : t -> Expression.t -> Type.t

val parse_annotation
  :  ?allow_untracked:bool ->
  ?allow_invalid_type_parameters:bool ->
  ?allow_primitives_from_empty_stubs:bool ->
  t ->
  Expression.t ->
  Type.t

val class_definition : t -> Type.t -> Class.t Node.t option

val solve_ordered_types_less_or_equal
  :  t ->
  left:Type.OrderedTypes.t ->
  right:Type.OrderedTypes.t ->
  constraints:TypeConstraints.t ->
  TypeConstraints.t list

val source_is_unit_test : t -> source:Ast.Source.t -> bool

val class_extends_placeholder_stub_class : t -> Statement.Class.t -> bool

val solve_constraints
  :  ?any_is_bottom:bool ->
  t ->
  TypeConstraints.t ->
  TypeConstraints.Solution.t option

val partial_solve_constraints
  :  t ->
  TypeConstraints.t ->
  variables:Type.Variable.t list ->
  (TypeConstraints.t * TypeConstraints.Solution.t) option

val constraints_solution_exists : left:Type.t -> right:Type.t -> t -> bool

val solve_less_or_equal
  :  ?any_is_bottom:bool ->
  t ->
  constraints:TypeConstraints.t ->
  left:Type.t ->
  right:Type.t ->
  TypeConstraints.t list

val is_invariance_mismatch : t -> left:Type.t -> right:Type.t -> bool

val variables
  :  ?default:ClassHierarchy.variables option ->
  t ->
  Type.Primitive.t ->
  ClassHierarchy.variables option

val check_invalid_type_parameters : t -> Type.t -> type_parameters_mismatch list * Type.t

val parse_reference : ?allow_untracked:bool -> t -> Reference.t -> Type.t

val parse_as_list_variadic : t -> Expression.t -> Type.Variable.Variadic.List.t option

val parse_as_list_variadic_map_operator : t -> Expression.t -> Type.OrderedTypes.Map.t option

val join : t -> Type.t -> Type.t -> Type.t

val meet : t -> Type.t -> Type.t -> Type.t

val widen
  :  t ->
  widening_threshold:int ->
  previous:Type.t ->
  next:Type.t ->
  iteration:int ->
  Type.t

val resolve_exports : t -> reference:Reference.t -> Reference.t

val aliases : t -> Type.Primitive.t -> Type.alias option

val module_definition : t -> Reference.t -> Module.t option

val class_metadata : t -> Type.t -> class_metadata option

val is_protocol : t -> Type.t -> bool

module FunctionDefinitionsCache : sig
  val enable : unit -> unit

  val invalidate : unit -> unit
end

val function_definitions : t -> Reference.t -> Define.t Node.t list option

val is_suppressed_module : t -> Reference.t -> bool

(* Exposed only for parallelism. Future not guaranteed. *)
val undecorated_signature : t -> Reference.t -> Type.t Type.Callable.overload option

val less_or_equal : t -> left:Type.t -> right:Type.t -> bool

(* Only for use in monkey check. *)
val is_compatible_with : t -> left:Type.t -> right:Type.t -> bool

val is_instantiated : t -> Type.t -> bool

val is_tracked : t -> Type.Primitive.t -> bool

val contains_untracked : t -> Type.t -> bool

val is_string_to_any_mapping : t -> Type.t -> bool

val parse_as_parameter_specification_instance_annotation
  :  t ->
  variable_parameter_annotation:Expression.t ->
  keywords_parameter_annotation:Expression.t ->
  Type.Variable.Variadic.Parameters.t option

val consistent_solution_exists : t -> Type.t -> Type.t -> bool

val global : t -> Reference.t -> global option
