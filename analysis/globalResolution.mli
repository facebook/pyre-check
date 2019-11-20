(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Statement

type t

val create : ?dependency:SharedMemoryKeys.dependency -> AnnotatedGlobalEnvironment.ReadOnly.t -> t

val resolve_literal : t -> Expression.t -> Type.t

val parse_annotation
  :  t ->
  ?allow_untracked:bool ->
  ?allow_invalid_type_parameters:bool ->
  ?allow_primitives_from_empty_stubs:bool ->
  Expression.t ->
  Type.t

val class_definition : t -> Type.t -> ClassSummary.t Node.t option

val define_body : t -> Reference.t -> Define.t Node.t option

val solve_ordered_types_less_or_equal
  :  t ->
  left:Type.OrderedTypes.t ->
  right:Type.OrderedTypes.t ->
  constraints:TypeConstraints.t ->
  TypeConstraints.t list

val source_is_unit_test : t -> source:Ast.Source.t -> bool

val class_extends_placeholder_stub_class : t -> ClassSummary.t -> bool

val solve_constraints : t -> TypeConstraints.t -> TypeConstraints.Solution.t option

val partial_solve_constraints
  :  t ->
  TypeConstraints.t ->
  variables:Type.Variable.t list ->
  (TypeConstraints.t * TypeConstraints.Solution.t) option

val constraints_solution_exists : t -> left:Type.t -> right:Type.t -> bool

val solve_less_or_equal
  :  t ->
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

val check_invalid_type_parameters
  :  t ->
  Type.t ->
  AttributeResolution.type_parameters_mismatch list * Type.t

val parse_reference : ?allow_untracked:bool -> t -> Reference.t -> Type.t

val parse_as_list_variadic : t -> Expression.t -> Type.Variable.Variadic.List.t option

val parse_as_concatenation
  :  t ->
  Expression.t ->
  (Type.t Type.OrderedTypes.Concatenation.Middle.t, Type.t) Type.OrderedTypes.Concatenation.t option

val join : t -> Type.t -> Type.t -> Type.t

val meet : t -> Type.t -> Type.t -> Type.t

val widen : t -> widening_threshold:int -> previous:Type.t -> next:Type.t -> iteration:int -> Type.t

val resolve_exports : t -> reference:Reference.t -> Reference.t

val ast_environment : t -> AstEnvironment.ReadOnly.t

val aliases : t -> Type.Primitive.t -> Type.alias option

val module_definition : t -> Reference.t -> Module.t option

val class_metadata : t -> Type.t -> ClassMetadataEnvironment.class_metadata option

val is_protocol : t -> Type.t -> bool

module FunctionDefinitionsCache : sig
  val enable : unit -> unit

  val invalidate : unit -> unit
end

module ClassDefinitionsCache : sig
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

val parse_as_parameter_specification_instance_annotation
  :  t ->
  variable_parameter_annotation:Expression.t ->
  keywords_parameter_annotation:Expression.t ->
  Type.Variable.Variadic.Parameters.t option

val global : t -> Reference.t -> AnnotatedGlobalEnvironment.global option

val class_hierarchy : t -> (module ClassHierarchy.Handler)

val attribute : t -> parent:Type.t -> name:string -> AnnotatedAttribute.t option

val annotation_parser : t -> AnnotatedCallable.annotation_parser

val class_definitions : t -> Reference.t -> Class.t Node.t list option

val resolve_mutable_literals
  :  t ->
  resolve:(Expression.expression Node.t -> Type.t) ->
  expression:Ast.Expression.t option ->
  resolved:Type.t ->
  expected:Type.t ->
  Type.t

val is_consistent_with
  :  t ->
  resolve:(Expression.expression Node.t -> Type.t) ->
  Type.t ->
  Type.t ->
  expression:Ast.Expression.t option ->
  bool

val resolve_class : t -> Type.t -> UnannotatedGlobalEnvironment.class_data list option

val attributes
  :  resolution:t ->
  ?transitive:bool ->
  ?class_attributes:bool ->
  ?include_generated_attributes:bool ->
  ?instantiated:Type.t ->
  ClassSummary.t Node.t ->
  AnnotatedAttribute.t list

val metaclass : resolution:t -> ClassSummary.t Node.t -> Type.t

val c_attribute
  :  resolution:t ->
  ?transitive:bool ->
  ?class_attributes:bool ->
  ?special_method:bool ->
  ClassSummary.t Node.t ->
  name:Identifier.t ->
  instantiated:Type.t ->
  AnnotatedAttribute.t

val attribute_table
  :  resolution:t ->
  transitive:bool ->
  class_attributes:bool ->
  include_generated_attributes:bool ->
  ?special_method:bool ->
  ?instantiated:Type.t ->
  ClassSummary.t Node.t ->
  AnnotatedAttribute.Table.t

val generics : resolution:t -> ClassSummary.t Node.t -> Type.OrderedTypes.t

val superclasses : resolution:t -> ClassSummary.t Node.t -> ClassSummary.t Node.t list

val successors : resolution:t -> ClassSummary.t Node.t -> string list

val create_attribute
  :  resolution:t ->
  parent:ClassSummary.t Node.t ->
  ?instantiated:Type.t ->
  ?defined:bool ->
  ?inherited:bool ->
  ?default_class_attribute:bool ->
  Attribute.t ->
  AnnotatedAttribute.t

val constraints
  :  resolution:t ->
  ?target:ClassSummary.t Node.t ->
  ?parameters:Type.OrderedTypes.t ->
  ClassSummary.t Node.t ->
  instantiated:Type.t ->
  TypeConstraints.Solution.t

val signature_select
  :  global_resolution:t ->
  resolve:(Expression.expression Node.t -> Type.t) ->
  arguments:Expression.Call.Argument.t list ->
  callable:Type.Callable.t ->
  AttributeResolution.sig_t

val create_callable
  :  resolution:t ->
  parent:Type.t option ->
  name:Identifier.t ->
  (bool * Type.t Type.Callable.overload) list ->
  Type.Callable.t

val apply_decorators : resolution:t -> Define.Signature.t Node.t -> Type.t Type.Callable.overload

val constructor : resolution:t -> ClassSummary.t Node.t -> instantiated:Type.t -> Type.t
