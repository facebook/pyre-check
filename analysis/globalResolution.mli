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

val function_definition : t -> Reference.t -> FunctionDefinition.t option

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
  :  ?default:ClassHierarchy.Variable.t list option ->
  t ->
  Type.Primitive.t ->
  ClassHierarchy.Variable.t list option

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

val annotated_global_environment : t -> AnnotatedGlobalEnvironment.ReadOnly.t

val class_metadata_environment : t -> ClassMetadataEnvironment.ReadOnly.t

val undecorated_function_environment : t -> UndecoratedFunctionEnvironment.ReadOnly.t

val class_hierarchy_environment : t -> ClassHierarchyEnvironment.ReadOnly.t

val alias_environment : t -> AliasEnvironment.ReadOnly.t

val empty_stub_environment : t -> EmptyStubEnvironment.ReadOnly.t

val unannotated_global_environment : t -> UnannotatedGlobalEnvironment.ReadOnly.t

val aliases : t -> Type.Primitive.t -> Type.alias option

val module_exists : t -> Reference.t -> bool

val class_metadata : t -> Type.t -> ClassMetadataEnvironment.class_metadata option

val is_protocol : t -> Type.t -> bool

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

val attribute_from_annotation
  :  t ->
  parent:Type.t ->
  name:string ->
  AnnotatedAttribute.instantiated option

val annotation_parser
  :  ?allow_invalid_type_parameters:bool ->
  t ->
  AnnotatedCallable.annotation_parser

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

val is_transitive_successor : t -> predecessor:string -> successor:string -> bool

val attributes
  :  resolution:t ->
  ?transitive:bool ->
  ?class_attributes:bool ->
  ?include_generated_attributes:bool ->
  Type.Primitive.t ->
  AttributeResolution.uninstantiated_attribute list option

val instantiate_attribute
  :  resolution:t ->
  ?instantiated:Type.t ->
  AttributeResolution.uninstantiated_attribute ->
  AnnotatedAttribute.instantiated

val metaclass : resolution:t -> ClassSummary.t Node.t -> Type.t

val attribute_from_class_name
  :  resolution:t ->
  ?transitive:bool ->
  ?class_attributes:bool ->
  ?special_method:bool ->
  Type.Primitive.t ->
  name:Identifier.t ->
  instantiated:Type.t ->
  AnnotatedAttribute.instantiated option

val superclasses : resolution:t -> ClassSummary.t Node.t -> ClassSummary.t Node.t list

val successors : resolution:t -> ClassSummary.t Node.t -> string list

val constraints
  :  resolution:t ->
  target:Type.Primitive.t ->
  ?parameters:Type.Parameter.t list ->
  instantiated:Type.t ->
  unit ->
  TypeConstraints.Solution.t

val signature_select
  :  global_resolution:t ->
  resolve:(Expression.expression Node.t -> Type.t) ->
  arguments:Expression.Call.Argument.t list ->
  callable:Type.Callable.t ->
  AttributeResolution.sig_t

val create_overload : resolution:t -> Define.Signature.t -> Type.t Type.Callable.overload

val constructor : resolution:t -> ClassSummary.t Node.t -> instantiated:Type.t -> Type.t

val attribute_names
  :  resolution:t ->
  ?transitive:bool ->
  ?class_attributes:bool ->
  ?include_generated_attributes:bool ->
  ?instantiated:Type.t ->
  Type.Primitive.t ->
  string list option

val global_location : t -> Reference.t -> Location.t option
