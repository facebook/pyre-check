(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Statement

type t

val create
  :  ?dependency:SharedMemoryKeys.DependencyKey.registered ->
  AnnotatedGlobalEnvironment.ReadOnly.t ->
  t

val resolve_literal : t -> Expression.t -> Type.t

val parse_annotation
  :  t ->
  ?validation:SharedMemoryKeys.ParseAnnotationKey.type_validation_policy ->
  Expression.t ->
  Type.t

val class_summary : t -> Type.t -> ClassSummary.t Node.t option

val define_body : t -> Reference.t -> Define.t Node.t option

val function_definition : t -> Reference.t -> FunctionDefinition.t option

val source_is_unit_test : t -> source:Ast.Source.t -> bool

val constraints_solution_exists : t -> left:Type.t -> right:Type.t -> bool

module ConstraintsSet : sig
  val add
    :  ConstraintsSet.t ->
    new_constraint:ConstraintsSet.kind ->
    global_resolution:t ->
    ConstraintsSet.t

  val solve : ConstraintsSet.t -> global_resolution:t -> ConstraintsSet.Solution.t option

  type order = ConstraintsSet.order

  module Solution : sig
    type t = ConstraintsSet.Solution.t
  end
end

val is_invariance_mismatch : t -> left:Type.t -> right:Type.t -> bool

val variables
  :  ?default:Type.Variable.t list option ->
  t ->
  Type.Primitive.t ->
  Type.Variable.t list option

val check_invalid_type_parameters
  :  t ->
  Type.t ->
  AttributeResolution.type_parameters_mismatch list * Type.t

val parse_reference : ?allow_untracked:bool -> t -> Reference.t -> Type.t

val legacy_resolve_exports : t -> reference:Reference.t -> Reference.t

val resolve_exports
  :  t ->
  ?from:Reference.t ->
  Reference.t ->
  UnannotatedGlobalEnvironment.ResolvedReference.t option

val ast_environment : t -> AstEnvironment.ReadOnly.t

val annotated_global_environment : t -> AnnotatedGlobalEnvironment.ReadOnly.t

val class_metadata_environment : t -> ClassMetadataEnvironment.ReadOnly.t

val class_hierarchy_environment : t -> ClassHierarchyEnvironment.ReadOnly.t

val alias_environment : t -> AliasEnvironment.ReadOnly.t

val empty_stub_environment : t -> EmptyStubEnvironment.ReadOnly.t

val unannotated_global_environment : t -> UnannotatedGlobalEnvironment.ReadOnly.t

val aliases
  :  t ->
  ?replace_unbound_parameters_with_any:bool ->
  Type.Primitive.t ->
  Type.alias option

val base_is_from_placeholder_stub : t -> Expression.t -> bool

val module_exists : t -> Reference.t -> bool

val get_module_metadata : t -> Reference.t -> Module.t option

val class_metadata : t -> Type.t -> ClassMetadataEnvironment.class_metadata option

val is_protocol : t -> Type.t -> bool

val function_definitions : t -> Reference.t -> Define.t Node.t list option

val is_suppressed_module : t -> Reference.t -> bool

val full_order : t -> ConstraintsSet.order

val less_or_equal : t -> left:Type.t -> right:Type.t -> bool

val join : t -> Type.t -> Type.t -> Type.t

val meet : t -> Type.t -> Type.t -> Type.t

val widen : t -> widening_threshold:int -> previous:Type.t -> next:Type.t -> iteration:int -> Type.t

val types_are_orderable : t -> Type.t -> Type.t -> bool

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

val global : t -> Reference.t -> AttributeResolution.Global.t option

val class_hierarchy : t -> (module ClassHierarchy.Handler)

val attribute_from_annotation
  :  ?special_method:bool ->
  t ->
  parent:Type.t ->
  name:string ->
  AnnotatedAttribute.instantiated option

val annotation_parser
  :  ?allow_invalid_type_parameters:bool ->
  t ->
  AnnotatedCallable.annotation_parser

val resolved_type : WeakenMutableLiterals.weakened_type -> Type.t

val resolve_mutable_literals
  :  t ->
  resolve:(Expression.expression Node.t -> Type.t) ->
  expression:Ast.Expression.t option ->
  resolved:Type.t ->
  expected:Type.t ->
  WeakenMutableLiterals.weakened_type

val get_typed_dictionary
  :  resolution:t ->
  Type.t ->
  Type.t Type.Record.TypedDictionary.record option

val is_typed_dictionary : resolution:t -> Type.t -> bool

val is_consistent_with
  :  t ->
  resolve:(Expression.expression Node.t -> Type.t) ->
  Type.t ->
  Type.t ->
  expression:Ast.Expression.t option ->
  bool

val is_transitive_successor
  :  ?placeholder_subclass_extends_all:bool ->
  t ->
  predecessor:string ->
  successor:string ->
  bool

val attributes
  :  resolution:t ->
  ?transitive:bool ->
  ?accessed_through_class:bool ->
  ?include_generated_attributes:bool ->
  Type.Primitive.t ->
  AttributeResolution.uninstantiated_attribute list option

val instantiate_attribute
  :  resolution:t ->
  ?instantiated:Type.t ->
  accessed_through_class:bool ->
  AttributeResolution.uninstantiated_attribute ->
  AnnotatedAttribute.instantiated

val metaclass : resolution:t -> Type.Primitive.t -> Type.t option

val attribute_from_class_name
  :  resolution:t ->
  ?transitive:bool ->
  ?accessed_through_class:bool ->
  ?special_method:bool ->
  Type.Primitive.t ->
  name:Identifier.t ->
  instantiated:Type.t ->
  AnnotatedAttribute.instantiated option

val successors : resolution:t -> Type.Primitive.t -> string list

val immediate_parents : resolution:t -> Type.Primitive.t -> string list

val constraints
  :  resolution:t ->
  target:Type.Primitive.t ->
  ?parameters:Type.Parameter.t list ->
  instantiated:Type.t ->
  unit ->
  ConstraintsSet.Solution.t

val signature_select
  :  global_resolution:t ->
  resolve_with_locals:
    (locals:(Reference.t * Annotation.t) list -> Expression.expression Node.t -> Type.t) ->
  arguments:AttributeResolution.Argument.t list ->
  callable:Type.Callable.t ->
  self_argument:Type.t option ->
  SignatureSelectionTypes.instantiated_return_annotation

val resolve_define
  :  resolution:t ->
  implementation:Define.Signature.t option ->
  overloads:Define.Signature.t list ->
  AttributeResolution.resolved_define

val attribute_names
  :  resolution:t ->
  ?transitive:bool ->
  ?accessed_through_class:bool ->
  ?include_generated_attributes:bool ->
  ?instantiated:Type.t ->
  Type.Primitive.t ->
  string list option

val global_location : t -> Reference.t -> Location.WithModule.t option

val class_exists : t -> Type.Primitive.t -> bool

val overrides
  :  Type.Primitive.t ->
  resolution:t ->
  name:Identifier.t ->
  AnnotatedAttribute.instantiated option

(* If the given type is a subtype of generic type `AsName[X]`, return X *)
val extract_type_parameters : t -> source:Type.t -> target:string -> Type.t list option

val type_of_iteration_value : global_resolution:t -> Type.t -> Type.t option

val type_of_generator_send_and_return : global_resolution:t -> Type.t -> Type.t * Type.t

val define : t -> Reference.t -> Define.t option

val refine : global_resolution:t -> Annotation.t -> Type.t -> Annotation.t
