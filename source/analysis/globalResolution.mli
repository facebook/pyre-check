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

val module_path_of_qualifier : t -> Ast.Reference.t -> Ast.ModulePath.t option

val relative_path_of_qualifier : t -> Ast.Reference.t -> string option

val source_of_qualifier : t -> Reference.t -> Source.t option

val is_protocol : t -> Type.t -> bool

val is_special_form : t -> Type.t -> bool

val first_matching_class_decorator
  :  t ->
  names:string list ->
  ClassSummary.t Node.t ->
  Ast.Statement.Decorator.t option

val get_class_summary : t -> Type.Primitive.t -> ClassSummary.t Node.t option

(* This will return an empty list if the qualifier isn't part of the project we are type
   checking. *)
val get_define_names_for_qualifier_in_project : t -> Reference.t -> Reference.t list

(* This will return None if called on a function definition that is not part of the project we are
   type checking (i.e. defined in dependencies). *)
val get_function_definition_in_project : t -> Reference.t -> FunctionDefinition.t option

(* This will return None if called on a function definition that is not part of the project we are
   type checking (i.e. defined in dependencies). *)
val get_define_body_in_project : t -> Reference.t -> Define.t Node.t option

val module_exists : t -> Reference.t -> bool

val class_exists : t -> Type.Primitive.t -> bool

val get_module_metadata : t -> Reference.t -> Module.Metadata.t option

val resolve_exports : t -> ?from:Reference.t -> Reference.t -> ResolvedReference.t option

val get_type_alias : t -> Type.Primitive.t -> Type.t option

val get_variable : t -> Type.Primitive.t -> Type.Variable.t option

val parse_annotation_without_sanitizing_type_arguments
  :  t ->
  ?modify_aliases:(?replace_unbound_parameters_with_any:bool -> Type.t -> Type.t) ->
  variables:(string -> Type.Variable.t option) ->
  ?allow_untracked:bool ->
  Expression.t ->
  Type.t

val param_spec_from_vararg_annotations
  :  t ->
  args_annotation:Expression.t ->
  kwargs_annotation:Expression.t ->
  Type.Variable.ParamSpec.t option

val immediate_parents : t -> Type.Primitive.t -> string list

val generic_parameters : t -> Type.Primitive.t -> Type.GenericParameter.t list option

val generic_parameters_as_variables : t -> Type.Primitive.t -> Type.Variable.t list option

val has_transitive_successor : t -> successor:Type.Primitive.t -> Type.Primitive.t -> bool

val successors : t -> Type.Primitive.t -> string list

val get_class_metadata
  :  t ->
  Type.Primitive.t ->
  ClassSuccessorMetadataEnvironment.class_metadata option

val full_order : t -> ConstraintsSet.order

val parse_annotation
  :  t ->
  ?validation:SharedMemoryKeys.ParseAnnotationKey.type_validation_policy ->
  Expression.t ->
  Type.t

val global : t -> Reference.t -> AttributeResolution.Global.t option

val get_typed_dictionary : t -> Type.t -> Type.TypedDictionary.t option

val constraints_solution_exists
  :  t ->
  get_typed_dictionary_override:(Type.t -> Type.TypedDictionary.t option) ->
  left:Type.t ->
  right:Type.t ->
  bool

val uninstantiated_attributes
  :  t ->
  ?transitive:bool ->
  ?accessed_through_class:bool ->
  ?include_generated_attributes:bool ->
  Type.Primitive.t ->
  AnnotatedAttribute.uninstantiated list option

val instantiate_attribute
  :  t ->
  ?instantiated:Type.t ->
  accessed_through_class:bool ->
  accessed_through_readonly:bool ->
  AnnotatedAttribute.uninstantiated ->
  AnnotatedAttribute.instantiated

val metaclass : t -> Type.Primitive.t -> Type.t option

val resolve_mutable_literals
  :  t ->
  resolve:(Expression.expression Node.t -> Type.t) ->
  expression:Ast.Expression.t option ->
  resolved:Type.t ->
  expected:Type.t ->
  WeakenMutableLiterals.weakened_type

val resolve_define
  :  t ->
  callable_name:Reference.t option ->
  implementation:Define.Signature.t option ->
  overloads:Define.Signature.t list ->
  scoped_type_variables:Type.Variable.t Identifier.Map.t option ->
  AttributeResolution.resolved_define

val resolve_define_undecorated
  :  t ->
  callable_name:Reference.t option ->
  implementation:Define.Signature.t option ->
  overloads:Define.Signature.t list ->
  scoped_type_variables:Type.Variable.t Identifier.Map.t option ->
  AnnotatedAttribute.decorated_method

val signature_select
  :  t ->
  resolve_with_locals:
    (locals:(Reference.t * TypeInfo.Unit.t) list -> Expression.expression Node.t -> Type.t) ->
  arguments:Type.t SignatureSelection.Argument.t list ->
  location:Location.t ->
  callable:Type.Callable.t ->
  self_argument:Type.t option ->
  SignatureSelectionTypes.instantiated_return_annotation

val validate_and_sanitize_type_arguments
  :  t ->
  Type.t ->
  AttributeResolution.type_parameters_mismatch list * Type.t

val location_of_global : t -> Reference.t -> Location.WithModule.t option

val class_hierarchy : t -> (module ClassHierarchy.Handler)

val parse_reference : ?allow_untracked:bool -> t -> Reference.t -> Type.t

val less_or_equal : t -> left:Type.t -> right:Type.t -> bool

val join : t -> Type.t -> Type.t -> Type.t

(** A variant of [join] used to join types when control flow merges. For performance reasons, [join]
    joins types into a non-union type whenever possible. In particular, it weakens literals - for
    example, joining [Literal\[1\]] and [Literal\[2\]] into [int]. This is undesirable in cases
    where we want to preserve literal type information, like when different literal values are
    assigned to the same name in if/else branches. *)
val join_for_branch_merge : t -> Type.t -> Type.t -> Type.t

val meet : t -> Type.t -> Type.t -> Type.t

val widen : t -> widening_threshold:int -> previous:Type.t -> next:Type.t -> iteration:int -> Type.t

val is_invariance_mismatch : t -> left:Type.t -> right:Type.t -> bool

val attribute_from_class_name
  :  t ->
  ?transitive:bool ->
  ?accessed_through_class:bool ->
  ?accessed_through_readonly:bool ->
  ?special_method:bool ->
  Type.Primitive.t ->
  name:Identifier.t ->
  instantiated:Type.t ->
  AnnotatedAttribute.instantiated option

val attribute_from_annotation
  :  ?special_method:bool ->
  t ->
  parent:Type.t ->
  name:string ->
  AnnotatedAttribute.instantiated option

val is_typed_dictionary : t -> Type.t -> bool

val is_enum : t -> Type.t -> bool

val is_consistent_with
  :  t ->
  resolve:(Expression.expression Node.t -> Type.t) ->
  Type.t ->
  Type.t ->
  expression:Ast.Expression.t option ->
  bool

val type_of_iteration_value : t -> Type.t -> Type.t option

val type_of_awaited_value : t -> Type.t -> Type.t option

val type_of_mapping_key_and_value : t -> Type.t -> (Type.t * Type.t) option

val type_of_generator_send_and_return : t -> Type.t -> Type.t * Type.t

val annotation_parser : t -> AnnotatedCallable.annotation_parser

val nonvalidating_annotation_parser : t -> AnnotatedCallable.annotation_parser

val overrides : t -> Type.Primitive.t -> name:Identifier.t -> AnnotatedAttribute.instantiated option

val refine : t -> TypeInfo.Unit.t -> Type.t -> TypeInfo.Unit.t

(* Exposed in the API for testing only *)
val extract_unary_type_arguments__unsafe : t -> source:Type.t -> target:string -> Type.t list option

module Testing : sig
  val constraints
    :  t ->
    target:Type.Primitive.t ->
    instantiated:Type.t ->
    unit ->
    TypeConstraints.Solution.t
end
