(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

(* PyrePysaApi is a wrapper around a type checker API, which exposes source code, ASTs and type
   information about the code to analyze. Right now, this wraps either the old Pyre 1 API provided
   by `Analysis.PyrePysaEnvironment` or the Pyrefly API provided by `Interprocedural.Pyrefly`. *)

module ReadWrite : sig
  type t

  val load_from_cache
    :  configuration:Configuration.Analysis.t ->
    pyrefly_results:PyrePath.t option ->
    t

  val create_with_cold_start
    :  scheduler:Scheduler.t ->
    scheduler_policies:Configuration.SchedulerPolicies.t ->
    configuration:Configuration.Analysis.t ->
    pyrefly_results:PyrePath.t option ->
    decorator_configuration:Analysis.DecoratorPreprocessing.Configuration.t ->
    skip_type_checking_callables:Ast.Reference.SerializableSet.t ->
    callback_with_qualifiers_and_definitions:
      ((lookup_source:(ArtifactPath.t -> SourcePath.t option) -> Ast.Reference.t -> string option) ->
      Ast.Reference.t list ->
      Ast.Reference.t list ->
      unit) ->
    t

  val configuration : t -> Configuration.Analysis.t

  (* Only used for caching *)
  val module_paths : t -> Ast.ModulePath.t list

  (* Only used for caching *)
  val module_paths_from_disk : t -> Ast.ModulePath.t list

  (* Only used for caching *)
  val save : t -> unit

  val purge_sources_from_shared_memory : t -> unit
end

module ReadOnly : sig
  type t =
    | Pyre1 of Analysis.PyrePysaEnvironment.ReadOnly.t
    | Pyrefly of PyreflyApi.ReadOnly.t

  val of_read_write_api : ReadWrite.t -> t

  val from_pyre1_environment
    :  type_environment:Analysis.TypeEnvironment.TypeEnvironmentReadOnly.t ->
    global_module_paths_api:Analysis.GlobalModulePathsApi.t ->
    t

  val from_pyre1_api : Analysis.PyrePysaEnvironment.ReadOnly.t -> t

  val explicit_qualifiers : t -> Ast.Reference.t list

  val absolute_source_path_of_qualifier
    :  lookup_source:(ArtifactPath.t -> SourcePath.t option) ->
    t ->
    Ast.Reference.t ->
    string option

  val relative_path_of_qualifier : t -> Ast.Reference.t -> string option

  val source_of_qualifier : t -> Ast.Reference.t -> Ast.Source.t option

  val get_class_names_for_qualifier
    :  t ->
    exclude_test_modules:bool ->
    Ast.Reference.t ->
    Ast.Reference.t list

  val get_define_names_for_qualifier
    :  t ->
    exclude_test_modules:bool ->
    Ast.Reference.t ->
    Ast.Reference.t list

  val module_exists : t -> Ast.Reference.t -> bool

  val parse_annotation
    :  t ->
    ?validation:Analysis.AttributeResolution.type_validation_policy ->
    Ast.Expression.t ->
    Type.t

  val get_class_summary : t -> string -> Analysis.ClassSummary.t Ast.Node.t option

  val source_is_unit_test : t -> source:Ast.Source.t -> bool

  val class_immediate_parents : t -> string -> string list

  val parse_reference : t -> Ast.Reference.t -> Type.t

  val class_exists : t -> string -> bool

  val get_define_body : t -> Ast.Reference.t -> Ast.Statement.Define.t Ast.Node.t option

  val get_variable : t -> string -> Type.Variable.t option

  val resolve_define
    :  t ->
    callable_name:Ast.Reference.t option ->
    implementation:Ast.Statement.Define.Signature.t option ->
    overloads:Ast.Statement.Define.Signature.t list ->
    scoped_type_variables:Type.Variable.t Ast.Identifier.Map.t option ->
    Analysis.AttributeResolution.resolved_define

  val resolve_define_undecorated
    :  t ->
    callable_name:Ast.Reference.t option ->
    implementation:Ast.Statement.Define.Signature.t option ->
    overloads:Ast.Statement.Define.Signature.t list ->
    scoped_type_variables:Type.Variable.t Ast.Identifier.Map.t option ->
    Analysis.AnnotatedAttribute.decorated_method

  val global : t -> Ast.Reference.t -> Analysis.AttributeResolution.Global.t option

  val overrides : t -> string -> name:string -> Analysis.AnnotatedAttribute.instantiated option

  val annotation_parser : t -> Analysis.AnnotatedCallable.annotation_parser

  val typed_dictionary_field_names : t -> Type.t -> string list

  val less_or_equal : t -> left:Type.t -> right:Type.t -> bool

  val resolve_exports
    :  t ->
    ?from:Ast.Reference.t ->
    Ast.Reference.t ->
    Analysis.ResolvedReference.t option

  val successors : t -> string -> string list

  val location_of_global : t -> Ast.Reference.t -> Ast.Location.WithModule.t option

  val get_function_definition : t -> Ast.Reference.t -> Analysis.FunctionDefinition.t option

  val attribute_from_class_name
    :  t ->
    ?transitive:bool ->
    ?accessed_through_class:bool ->
    ?accessed_through_readonly:bool ->
    ?special_method:bool ->
    string ->
    name:string ->
    type_for_lookup:Type.t ->
    Analysis.AnnotatedAttribute.instantiated option

  val has_transitive_successor : t -> successor:string -> string -> bool

  val exists_matching_class_decorator
    :  t ->
    ?dependency:Analysis.SharedMemoryKeys.DependencyKey.registered ->
    names:string list ->
    Analysis.ClassSummary.t Ast.Node.t ->
    bool

  val generic_parameters_as_variables : t -> string -> Type.Variable.t list option

  val decorated_define : t -> Ast.Statement.Define.t Ast.Node.t -> Ast.Statement.Define.t Ast.Node.t

  val named_tuple_attributes : t -> string -> string list option

  val resolve_expression_to_type_info : t -> Ast.Expression.t -> Analysis.TypeInfo.Unit.t

  val get_unannotated_global
    :  t ->
    ?dependency:Analysis.SharedMemoryKeys.DependencyKey.registered ->
    Ast.Reference.t ->
    Analysis.Module.UnannotatedGlobal.t option

  val all_classes : t -> scheduler:Scheduler.t -> string list

  val all_unannotated_globals : t -> scheduler:Scheduler.t -> Ast.Reference.t list
end

module InContext : sig
  type t

  val create_at_global_scope : ReadOnly.t -> t

  val create_at_statement_key
    :  ReadOnly.t ->
    define_name:Ast.Reference.t ->
    define:Ast.Statement.Define.t Ast.Node.t ->
    statement_key:int ->
    t

  val pyre_api : t -> ReadOnly.t

  val is_global : t -> reference:Ast.Reference.t -> bool

  val resolve_reference : t -> Ast.Reference.t -> Type.t

  val resolve_assignment : t -> Ast.Statement.Assign.t -> t

  val resolve_expression_to_type : t -> Ast.Expression.t -> Type.t

  val resolve_attribute_access : t -> base_type:Type.t -> attribute:string -> Type.t

  val fallback_attribute
    :  t ->
    ?accessed_through_class:bool ->
    ?type_for_lookup:Type.t option ->
    name:string ->
    string ->
    Analysis.AnnotatedAttribute.instantiated option

  val resolve_generators : t -> Ast.Expression.Comprehension.Generator.t list -> t
end

module ModelQueries : sig
  val property_decorators : String.Set.t

  module Function = Analysis.PyrePysaEnvironment.ModelQueries.Function
  module Global = Analysis.PyrePysaEnvironment.ModelQueries.Global

  val resolve_qualified_name_to_global
    :  ReadOnly.t ->
    is_property_getter:bool ->
    is_property_setter:bool ->
    Ast.Reference.t ->
    Global.t option

  val class_method_signatures
    :  ReadOnly.t ->
    Ast.Reference.t ->
    (Ast.Reference.t * Ast.Statement.Define.Signature.t) list option

  val invalidate_cache : ReadOnly.t -> unit
end
