(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module ErrorsEnvironmentReadOnly : sig
  include Environment.ReadOnly

  val type_environment : t -> TypeEnvironment.ReadOnly.t

  val get_untracked_source_code_api : t -> SourceCodeApi.t

  val controls : t -> EnvironmentControls.t

  val get_errors_for_qualifier : t -> Ast.Reference.t -> AnalysisError.t list

  val get_errors_for_qualifiers : t -> Ast.Reference.t list -> AnalysisError.t list
end

include
  Environment.S
    with module ReadOnly = ErrorsEnvironmentReadOnly
     and module PreviousEnvironment = TypeEnvironment

module AssumeDownstreamNeverNeedsUpdates : sig
  val type_environment : t -> TypeEnvironment.t

  val class_metadata_environment : t -> ClassSuccessorMetadataEnvironment.t

  val unannotated_global_environment : t -> UnannotatedGlobalEnvironment.t
end

module AssumeGlobalModuleListing : sig
  val global_module_paths_api : t -> GlobalModulePathsApi.t
end

val populate_for_modules : scheduler:Scheduler.t -> t -> Ast.Reference.t list -> unit

module UpdateStatistics : sig
  type t = {
    module_updates_count: int;
    invalidated_modules_count: int;
    rechecked_modules_count: int;
    rechecked_functions_count: int;
  }

  val count_updates : UpdateResult.t -> t
end

module Testing : sig
  module ReadOnly : sig
    val errors_environment : ReadOnly.t -> ReadOnly.t

    val type_environment : ReadOnly.t -> TypeEnvironment.ReadOnly.t

    val function_definition_environment : ReadOnly.t -> FunctionDefinitionEnvironment.ReadOnly.t

    val annotated_global_environment : ReadOnly.t -> AnnotatedGlobalEnvironment.ReadOnly.t

    val attribute_resolution : ReadOnly.t -> AttributeResolution.ReadOnly.t

    val class_metadata_environment : ReadOnly.t -> ClassSuccessorMetadataEnvironment.ReadOnly.t

    val class_hierarchy_environment : ReadOnly.t -> ClassHierarchyEnvironment.ReadOnly.t

    val alias_environment : ReadOnly.t -> TypeAliasEnvironment.ReadOnly.t

    val empty_stub_environment : ReadOnly.t -> EmptyStubEnvironment.ReadOnly.t

    val unannotated_global_environment : ReadOnly.t -> UnannotatedGlobalEnvironment.ReadOnly.t
  end

  module UpdateResult : sig
    val errors_environment : UpdateResult.t -> UpdateResult.t

    val type_environment : UpdateResult.t -> TypeEnvironment.UpdateResult.t

    val function_definition_environment
      :  UpdateResult.t ->
      FunctionDefinitionEnvironment.UpdateResult.t

    val annotated_global_environment : UpdateResult.t -> AnnotatedGlobalEnvironment.UpdateResult.t

    val attribute_resolution : UpdateResult.t -> AttributeResolution.UpdateResult.t

    val class_metadata_environment
      :  UpdateResult.t ->
      ClassSuccessorMetadataEnvironment.UpdateResult.t

    val class_hierarchy_environment : UpdateResult.t -> ClassHierarchyEnvironment.UpdateResult.t

    val alias_environment : UpdateResult.t -> TypeAliasEnvironment.UpdateResult.t

    val empty_stub_environment : UpdateResult.t -> EmptyStubEnvironment.UpdateResult.t

    val unannotated_global_environment
      :  UpdateResult.t ->
      UnannotatedGlobalEnvironment.UpdateResult.t
  end
end

val check_and_postprocess
  :  scheduler:Scheduler.t ->
  scheduler_policies:Configuration.SchedulerPolicies.t ->
  t ->
  Ast.Reference.t list ->
  unit

(* Convenience function to create an AstEnvironment.t and use that for the
   UnannotatedGlobalEnvironment.CreateHandle.t *)
val create_with_ast_environment : EnvironmentControls.t -> t
