(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module ErrorsEnvironmentReadOnly : sig
  include Environment.ReadOnly

  val type_environment : t -> TypeEnvironment.ReadOnly.t

  val ast_environment : t -> AstEnvironment.ReadOnly.t

  val module_tracker : t -> ModuleTracker.ReadOnly.t

  val controls : t -> EnvironmentControls.t

  val project_qualifiers : t -> Ast.Reference.t list

  val get_errors_for_qualifier : t -> Ast.Reference.t -> AnalysisError.t list

  val get_errors_for_qualifiers : t -> Ast.Reference.t list -> AnalysisError.t list

  (* Get all errors for in-project modules; use this to grab errors that are already computed *)
  val get_all_errors : t -> AnalysisError.t list
end

include
  Environment.S
    with module ReadOnly = ErrorsEnvironmentReadOnly
     and module PreviousEnvironment = TypeEnvironment

val type_environment : t -> TypeEnvironment.t

val module_tracker : t -> ModuleTracker.t

val populate_for_modules : scheduler:Scheduler.t -> t -> Ast.Reference.t list -> unit

val project_qualifiers : t -> Ast.Reference.t list

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

    val annotated_global_environment : ReadOnly.t -> AnnotatedGlobalEnvironment.ReadOnly.t

    val attribute_resolution : ReadOnly.t -> AttributeResolution.ReadOnly.t

    val class_metadata_environment : ReadOnly.t -> ClassMetadataEnvironment.ReadOnly.t

    val class_hierarchy_environment : ReadOnly.t -> ClassHierarchyEnvironment.ReadOnly.t

    val alias_environment : ReadOnly.t -> AliasEnvironment.ReadOnly.t

    val empty_stub_environment : ReadOnly.t -> EmptyStubEnvironment.ReadOnly.t

    val unannotated_global_environment : ReadOnly.t -> UnannotatedGlobalEnvironment.ReadOnly.t
  end

  module UpdateResult : sig
    val errors_environment : UpdateResult.t -> UpdateResult.t

    val type_environment : UpdateResult.t -> TypeEnvironment.UpdateResult.t

    val annotated_global_environment : UpdateResult.t -> AnnotatedGlobalEnvironment.UpdateResult.t

    val attribute_resolution : UpdateResult.t -> AttributeResolution.UpdateResult.t

    val class_metadata_environment : UpdateResult.t -> ClassMetadataEnvironment.UpdateResult.t

    val class_hierarchy_environment : UpdateResult.t -> ClassHierarchyEnvironment.UpdateResult.t

    val alias_environment : UpdateResult.t -> AliasEnvironment.UpdateResult.t

    val empty_stub_environment : UpdateResult.t -> EmptyStubEnvironment.UpdateResult.t

    val unannotated_global_environment
      :  UpdateResult.t ->
      UnannotatedGlobalEnvironment.UpdateResult.t
  end
end

val check_and_preprocess : scheduler:Scheduler.t -> t -> Ast.Reference.t list -> unit
