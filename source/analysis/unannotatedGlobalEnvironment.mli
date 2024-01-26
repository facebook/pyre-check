(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Statement
open SharedMemoryKeys

module ReadOnly : sig
  type t

  val get_tracked_source_code_api
    :  t ->
    dependency:SharedMemoryKeys.DependencyKey.registered ->
    SourceCodeApi.t

  val get_untracked_source_code_api : t -> SourceCodeApi.t

  val controls : t -> EnvironmentControls.t

  val unannotated_global_environment : t -> t

  (* All other functions are dependency tracked *)

  val get_module_metadata
    :  t ->
    ?dependency:DependencyKey.registered ->
    Reference.t ->
    Module.t option

  val get_define_names
    :  t ->
    ?dependency:DependencyKey.registered ->
    Reference.t ->
    Reference.t list

  val get_class_summary
    :  t ->
    ?dependency:DependencyKey.registered ->
    string ->
    ClassSummary.t Node.t option

  val class_exists : t -> ?dependency:DependencyKey.registered -> string -> bool

  val module_exists : t -> ?dependency:DependencyKey.registered -> Reference.t -> bool

  val get_function_definition
    :  t ->
    ?dependency:DependencyKey.registered ->
    Reference.t ->
    FunctionDefinition.t option

  val get_define_body
    :  t ->
    ?dependency:DependencyKey.registered ->
    Reference.t ->
    Define.t Node.t option

  val get_unannotated_global
    :  t ->
    ?dependency:DependencyKey.registered ->
    Reference.t ->
    UnannotatedGlobal.t option

  val is_protocol : t -> ?dependency:DependencyKey.registered -> Type.t -> bool

  val legacy_resolve_exports
    :  t ->
    ?dependency:DependencyKey.registered ->
    Reference.t ->
    Reference.t

  val resolve_exports
    :  t ->
    ?dependency:DependencyKey.registered ->
    ?from:Reference.t ->
    Reference.t ->
    ResolvedReference.t option

  val first_matching_class_decorator
    :  t ->
    ?dependency:SharedMemoryKeys.DependencyKey.registered ->
    names:string list ->
    ClassSummary.t Node.t ->
    Ast.Statement.Decorator.t option

  val exists_matching_class_decorator
    :  t ->
    ?dependency:SharedMemoryKeys.DependencyKey.registered ->
    names:string list ->
    ClassSummary.t Node.t ->
    bool

  (* These functions are not dependency tracked and should only be used:
   * - in bulk queries (e.g. to help with Pysa analysis)
   * - for debugging and testing
   *
   * They cannot be used in some contexts, e.g. a lazy environment for
   * powering IDEs.
   *)
  module GlobalApis : sig
    val all_classes : t -> global_module_paths_api:GlobalModulePathsApi.t -> Type.Primitive.t list

    val all_unannotated_globals
      :  t ->
      global_module_paths_api:GlobalModulePathsApi.t ->
      Reference.t list
  end
end

module UpdateResult : sig
  (* This type is sealed to reify that Environment updates must follow and be based off of
     preenvironment updates *)
  type t

  val locally_triggered_dependencies : t -> DependencyKey.RegisteredSet.t

  val invalidated_modules : t -> Reference.t list

  val module_updates : t -> SourceCodeIncrementalApi.UpdateResult.ModuleUpdate.t list

  val all_triggered_dependencies : t -> DependencyKey.RegisteredSet.t list

  val unannotated_global_environment_update_result : t -> t
end

type t

val create : EnvironmentControls.t -> t

(* This handle to self is needed to fulfill the (recursive) interface used in the Environment.ml
   functor *)
val unannotated_global_environment : t -> t

val global_module_paths_api : t -> GlobalModulePathsApi.t

val ast_environment : t -> AstEnvironment.t

val controls : t -> EnvironmentControls.t

val read_only : t -> ReadOnly.t

val update_this_and_all_preceding_environments
  :  t ->
  scheduler:Scheduler.t ->
  ArtifactPath.Event.t list ->
  UpdateResult.t

val store : t -> unit

val load : EnvironmentControls.t -> t

module Overlay : sig
  type t

  val create : ReadOnly.t -> t

  (* This handle to self is needed to fulfill the recursive interface of the Environment functor *)
  val unannotated_global_environment : t -> t

  val owns_qualifier : t -> Ast.Reference.t -> bool

  val owns_reference : t -> Ast.Reference.t -> bool

  val owns_identifier : t -> Ast.Identifier.t -> bool

  val update_overlaid_code
    :  t ->
    code_updates:SourceCodeIncrementalApi.Overlay.CodeUpdates.t ->
    UpdateResult.t

  val propagate_parent_update : t -> UpdateResult.t -> UpdateResult.t

  val read_only : t -> ReadOnly.t
end
