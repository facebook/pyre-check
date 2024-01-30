(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

module TypeEnvironmentReadOnly : sig
  type t

  val controls : t -> EnvironmentControls.t

  val global_environment : t -> AnnotatedGlobalEnvironment.ReadOnly.t

  val global_resolution : t -> GlobalResolution.t

  val unannotated_global_environment : t -> UnannotatedGlobalEnvironment.ReadOnly.t

  val get_tracked_source_code_api
    :  t ->
    dependency:SharedMemoryKeys.DependencyKey.registered ->
    SourceCodeApi.t

  val get_untracked_source_code_api : t -> SourceCodeApi.t

  val get_errors
    :  t ->
    ?dependency:SharedMemoryKeys.DependencyKey.registered ->
    Reference.t ->
    AnalysisError.t list

  val get_local_annotations
    :  t ->
    ?dependency:SharedMemoryKeys.DependencyKey.registered ->
    Reference.t ->
    LocalAnnotationMap.ReadOnly.t option

  val get_or_recompute_local_annotations : t -> Reference.t -> LocalAnnotationMap.ReadOnly.t option
end

include
  Environment.S
    with module ReadOnly = TypeEnvironmentReadOnly
     and module PreviousEnvironment = AnnotatedGlobalEnvironment

val global_environment : t -> AnnotatedGlobalEnvironment.t

val collect_definitions : scheduler:Scheduler.t -> t -> Ast.Reference.t list -> Ast.Reference.t list

val populate_for_definitions : scheduler:Scheduler.t -> t -> Ast.Reference.t list -> unit

val populate_for_modules : scheduler:Scheduler.t -> t -> Ast.Reference.t list -> unit

module AssumeGlobalModuleListing : sig
  val global_module_paths_api : t -> GlobalModulePathsApi.t
end

module AssumeAstEnvironment : sig
  val store : t -> unit

  val load : EnvironmentControls.t -> t

  val store_without_dependency_keys : t -> unit

  val load_without_dependency_keys : EnvironmentControls.t -> t
end
