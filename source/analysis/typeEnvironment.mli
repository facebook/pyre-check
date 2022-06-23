(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
module Error = AnalysisError

module TypeEnvironmentReadOnly : sig
  type t

  val controls : t -> EnvironmentControls.t

  val global_environment : t -> AnnotatedGlobalEnvironment.ReadOnly.t

  val global_resolution : t -> GlobalResolution.t

  val ast_environment : t -> AstEnvironment.ReadOnly.t

  val module_tracker : t -> ModuleTracker.ReadOnly.t

  val unannotated_global_environment : t -> UnannotatedGlobalEnvironment.ReadOnly.t

  val get_errors
    :  t ->
    ?dependency:SharedMemoryKeys.DependencyKey.registered ->
    Reference.t ->
    Error.t list

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

val ast_environment : t -> AstEnvironment.t

val module_tracker : t -> ModuleTracker.t

val populate_for_definitions : scheduler:Scheduler.t -> t -> Ast.Reference.t list -> unit

val populate_for_modules : scheduler:Scheduler.t -> t -> Ast.Reference.t list -> unit
