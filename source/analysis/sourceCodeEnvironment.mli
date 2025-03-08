(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module ReadOnly : sig
  type t

  val source_code_read_only : t -> SourceCodeIncrementalApi.ReadOnly.t

  val controls : t -> EnvironmentControls.t
end

module UpdateResult : sig
  type t

  val locally_triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.RegisteredSet.t

  val all_triggered_dependencies : t -> SharedMemoryKeys.DependencyKey.RegisteredSet.t list

  val source_code_update_result : t -> SourceCodeIncrementalApi.UpdateResult.t

  val invalidated_modules : t -> Ast.Reference.t list

  val module_updates : t -> SourceCodeIncrementalApi.UpdateResult.ModuleUpdate.t list

  val modules_with_invalidated_type_check : t -> Ast.Reference.Set.t
end

module Overlay : sig
  type t

  val source_code_overlay : t -> SourceCodeIncrementalApi.Overlay.t

  val update_overlaid_code
    :  t ->
    code_updates:SourceCodeIncrementalApi.Overlay.CodeUpdates.t ->
    UpdateResult.t

  val propagate_parent_update : t -> UpdateResult.t -> UpdateResult.t

  val filter_update : t -> UpdateResult.t -> UpdateResult.t

  val read_only : t -> ReadOnly.t

  val owns_qualifier : t -> Ast.Reference.t -> bool

  val owns_reference : t -> Ast.Reference.t -> bool

  val owns_identifier : t -> Ast.Identifier.t -> bool
end

type t

val create : t -> t

val source_code_base : t -> SourceCodeIncrementalApi.Base.t

val read_only : t -> ReadOnly.t

val overlay : t -> Overlay.t

val update_this_and_all_preceding_environments
  :  t ->
  scheduler:Scheduler.t ->
  ArtifactPath.Event.t list ->
  UpdateResult.t

val of_ast_environment : AstEnvironment.t -> t

val of_source_code_base : SourceCodeIncrementalApi.Base.t -> t

module AssumeAstEnvironment : sig
  val ast_environment : t -> AstEnvironment.t

  val store : t -> unit

  val load : EnvironmentControls.t -> t
end
