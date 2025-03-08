(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open SharedMemoryKeys

module ReadOnly : sig
  type t

  val get_tracked_source_code_api
    :  t ->
    dependency:SharedMemoryKeys.DependencyKey.registered ->
    SourceCodeApi.t

  val get_untracked_source_code_api : t -> SourceCodeApi.t

  val controls : t -> EnvironmentControls.t

  val source_code_read_only : t -> SourceCodeIncrementalApi.ReadOnly.t

  (* All other functions are dependency tracked *)

  val get_module_metadata
    :  t ->
    ?dependency:DependencyKey.registered ->
    Reference.t ->
    Module.Metadata.t option

  val get_class_summary
    :  t ->
    ?dependency:DependencyKey.registered ->
    string ->
    ClassSummary.t Node.t option

  val class_exists : t -> ?dependency:DependencyKey.registered -> string -> bool

  val module_exists : t -> ?dependency:DependencyKey.registered -> Reference.t -> bool

  val get_unannotated_global
    :  t ->
    ?dependency:DependencyKey.registered ->
    Reference.t ->
    Module.UnannotatedGlobal.t option

  val is_protocol : t -> ?dependency:DependencyKey.registered -> Type.t -> bool

  val is_special_form : t -> ?dependency:DependencyKey.registered -> Type.t -> bool

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
    val all_classes
      :  t ->
      scheduler:Scheduler.t ->
      global_module_paths_api:GlobalModulePathsApi.t ->
      Type.Primitive.t list

    val all_unannotated_globals
      :  t ->
      scheduler:Scheduler.t ->
      global_module_paths_api:GlobalModulePathsApi.t ->
      Reference.t list
  end
end

module UpdateResult : Environment.UpdateResult.S

module Overlay : sig
  type t

  (* This handle to self is needed to fulfill the recursive interface of the Environment functor *)
  val source_code_overlay : t -> SourceCodeIncrementalApi.Overlay.t

  val update_overlaid_code
    :  t ->
    code_updates:SourceCodeIncrementalApi.Overlay.CodeUpdates.t ->
    UpdateResult.t

  val propagate_parent_update : t -> UpdateResult.t -> UpdateResult.t

  val read_only : t -> ReadOnly.t
end

type t

val create : SourceCodeEnvironment.t -> t

(* This handle to self is needed to fulfill the (recursive) interface used in the Environment.ml
   functor *)
val source_code_base : t -> SourceCodeIncrementalApi.Base.t

val controls : t -> EnvironmentControls.t

val read_only : t -> ReadOnly.t

val overlay : t -> Overlay.t

val update_this_and_all_preceding_environments
  :  t ->
  scheduler:Scheduler.t ->
  ArtifactPath.Event.t list ->
  UpdateResult.t

module AssumeGlobalModuleListing : sig
  val global_module_paths_api : t -> GlobalModulePathsApi.t
end

module AssumeAstEnvironment : sig
  val ast_environment : t -> AstEnvironment.t

  val store : t -> unit

  val load : EnvironmentControls.t -> t
end
