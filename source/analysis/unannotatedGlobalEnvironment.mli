(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Statement
open SharedMemoryKeys

module ResolvedReference : sig
  type export =
    | FromModuleGetattr
    | Exported of Module.Export.Name.t
  [@@deriving sexp, compare, hash]

  type t =
    | Module of Reference.t
    | ModuleAttribute of {
        from: Reference.t;
        name: Identifier.t;
        export: export;
        remaining: Identifier.t list;
      }
    | PlaceholderStub of {
        stub_module: Reference.t;
        remaining: Identifier.t list;
      }
  [@@deriving sexp, compare, hash]

  val as_module_toplevel_reference : t -> Reference.t option
end

module ReadOnly : sig
  type t

  val ast_environment : t -> AstEnvironment.ReadOnly.t

  val controls : t -> EnvironmentControls.t

  val unannotated_global_environment : t -> t

  (* These functions are not dependency tracked and should only be used:
   * - for testing
   * - to wipe all keys from environments
   * - (only all_defines_in_module) to help decide which defines need to be
   *   typechecked; we rely on both listing all defines in directly changed
   *   modules and on dependency tracking for indirect changes.
   *)

  val all_classes : t -> Type.Primitive.t list

  val all_indices : t -> IndexTracker.t list

  val all_unannotated_globals : t -> Reference.t list

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

  val contains_untracked : t -> ?dependency:DependencyKey.registered -> Type.t -> bool

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
end

module UpdateResult : sig
  (* This type is sealed to reify that Environment updates must follow and be based off of
     preenvironment updates *)
  type t

  val locally_triggered_dependencies : t -> DependencyKey.RegisteredSet.t

  val invalidated_modules : t -> Reference.t list

  val module_updates : t -> ModuleTracker.IncrementalUpdate.t list

  val all_triggered_dependencies : t -> DependencyKey.RegisteredSet.t list

  val unannotated_global_environment_update_result : t -> t
end

type t

val create : EnvironmentControls.t -> t

val create_for_testing : EnvironmentControls.t -> (Ast.ModulePath.t * string) list -> t

val ast_environment : t -> AstEnvironment.t

val controls : t -> EnvironmentControls.t

val read_only : t -> ReadOnly.t

val update_this_and_all_preceding_environments
  :  t ->
  scheduler:Scheduler.t ->
  ArtifactPath.t list ->
  UpdateResult.t

val store : t -> unit

val load : EnvironmentControls.t -> t

module Overlay : sig
  type t

  val create : ReadOnly.t -> t

  val module_tracker : t -> ModuleTracker.Overlay.t

  val update_overlaid_code
    :  t ->
    code_updates:(ArtifactPath.t * ModuleTracker.Overlay.CodeUpdate.t) list ->
    UpdateResult.t

  val propagate_parent_update : t -> UpdateResult.t -> UpdateResult.t

  val read_only : t -> ReadOnly.t
end
