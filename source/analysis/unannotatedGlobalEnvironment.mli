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
end

module ReadOnly : sig
  type t

  val ast_environment : t -> AstEnvironment.ReadOnly.t

  val class_exists : t -> ?dependency:DependencyKey.registered -> string -> bool

  val contains_untracked : t -> ?dependency:DependencyKey.registered -> Type.t -> bool

  val unannotated_global_environment : t -> t

  (* APIs that start with prefix `all_` are not dependency tracked and are for testing purpose only.
     DO NOT USE THEM IN PROD. *)

  val all_classes : t -> Type.Primitive.t list

  val all_indices : t -> IndexTracker.t list

  val all_unannotated_globals : t -> Reference.t list

  val all_defines : t -> Reference.t list

  val all_defines_in_module : t -> Reference.t -> Reference.t list

  val get_class_definition
    :  t ->
    ?dependency:DependencyKey.registered ->
    string ->
    ClassSummary.t Node.t option

  val get_unannotated_global
    :  t ->
    ?dependency:DependencyKey.registered ->
    Reference.t ->
    UnannotatedGlobal.t option

  val get_define
    :  t ->
    ?dependency:DependencyKey.registered ->
    Reference.t ->
    FunctionDefinition.t option

  val get_define_body
    :  t ->
    ?dependency:DependencyKey.registered ->
    Reference.t ->
    Define.t Node.t option

  val is_protocol : t -> ?dependency:DependencyKey.registered -> Type.t -> bool

  val get_module_metadata
    :  t ->
    ?dependency:DependencyKey.registered ->
    Reference.t ->
    Module.t option

  val module_exists : t -> ?dependency:DependencyKey.registered -> Reference.t -> bool

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

  val resolve_decorator_if_matches
    :  t ->
    ?dependency:SharedMemoryKeys.DependencyKey.registered ->
    Ast.Expression.t ->
    target:string ->
    Ast.Statement.Decorator.t option

  val get_decorator
    :  t ->
    ?dependency:SharedMemoryKeys.DependencyKey.registered ->
    ClassSummary.t Node.t ->
    decorator:string ->
    Ast.Statement.Decorator.t list
end

module UpdateResult : sig
  (* This type is sealed to reify that Environment updates must follow and be based off of
     preenvironment updates *)
  type t

  type read_only = ReadOnly.t

  val previous_unannotated_globals : t -> Reference.Set.t

  val previous_classes : t -> Type.Primitive.Set.t

  val previous_defines : t -> Reference.Set.t

  val define_additions : t -> Reference.Set.t

  val locally_triggered_dependencies : t -> DependencyKey.RegisteredSet.t

  val upstream : t -> AstEnvironment.UpdateResult.t

  val all_triggered_dependencies : t -> DependencyKey.RegisteredSet.t list

  val unannotated_global_environment_update_result : t -> t

  val ast_environment_update_result : t -> AstEnvironment.UpdateResult.t

  val read_only : t -> read_only
end

type t

val create : AstEnvironment.t -> t

val ast_environment : t -> AstEnvironment.t

val read_only : t -> ReadOnly.t

val update_this_and_all_preceding_environments
  :  t ->
  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  AstEnvironment.trigger ->
  UpdateResult.t
