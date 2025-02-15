(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open SharedMemoryKeys

module HierarchyReadOnly : sig
  include Environment.ReadOnly

  val get_edges
    :  t ->
    ?dependency:DependencyKey.registered ->
    Ast.Identifier.t ->
    ClassHierarchy.Edges.t option

  val alias_environment : t -> TypeAliasEnvironment.ReadOnly.t

  val unannotated_global_environment : t -> UnannotatedGlobalEnvironment.ReadOnly.t

  val class_hierarchy : ?dependency:DependencyKey.registered -> t -> (module ClassHierarchy.Handler)

  val generic_parameters
    :  t ->
    ?dependency:DependencyKey.registered ->
    ?empty_for_nongeneric:bool ->
    Type.Primitive.t ->
    Type.GenericParameter.t list option

  val generic_parameters_as_variables
    :  t ->
    ?dependency:DependencyKey.registered ->
    ?empty_for_nongeneric:bool ->
    Type.Primitive.t ->
    Type.Variable.t list option

  (* This function is not used in production, but in the past it has been useful to run it after
     incremental updates when debugging bugs in incremental logic *)
  val check_integrity
    :  t ->
    scheduler:Scheduler.t ->
    global_module_paths_api:GlobalModulePathsApi.t ->
    (unit, ClassHierarchy.CheckIntegrityError.t) result
end

include
  Environment.S
    with module ReadOnly = HierarchyReadOnly
     and module PreviousEnvironment = TypeAliasEnvironment

(* Exposed for testing purpose only *)
val compute_generic_base : Type.t list -> Type.t option
