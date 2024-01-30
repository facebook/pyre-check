(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module ReadOnly : sig
  type t

  val controls : t -> EnvironmentControls.t

  val look_up_qualifier : t -> Ast.Reference.t -> SourceCodeApi.ModuleLookup.t

  val code_of_module_path : t -> Ast.ModulePath.t -> Parsing.LoadResult.t
end

module Overlay : sig
  type t

  val owns_qualifier : t -> Ast.Reference.t -> bool

  val update_overlaid_code
    :  t ->
    code_updates:SourceCodeIncrementalApi.Overlay.CodeUpdates.t ->
    SourceCodeIncrementalApi.UpdateResult.ModuleUpdate.t list

  val read_only : t -> ReadOnly.t
end

type t

val create : EnvironmentControls.t -> t

(* This function returns all SourcePaths that are tracked, including the shadowed ones. *)
val all_module_paths : t -> Ast.ModulePath.t list

val module_paths : t -> Ast.ModulePath.t list

val controls : t -> EnvironmentControls.t

val update
  :  t ->
  events:ArtifactPath.Event.t list ->
  SourceCodeIncrementalApi.UpdateResult.ModuleUpdate.t list

module Serializer : sig
  val store_layouts : t -> unit

  val from_stored_layouts : controls:EnvironmentControls.t -> unit -> t
end

val read_only : t -> ReadOnly.t

val overlay : t -> Overlay.t

module AssumeGlobalModuleListing : sig
  val global_module_paths_api : t -> GlobalModulePathsApi.t
end
