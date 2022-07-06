(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type raw_code = string

type message = string

module IncrementalUpdate : sig
  type t =
    | NewExplicit of Ast.ModulePath.t
    | NewImplicit of Ast.Reference.t
    | Delete of Ast.Reference.t
  [@@deriving show, sexp, compare, eq]
end

module PathLookup : sig
  type t =
    | Found of Ast.ModulePath.t
    | ShadowedBy of Ast.ModulePath.t
    | NotFound
  [@@deriving show, sexp, compare]
end

module ReadOnly : sig
  type t

  val controls : t -> EnvironmentControls.t

  val lookup_module_path : t -> Ast.Reference.t -> Ast.ModulePath.t option

  val lookup_path : t -> ArtifactPath.t -> PathLookup.t

  val module_paths : t -> Ast.ModulePath.t list

  val project_qualifiers : t -> Ast.Reference.t list

  (* This function returns all explicit modules (i.e. those backed up by a source path) that are
     tracked *)
  val tracked_explicit_modules : t -> Ast.Reference.t list

  val is_module_tracked : t -> Ast.Reference.t -> bool

  (* Either `Ok (raw_code)` or `Error (message)` *)
  val get_raw_code : t -> Ast.ModulePath.t -> (raw_code, message) Result.t
end

type t

val create : EnvironmentControls.t -> t

val create_for_testing : EnvironmentControls.t -> (Ast.ModulePath.t * raw_code) list -> t

(* This function returns all SourcePaths that are tracked, including the shadowed ones. *)
val all_module_paths : t -> Ast.ModulePath.t list

val module_paths : t -> Ast.ModulePath.t list

val controls : t -> EnvironmentControls.t

val update : t -> artifact_paths:ArtifactPath.t list -> IncrementalUpdate.t list

module Serializer : sig
  val store_layouts : t -> unit

  val from_stored_layouts : controls:EnvironmentControls.t -> unit -> t
end

val read_only : t -> ReadOnly.t

module Overlay : sig
  module CodeUpdate : sig
    type t =
      | NewCode of raw_code
      | ResetCode
  end

  type t

  val create : ReadOnly.t -> t

  val owns_qualifier : t -> Ast.Reference.t -> bool

  val owns_reference : t -> Ast.Reference.t -> bool

  val owns_identifier : t -> Ast.Identifier.t -> bool

  val update_overlaid_code
    :  t ->
    code_updates:(ArtifactPath.t * CodeUpdate.t) list ->
    IncrementalUpdate.t list

  val read_only : t -> ReadOnly.t
end
