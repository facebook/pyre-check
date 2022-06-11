(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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

  val configuration : t -> Configuration.Analysis.t

  val lookup_source_path : t -> Ast.Reference.t -> Ast.ModulePath.t option

  val lookup_path : t -> ArtifactPath.t -> PathLookup.t

  val source_paths : t -> Ast.ModulePath.t list

  val project_qualifiers : t -> Ast.Reference.t list

  (* This function returns all explicit modules (i.e. those backed up by a source path) that are
     tracked *)
  val tracked_explicit_modules : t -> Ast.Reference.t list

  val is_module_tracked : t -> Ast.Reference.t -> bool

  (* Either `Ok (raw_code)` or `Error (message)` *)
  val get_raw_code : t -> Ast.ModulePath.t -> (string, string) Result.t
end

type t

val create : Configuration.Analysis.t -> t

val create_for_testing : Configuration.Analysis.t -> (Ast.ModulePath.t * string) list -> t

(* This function returns all SourcePaths that are tracked, including the shadowed ones. *)
val all_source_paths : t -> Ast.ModulePath.t list

val source_paths : t -> Ast.ModulePath.t list

val configuration : t -> Configuration.Analysis.t

val update : t -> artifact_paths:ArtifactPath.t list -> IncrementalUpdate.t list

module Serializer : sig
  val store_layouts : t -> unit

  val from_stored_layouts : configuration:Configuration.Analysis.t -> unit -> t
end

val read_only : t -> ReadOnly.t

module Overlay : sig
  type t

  val create : ReadOnly.t -> t

  val owns_qualifier : t -> Ast.Reference.t -> bool

  val update_overlaid_code
    :  t ->
    code_updates:(ArtifactPath.t * string) list ->
    IncrementalUpdate.t list

  val read_only : t -> ReadOnly.t
end
