(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module IncrementalUpdate : sig
  type t =
    | NewExplicit of Ast.SourcePath.t
    | NewImplicit of Ast.Reference.t
    | Delete of Ast.Reference.t
  [@@deriving show, sexp, compare, eq]
end

module PathLookup : sig
  type t =
    | Found of Ast.SourcePath.t
    | ShadowedBy of Ast.SourcePath.t
    | NotFound
  [@@deriving show, sexp, compare]
end

type t

val create : Configuration.Analysis.t -> t

val create_for_testing : Configuration.Analysis.t -> (Ast.SourcePath.t * string) list -> t

(* This function returns all SourcePaths that are tracked, including the shadowed ones. *)
val all_source_paths : t -> Ast.SourcePath.t list

val source_paths : t -> Ast.SourcePath.t list

val configuration : t -> Configuration.Analysis.t

val update : paths:PyrePath.Built.t list -> t -> IncrementalUpdate.t list

module Serializer : sig
  val store_layouts : t -> unit

  val from_stored_layouts : configuration:Configuration.Analysis.t -> unit -> t
end

module ReadOnly : sig
  type t

  val configuration : t -> Configuration.Analysis.t

  val lookup_source_path : t -> Ast.Reference.t -> Ast.SourcePath.t option

  val lookup_path : t -> PyrePath.Built.t -> PathLookup.t

  val source_paths : t -> Ast.SourcePath.t list

  val project_qualifiers : t -> Ast.Reference.t list

  (* This function returns all explicit modules (i.e. those backed up by a source path) that are
     tracked *)
  val tracked_explicit_modules : t -> Ast.Reference.t list

  val is_module_tracked : t -> Ast.Reference.t -> bool

  (* Either `Ok (raw_code)` or `Error (message)` *)
  val get_raw_code : t -> Ast.SourcePath.t -> (string, string) Result.t
end

val read_only : t -> ReadOnly.t
