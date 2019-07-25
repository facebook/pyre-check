(** Copyright (c) 2019-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the LICENSE file in the root
    directory of this source tree. *)

open Pyre

module IncrementalUpdate : sig
  type t =
    | New of Ast.SourcePath.t
    | Delete of Ast.Reference.t
  [@@deriving sexp, compare, eq]
end

type t

val create : Configuration.Analysis.t -> t

val lookup : t -> Ast.Reference.t -> Ast.SourcePath.t option

val lookup_path : configuration:Configuration.Analysis.t -> t -> Path.t -> Ast.SourcePath.t option

val mem : t -> Ast.Reference.t -> bool

val source_paths : t -> Ast.SourcePath.t list

(* This function returns all SourcePaths that are tracked, including the shadowed ones *)
val all_source_paths : t -> Ast.SourcePath.t list

val qualifiers : t -> Ast.Reference.t list

val length : t -> int

val update
  :  configuration:Configuration.Analysis.t ->
  paths:Path.t list ->
  t ->
  IncrementalUpdate.t list

module SharedMemory : sig
  val store : t -> unit

  val load : unit -> t
end
