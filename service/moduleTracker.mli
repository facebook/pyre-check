(** Copyright (c) 2019-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the LICENSE file in the root
    directory of this source tree. *)

open Pyre

module SourceFile : sig
  type t = private {
    relative_path: Path.RelativePath.t;
    priority: int;
    is_stub: bool;
    is_external: bool;
    is_init: bool
  }
  [@@deriving sexp, compare, eq]

  val pp : Format.formatter -> t -> unit

  val create : configuration:Configuration.Analysis.t -> Path.t -> t option

  val qualifier : t -> Ast.Reference.t

  (* Expose for testing *)
  val same_module_compare : t -> t -> int
end

module IncrementalUpdate : sig
  type t =
    | New of SourceFile.t
    | Delete of Ast.Reference.t
  [@@deriving sexp, compare, eq]
end

type t

val create : Configuration.Analysis.t -> t

val lookup : t -> Ast.Reference.t -> SourceFile.t option

val source_files : t -> SourceFile.t list

val source_file_paths : t -> Path.t list

val update
  :  configuration:Configuration.Analysis.t ->
  paths:Path.t list ->
  t ->
  IncrementalUpdate.t list

module SharedMemory : sig
  val store : t -> unit

  val load : unit -> t
end
