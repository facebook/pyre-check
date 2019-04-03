(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast


type t = {
  full: int;
  partial: int;
  untyped: int;
  ignore: int;
  crashes: int;
}
[@@deriving show]

val create
  :  ?full: int
  -> ?partial: int
  -> ?untyped: int
  -> ?ignore: int
  -> ?crashes: int
  -> unit
  -> t

val full: t -> int
val partial: t -> int
val untyped: t -> int
val ignore: t -> int
val crashes: t -> int

val sum: t -> t -> t
val aggregate: Annotation.t list -> t
val aggregate_over_source: source: Source.t -> t list -> t

val log: t -> total_errors: int -> path: string -> unit

val add: t -> handle: File.Handle.t -> unit
val get: handle: File.Handle.t -> t option

type aggregate = {
  strict_coverage: int;
  declare_coverage: int;
  default_coverage: int;
  source_files: int;
}

val coverage: number_of_files: int -> sources: File.Handle.t list -> aggregate
