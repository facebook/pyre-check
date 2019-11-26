(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast

type t = {
  full: int;
  partial: int;
  untyped: int;
  crashes: int;
}
[@@deriving eq, show]

val create : ?full:int -> ?partial:int -> ?untyped:int -> ?crashes:int -> unit -> t

val full : t -> int

val partial : t -> int

val untyped : t -> int

val crashes : t -> int

val sum : t -> t -> t

val aggregate_over_types : Type.t list -> t

val aggregate : t list -> t

type aggregate = {
  strict_coverage: int;
  declare_coverage: int;
  default_coverage: int;
  source_files: int;
}

val coverage
  :  configuration:Configuration.Analysis.t ->
  ast_environment:AstEnvironment.ReadOnly.t ->
  Reference.t list ->
  aggregate
