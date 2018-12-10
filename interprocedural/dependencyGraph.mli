(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Analysis


type t = (Callable.t list) Callable.Map.t

val empty: t

val create:
  environment: (module Environment.Handler)
  -> source: Source.t
  -> t

(** Returns a partition of nodes for strongly connected components in the call graph *)
val partition: edges: t -> (Callable.t list) list

(** Reverse edges in the call graph *)
val reverse: t -> t

val pp: Format.formatter -> t -> unit
val pp_partitions: Format.formatter -> (Callable.t list) list -> unit

val dump: t -> configuration: Configuration.Analysis.t -> unit
