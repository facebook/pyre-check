(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Statement
open Analysis


type t = (Callable.t list) Callable.Map.t
type callgraph = (Callable.t list) Callable.RealMap.t
(* Maps method names to closest sub-types that override them next *)
type overrides = (Access.t list) Access.Map.t

val empty: t
val empty_callgraph: callgraph
val empty_overrides: overrides

(** Returns a partition of nodes for strongly connected components in the dependency graph *)
val partition: edges: t -> (Callable.t list) list

(** Reverse edges in the dependency graph *)
val reverse: t -> t

val pp: Format.formatter -> t -> unit
val pp_partitions: Format.formatter -> (Callable.t list) list -> unit

val dump: t -> configuration: Configuration.Analysis.t -> unit

val from_overrides: overrides -> t
val from_callgraph: callgraph -> t

val create_callgraph:
  environment: (module Environment.Handler)
  -> source: Source.t
  -> callgraph

val create_overrides:
  environment: (module Environment.Handler)
  -> source: Source.t
  -> overrides

val union: t -> t -> t

val expand_callees: Callable.t list -> Callable.non_override_target list
