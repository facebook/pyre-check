(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Analysis

type t = Target.t list Target.Map.t

type callgraph = Target.t list Target.Map.t

module CallGraphSharedMemory : sig
  val store : Target.t list Target.Map.Tree.t -> unit

  val load : unit -> Target.t list Target.Map.Tree.t
end

(* Maps method names to closest sub-types that override them next *)
type overrides = Reference.t list Reference.Map.t

module OverridesSharedMemory : sig
  val store : Reference.t list Reference.Map.Tree.t -> unit

  val load : unit -> Reference.t list Reference.Map.Tree.t
end

val empty : t

val empty_callgraph : callgraph

val empty_overrides : overrides

val partition : edges:t -> Target.t list list
(** Returns a partition of nodes for strongly connected components in the dependency graph *)

val reverse : t -> t
(** Reverse edges in the dependency graph *)

val pp : Format.formatter -> t -> unit

val pp_partitions : Format.formatter -> Target.t list list -> unit

val dump : t -> path:PyrePath.t -> unit

val from_overrides : overrides -> t

val from_callgraph : callgraph -> t

val create_overrides : environment:TypeEnvironment.ReadOnly.t -> source:Source.t -> overrides

val union : t -> t -> t

val expand_callees : Target.t list -> Target.t list

type prune_result = {
  dependencies: t;
  pruned_callables: Target.t list;
}

(* Our analyses distinguish callables which are part of the project being analyzed and those
   belonging to dependencies. The prune operation restricts our callgraph to the subgraph reachable
   from the project callables. During this operation, we also return a list of pruned callables to
   analyze, i.e. we remove irrelevant dependencies from consideration. *)
val prune : t -> callables_with_dependency_information:(Target.t * bool) list -> prune_result
