(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = Target.t list Target.Map.t

type callgraph = Target.t list Target.Map.t

module CallGraphSharedMemory : sig
  val store : Target.t list Target.Map.Tree.t -> unit

  val load : unit -> Target.t list Target.Map.Tree.t
end

val empty : t

val empty_callgraph : callgraph

val partition : edges:t -> Target.t list list
(** Returns a partition of nodes for strongly connected components in the dependency graph *)

val reverse : t -> t
(** Reverse edges in the dependency graph *)

val pp : Format.formatter -> t -> unit

val pp_partitions : Format.formatter -> Target.t list list -> unit

val dump : t -> path:PyrePath.t -> unit

val from_overrides : OverrideGraph.Heap.t -> t

val from_callgraph : callgraph -> t

val union : t -> t -> t

type prune_result = {
  dependencies: t;
  pruned_callables: Target.t list;
}

(* Our analyses distinguish callables which are part of the project being analyzed and those
   belonging to dependencies. The prune operation restricts our callgraph to the subgraph reachable
   from the project callables. During this operation, we also return a list of pruned callables to
   analyze, i.e. we remove irrelevant dependencies from consideration. *)
val prune : t -> initial_callables:FetchCallables.t -> prune_result
