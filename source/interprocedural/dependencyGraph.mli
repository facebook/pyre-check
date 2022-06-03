(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t
(** Represents a dependency graph, i.e a mapping from a callee to its callers.

    This is used in the fixpoint to infer which targets to re-analyze when we discovered a new
    source or sink. *)

val empty : t

val dependencies : t -> Target.t -> Target.t list

val to_target_graph : t -> TargetGraph.t

(** Represents a reversed dependency graph, i.e a mapping from callers to callees. *)
module Reversed : sig
  type nonrec dependency_graph = t

  type t

  val disjoint_union : t -> t -> t
  (** Merge two reverse dependency graph that do not have common callees. *)

  val from_call_graph : CallGraph.WholeProgramCallGraph.t -> t
  (** Create a reverse dependency graph from a call graph. *)

  val from_overrides : OverrideGraph.Heap.t -> t
  (** Create a reverse dependency graph from an override graph. *)

  type prune_result = {
    reverse_dependency_graph: t;
    (* All targets reachable by internal callables, in the depth first search order, without
       duplicates. *)
    callables_kept: Target.t list;
  }

  val prune : t -> initial_callables:FetchCallables.t -> prune_result
  (** Our analyses distinguish callables which are part of the project being analyzed and those
      belonging to dependencies. The prune operation restricts our callgraph to the subgraph
      reachable from the project callables. During this operation, we also return a list of pruned
      callables to analyze, i.e. we remove irrelevant dependencies from consideration. *)

  val to_target_graph : t -> TargetGraph.t

  val reverse : t -> dependency_graph
end

type whole_program_dependency_graph = {
  dependency_graph: t;
  override_targets: Target.t list;
  callables_kept: Target.t list;
  (* All callables to analyze (including overrides), sorted in preferred analysis order. *)
  callables_to_analyze: Target.t list;
}

val build_whole_program_dependency_graph
  :  prune:bool ->
  initial_callables:FetchCallables.t ->
  call_graph:CallGraph.WholeProgramCallGraph.t ->
  overrides:OverrideGraph.Heap.t ->
  whole_program_dependency_graph
(** Merge overrides and callgraph into a combined dependency graph, and prune anything not linked to
    the callables we are actually analyzing. Then reverse the graph, which maps dependers to
    dependees (i.e. override targets to overrides + callers to callees) into a scheduling graph that
    maps dependees to dependers. *)
