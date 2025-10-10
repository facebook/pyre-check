(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Represents a dependency graph, i.e a mapping from a callee to its callers. *)
type t

val empty : t

val is_empty : t -> bool

val merge : t -> t -> t

val add : callee:Target.t -> caller:Target.t -> t -> t

val dependencies : t -> Target.t -> Target.t list

val keys : t -> Target.t list

val to_target_graph : t -> TargetGraph.t

module PruneMethod : sig
  type t =
    | None
    | Internals
    | Entrypoints of Target.t list
end

(** Represents a reversed dependency graph, i.e a mapping from callers to callees. *)
module Reversed : sig
  type nonrec dependency_graph = t

  type t

  (** Merge two reverse dependency graph that do not have common callees. *)
  val disjoint_union : t -> t -> t

  (** Create a reverse dependency graph from a call graph. *)
  val from_call_graph : CallGraph.WholeProgramCallGraph.t -> t

  (** Create a reverse dependency graph from an override graph. *)
  val from_overrides : OverrideGraph.Heap.t -> t

  type prune_result = {
    reverse_dependency_graph: t;
    (* All targets reachable by internal callables, in the depth first search order, without
       duplicates. *)
    callables_kept: Target.t list;
  }

  (** Our analyses distinguish callables which are part of the project being analyzed and those
      belonging to dependencies. The prune operation restricts our callgraph to the subgraph
      reachable from the project callables. During this operation, we also return a list of pruned
      callables to analyze, i.e. we remove irrelevant dependencies from consideration. *)
  val prune : t -> callables_to_analyze:Target.t list -> prune_result

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

(** Merge overrides and callgraph into a combined dependency graph, and prune anything not linked to
    the callables we are actually analyzing. Then reverse the graph, which maps dependers to
    dependees (i.e. override targets to overrides + callers to callees) into a scheduling graph that
    maps dependees to dependers. Always include the decorated targets from `decorator_resolution`. *)
val build_whole_program_dependency_graph
  :  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  prune:PruneMethod.t ->
  initial_callables:FetchCallables.t ->
  call_graph:CallGraph.WholeProgramCallGraph.t ->
  overrides:OverrideGraph.Heap.t ->
  ignore_decorated_targets:bool ->
  whole_program_dependency_graph
