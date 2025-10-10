(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* DependencyGraph: represents a dependency graph, i.e a mapping from a callee
 * to its callers.
 *
 * This is used in the interprocedural fixpoint to infer which targets need to
 * be re-analyzed when a model grows (e.g., when new sources or sinks are
 * discovered in the taint analysis).
 *)

open Core
open Pyre

type t = Target.t list Target.Map.Tree.t

let empty = Target.Map.Tree.empty

let is_empty = Target.Map.Tree.is_empty

let dependencies dependency_graph target =
  Target.Map.Tree.find dependency_graph target |> Option.value ~default:[]


let keys = Target.Map.Tree.keys

let add ~callee ~caller map =
  Target.Map.Tree.update map callee ~f:(function
      | Some callers -> caller :: callers
      | None -> [caller])


let merge left right =
  Target.Map.Tree.merge_skewed left right ~combine:(fun ~key:_ left right ->
      List.rev_append left right)


let to_target_graph = Fn.id

module PruneMethod = struct
  type t =
    (* Do not perform pruning on the call graph *)
    | None
    (* Keep callables in the source path (as opposed to the search path) and all callables reachable
       from them *)
    | Internals
    (* Prune the call graph based on a list of entrypoints found from the model file *)
    | Entrypoints of Target.t list
end

(** Represents a reversed dependency graph, i.e a mapping from callers to callees. *)
module Reversed = struct
  type nonrec dependency_graph = t

  type t = Target.t list Target.Map.Tree.t

  (** Merge two reversed dependency graph that do not have common callees. *)
  let disjoint_union left right =
    let combine ~key:_ left right = List.rev_append left right in
    Target.Map.Tree.merge_skewed ~combine left right


  (** Create a reverse dependency graph from a call graph. *)
  let from_call_graph callgraph =
    let add ~target ~callees result =
      let callees = List.filter ~f:(fun target -> target |> Target.is_object |> Core.not) callees in
      Target.Map.Tree.set result ~key:target ~data:callees
    in
    CallGraph.WholeProgramCallGraph.fold callgraph ~f:add ~init:Target.Map.Tree.empty


  (** Create a reverse dependency graph from an override graph. *)
  let from_overrides overrides =
    let override_map, all_overrides =
      let add ~member:method_name ~subtypes (override_map, all_overrides) =
        let key =
          method_name
          |> Target.as_regular_exn
          (* TODO(T204630385): Handle `Target.Parameterized` with `Override`. *)
          |> Target.Regular.get_corresponding_override_exn
          |> Target.from_regular
        in
        let data =
          List.map subtypes ~f:(fun at_type ->
              key
              |> Target.as_regular_exn
              (* TODO(T204630385): Handle `Target.Parameterized` with `Override`. *)
              |> Target.Regular.create_derived_override_exn ~at_type
              |> Target.from_regular)
        in
        ( Target.Map.Tree.set override_map ~key ~data,
          Target.Set.union all_overrides (Target.Set.of_list data) )
      in
      OverrideGraph.Heap.fold overrides ~f:add ~init:(Target.Map.Tree.empty, Target.Set.empty)
    in
    (* Create empty entries for leaves, so `override_to_method_edge` can add self-links *)
    let override_map =
      Target.Set.fold
        (fun override override_map ->
          if not (Target.Map.Tree.mem override_map override) then
            Target.Map.Tree.set override_map ~key:override ~data:[]
          else
            override_map)
        all_overrides
        override_map
    in
    let overrides_to_methods =
      let override_to_method_edge override =
        if Target.is_override override then
          let corresponding_method =
            (* TODO(T204630385): Handle `Target.Parameterized` with `Override`. *)
            Target.get_corresponding_method_exn ~must_be_regular:true override
          in
          Some (override, [corresponding_method])
        else
          None
      in
      Target.Map.Tree.keys override_map
      |> List.filter_map ~f:override_to_method_edge
      |> Target.Map.Tree.of_alist_exn
    in
    disjoint_union override_map overrides_to_methods


  (** Returns forest of nodes in reverse finish time order. *)
  let depth_first_search edges nodes =
    let visited = Target.HashMap.create ~size:(2 * List.length nodes) () in
    let rec visit accumulator node =
      if Hashtbl.mem visited node then
        accumulator
      else (
        Hashtbl.add_exn visited ~key:node ~data:();
        let successors = Target.Map.Tree.find edges node |> Option.value ~default:[] in
        node :: List.fold successors ~init:accumulator ~f:visit)
    in
    let partition accumulator node =
      match visit [] node with
      | [] -> accumulator
      | tree -> tree :: accumulator
    in
    List.fold ~init:[] ~f:partition nodes


  type prune_result = {
    reverse_dependency_graph: t;
    (* All targets reachable by internal callables, in the depth first search order, without
       duplicates. *)
    callables_kept: Target.t list;
  }

  let prune reverse_dependency_graph ~callables_to_analyze =
    (* We have an implicit edge from a method to the override it corresponds to during the analysis.
       During the pruning, we make the edges from the method to the override explicit to make sure
       the DFS captures the interesting overrides. *)
    let callables_to_keep =
      depth_first_search reverse_dependency_graph callables_to_analyze |> List.concat
    in
    let reverse_dependency_graph =
      (* We only keep the keys which were in the original dependency graph to avoid introducing
         spurious override leaves. *)
      let to_edge callable =
        Target.Map.Tree.find reverse_dependency_graph callable >>| fun values -> callable, values
      in
      Target.Map.Tree.of_alist_exn (List.filter_map callables_to_keep ~f:to_edge)
    in
    { reverse_dependency_graph; callables_kept = callables_to_keep }


  let to_target_graph = Fn.id

  let reverse reverse_dependency_graph =
    let reverse ~key ~data dependency_graph =
      List.fold data ~init:dependency_graph ~f:(fun dependency_graph callee ->
          Target.Map.Tree.add_multi dependency_graph ~key:callee ~data:key)
    in
    Target.Map.Tree.fold reverse_dependency_graph ~init:Target.Map.Tree.empty ~f:reverse
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
let build_whole_program_dependency_graph
    ~static_analysis_configuration
    ~prune
    ~initial_callables
    ~call_graph
    ~overrides
    ~ignore_decorated_targets
  =
  let reverse_dependency_graph = Reversed.from_overrides overrides in
  let override_targets = Target.Map.Tree.keys reverse_dependency_graph in
  let reverse_dependency_graph =
    Reversed.from_call_graph call_graph |> Reversed.disjoint_union reverse_dependency_graph
  in
  let create_dependency_graph_from_prune_result
      { Reversed.reverse_dependency_graph; callables_kept }
    =
    let dependency_graph = Reversed.reverse reverse_dependency_graph in
    (* Analyze callables in the reverse weak topological order. *)
    let initial_callable_set =
      FetchCallables.get_definitions initial_callables |> Target.HashSet.of_list
    in
    let callables_to_analyze =
      callables_kept
      |> List.filter ~f:(fun callable ->
             Target.is_override callable
             (* Since parameterized targets are created by higher order call graph building, they
                are not included in `initial_callables`. *)
             || Hash_set.mem
                  initial_callable_set
                  (callable |> Target.get_regular |> Target.from_regular))
      |> List.rev
    in
    { dependency_graph; override_targets; callables_kept; callables_to_analyze }
  in
  let callables_to_analyze =
    match prune with
    | PruneMethod.None ->
        initial_callables |> FetchCallables.get_definitions |> List.rev_append override_targets
    | PruneMethod.Internals -> FetchCallables.get_internal_definitions initial_callables
    | PruneMethod.Entrypoints entrypoints -> entrypoints
  in
  (* Always add the `kind=Decorated` targets from `decorator_resolution` in the dependency graph,
     since this is also used to compute dependencies for the call graph fixpoint. For the taint
     fixpoint, dependencies are computed with an empty `decorator_resolution`. *)
  let callables_to_analyze =
    if ignore_decorated_targets then
      List.filter ~f:(fun target -> not (Target.is_decorated target)) callables_to_analyze
    else
      callables_to_analyze
  in
  let ({ dependency_graph; _ } as whole_program_dependency_graph) =
    reverse_dependency_graph
    |> Reversed.prune ~callables_to_analyze
    |> create_dependency_graph_from_prune_result
  in
  let () =
    match static_analysis_configuration.Configuration.StaticAnalysis.save_results_to with
    | Some path ->
        let path = PyrePath.append path ~element:"dependency-graph.json" in
        Log.info "Writing the dependency graph to `%s`" (PyrePath.absolute path);
        TargetGraph.dump ~path dependency_graph
    | None -> ()
  in
  whole_program_dependency_graph
