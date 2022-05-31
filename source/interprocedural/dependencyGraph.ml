(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre

type t = Target.t list Target.Map.t

type callgraph = Target.t list Target.Map.t

module CallGraphSharedMemory = Memory.Serializer (struct
  type t = Target.t list Target.Map.Tree.t

  module Serialized = struct
    type t = Target.t list Target.Map.Tree.t

    let prefix = Prefix.make ()

    let description = "Call graph"
  end

  let serialize = Fn.id

  let deserialize = Fn.id
end)

let empty = Target.Map.empty

let empty_callgraph = Target.Map.empty

(* Returns forest of nodes in reverse finish time order. *)
let depth_first_search edges nodes =
  let visited = Target.HashMap.create ~size:(2 * List.length nodes) () in
  let rec visit accumulator node =
    if Hashtbl.mem visited node then
      accumulator
    else (
      Hashtbl.add_exn visited ~key:node ~data:();
      let successors = Target.Map.find edges node |> Option.value ~default:[] in
      node :: List.fold successors ~init:accumulator ~f:visit)
  in
  let partition accumulator node =
    match visit [] node with
    | [] -> accumulator
    | tree -> tree :: accumulator
  in
  List.fold ~init:[] ~f:partition nodes


let reverse_edges edges =
  let reverse_edges = Target.HashMap.create () in
  let walk_reverse_edges ~key:caller ~data:callees =
    let walk_callees callee =
      match Target.HashMap.find reverse_edges callee with
      | None -> Target.HashMap.add_exn reverse_edges ~key:callee ~data:[caller]
      | Some callers -> Target.HashMap.set reverse_edges ~key:callee ~data:(caller :: callers)
    in
    List.iter callees ~f:walk_callees
  in
  let () = Target.Map.iteri edges ~f:walk_reverse_edges in
  let accumulate ~key ~data map =
    Target.Map.set map ~key ~data:(List.dedup_and_sort ~compare:Target.compare data)
  in
  Target.HashMap.fold reverse_edges ~init:Target.Map.empty ~f:accumulate


let reverse call_graph =
  let reverse ~key ~data reverse_map =
    List.fold data ~init:reverse_map ~f:(fun reverse_map callee ->
        Target.Map.add_multi reverse_map ~key:callee ~data:key)
  in
  Map.fold call_graph ~init:Target.Map.empty ~f:reverse


let pp_partitions formatter partitions =
  let print_partition partitions =
    let print_partition index accumulator nodes =
      let nodes_to_string = List.map ~f:Target.show_internal nodes |> String.concat ~sep:" " in
      let partition = Format.sprintf "Partition %d: [%s]" index nodes_to_string in
      accumulator ^ "\n" ^ partition
    in
    List.foldi partitions ~init:"" ~f:print_partition
  in
  Format.fprintf formatter "%s" (print_partition partitions)


let partition ~edges =
  let result = depth_first_search edges (Target.Map.keys edges) in
  let reverse_edges = reverse_edges edges in
  let partitions = depth_first_search reverse_edges (List.concat result) in
  Log.log ~section:`DependencyGraph "%a" pp_partitions partitions;
  partitions


let pp formatter edges =
  let pp_edge (callable, data) =
    let targets =
      List.map data ~f:Target.show_pretty_with_kind
      |> List.sort ~compare:String.compare
      |> String.concat ~sep:" "
    in
    Format.fprintf formatter "%a -> [%s]\n" Target.pp_pretty_with_kind callable targets
  in
  let compare (left, _) (right, _) =
    String.compare (Target.show_internal left) (Target.show_internal right)
  in
  Target.Map.to_alist edges |> List.sort ~compare |> List.iter ~f:pp_edge


let from_callgraph callgraph =
  let add ~target ~callees result =
    let callees =
      List.filter
        ~f:(function
          | Target.Object _ -> false
          | _ -> true)
        callees
    in
    Target.Map.set result ~key:target ~data:callees
  in
  CallGraph.ProgramCallGraphHeap.fold callgraph ~f:add ~init:Target.Map.empty


let union left right =
  let combine ~key:_ left right = List.rev_append left right in
  Map.merge_skewed ~combine left right


let from_overrides overrides =
  let override_map, all_overrides =
    let add ~member:method_name ~subtypes (override_map, all_overrides) =
      let key = Target.get_corresponding_override method_name in
      let data =
        List.map subtypes ~f:(fun at_type -> Target.create_derived_override key ~at_type)
      in
      ( Target.Map.set override_map ~key ~data,
        Target.Set.union all_overrides (Target.Set.of_list data) )
    in
    OverrideGraph.Heap.fold overrides ~f:add ~init:(Target.Map.empty, Target.Set.empty)
  in
  let connect_overrides_to_methods override_graph =
    let overrides_to_methods =
      let override_to_method_edge override =
        match override with
        | Target.Override _ as override ->
            let corresponding_method = Target.get_corresponding_method override in
            Some (override, [corresponding_method])
        | _ -> None
      in
      Target.Map.keys override_graph
      |> List.filter_map ~f:override_to_method_edge
      |> Target.Map.of_alist_exn
    in
    union overrides_to_methods override_graph
  in
  (* Create empty entries for leaves, so connect_overrides_to_methods can add self-links *)
  Target.Set.fold
    (fun override override_map ->
      if not (Target.Map.mem override_map override) then
        Target.Map.set override_map ~key:override ~data:[]
      else
        override_map)
    all_overrides
    override_map
  |> connect_overrides_to_methods


type prune_result = {
  dependencies: t;
  pruned_callables: Target.t list;
}

let prune dependency_graph ~initial_callables =
  let internal_callables = FetchCallables.get_internals initial_callables in
  (* We have an implicit edge from a method to the override it corresponds to during the analysis.
     During the pruning, we make the edges from the method to the override explicit to make sure the
     DFS captures the interesting overrides. *)
  let callables_to_keep =
    depth_first_search dependency_graph internal_callables
    |> List.concat
    |> List.dedup_and_sort ~compare:Target.compare
  in
  let dependency_graph =
    (* We only keep the keys which were in the original dependency graph to avoid introducing
       spurious override leaves. *)
    let to_edge callable =
      Target.Map.find dependency_graph callable >>| fun values -> callable, values
    in
    Target.Map.of_alist_exn (List.filter_map callables_to_keep ~f:to_edge)
  in
  let callables_to_keep = Target.Set.of_list callables_to_keep in
  {
    dependencies = dependency_graph;
    pruned_callables =
      initial_callables
      |> FetchCallables.get_callables
      |> List.filter_map ~f:(fun callable ->
             Option.some_if (Target.Set.mem callable callables_to_keep) callable);
  }
