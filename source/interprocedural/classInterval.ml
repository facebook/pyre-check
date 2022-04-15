(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open ClassHierarchyGraph
include Interval.Int

let lower_bound_exn interval = Option.value_exn (Interval.Int.lbound interval)

let upper_bound_exn interval = Option.value_exn (Interval.Int.ubound interval)

let meet left right = Interval.Int.intersect left right

let join left right = Interval.Int.convex_hull [left; right]

let equal left right =
  Interval.Int.is_subset left ~of_:right && Interval.Int.is_subset right ~of_:left


let is_empty = Interval.Int.is_empty

let bottom = empty

let top = create min_int max_int

let is_top interval =
  (not (is_empty interval)) && lbound_exn interval == min_int && ubound_exn interval == max_int


let less_or_equal ~left ~right = Interval.Int.is_subset left ~of_:right

let pp_interval formatter interval =
  if Interval.Int.is_empty interval then
    Format.fprintf formatter "<empty>"
  else
    Format.fprintf
      formatter
      "%d,%d"
      (Option.value_exn (Interval.Int.lbound interval))
      (Option.value_exn (Interval.Int.ubound interval))


let pp formatter interval = Format.fprintf formatter "@[[%a]@]" pp_interval interval

let show = Format.asprintf "%a" pp

type dfs_state =
  | Grey
  | Black

let compute_intervals class_hierarchy =
  let roots = ClassHierarchyGraph.roots class_hierarchy in
  let add_direct_cross_edge ~from_ ~to_ cross_edges =
    let update = function
      | Some nodes -> Some (ClassNameSet.add to_ nodes)
      | None -> Some (ClassNameSet.singleton to_)
    in
    ClassNameMap.update from_ update cross_edges
  in
  let add_indirect_cross_edge ~from_ ~to_ cross_edges =
    match ClassNameMap.find_opt to_ cross_edges with
    | None -> cross_edges
    | Some nodes ->
        let update = function
          | Some original_nodes -> Some (ClassNameSet.union nodes original_nodes)
          | None -> Some nodes
        in
        ClassNameMap.update from_ update cross_edges
  in

  (* To compute cross edges, the key observation is that, during the DFS, if a node is black, then
   *  its associated cross edges are final (i.e., we cannot discover any new indirect or direct edges
   *  that originate from that node) *)
  let rec depth_first_search node (intervals, states, cross_edges, time) =
    let time = time + 1 in
    let start = time in
    let states = ClassNameMap.add node Grey states in
    let intervals, states, cross_edges, time =
      let visit_child child ((intervals, states, cross_edges, time) as accumulator) =
        match ClassNameMap.find_opt child states with
        | None ->
            let intervals, states, cross_edges, time = depth_first_search child accumulator in
            (* Now the child is black *)
            let cross_edges = add_indirect_cross_edge ~from_:node ~to_:child cross_edges in
            intervals, states, cross_edges, time
        | Some Grey ->
            failwith
              (Format.asprintf "Found a back edge from %s to %s in the class hierarchy" node child)
        | Some Black ->
            let cross_edges =
              cross_edges
              |> add_direct_cross_edge ~from_:node ~to_:child
              (* All cross edges of a black node are indirect cross edges *)
              |> add_indirect_cross_edge ~from_:node ~to_:child
            in
            intervals, states, cross_edges, time
      in
      ClassNameSet.fold
        visit_child
        (children class_hierarchy node)
        (intervals, states, cross_edges, time)
    in
    let time = time + 1 in
    let finish = time in
    let intervals = ClassNameMap.add node (create start finish) intervals in
    intervals, ClassNameMap.add node Black states, cross_edges, time
  in
  let intervals, _, cross_edges, _ =
    ClassNameSet.fold
      depth_first_search
      roots
      (ClassNameMap.empty, ClassNameMap.empty, ClassNameMap.empty, 0)
  in
  let join_intervals_from nodes =
    ClassNameSet.fold
      (fun node accumulator ->
        match ClassNameMap.find_opt node intervals with
        | None -> failwith (Format.asprintf "Node %s should have an interval" node)
        | Some interval -> join interval accumulator)
      nodes
      Interval.Int.empty
  in
  let add_interval_with_cross_edges node interval accumulator =
    let interval =
      match ClassNameMap.find_opt node cross_edges with
      | None -> interval
      | Some nodes -> join interval (join_intervals_from nodes)
    in
    ClassNameMap.add node interval accumulator
  in
  ClassNameMap.fold add_interval_with_cross_edges intervals ClassNameMap.empty


module SharedMemory = struct
  include
    Memory.WithCache.Make
      (Analysis.SharedMemoryKeys.StringKey)
      (struct
        type t = Interval.Int.t

        let prefix = Prefix.make ()

        let description = "class intervals of classes"
      end)

  let add ~class_name ~interval = add class_name interval

  let get ~class_name = get class_name

  let store intervals =
    ClassNameMap.iter (fun class_name interval -> add ~class_name ~interval) intervals


  let of_class class_name = get ~class_name |> Option.value ~default:top

  let of_type = function
    | Some (Type.Primitive class_name) -> of_class class_name
    | _ -> top


  let of_definition definition =
    match Target.create definition |> Target.class_name with
    | Some class_name -> of_class class_name
    | None -> top
end
