(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open ClassHierarchyGraph

type dfs_state =
  | Grey
  | Black

module Heap = struct
  type t = ClassIntervalSet.t ClassHierarchyGraph.ClassNameMap.t

  let from_class_hierarchy class_hierarchy =
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
                (Format.asprintf
                   "Found a back edge from %s to %s in the class hierarchy"
                   node
                   child)
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
      let intervals = ClassNameMap.add node (ClassInterval.create start finish) intervals in
      intervals, ClassNameMap.add node Black states, cross_edges, time
    in
    let intervals, _, cross_edges, _ =
      ClassNameSet.fold
        depth_first_search
        roots
        (ClassNameMap.empty, ClassNameMap.empty, ClassNameMap.empty, 0)
    in
    let join_intervals_from nodes interval =
      ClassNameSet.fold
        (fun node accumulator ->
          match ClassNameMap.find_opt node intervals with
          | None -> failwith (Format.asprintf "Node %s should have an interval" node)
          | Some child_interval -> child_interval :: accumulator)
        nodes
        [interval]
      |> ClassIntervalSet.of_list
    in
    let add_interval_with_cross_edges node interval accumulator =
      let interval =
        match ClassNameMap.find_opt node cross_edges with
        | None -> ClassIntervalSet.of_list [interval]
        | Some nodes -> join_intervals_from nodes interval
      in
      ClassNameMap.add node interval accumulator
    in
    ClassNameMap.fold add_interval_with_cross_edges intervals ClassNameMap.empty
end

module SharedMemory = struct
  include
    Memory.WithCache.Make
      (Analysis.SharedMemoryKeys.StringKey)
      (struct
        type t = ClassIntervalSet.t

        let prefix = Prefix.make ()

        let description = "class intervals of classes"
      end)

  type t = Handle

  let get_for_testing_only () = Handle

  let add Handle ~class_name ~interval = add class_name interval

  let get Handle ~class_name = get class_name

  let from_heap intervals =
    let () =
      ClassNameMap.iter (fun class_name interval -> add Handle ~class_name ~interval) intervals
    in
    Handle


  let of_class Handle class_name =
    get Handle ~class_name |> Option.value ~default:ClassIntervalSet.top


  let of_type Handle = function
    | Some (Type.Primitive class_name) -> of_class Handle class_name
    | _ -> ClassIntervalSet.top


  let of_definition Handle definition =
    match Target.create definition |> Target.class_name with
    | Some class_name -> of_class Handle class_name
    | None -> ClassIntervalSet.top
end
