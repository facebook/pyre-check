(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* ClassHierarchyGraph: defines a class hierarchy graph. This can be used to
 * find all classes deriving from a given class. *)

open Core
open Ast
module PyrePysaLogic = Analysis.PyrePysaLogic

type class_name = string

module ClassNameSet = Stdlib.Set.Make (String)

module ClassNameMap = struct
  include Stdlib.Map.Make (String)

  let pp_map pp_value formatter map =
    let pp_pairs pp_value formatter pairs =
      let pp_pair formatter key value =
        Format.fprintf formatter "@,%s -> [%a]" key pp_value value
      in
      iter (pp_pair formatter) pairs
    in
    Format.fprintf formatter "{@[<v 2>%a@]@,}" (pp_pairs pp_value) map


  let show ~pp_value map = Format.asprintf "%a" (pp_map pp_value) map
end

(** Graph of root classes and their children, stored in the ocaml heap. *)
module Heap = struct
  type t = {
    roots: ClassNameSet.t;
    edges: ClassNameSet.t ClassNameMap.t;
  }
  [@@deriving equal]

  let pp_set formatter set =
    ClassNameSet.iter (fun element -> Format.fprintf formatter "@[%s,@]" element) set


  let pp formatter { roots; edges } =
    Format.fprintf
      formatter
      "roots: @[[%a]@]@\nedges: {@[<v 2>%a@]@,}"
      pp_set
      roots
      (ClassNameMap.pp_map pp_set)
      edges


  let show = Format.asprintf "%a" pp

  let empty = { roots = ClassNameSet.empty; edges = ClassNameMap.empty }

  let set_of_children = function
    | None -> ClassNameSet.empty
    | Some children -> children


  (* Return the immediate children *)
  let children { edges; _ } parent = set_of_children (ClassNameMap.find_opt parent edges)

  (* Add an edge in the graph *)
  let add { roots; edges } ~parent:parent_class ~child:child_class =
    let new_roots =
      let new_roots =
        if ClassNameMap.mem parent_class edges then
          roots
        else
          ClassNameSet.add parent_class roots
      in
      ClassNameSet.remove child_class new_roots
    in
    let new_edges =
      let add_child parent = Some (ClassNameSet.add child_class (set_of_children parent)) in
      let update_children ~parent ~update edges = ClassNameMap.update parent update edges in
      edges
      |> update_children ~parent:parent_class ~update:add_child
      |> update_children ~parent:child_class ~update:(fun key -> Some (set_of_children key))
    in
    { roots = new_roots; edges = new_edges }


  let roots { roots; _ } = roots

  let from_qualifier ~pyre_api ~qualifier =
    let register_immediate_subclasses accumulator class_name =
      let class_name = Reference.show class_name in
      let parents = PyrePysaApi.ReadOnly.class_immediate_parents pyre_api class_name in
      List.fold ~init:accumulator parents ~f:(fun accumulator parent ->
          add accumulator ~parent ~child:class_name)
    in
    PyrePysaApi.ReadOnly.get_class_names_for_qualifier pyre_api ~exclude_test_modules:true qualifier
    |> List.fold ~init:empty ~f:register_immediate_subclasses


  let create ~roots ~edges =
    let roots = ClassNameSet.of_list roots in
    let edges =
      List.fold edges ~init:ClassNameMap.empty ~f:(fun accumulator (parent, children) ->
          ClassNameMap.add parent (ClassNameSet.of_list children) accumulator)
    in
    { roots; edges }


  let join ({ roots = _; edges = edges_left } as left) { roots = roots_right; edges = edges_right } =
    let add_edges parent_right children_right { roots; edges } =
      if ClassNameSet.is_empty children_right && not (ClassNameMap.mem parent_right edges) then
        let edges = ClassNameMap.add parent_right ClassNameSet.empty edges in
        { roots; edges }
      else
        let update = function
          | Some children_left -> Some (ClassNameSet.union children_left children_right)
          | None -> Some children_right
        in
        let edges = ClassNameMap.update parent_right update edges in
        (* Remove roots that now have incoming edges *)
        let roots = ClassNameSet.diff roots children_right in
        { roots; edges }
    in
    let { roots; edges } = ClassNameMap.fold add_edges edges_right left in
    let add_root root_right accumulator =
      if ClassNameMap.mem root_right edges_left then
        accumulator
      else
        ClassNameSet.add root_right accumulator
    in
    let roots = ClassNameSet.fold add_root roots_right roots in
    { roots; edges }


  let from_qualifiers ~scheduler ~scheduler_policies ~pyre_api ~qualifiers =
    let build_class_hierarchy_graph qualifiers =
      List.fold qualifiers ~init:empty ~f:(fun accumulator qualifier ->
          let graph = from_qualifier ~pyre_api ~qualifier in
          join accumulator graph)
    in
    let scheduler_policy =
      Scheduler.Policy.from_configuration_or_default
        scheduler_policies
        Configuration.ScheduleIdentifier.ClassHierarchyGraph
        ~default:
          (Scheduler.Policy.fixed_chunk_count
             ~minimum_chunks_per_worker:1
             ~minimum_chunk_size:1
             ~preferred_chunks_per_worker:1
             ())
    in
    Scheduler.map_reduce
      scheduler
      ~policy:scheduler_policy
      ~initial:empty
      ~map:build_class_hierarchy_graph
      ~reduce:join
      ~inputs:qualifiers
      ()
end

(** Mapping from a class name to the set of its direct children, stored in shared memory. *)
module SharedMemory = struct
  module DirectChildren =
    Memory.WithCache.Make
      (PyrePysaLogic.SharedMemoryKeys.StringKey)
      (struct
        type t = ClassNameSet.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "direct children of classes"
      end)

  module TransitiveChildren =
    Memory.WithCache.Make
      (PyrePysaLogic.SharedMemoryKeys.StringKey)
      (struct
        type t = ClassNameSet.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "transitive children of classes"
      end)

  type t = Handle

  let add Handle ~class_name ~direct_children = DirectChildren.add class_name direct_children

  let get Handle ~class_name =
    DirectChildren.get class_name |> Option.value ~default:ClassNameSet.empty


  let rec add_children ~class_hierarchy_graph =
    ClassNameSet.iter (fun class_name ->
        let direct_children = Heap.children class_hierarchy_graph class_name in
        add Handle ~class_name ~direct_children;
        add_children ~class_hierarchy_graph direct_children)


  let add_transitive Handle ~class_name ~transitive_children =
    TransitiveChildren.add class_name transitive_children


  let get_transitive Handle ~class_name = TransitiveChildren.get class_name

  let rec find_children_transitive ~class_hierarchy_graph to_process result =
    match to_process with
    | [] -> result
    | class_name :: rest ->
        let child_name_set = Heap.children class_hierarchy_graph class_name in
        let new_children = ClassNameSet.elements child_name_set in
        let result = List.fold ~f:(Fn.flip ClassNameSet.add) ~init:result new_children in
        find_children_transitive ~class_hierarchy_graph (List.rev_append new_children rest) result


  let add_children_transitive ~class_hierarchy_graph =
    List.iter ~f:(fun class_name ->
        let transitive_children =
          find_children_transitive ~class_hierarchy_graph [class_name] ClassNameSet.empty
        in
        add_transitive Handle ~class_name ~transitive_children)


  let from_heap ~store_transitive_children_for class_hierarchy_graph =
    let () = add_children ~class_hierarchy_graph (Heap.roots class_hierarchy_graph) in
    let () = add_children_transitive ~class_hierarchy_graph store_transitive_children_for in
    Handle
end
