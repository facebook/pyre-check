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
open Statement
open Analysis

type class_name = string

module ClassNameSet = Caml.Set.Make (String)

module ClassNameMap = struct
  include Caml.Map.Make (String)

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
  [@@deriving eq]

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

  let from_source ~environment ~source =
    let resolution = TypeEnvironment.ReadOnly.global_resolution environment in
    if GlobalResolution.source_is_unit_test resolution ~source then
      empty
    else
      let register_immediate_subclasses
          accumulator
          { Node.value = { Class.name = class_name; _ }; _ }
        =
        let class_name = Reference.show class_name in
        let parents = GlobalResolution.immediate_parents ~resolution class_name in
        List.fold ~init:accumulator parents ~f:(fun accumulator parent ->
            add accumulator ~parent ~child:class_name)
      in
      Preprocessing.classes source |> List.fold ~init:empty ~f:register_immediate_subclasses


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


  let get_source ~environment qualifier =
    let ast_environment = TypeEnvironment.ReadOnly.ast_environment environment in
    AstEnvironment.ReadOnly.get_processed_source ast_environment qualifier


  let from_qualifiers ~scheduler ~environment ~qualifiers =
    let build_class_hierarchy_graph _ qualifiers =
      List.fold qualifiers ~init:empty ~f:(fun accumulator qualifier ->
          match get_source ~environment qualifier with
          | Some source ->
              let graph = from_source ~environment ~source in
              join accumulator graph
          | None -> accumulator)
    in
    Scheduler.map_reduce
      scheduler
      ~policy:
        (Scheduler.Policy.fixed_chunk_count
           ~minimum_chunks_per_worker:1
           ~minimum_chunk_size:100
           ~preferred_chunks_per_worker:1
           ())
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
      (Analysis.SharedMemoryKeys.StringKey)
      (struct
        type t = ClassNameSet.t

        let prefix = Prefix.make ()

        let description = "direct children of classes"
      end)

  module TransitiveChildren =
    Memory.WithCache.Make
      (Analysis.SharedMemoryKeys.StringKey)
      (struct
        type t = ClassNameSet.t

        let prefix = Prefix.make ()

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
