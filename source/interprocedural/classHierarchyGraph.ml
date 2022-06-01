(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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


let children { edges; _ } parent = set_of_children (ClassNameMap.find_opt parent edges)

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
    let register_immediate_subclasses accumulator { Node.value = { Class.name = class_name; _ }; _ }
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
    ~policy:(Scheduler.Policy.legacy_fixed_chunk_count ())
    ~initial:empty
    ~map:build_class_hierarchy_graph
    ~reduce:join
    ~inputs:qualifiers
    ()
