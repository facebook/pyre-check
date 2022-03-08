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

module ClassNameMap = Core.Map.Make (String)
module ClassNameSet = Caml.Set.Make (String)

type t = {
  roots: ClassNameSet.t;
  edges: ClassNameSet.t ClassNameMap.t;
}
[@@deriving eq]

let pp formatter { roots; edges } =
  let pp_set formatter set =
    ClassNameSet.iter (fun e -> Format.fprintf formatter "@[%s,@]" e) set
  in
  let pp_pairs formatter =
    let pp_pair formatter (key, value) = Format.fprintf formatter "@,%s -> [%a]" key pp_set value in
    List.iter ~f:(pp_pair formatter)
  in
  edges
  |> ClassNameMap.to_alist
  |> Format.fprintf formatter "roots: @[[%a]@]@\nedges: {@[<v 2>%a@]@,}" pp_set roots pp_pairs


let show = Format.asprintf "%a" pp

let empty = { roots = ClassNameSet.empty; edges = ClassNameMap.empty }

let set_of_children = function
  | None -> ClassNameSet.empty
  | Some children -> children


let children { edges; _ } parent = set_of_children (ClassNameMap.find edges parent)

let add { roots; edges } ~parent:parent_class ~child:child_class =
  let new_roots =
    let new_roots =
      if ClassNameMap.mem edges parent_class then
        roots
      else
        ClassNameSet.add parent_class roots
    in
    ClassNameSet.remove child_class new_roots
  in
  let new_edges =
    let add_child parent = ClassNameSet.add child_class (set_of_children parent) in
    let update_children ~parent ~update edges = ClassNameMap.update edges parent ~f:update in
    edges
    |> update_children ~parent:parent_class ~update:add_child
    |> update_children ~parent:child_class ~update:set_of_children
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
        ClassNameMap.set accumulator ~key:parent ~data:(ClassNameSet.of_list children))
  in
  { roots; edges }
