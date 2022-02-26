(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module Component = struct
  type t = {
    id: int;
    kind: kind;
  }
  [@@deriving compare, sexp]

  and kind =
    | Node of Cfg.Node.t
    | Cycle of {
        head: Cfg.Node.t;
        components: t list;
      }
  [@@deriving compare, sexp]
end

type t = Component.t list [@@deriving compare, sexp]

let create ~cfg ~entry_index ~successors =
  (* The construction of weak topological orderings is based on F. Bourdoncle's
   * paper: "Efficient chaotic iteration strategies with widenings", Formal
   * Methods in Programming and Their Applications, 1993, pages 128-141.
   *)
  let depth_first_numbers = Int.Table.create () in
  let stack = Stack.create () in

  let current_depth_first_number = ref 0 in
  let make_depth_first_number () =
    incr current_depth_first_number;
    !current_depth_first_number
  in

  let get_depth_first_number node_index =
    Hashtbl.find depth_first_numbers node_index |> Option.value ~default:0
  in

  let current_component_id = ref 0 in
  let make_component kind =
    let id = !current_component_id in
    incr current_component_id;
    { Component.id; kind }
  in

  let rec visit components node_index =
    Stack.push stack node_index;

    let head_dfn = make_depth_first_number () in
    Hashtbl.set depth_first_numbers ~key:node_index ~data:head_dfn;

    let visit_successors (components, minimum_dfn, loop) successor_id =
      let components, successor_dfn =
        match get_depth_first_number successor_id with
        | 0 -> visit components successor_id
        | successor_dfn -> components, successor_dfn
      in
      if successor_dfn <= minimum_dfn then
        components, successor_dfn, true
      else
        components, minimum_dfn, loop
    in

    let node = Cfg.node cfg ~id:node_index in
    let components, minimum_dfn, loop =
      node |> successors |> Set.fold ~init:(components, head_dfn, false) ~f:visit_successors
    in

    let components =
      if Int.equal minimum_dfn (get_depth_first_number node_index) then (
        (* Found a new component. *)
        Hashtbl.set depth_first_numbers ~key:node_index ~data:max_int;
        let element_id = Stack.pop_exn stack in

        if loop then (
          (* Found a new cycle. *)
          let rec reset_dfn element_id =
            if not (Int.equal element_id node_index) then (
              Hashtbl.set depth_first_numbers ~key:element_id ~data:0;
              reset_dfn (Stack.pop_exn stack))
          in
          reset_dfn element_id;
          let visit_successors components successor_id =
            match get_depth_first_number successor_id with
            | 0 ->
                let components, _ = visit components successor_id in
                components
            | _ -> components
          in
          let new_components = node |> successors |> Set.fold ~init:[] ~f:visit_successors in
          let component =
            make_component (Component.Cycle { head = node; components = new_components })
          in
          component :: components)
        else (* Found a single node. *)
          let component = make_component (Component.Node node) in
          component :: components)
      else
        components
    in
    components, minimum_dfn
  in

  let components, _ = visit [] entry_index in
  components
