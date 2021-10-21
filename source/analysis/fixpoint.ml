(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre

module type State = sig
  type t [@@deriving show]

  val bottom : t

  val less_or_equal : left:t -> right:t -> bool

  val join : t -> t -> t

  val widen : previous:t -> next:t -> iteration:int -> t

  val forward : statement_key:int -> t -> statement:Statement.t -> t

  val backward : statement_key:int -> t -> statement:Statement.t -> t
end

module type Fixpoint = sig
  type state

  type t = {
    preconditions: state Int.Table.t;
    postconditions: state Int.Table.t;
  }
  [@@deriving show]

  val entry : t -> state option

  val normal_exit : t -> state option

  val exit : t -> state option

  val forward : cfg:Cfg.t -> initial:state -> t

  val backward : cfg:Cfg.t -> initial:state -> t

  val equal : f:(state -> state -> bool) -> t -> t -> bool
end

module Make (State : State) = struct
  type state = State.t

  type t = {
    preconditions: State.t Int.Table.t;
    postconditions: State.t Int.Table.t;
  }

  let equal ~f left right =
    Core.Hashtbl.equal f left.preconditions right.preconditions
    && Core.Hashtbl.equal f left.postconditions right.postconditions


  let pp format { preconditions; postconditions } =
    let print_state ~name ~key ~data =
      Format.fprintf format "%s %d -> %a\n" name key State.pp data
    in
    Hashtbl.iteri preconditions ~f:(print_state ~name:"Precondition");
    Hashtbl.iteri postconditions ~f:(print_state ~name:"Postcondition")


  let show fixpoint = Format.asprintf "%a" pp fixpoint

  let entry { preconditions; _ } = Hashtbl.find preconditions Cfg.entry_index

  let normal_exit { postconditions; _ } = Hashtbl.find postconditions Cfg.normal_index

  let exit { postconditions; _ } = Hashtbl.find postconditions Cfg.exit_index

  let compute_fixpoint cfg ~initial_index ~initial ~predecessors ~successors ~transition =
    (*
     * This is the implementation of a monotonically increasing chaotic fixpoint
     * iteration sequence with widening over a control-flow graph (CFG) using the
     * recursive iteration strategy induced by a weak topological ordering of the
     * nodes in the control-flow graph. The recursive iteration strategy is
     * described in Bourdoncle's paper on weak topological orderings:
     *
     *   F. Bourdoncle. Efficient chaotic iteration strategies with widenings.
     *   In Formal Methods in Programming and Their Applications, pp 128-141.
     *)
    let components = WeakTopologicalOrder.create ~cfg ~entry_index:initial_index ~successors in

    let preconditions = Int.Table.create () in
    let postconditions = Int.Table.create () in

    let join_with_predecessors_postconditions node state =
      if Int.equal (Cfg.Node.id node) initial_index then
        State.join state initial
      else
        predecessors node
        |> Set.fold ~init:state ~f:(fun sofar predecessor_index ->
               Hashtbl.find postconditions predecessor_index
               |> Option.value ~default:State.bottom
               |> State.join sofar)
    in
    let analyze_node node =
      let node_id = Cfg.Node.id node in
      let precondition =
        Hashtbl.find preconditions node_id
        |> Option.value ~default:State.bottom
        |> join_with_predecessors_postconditions node
      in
      Hashtbl.set preconditions ~key:node_id ~data:precondition;
      let postcondition = transition node_id precondition (Cfg.Node.statements node) in
      Hashtbl.set postconditions ~key:node_id ~data:postcondition
    in
    let rec analyze_component = function
      | { WeakTopologicalOrder.Component.kind = Node node; _ } -> analyze_node node
      | { kind = Cycle { head; components }; _ } ->
          let head_id = Cfg.Node.id head in
          let rec iterate local_iteration =
            analyze_node head;
            List.iter ~f:analyze_component components;
            let current_head_precondition = Hashtbl.find_exn preconditions head_id in
            let new_head_precondition =
              join_with_predecessors_postconditions head current_head_precondition
            in
            let converged =
              State.less_or_equal ~left:new_head_precondition ~right:current_head_precondition
            in
            Log.log
              ~section:`Fixpoint
              "\n%a\n  { <= (result %b) (iteration = %d) }\n\n%a"
              State.pp
              new_head_precondition
              converged
              local_iteration
              State.pp
              current_head_precondition;
            if not converged then (
              let precondition =
                State.widen
                  ~previous:current_head_precondition
                  ~next:new_head_precondition
                  ~iteration:local_iteration
              in
              Hashtbl.set preconditions ~key:head_id ~data:precondition;
              iterate (local_iteration + 1))
            else
              (* At this point, we know we have a local fixpoint.
               * Since operators are monotonic, `new_head_precondition` is also
               * a post fixpoint. This is basically the argument for performing
               * decreasing iteration sequence with a narrowing operator.
               * Therefore, `new_head_precondition` might be more precise,
               * let's use it at the result.
               *)
              Hashtbl.set preconditions ~key:head_id ~data:new_head_precondition
          in
          iterate 0
    in
    List.iter ~f:analyze_component components;
    { preconditions; postconditions }


  let forward ~cfg ~initial =
    let transition node_id init statements =
      let forward statement_index before statement =
        let statement_key = [%hash: int * int] (node_id, statement_index) in
        let after = State.forward ~statement_key before ~statement in
        Log.log
          ~section:`Fixpoint
          "\n%a\n  {  %a  }\n\n%a"
          State.pp
          before
          Statement.pp
          statement
          State.pp
          after;
        after
      in
      List.foldi ~f:forward ~init statements
    in
    compute_fixpoint
      cfg
      ~initial_index:Cfg.entry_index
      ~initial
      ~predecessors:Cfg.Node.predecessors
      ~successors:Cfg.Node.successors
      ~transition


  let backward ~cfg ~initial =
    let transition node_id init statements =
      let statement_index = ref (List.length statements) in
      let backward statement =
        statement_index := !statement_index - 1;
        let statement_key = [%hash: int * int] (node_id, !statement_index) in
        State.backward ~statement_key ~statement
      in
      List.fold_right ~f:backward ~init statements
    in
    compute_fixpoint
      cfg
      ~initial_index:Cfg.exit_index
      ~initial
      ~predecessors:Cfg.Node.successors
      ~successors:Cfg.Node.predecessors
      ~transition
end
