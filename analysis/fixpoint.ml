(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Pyre


module type State = sig
  type t
  [@@deriving show]
  val less_or_equal: left: t -> right: t -> bool
  val join: t -> t -> t
  val widen: previous: t -> next: t -> iteration: int -> t
  val forward: ?key:int -> t -> statement: Statement.t -> t
  val backward: ?key:int -> t -> statement: Statement.t -> t
end

module type Fixpoint = sig
  type state
  (* Mapping from node to preconditions. *)
  type t = state Int.Table.t
  [@@deriving show]

  val entry: t -> state option
  val normal_exit: t -> state option
  val exit: t -> state option

  val forward: cfg: Cfg.t -> initial: state -> t
  val backward: cfg: Cfg.t -> initial: state -> t

  val equal: f: (state -> state -> bool) -> t -> t -> bool
end

module Make (State: State) = struct
  type state = State.t
  type t = State.t Int.Table.t

  let equal ~f left right =
    Int.Table.equal left right f

  let pp format fixpoint =
    let print_state ~key ~data =
      Format.fprintf format "%d -> %a\n" key State.pp data in
    Hashtbl.iteri fixpoint ~f:print_state

  let show fixpoint =
    Format.asprintf "%a" pp fixpoint

  let entry fixpoint =
    Hashtbl.find fixpoint Cfg.entry_index

  let normal_exit fixpoint =
    Hashtbl.find fixpoint Cfg.normal_index

  let exit fixpoint =
    Hashtbl.find fixpoint Cfg.exit_index

  let compute_fixpoint cfg ~initial_index ~initial ~successors ~transition =
    let fixpoint = Int.Table.create () in
    Hashtbl.set fixpoint ~key:initial_index ~data:initial;

    let iterations = Int.Table.create () in
    Hashtbl.set iterations ~key:initial_index ~data:0;

    let worklist = Queue.create () in
    Queue.enqueue worklist initial_index;

    let rec iterate worklist =
      match Queue.dequeue worklist with
      | Some current_id ->
          let current = Cfg.node cfg ~id:current_id in

          (* Transfer state. *)
          let precondition = Hashtbl.find_exn fixpoint current_id in
          let postcondition = transition current_id precondition (Cfg.Node.statements current) in

          (* Update successors. *)
          let update_successor successor_id =
            match Hashtbl.find fixpoint successor_id with
            | Some successor_precondition ->
                let iteration = Hashtbl.find_exn iterations successor_id in
                let widened =
                  State.widen
                    ~previous:successor_precondition
                    ~next:postcondition
                    ~iteration
                in

                let converged =
                  State.less_or_equal
                    ~left:widened
                    ~right:successor_precondition
                in
                Log.log
                  ~section:`Fixpoint
                  "\n%a\n  { <= (result %b) (iteration = %d) }\n\n%a"
                  State.pp widened
                  converged
                  iteration
                  State.pp successor_precondition;

                if not converged then
                  begin
                    Hashtbl.set fixpoint ~key:successor_id ~data:widened;
                    Hashtbl.set iterations ~key:successor_id ~data:(iteration + 1);
                    Queue.enqueue worklist successor_id;
                  end
            | None ->
                Hashtbl.set fixpoint ~key:successor_id ~data:postcondition;
                Hashtbl.set iterations ~key:successor_id ~data:0;
                Queue.enqueue worklist successor_id
          in

          successors current |> Set.iter ~f:update_successor;
          iterate worklist
      | None -> ()
    in

    iterate worklist;
    fixpoint

  let forward ~cfg ~initial =
    let transition node_id init statements =
      let forward statement_index before statement =
        let after =
          State.forward
            ~key:([%hash: int * int] (node_id, statement_index))
            before
            ~statement
        in
        Log.log
          ~section:`Fixpoint
          "\n%a\n  {  %a  }\n\n%a"
          State.pp before
          Statement.pp statement
          State.pp after;
        after
      in
      List.foldi ~f:forward ~init statements
    in
    compute_fixpoint
      cfg
      ~initial_index:Cfg.entry_index
      ~initial
      ~successors:Cfg.Node.successors
      ~transition

  let backward ~cfg ~initial =
    let transition node_id init statements =
      let statement_index = ref (List.length statements) in
      let backward statement =
        statement_index := !statement_index - 1;
        State.backward
          ~key:([%hash: int * int] (node_id, !statement_index))
          ~statement
      in
      List.fold_right ~f:backward ~init statements
    in
    compute_fixpoint
      cfg
      ~initial_index:Cfg.exit_index
      ~initial
      ~successors:Cfg.Node.predecessors
      ~transition
end
