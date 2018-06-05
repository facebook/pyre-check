(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Pyre

module Cfg = AnalysisCfg

module type State = sig
  type t
  [@@deriving eq, show]
  val less_or_equal: left: t -> right: t -> bool
  val join: t -> t -> t
  val meet: t -> t -> t
  val widening_threshold: int
  val widen: previous: t -> next: t -> iteration: int -> t
  val forward: t -> statement: Ast.Statement.t -> t
  val backward: t -> statement: Ast.Statement.t -> t
end

module type Fixpoint = sig
  type state
  (* Mapping from node to preconditions. *)
  type t = state Int.Table.t
  [@@deriving eq, show]

  val entry: t -> state option
  val exit: t -> state option

  val forward: cfg: Cfg.t -> initial: state -> t
  val backward: cfg: Cfg.t -> initial: state -> t
end

module Make (State: State) = struct
  type t = State.t Int.Table.t

  let equal left right =
    Hashtbl.equal left right State.equal

  let pp format fixpoint =
    let print_state ~key ~data =
      Format.fprintf format "%d -> %a\n" key State.pp data in
    Hashtbl.iteri fixpoint ~f:print_state

  let show fixpoint =
    Format.asprintf "%a" pp fixpoint

  let entry fixpoint =
    Hashtbl.find fixpoint Cfg.entry_index

  let exit fixpoint =
    Hashtbl.find fixpoint Cfg.exit_index

  let widening_threshold =
    State.widening_threshold

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
          let postcondition = transition precondition (Cfg.Node.statements current) in

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
    let transition init statements =
      let forward before statement =
        let after = State.forward before ~statement in
        Log.log
          ~section:`Fixpoint
          "\n%a\n  {  %a  }\n\n%a"
          State.pp before
          Ast.Statement.pp statement
          State.pp after;
        after
      in
      List.fold_left ~f:forward ~init statements
    in
    compute_fixpoint
      cfg
      ~initial_index:Cfg.entry_index
      ~initial
      ~successors:Cfg.Node.successors
      ~transition

  let backward ~cfg ~initial =
    let transition init statements =
      List.fold_right ~f:(fun statement -> State.backward ~statement) ~init statements
    in
    compute_fixpoint
      cfg
      ~initial_index:Cfg.exit_index
      ~initial
      ~successors:Cfg.Node.predecessors
      ~transition
end
