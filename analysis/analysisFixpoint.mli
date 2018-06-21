(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast

module Cfg = AnalysisCfg


module type State = sig
  type t
  [@@deriving eq, show]
  val less_or_equal: left: t -> right: t -> bool
  val join: t -> t -> t
  val meet: t -> t -> t
  val widening_threshold: int
  val widen: previous: t -> next: t -> iteration: int -> t
  val forward: ?key:int -> t -> statement: Statement.t -> t
  val backward: t -> statement: Statement.t -> t
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

module Make(State: State) : sig
  type t = State.t Int.Table.t
  [@@deriving eq, show]

  val entry: t -> State.t option
  val exit: t -> State.t option

  val forward: cfg: Cfg.t -> initial: State.t -> t
  val backward: cfg: Cfg.t -> initial: State.t -> t
end
