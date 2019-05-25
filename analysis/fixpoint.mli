(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast

module type State = sig
  type t [@@deriving show]

  val less_or_equal : left:t -> right:t -> bool

  val join : t -> t -> t

  val widen : previous:t -> next:t -> iteration:int -> t

  val forward : ?key:int -> t -> statement:Statement.t -> t

  val backward : ?key:int -> t -> statement:Statement.t -> t
end

module type Fixpoint = sig
  type state

  (* Mapping from node to preconditions. *)
  type t = state Int.Table.t [@@deriving show]

  val entry : t -> state option

  val normal_exit : t -> state option

  val exit : t -> state option

  val forward : cfg:Cfg.t -> initial:state -> t

  val backward : cfg:Cfg.t -> initial:state -> t

  val equal : f:(state -> state -> bool) -> t -> t -> bool
end

module Make (State : State) : Fixpoint with type state = State.t
