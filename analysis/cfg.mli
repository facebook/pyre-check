(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Statement

module Node : sig
  type kind =
    | Block of Statement.t list
    | Dispatch
    | Entry
    | Error
    | Normal
    | Final
    | For of For.t
    | If of Statement.t If.t
    | Join
    | Try of Try.t
    | With of With.t
    | While of Statement.t While.t
    | Yield
  [@@deriving compare, eq, show]

  type t [@@deriving compare, eq]

  val create : int -> kind -> Int.Set.t -> Int.Set.t -> t

  val statements : t -> Statement.t list

  val successors : t -> Int.Set.t

  val predecessors : t -> Int.Set.t

  val description : t -> string
end

type t = Node.t Int.Table.t [@@deriving eq, show]

val entry_index : int

val normal_index : int

val exit_index : int

val to_dot
  :  ?precondition:(int -> string) ->
  ?sort_labels:bool ->
  (int, Node.t) Hashtbl.t ->
  string

val create : Define.t -> t

val node : t -> id:int -> Node.t
