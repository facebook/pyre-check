(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression


type t = {
  overloads: Access.t Access.Table.t;
  callers: Access.t list;
  call_edges: Access.t Access.Table.t;
}

val create: unit -> t

module type Handler = sig
  (* Module providing interface to access data structure. *)
  val register_overload: access: Access.t -> overload: Access.t -> unit
  val register_caller: path: string -> caller: Access.t -> unit
  val register_call_edge: caller: Access.t -> callee: Access.t -> unit
  val callers: path: string -> Access.t list option
  val callees: caller: Access.t -> Access.t list option
end

val handler: t -> (module Handler)

val stub: unit -> (module Handler)
