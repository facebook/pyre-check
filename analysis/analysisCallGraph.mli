(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression


type t = {
  overloads: Access.t Access.Table.t;
}

val create: unit -> t

module type Handler = sig
  (* Module providing interface to access data structure. *)
  val register_overload: access: Access.t -> overload: Access.t -> unit
end

val handler: t -> (module Handler)
