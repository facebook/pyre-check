(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression

val add_callers: path:File.Handle.t -> Access.t list -> unit
val get_callers: path:File.Handle.t -> Access.t list option

val add_call_edges: caller: Access.t -> callees: Access.t list -> unit
val get_call_edges: caller: Access.t -> Access.t list option

val add_overrides: ancestor:Access.t -> children: Access.t list -> unit
val get_overrides: ancestor:Access.t -> Access.t list option
