(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast

type t = Location.t Location.Table.t

val create: unit -> t

val update
  :  t
  -> element:Annotated.Access.Element.t
  -> unit

val get_definition
  : t
  -> Location.position
  -> Location.t option
