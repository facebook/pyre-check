(** Copyright 2016-present Facebook. All rights reserved. **)

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
