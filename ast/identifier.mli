(** Copyright 2016-present Facebook. All rights reserved. **)

open Core

type t
[@@deriving compare, eq, sexp, show]

module Map : Map.S with type Key.t = t

val create: string -> t

val length: t -> int
val append: separator:string -> t -> t -> t
