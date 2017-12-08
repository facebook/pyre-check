(** Copyright 2016-present Facebook. All rights reserved. **)

type t

val start: unit -> t
val stop: t -> float
val stop_in_ms: t -> int
