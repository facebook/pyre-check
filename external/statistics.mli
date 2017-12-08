(** Copyright 2016-present Facebook. All rights reserved. **)

open Core

val performance
  :  ?flush: bool
  -> name: string
  -> time: int
  -> labels: (string * string) list
  -> (unit, Unix.Exit_or_signal.error) result

val coverage
  :  ?flush: bool
  -> coverage: (string * int) list
  -> labels: (string * string) list
  -> (unit, Unix.Exit_or_signal.error) result
