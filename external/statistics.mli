(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

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
