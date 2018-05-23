(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

val run
  :  bool
  -> string option
  -> string list
  -> bool
  -> bool
  -> bool
  -> bool
  -> bool
  -> bool
  -> bool
  -> bool
  -> int
  -> string
  -> string
  -> string list
  -> string
  -> unit
  -> unit

val command: Core.Command.t
