(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis

module Error = PyreError

type result = {
  handles: File.Handle.t list;
  environment: (module Environment.Reader);
  errors: Error.t list
}

val check: Configuration.t -> Service.t option -> unit -> result

val spec
  : (bool
     -> string list
     -> bool
     -> bool
     -> bool
     -> bool
     -> bool
     -> bool
     -> bool
     -> bool
     -> string list
     -> string
     -> unit
     -> unit,
     unit -> unit)
    Command.Spec.t

val run_check
  : bool
  -> string list
  -> bool
  -> bool
  -> bool
  -> bool
  -> bool
  -> bool
  -> bool
  -> bool
  -> string list
  -> string
  -> unit
  -> unit
val check_command: Command.t

val run_incremental
  :  bool
  -> bool
  -> string list
  -> bool
  -> bool
  -> bool
  -> bool
  -> bool
  -> bool
  -> bool
  -> bool
  -> string list
  -> string
  -> unit
  -> unit
val incremental_command: Command.t
