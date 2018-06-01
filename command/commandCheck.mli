(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis

module Scheduler = Service.Scheduler


type result = {
  handles: File.Handle.t list;
  environment: (module Environment.Handler);
  errors: Error.t list;
  lookups: Lookup.t String.Map.t;
}

val check: Configuration.t -> Scheduler.t option -> unit -> result

val run_check
  : bool
  -> string option
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
  -> string option
  -> string
  -> unit
  -> unit
val check_command: Command.t
