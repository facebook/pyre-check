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
}

val check: Configuration.t -> Scheduler.t option -> unit -> result

val check_command: Command.t
