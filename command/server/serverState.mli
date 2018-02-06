(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis
open Service

module Error = Error
module Socket = CommandSocket


type client = {
  failures: int;
}

type connections = {
  socket: Socket.t;
  persistent_clients: client Unix.File_descr.Table.t;
  file_notifiers: Socket.t list;
}

type t = {
  deferred_requests: ServerProtocol.Request.t list;
  environment: (module Analysis.Environment.Handler);
  initial_errors: Error.Hash_set.t;
  errors: (Error.t list) File.Handle.Table.t;
  handles: File.Handle.Set.t;
  lookups: Analysis.Lookup.t String.Table.t;
  scheduler: Scheduler.t;
  lock: Mutex.t;
  last_request_time: float;
  connections: connections ref;
}

val failure_threshold: int

val stop_after_idle_for: float
