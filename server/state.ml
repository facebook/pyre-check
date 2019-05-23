(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

open Network

module Error = Analysis.Error

type connections = {
  socket: Socket.t;
  json_socket: Socket.t;
  persistent_clients: int Socket.Table.t;
  file_notifiers: Socket.t list;
}

type t = {
  environment: (module Analysis.Environment.Handler);
  errors: (Error.t list) File.Handle.Table.t;
  lookups: Analysis.Lookup.t String.Table.t;
  scheduler: Scheduler.t;
  lock: Mutex.t;
  last_integrity_check: float;
  last_request_time: float;
  connections: connections ref;
}
