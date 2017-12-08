(** Copyright 2016-present Facebook. All rights reserved. **)

open Core

open Analysis

module Error = PyreError
module Socket = PyreSocket

type connections = {
  socket: Socket.t;
  persistent_clients: Socket.t list;
  file_notifiers: Socket.t list;
}

type t = {
  deferred_requests: Protocol.Request.t list;
  environment: (module Analysis.Environment.Reader);
  initial_errors: Error.Hash_set.t;
  errors: (Error.t list) File.Handle.Table.t;
  handles: File.Handle.Set.t;
  lookups: Analysis.Lookup.t String.Table.t;
  service: Service.t;
  lock: Mutex.t;
  connections: connections ref;
}
