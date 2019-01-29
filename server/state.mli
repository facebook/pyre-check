(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Network

module Error = Analysis.Error


type connections = {
  socket: Socket.t;
  persistent_clients: int Socket.Table.t;
  file_notifiers: Socket.t list;
  watchman_pid: Pid.t option;
}

type lookups_cache_entry = {
  table: Analysis.Lookup.t;
  source: string;
}

module Deferred : sig
  type t

  val of_list: File.t list -> t
  val add: t -> File.Set.t -> t
  val take_n: elements: int -> t -> (File.t list * t)
  val take_all: t -> (File.t list * t)
  val is_empty: t -> bool
  val length: t -> int
end

type t = {
  deferred_state: Deferred.t;
  environment: (module Analysis.Environment.Handler);
  errors: (Error.t list) File.Handle.Table.t;
  lookups: lookups_cache_entry String.Table.t;
  scheduler: Scheduler.t;
  lock: Mutex.t;
  last_integrity_check: float;
  last_request_time: float;
  connections: connections ref;
}
