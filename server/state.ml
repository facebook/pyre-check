(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Network

module Error = Analysis.Error


type connections = {
  socket: Socket.t;
  json_socket: Socket.t;
  persistent_clients: int Socket.Table.t;
  file_notifiers: Socket.t list;
  watchman_pid: Pid.t option;
}

type lookups_cache_entry = {
  table: Analysis.Lookup.t;
  source: string;
}

module Deferred = struct
  type t = File.Set.t

  let of_list files = File.Set.of_list files

  let add files_to_analyze ~files =
    Set.union files_to_analyze files

  let take_n files_to_analyze ~elements =
    let taken, remaining =
      File.Set.to_list files_to_analyze
      |> (fun to_analyze -> List.split_n to_analyze elements)
    in
    taken, File.Set.of_list remaining

  let is_empty files_to_analyze =
    File.Set.is_empty files_to_analyze

  let length files_to_analyze =
    File.Set.length files_to_analyze
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
