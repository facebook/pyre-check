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

module Deferred = struct
  type t = {
    files_to_analyze_ordered: File.t list;
    files_to_analyze: File.Set.t;
  }

  let of_list files =
    {
      files_to_analyze_ordered = files;
      files_to_analyze = File.Set.of_list files;
    }

  let add { files_to_analyze_ordered; files_to_analyze } files =
    let files = Set.diff files files_to_analyze in
    {
      files_to_analyze_ordered = List.append files_to_analyze_ordered (File.Set.to_list files);
      files_to_analyze = Set.union files files_to_analyze;
    }

  let take_n ~elements { files_to_analyze_ordered; files_to_analyze } =
    let taken, remaining = List.split_n files_to_analyze_ordered elements in
    let state =
      {
        files_to_analyze_ordered = remaining;
        files_to_analyze = Set.diff files_to_analyze (File.Set.of_list taken);
      }
    in
    taken, state

  let take_all { files_to_analyze_ordered; _ } =
    let state =
      {
        files_to_analyze_ordered = [];
        files_to_analyze = File.Set.empty;
      }
    in
    files_to_analyze_ordered, state

  let is_empty { files_to_analyze; _ } =
    File.Set.is_empty files_to_analyze

  let length { files_to_analyze; _ } =
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
