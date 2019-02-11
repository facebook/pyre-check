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
    visited_since_last_reset: File.Set.t;
  }

  let of_list files =
    {
      files_to_analyze_ordered = files;
      files_to_analyze = File.Set.of_list files;
      visited_since_last_reset = File.Set.empty;
    }

  let add { files_to_analyze_ordered; files_to_analyze; visited_since_last_reset } ~files =
    let files =
      let files = Set.diff files files_to_analyze in
      Set.diff files visited_since_last_reset
    in
    {
      files_to_analyze_ordered = List.append files_to_analyze_ordered (File.Set.to_list files);
      files_to_analyze = Set.union files files_to_analyze;
      visited_since_last_reset = visited_since_last_reset;
    }

  let take_n { files_to_analyze_ordered; files_to_analyze; visited_since_last_reset } ~elements =
    let taken, remaining = List.split_n files_to_analyze_ordered elements in
    let taken_set = (File.Set.of_list taken) in
    let state =
      {
        files_to_analyze_ordered = remaining;
        files_to_analyze = Set.diff files_to_analyze taken_set;
        visited_since_last_reset = Set.union visited_since_last_reset taken_set;
      }
    in
    taken, state

  let take_all { files_to_analyze_ordered; files_to_analyze; visited_since_last_reset; } =
    let state =
      {
        files_to_analyze_ordered = [];
        files_to_analyze = File.Set.empty;
        visited_since_last_reset = Set.union visited_since_last_reset files_to_analyze;
      }
    in
    files_to_analyze_ordered, state

  let add_visited ({ visited_since_last_reset; _ } as state) ~visited =
    { state with visited_since_last_reset = Set.union visited_since_last_reset visited }

  let reset_visited state =
    { state with visited_since_last_reset = File.Set.empty }

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
