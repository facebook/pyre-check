(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

type errors = (File.Handle.t * State.Error.t list) list

val build_file_to_error_map
  :  ?checked_files: File.Handle.t list option
  -> state: State.t
  -> State.Error.t list
  -> errors

val recheck
  :  state: State.t
  -> configuration: Configuration.Analysis.t
  -> files: File.t list
  -> should_analyze_dependencies: bool
  -> State.t * errors
