(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open Configuration

exception IncompatibleState

val load
  :  server_configuration: ServerConfiguration.t
  -> lock: Mutex.t
  -> connections: State.connections ref
  -> State.t

val save
  :  configuration: Configuration.t
  -> errors: (Analysis.Error.t list) File.Handle.Table.t
  -> saved_state_path: string
  -> unit
