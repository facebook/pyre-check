(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


exception IncompatibleState of string

val load
  :  server_configuration: Configuration.Server.t
  -> lock: Mutex.t
  -> connections: State.connections ref
  -> State.t

val save
  :  configuration: Configuration.Analysis.t
  -> errors: (Analysis.Error.t list) File.Handle.Table.t
  -> saved_state_path: string
  -> unit
