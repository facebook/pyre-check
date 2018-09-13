(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


val load
  :  server_configuration: ServerConfiguration.t
  -> lock: Mutex.t
  -> connections: State.connections ref
  -> State.t

val save: saved_state_path: string -> unit
