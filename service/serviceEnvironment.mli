(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Analysis


val in_process_handler
  :  configuration: Configuration.t
  -> stubs: File.Handle.t list
  -> sources: File.Handle.t list
  -> (module Environment.Handler)

val shared_memory_handler
  :  configuration: Configuration.t
  -> stubs: File.Handle.t list
  -> sources: File.Handle.t list
  -> (module Environment.Handler)
