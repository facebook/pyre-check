(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Analysis
open Pyre

val in_process_reader
  :  Service.t
  -> configuration: Configuration.t
  -> stubs: File.Handle.t list
  -> sources: File.Handle.t list
  -> (module Environment.Reader)

val shared_memory_reader
  :  Service.t
  -> configuration: Configuration.t
  -> stubs: File.Handle.t list
  -> sources: File.Handle.t list
  -> (module Environment.Reader)

val repopulate
  :  (module Environment.Reader)
  -> root: Path.t
  -> handles: File.Handle.t list
  -> unit
