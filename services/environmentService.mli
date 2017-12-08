(** Copyright 2016-present Facebook. All rights reserved. **)

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
