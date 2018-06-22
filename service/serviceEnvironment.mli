(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Pyre
open Analysis


val populate
  :  (module Environment.Handler)
  -> configuration: Configuration.t
  -> ?source_root: Path.t
  -> ?check_dependency_exists: bool
  -> Source.t list
  -> unit

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
