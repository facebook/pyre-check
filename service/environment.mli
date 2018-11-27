(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Analysis

val populate
  :  (module Environment.Handler)
  -> configuration: Configuration.Analysis.t
  -> Source.t list
  -> unit

(* Exposed in order to support loading saved states. *)
module SharedHandler: Environment.Handler

val populate_shared_memory
  :  configuration: Configuration.Analysis.t
  -> stubs: File.Handle.t list
  -> sources: File.Handle.t list
  -> unit
