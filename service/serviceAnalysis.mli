(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Analysis
open Expression


val call_graph_of_source:
  (module Environment.Handler)
  -> Source.t
  -> (Access.t list) Access.Map.t

val overrides_of_source:
  (module Environment.Handler)
  -> Source.t
  -> (Access.t list) Access.Map.t

val analyze
  :  scheduler: ServiceScheduler.t
  -> configuration: Configuration.t
  -> environment: (module Environment.Handler)
  -> handles: File.Handle.t list
  -> unit
