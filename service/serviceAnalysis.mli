(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Analysis


val analyze
  :  scheduler: ServiceScheduler.t
  -> configuration: Configuration.t
  -> environment: (module Environment.Handler)
  -> call_graph: (module CallGraph.Handler)
  -> handles: File.Handle.t list
  -> unit
