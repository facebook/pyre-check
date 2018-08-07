(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Analysis
open Statement


val overrides_of_source:
  (module Environment.Handler)
  -> Source.t
  -> (Access.t list) Access.Map.t

val record_and_merge_call_graph:
  (module Environment.Handler)
  -> CallGraph.t
  -> File.Handle.t
  -> Source.t
  -> CallGraph.t

val record_overrides: (module Environment.Handler) -> Source.t -> unit

val record_path_of_definitions: File.Handle.t -> Source.t -> Define.t Node.t list

(** Populates shared memory with preexisting models. *)
val add_models: model_source: string -> unit

val analyze
  :  scheduler: ServiceScheduler.t
  -> configuration: Configuration.t
  -> environment: (module Environment.Handler)
  -> handles: File.Handle.t list
  -> unit
