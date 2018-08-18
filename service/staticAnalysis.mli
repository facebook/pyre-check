(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Analysis
open Ast
open Statement


val overrides_of_source
  :  environment: (module Environment.Handler)
  -> source: Source.t
  -> (Access.t list) Access.Map.t

val record_and_merge_call_graph
  :  environment: (module Environment.Handler)
  -> call_graph: CallGraph.t
  -> path: File.Handle.t
  -> source: Source.t
  -> CallGraph.t

val record_overrides: environment: (module Environment.Handler) -> source: Source.t -> unit

val record_path_of_definitions: path: File.Handle.t -> source: Source.t -> Define.t Node.t list

(** Populates shared memory with preexisting models. *)
val add_models: model_source: string -> unit

val analyze
  :  ?taint_models_directory: string
  -> scheduler: Scheduler.t
  -> configuration: Configuration.t
  -> environment: (module Environment.Handler)
  -> handles: File.Handle.t list
  -> unit
  -> Interprocedural.Error.t list
