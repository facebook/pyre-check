(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Analysis
open Ast
open Statement
open Interprocedural


val record_and_merge_call_graph
  : environment: (module Environment.Handler)
  -> call_graph: DependencyGraph.callgraph
  -> path: File.Handle.t
  -> source: Source.t
  -> DependencyGraph.callgraph

val record_overrides: DependencyGraph.overrides -> unit

val record_path_of_definitions
  : path: File.Handle.t
  -> source: Source.t
  -> (Callable.real_target * Define.t Node.t) list

(** Populates shared memory with models from model sources. NOTE: since models
    can be split across multiple files, it is important to pass all model sources
    to this function at once as it merges them.  *)
val add_models: environment: (module Environment.Handler) -> string list -> unit

val analyze
  :  ?taint_models_directory: string
  -> scheduler: Scheduler.t
  -> configuration: Configuration.StaticAnalysis.t
  -> environment: (module Environment.Handler)
  -> handles: File.Handle.t list
  -> unit
  -> Interprocedural.Error.t list
