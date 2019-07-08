(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Analysis
open Ast
open Statement
open Interprocedural

val record_and_merge_call_graph
  :  environment:(module Environment.Handler) ->
  call_graph:DependencyGraph.callgraph ->
  qualifier:Reference.t ->
  source:Source.t ->
  DependencyGraph.callgraph

val record_overrides : DependencyGraph.overrides -> unit

val callables
  :  resolution:Resolution.t ->
  source:Source.t ->
  (Callable.real_target * Define.t Node.t) list

val analyze
  :  scheduler:Scheduler.t ->
  configuration:Configuration.StaticAnalysis.t ->
  environment:(module Environment.Handler) ->
  qualifiers:Reference.t list ->
  unit ->
  Interprocedural.Error.t list
