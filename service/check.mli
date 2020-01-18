(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Analysis

type result = {
  module_tracker: ModuleTracker.t;
  ast_environment: Analysis.AstEnvironment.t;
  environment: TypeEnvironment.t;
  errors: Error.t list;
}

val check
  :  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  build_legacy_dependency_graph:bool ->
  call_graph_builder:(module Callgraph.Builder) ->
  result
