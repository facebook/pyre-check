(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Analysis

type result = {
  module_tracker: ModuleTracker.t;
  ast_environment: Analysis.AstEnvironment.t;
  environment: Environment.t;
  errors: Error.t list;
}

val run_check
  :  ?open_documents:(Ast.Reference.t -> bool) ->
  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  environment:Environment.t ->
  Ast.Source.t list ->
  (module Analysis.Check.Signature) ->
  Error.t list

val analyze_sources
  :  ?open_documents:(Ast.Reference.t -> bool) ->
  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  environment:Environment.t ->
  Ast.Source.t list ->
  Error.t list

val check : scheduler:Scheduler.t option -> configuration:Configuration.Analysis.t -> result
