(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast

type parse_result =
  | Success of Source.t
  | SyntaxError of string
  | SystemError of string

val parse_source : configuration:Configuration.Analysis.t -> SourcePath.t -> parse_result

type parse_sources_result = {
  parsed: Reference.t list;
  syntax_error: SourcePath.t list;
  system_error: SourcePath.t list;
}

val parse_sources
  :  configuration:Configuration.Analysis.t ->
  scheduler:Scheduler.t ->
  preprocessing_state:ProjectSpecificPreprocessing.state option ->
  ast_environment:Analysis.AstEnvironment.t ->
  SourcePath.t list ->
  parse_sources_result

val parse_all
  :  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  Analysis.ModuleTracker.t ->
  Source.t list * Analysis.AstEnvironment.t

(* Update the AstEnvironment and return the list of module names whose parsing result may have
   changed *)
val update
  :  configuration:Configuration.Analysis.t ->
  scheduler:Scheduler.t ->
  ast_environment:Analysis.AstEnvironment.t ->
  Analysis.ModuleTracker.IncrementalUpdate.t list ->
  Reference.t list
