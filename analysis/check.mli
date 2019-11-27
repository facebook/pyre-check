(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
module Error = AnalysisError

module type Signature = sig
  val name : string

  val run
    :  configuration:Configuration.Analysis.t ->
    environment:TypeEnvironment.t ->
    source:Source.t ->
    unit
end

val get_check_to_run : check_name:string -> (module Signature) option

val run_check
  :  ?open_documents:(Ast.Reference.t -> bool) ->
  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  environment:TypeEnvironment.t ->
  Ast.Reference.t list ->
  (module Signature) ->
  unit

val analyze_sources
  :  ?open_documents:(Ast.Reference.t -> bool) ->
  ?filter_external_sources:bool ->
  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  environment:TypeEnvironment.t ->
  Ast.Reference.t list ->
  unit

val analyze_and_postprocess
  :  ?open_documents:(Ast.Reference.t -> bool) ->
  ?filter_external_sources:bool ->
  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  environment:TypeEnvironment.t ->
  Ast.Reference.t list ->
  Error.t list
