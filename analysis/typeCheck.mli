(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Statement
module Error = AnalysisError

module type Context = sig
  val qualifier : Reference.t

  val debug : bool

  val define : Define.t Node.t

  module Builder : Callgraph.Builder
end

module type Signature = sig
  type t [@@deriving eq]

  val create : ?bottom:bool -> resolution:Resolution.t -> unit -> t

  val resolution : t -> Resolution.t

  (* Expose for testing *)
  val local_errors : t -> Error.t list

  val all_errors : t -> Error.t list

  val initial : resolution:Resolution.t -> t

  val parse_and_check_annotation : ?bind_variables:bool -> state:t -> Expression.t -> t * Type.t

  include Fixpoint.State with type t := t
end

module State (Context : Context) : Signature

val resolution
  :  GlobalResolution.t ->
  ?annotation_store:RefinementUnit.t Reference.Map.t ->
  unit ->
  Resolution.t

val get_or_recompute_local_annotations
  :  environment:TypeEnvironment.ReadOnly.t ->
  Reference.t ->
  LocalAnnotationMap.ReadOnly.t option

val resolution_with_key
  :  global_resolution:GlobalResolution.t ->
  local_annotations:LocalAnnotationMap.ReadOnly.t option ->
  parent:Reference.t option ->
  key:int ->
  Resolution.t

val name : string

val run_on_defines
  :  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  environment:TypeEnvironment.t ->
  ?call_graph_builder:(module Callgraph.Builder) ->
  Ast.Reference.t list ->
  unit

val legacy_run_on_modules
  :  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  environment:TypeEnvironment.t ->
  ?call_graph_builder:(module Callgraph.Builder) ->
  Ast.Reference.t list ->
  unit
