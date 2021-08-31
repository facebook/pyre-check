(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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

  val create : resolution:Resolution.t -> unit -> t

  val create_unreachable : unit -> t

  val all_errors : t -> Error.t list

  val resolution : t -> Resolution.t option

  val initial : resolution:Resolution.t -> t

  val parse_and_check_annotation
    :  ?bind_variables:bool ->
    resolution:Resolution.t ->
    Expression.t ->
    Error.t list * Type.t

  include Fixpoint.State with type t := t
end

val unpack_callable_and_self_argument
  :  signature_select:
       (arguments:AttributeResolution.Argument.t list ->
       callable:Type.Callable.t ->
       self_argument:Type.t option ->
       SignatureSelectionTypes.sig_t) ->
  global_resolution:GlobalResolution.t ->
  Type.t ->
  TypeOperation.callable_and_self_argument option

module State (Context : Context) : Signature

module DummyContext : Context

val resolution
  :  GlobalResolution.t ->
  ?annotation_store:Resolution.annotation_store ->
  (module Context) ->
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
  (module Context) ->
  Resolution.t

val name : string

val run_on_defines
  :  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  environment:TypeEnvironment.t ->
  ?call_graph_builder:(module Callgraph.Builder) ->
  (Ast.Reference.t * SharedMemoryKeys.DependencyKey.registered option) list ->
  unit

val legacy_run_on_modules
  :  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  environment:TypeEnvironment.t ->
  ?call_graph_builder:(module Callgraph.Builder) ->
  Ast.Reference.t list ->
  unit
