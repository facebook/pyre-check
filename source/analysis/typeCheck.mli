(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Statement
module Error = AnalysisError

module LocalErrorMap : sig
  type t

  val empty : unit -> t

  val all_errors : t -> Error.t list
end

module type Context = sig
  val qualifier : Reference.t

  val debug : bool

  val constraint_solving_style : Configuration.Analysis.constraint_solving_style

  val define : Define.t Node.t

  (* Where to store local annotations during the fixpoint. `None` discards them. *)
  val resolution_fixpoint : LocalAnnotationMap.t option

  (* Where to store errors found during the fixpoint. `None` discards them. *)
  val error_map : LocalErrorMap.t option

  module Builder : Callgraph.Builder
end

module type Signature = sig
  type t [@@deriving eq]

  val create : resolution:Resolution.t -> t

  val unreachable : t

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
       SignatureSelectionTypes.instantiated_return_annotation) ->
  global_resolution:GlobalResolution.t ->
  Type.t ->
  TypeOperation.callable_and_self_argument option

module State (Context : Context) : Signature

module DummyContext : Context

val resolution
  :  GlobalResolution.t ->
  ?annotation_store:Refinement.Store.t ->
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
  statement_key:int ->
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
