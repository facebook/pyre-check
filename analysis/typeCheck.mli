(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Statement
module Error = AnalysisError

module ErrorMap : sig
  type key = {
    location: Location.t;
    kind: int;
  }
  [@@deriving compare, sexp]

  module Map : Map.S with type Key.t = key

  type t = Error.t Map.t

  val add : errors:t -> Error.t -> t
end

module type Context = sig
  val qualifier : Reference.t

  val debug : bool

  val define : Define.t Node.t

  module Builder : Callgraph.Builder
end

module type Signature = sig
  type t [@@deriving eq]

  val create
    :  ?bottom:bool ->
    ?errors:ErrorMap.t ->
    resolution:Resolution.t ->
    ?resolution_fixpoint:LocalAnnotationMap.t ->
    unit ->
    t

  val resolution : t -> Resolution.t

  val error_map : t -> ErrorMap.t

  val errors : t -> Error.t list

  val initial : resolution:Resolution.t -> t

  type base =
    | Class of Type.t
    | Instance of Type.t
    | Super of Type.t

  and resolved = {
    state: t;
    resolved: Type.t;
    resolved_annotation: Annotation.t option;
    base: base option;
  }
  [@@deriving show]

  val parse_and_check_annotation : ?bind_variables:bool -> state:t -> Expression.t -> t * Type.t

  val forward_expression : state:t -> expression:Expression.t -> resolved

  val forward_statement : state:t -> statement:Statement.t -> t

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
  LocalAnnotationMap.t option

val resolution_with_key
  :  global_resolution:GlobalResolution.t ->
  local_annotations:LocalAnnotationMap.t option ->
  parent:Reference.t option ->
  key:int option ->
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
