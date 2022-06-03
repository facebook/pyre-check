(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis
open Interprocedural
open Domains

val at_callsite
  :  resolution:Resolution.t ->
  get_callee_model:(Target.t -> Model.t option) ->
  call_target:Target.t ->
  arguments:Expression.Call.Argument.t list ->
  Model.t

module ArgumentMatches : sig
  type t = {
    argument: Expression.t;
    sink_matches: AccessPath.argument_match list;
    tito_matches: AccessPath.argument_match list;
    sanitize_matches: AccessPath.argument_match list;
  }
end

val match_actuals_to_formals
  :  model:Model.t ->
  arguments:Expression.Call.Argument.t list ->
  ArgumentMatches.t list

(* A mapping from a taint-in-taint-out kind (e.g, `Sinks.LocalReturn`, `Sinks.ParameterUpdate` or
   `Sinks.AddFeatureToArgument`) to a tito taint (including features, return paths, depth). *)
module TaintInTaintOutMap : sig
  type t = (Sinks.t, BackwardState.Tree.t) Map.Poly.t

  val fold : t -> init:'a -> f:(kind:Sinks.t -> tito_tree:BackwardState.Tree.t -> 'a -> 'a) -> 'a
end

val taint_in_taint_out_mapping
  :  transform_non_leaves:(Features.ReturnAccessPath.t -> BackwardTaint.t -> BackwardTaint.t) ->
  model:Model.t ->
  tito_matches:AccessPath.argument_match list ->
  sanitize_matches:AccessPath.argument_match list ->
  TaintInTaintOutMap.t

val return_paths : kind:Sinks.t -> tito_taint:BackwardTaint.t -> Abstract.TreeDomain.Label.path list

val sink_trees_of_argument
  :  resolution:Resolution.t ->
  transform_non_leaves:(Features.ReturnAccessPath.t -> BackwardTaint.t -> BackwardTaint.t) ->
  model:Model.t ->
  location:Location.WithModule.t ->
  call_target:CallGraph.CallTarget.t ->
  arguments:Expression.Call.Argument.t list ->
  sink_matches:AccessPath.argument_match list ->
  is_self_call:bool ->
  caller_class_interval:Interprocedural.ClassIntervalSet.t ->
  receiver_class_interval:Interprocedural.ClassIntervalSet.t ->
  Issue.SinkTreeWithHandle.t list
