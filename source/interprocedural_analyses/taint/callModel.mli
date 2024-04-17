(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Interprocedural
open Domains

val at_callsite
  :  pyre_in_context:PyrePysaApi.InContext.t ->
  get_callee_model:(Target.t -> Model.t option) ->
  call_target:Target.t ->
  arguments:Expression.Call.Argument.t list ->
  Model.t

module ArgumentMatches : sig
  type t = {
    argument: Expression.t;
    generation_source_matches: AccessPath.argument_match list;
    sink_matches: AccessPath.argument_match list;
    tito_matches: AccessPath.argument_match list;
    sanitize_matches: AccessPath.argument_match list;
  }
  [@@deriving show]
end

val match_captures
  :  model:Model.t ->
  captures_taint:ForwardState.t ->
  location:Location.t ->
  ForwardState.Tree.t list * ArgumentMatches.t list

val captures_as_arguments : ArgumentMatches.t list -> Expression.Call.Argument.t list

val match_actuals_to_formals
  :  model:Model.t ->
  arguments:Expression.Call.Argument.t list ->
  ArgumentMatches.t list

(* A mapping from a taint-in-taint-out kind (e.g, `Sinks.LocalReturn`, `Sinks.ParameterUpdate` or
   `Sinks.AddFeatureToArgument`) to a tito taint (including features, return paths, depth) and the
   roots in the tito model whose trees contain this sink. *)
module TaintInTaintOutMap : sig
  module TreeRootsPair : sig
    type t = {
      tree: BackwardState.Tree.t;
      roots: AccessPath.Root.Set.t;
    }
  end

  type t = (Sinks.t, TreeRootsPair.t) Map.Poly.t

  val fold : t -> init:'a -> f:(kind:Sinks.t -> pair:TreeRootsPair.t -> 'a -> 'a) -> 'a
end

val taint_in_taint_out_mapping
  :  transform_non_leaves:(Features.ReturnAccessPath.t -> BackwardTaint.t -> BackwardTaint.t) ->
  taint_configuration:TaintConfiguration.Heap.t ->
  ignore_local_return:bool ->
  model:Model.t ->
  tito_matches:AccessPath.argument_match list ->
  sanitize_matches:AccessPath.argument_match list ->
  TaintInTaintOutMap.t

val return_paths_and_collapse_depths
  :  kind:Sinks.t ->
  tito_taint:BackwardTaint.t ->
  (AccessPath.Path.t * Features.CollapseDepth.t) list

val sink_trees_of_argument
  :  pyre_in_context:PyrePysaApi.InContext.t ->
  transform_non_leaves:(Features.ReturnAccessPath.t -> BackwardTaint.t -> BackwardTaint.t) ->
  model:Model.t ->
  location:Location.WithModule.t ->
  call_target:CallGraph.CallTarget.t ->
  arguments:Expression.Call.Argument.t list ->
  sink_matches:AccessPath.argument_match list ->
  is_class_method:bool ->
  is_static_method:bool ->
  call_info_intervals:Domains.ClassIntervals.t ->
  Domains.SinkTreeWithHandle.t list

val source_trees_of_argument
  :  pyre_in_context:PyrePysaApi.InContext.t ->
  model:Model.t ->
  location:Location.WithModule.t ->
  call_target:CallGraph.CallTarget.t ->
  arguments:Expression.Call.Argument.t list ->
  generation_source_matches:AccessPath.argument_match list ->
  is_class_method:bool ->
  is_static_method:bool ->
  call_info_intervals:Domains.ClassIntervals.t ->
  Domains.ForwardState.Tree.t

val type_breadcrumbs_of_calls : CallGraph.CallTarget.t list -> Features.BreadcrumbSet.t

module ExtraTraceForTransforms : sig
  (* Collect sink taints that will be used as first hops of extra traces, i.e., whose call info
     matches the given callee roots and whose taint match the given named transforms *)
  val from_sink_trees
    :  argument_access_path:AccessPath.Path.t ->
    named_transforms:TaintTransform.t list ->
    tito_roots:AccessPath.Root.Set.t ->
    sink_trees:Domains.SinkTreeWithHandle.t list ->
    ExtraTraceFirstHop.Set.t

  (* ExtraTraceSink is used to show taint transforms. Hence, if a function does not have a tito
     behavior on an access path, then this access path will not have any taint transform and hence
     we can remove ExtraTraceSink on the same access path *)
  val prune
    :  sink_tree:BackwardState.Tree.t ->
    tito_tree:BackwardState.Tree.t ->
    BackwardState.Tree.t
end

val transform_tito_depth_breadcrumb : BackwardTaint.t -> int

val string_combine_partial_sink_tree : TaintConfiguration.Heap.t -> BackwardState.Tree.t

(* Compute the arguments at a call site that formats strings, such as `str.__add__` and
   f-strings. *)
val arguments_for_string_format
  :  Expression.expression Node.t list ->
  string * Expression.expression Node.t list

(* At a call site, extract the returned sink from `sink_model` of `callee` *)
val return_sink
  :  pyre_in_context:PyrePysaApi.InContext.t ->
  location:Location.WithModule.t ->
  callee:Target.t ->
  sink_model:BackwardState.t ->
  BackwardState.Tree.t

module ImplicitArgument : sig
  module Forward : sig
    type t =
      | CalleeBase of ForwardState.Tree.t
      | Callee of ForwardState.Tree.t
      | None

    val from_call_target
      :  is_implicit_new:bool ->
      ?callee_base_taint:ForwardState.Tree.t option ->
      ?callee_taint:ForwardState.Tree.t option ->
      CallGraph.CallTarget.t ->
      t
  end

  module Backward : sig
    type t =
      | CalleeBase of BackwardState.Tree.t
      | Callee of BackwardState.Tree.t
      | None

    val join : t -> t -> t
  end
end
