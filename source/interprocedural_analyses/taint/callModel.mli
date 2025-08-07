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
  type_of_expression_shared_memory:Interprocedural.TypeOfExpressionSharedMemory.t ->
  caller:Target.t ->
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

val treat_tito_return_as_self_update : Target.t -> bool

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

  val empty : t

  val fold : t -> init:'a -> f:(kind:Sinks.t -> pair:TreeRootsPair.t -> 'a -> 'a) -> 'a
end

val taint_in_taint_out_mapping_for_argument
  :  transform_non_leaves:(Features.ReturnAccessPath.t -> BackwardTaint.t -> BackwardTaint.t) ->
  taint_configuration:TaintConfiguration.Heap.t ->
  ignore_local_return:bool ->
  model:Model.t ->
  callable:Target.t ->
  tito_matches:AccessPath.argument_match list ->
  sanitize_matches:AccessPath.argument_match list ->
  TaintInTaintOutMap.t

val return_paths_and_collapse_depths
  :  kind:Sinks.t ->
  tito_taint:BackwardTaint.t ->
  (AccessPath.Path.t * Features.CollapseDepth.t) list

val tito_intervals : BackwardTaint.t -> ClassIntervalSet.t

val sink_trees_of_argument
  :  pyre_in_context:PyrePysaApi.InContext.t ->
  type_of_expression_shared_memory:Interprocedural.TypeOfExpressionSharedMemory.t ->
  caller:Target.t ->
  transform_non_leaves:(Features.ReturnAccessPath.t -> BackwardTaint.t -> BackwardTaint.t) ->
  model:Model.t ->
  call_site:CallSite.t ->
  location:Location.t ->
  call_target:CallGraph.CallTarget.t ->
  arguments:Expression.Call.Argument.t list ->
  sink_matches:AccessPath.argument_match list ->
  is_class_method:bool ->
  is_static_method:bool ->
  call_info_intervals:Domains.ClassIntervals.t ->
  Domains.SinkTreeWithHandle.t list

val source_tree_of_argument
  :  pyre_in_context:PyrePysaApi.InContext.t ->
  type_of_expression_shared_memory:Interprocedural.TypeOfExpressionSharedMemory.t ->
  caller:Target.t ->
  model:Model.t ->
  call_site:CallSite.t ->
  location:Location.t ->
  call_target:CallGraph.CallTarget.t ->
  arguments:Expression.Call.Argument.t list ->
  is_class_method:bool ->
  is_static_method:bool ->
  call_info_intervals:Domains.ClassIntervals.t ->
  generation_source_match:AccessPath.argument_match ->
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

module StringFormatCall : sig
  type string_literal = {
    value: string;
    location: Location.t;
  }

  (* Represent what to analyze when creating strings from string formatting operations, such as
     `str.__add__`, `str.__mod__`, `e.format`, and f-strings. *)
  type t = {
    nested_expressions: Ast.Expression.Expression.t list;
    string_literal: string_literal;
    call_target: CallGraph.CallTarget.t;
    location: Location.t;
  }

  val declared_partial_sink_tree : TaintConfiguration.Heap.t -> BackwardState.Tree.t

  val apply_call
    :  callee:Target.t ->
    pyre_in_context:PyrePysaApi.InContext.t ->
    type_of_expression_shared_memory:Interprocedural.TypeOfExpressionSharedMemory.t ->
    caller:Target.t ->
    call_site:CallSite.t ->
    location:Location.t ->
    BackwardState.Tree.t ->
    BackwardState.Tree.t

  val implicit_string_literal_sources
    :  pyre_in_context:PyrePysaApi.InContext.t ->
    type_of_expression_shared_memory:Interprocedural.TypeOfExpressionSharedMemory.t ->
    caller:Target.t ->
    implicit_sources:TaintConfiguration.implicit_sources ->
    string_literal ->
    ForwardTaint.t

  val implicit_string_literal_sinks
    :  pyre_in_context:PyrePysaApi.InContext.t ->
    type_of_expression_shared_memory:Interprocedural.TypeOfExpressionSharedMemory.t ->
    caller:Target.t ->
    implicit_sinks:TaintConfiguration.implicit_sinks ->
    string_literal ->
    BackwardTaint.t

  module CallTarget : sig
    val create
      :  call_targets:CallGraph.CallTarget.t list ->
      default_target:CallGraph.CallTarget.t ->
      CallGraph.CallTarget.t

    val from_function_name : string -> CallGraph.CallTarget.t

    val from_format_string
      :  call_graph_of_define:CallGraph.DefineCallGraph.t ->
      location:Location.t ->
      CallGraph.CallTarget.t
  end
end

(* Compute the arguments at a call site that formats strings, such as `str.__add__` and
   f-strings. *)
val arguments_for_string_format
  :  Expression.expression Node.t list ->
  string * Expression.expression Node.t list

(* At a call site, extract the returned sink from `sink_model` of `callee` *)
val return_sink
  :  pyre_in_context:PyrePysaApi.InContext.t ->
  type_of_expression_shared_memory:Interprocedural.TypeOfExpressionSharedMemory.t ->
  caller:Target.t ->
  location:Location.t ->
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
    type t = {
      (* Taint on the base of the callee (such as `obj` in `obj.method`) *)
      callee_base: BackwardState.Tree.t;
      (* Taint on the entire callee *)
      callee: BackwardState.Tree.t;
    }

    val empty : t

    val for_callee : BackwardState.Tree.t -> t

    val for_callee_base : BackwardState.Tree.t -> t

    val join : t -> t -> t
  end
end

module Callee : sig
  type t = {
    (* Treat a callee expression at a call site as a `Name.t`, when applicable. *)
    name: Ast.Expression.Name.t option;
    location: Location.t;
  }

  val from_callee_expression : Expression.t -> t

  val from_stringify_call_target
    :  base:Expression.t ->
    stringify_origin:Ast.Expression.Origin.t option ->
    location:Location.t ->
    CallGraph.CallTarget.t ->
    t

  val as_argument : t -> Expression.Call.Argument.t

  val get_base : t -> Expression.t

  val is_self_call : t -> bool

  val is_cls_call : t -> bool
end
