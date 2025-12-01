(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* GlobalModel: implements utility functions used to perform a forward or backward
 * taint analysis of a global variable or attribute, by fetching user-declared
 * models for it.
 *)

open Core
open Ast
open Expression
open Pyre
open Domains
module CallGraph = Interprocedural.CallGraph
module AccessPath = Analysis.TaintAccessPath
module PyrePysaApi = Interprocedural.PyrePysaApi

type t = {
  models: Model.WithCallTarget.t list;
  pyre_in_context: PyrePysaApi.InContext.t;
  location: Location.t;
  interval: Interprocedural.ClassIntervalSet.t;
}

let get_global_targets_with_pyre1 ~call_graph ~expression =
  match Node.value expression with
  | Expression.Name (Name.Identifier identifier) ->
      CallGraph.DefineCallGraph.resolve_identifier
        call_graph
        ~location:(Node.location expression)
        ~identifier
      >>| (fun { global_targets; _ } -> global_targets)
      |> Option.value ~default:[]
  | Expression.Name (Name.Attribute attribute_access) ->
      CallGraph.DefineCallGraph.resolve_attribute_access
        call_graph
        ~location:(Node.location expression)
        ~attribute_access
      >>| (fun { global_targets; _ } -> global_targets)
      |> Option.value ~default:[]
  | _ -> []


let get_global_targets_with_pyrefly
    ~pyre_in_context
    ~type_of_expression_shared_memory
    ~get_callee_model
    ~expression
  =
  match Node.value expression with
  | Expression.Name (Name.Attribute { Name.Attribute.base; attribute; _ }) ->
      Interprocedural.TypeOfExpressionSharedMemory.compute_or_retrieve_pysa_type
        type_of_expression_shared_memory
        ~pyre_in_context
        base
      |> PyrePysaApi.ReadOnly.Type.get_class_names (PyrePysaApi.InContext.pyre_api pyre_in_context)
      |> (fun { PyrePysaApi.ClassNamesFromType.class_names; _ } -> class_names)
      |> List.map ~f:(fun class_name ->
             Format.sprintf "%s.%s" class_name attribute
             |> Reference.create
             |> Interprocedural.Target.create_object)
      |> List.filter ~f:(fun target -> Option.is_some (get_callee_model target))
      |> List.map ~f:(fun target -> CallGraph.CallTarget.create target)
  | _ -> []


let get_global_targets
    ~pyre_in_context
    ~type_of_expression_shared_memory
    ~call_graph
    ~get_callee_model
    ~expression
  =
  match pyre_in_context with
  | PyrePysaApi.InContext.Pyre1 _ -> get_global_targets_with_pyre1 ~call_graph ~expression
  | PyrePysaApi.InContext.Pyrefly _ ->
      get_global_targets_with_pyrefly
        ~pyre_in_context
        ~type_of_expression_shared_memory
        ~get_callee_model
        ~expression


let from_expression
    ~pyre_in_context
    ~type_of_expression_shared_memory
    ~call_graph
    ~get_callee_model
    ~expression
    ~interval
  =
  let fetch_model ({ CallGraph.CallTarget.target; _ } as call_target) =
    let model =
      CallModel.at_callsite
        ~pyre_in_context
        ~type_of_expression_shared_memory
        ~get_callee_model
        ~call_target:target
        ~arguments:[]
    in
    { Model.WithCallTarget.model; call_target }
  in
  let models =
    get_global_targets
      ~pyre_in_context
      ~type_of_expression_shared_memory
      ~call_graph
      ~get_callee_model
      ~expression
    |> List.map ~f:fetch_model
  in
  let location = Node.location expression in
  { models; pyre_in_context; location; interval }


let global_root =
  AccessPath.Root.PositionalParameter { position = 0; name = "$global"; positional_only = false }


let get_source ~type_of_expression_shared_memory { models; pyre_in_context; location; interval } =
  let to_source
      existing
      {
        Model.WithCallTarget.call_target = { target; _ };
        model = { Model.forward = { Model.Forward.generations }; _ };
        _;
      }
    =
    ForwardState.read ~root:AccessPath.Root.LocalResult ~path:[] generations
    |> ForwardState.Tree.apply_call
         ~pyre_in_context
         ~type_of_expression_shared_memory
         ~call_site:(CallSite.create location)
         ~location
         ~callee:target
         ~arguments:[]
         ~port:AccessPath.Root.LocalResult
         ~is_class_method:false
         ~is_static_method:false
         ~call_info_intervals:{ Domains.ClassIntervals.top with caller_interval = interval }
    |> ForwardState.Tree.join existing
  in
  List.fold ~init:ForwardState.Tree.bottom ~f:to_source models


let get_sinks ~type_of_expression_shared_memory { models; pyre_in_context; location; interval } =
  let to_sink_tree_with_identifier
      {
        Model.WithCallTarget.call_target = { target; _ } as call_target;
        model = { Model.backward = { Model.Backward.sink_taint; _ }; _ };
        _;
      }
    =
    let sink_tree =
      BackwardState.read ~root:global_root ~path:[] sink_taint
      |> BackwardState.Tree.apply_call
           ~pyre_in_context
           ~type_of_expression_shared_memory
           ~call_site:(CallSite.create location)
           ~location
           ~callee:target
           ~arguments:[]
           ~port:AccessPath.Root.LocalResult
           ~is_class_method:false
           ~is_static_method:false
           ~call_info_intervals:{ Domains.ClassIntervals.top with caller_interval = interval }
    in
    {
      Domains.SinkTreeWithHandle.sink_tree;
      handle = IssueHandle.Sink.make_global ~call_target;
      port = AccessPath.Root.LocalResult;
    }
  in
  List.map ~f:to_sink_tree_with_identifier models |> Domains.SinkTreeWithHandle.filter_bottom


let get_tito { models; _ } =
  let to_tito
      existing
      {
        Model.WithCallTarget.model =
          { Model.backward = { Model.Backward.taint_in_taint_out; _ }; _ };
        _;
      }
    =
    BackwardState.read ~root:global_root ~path:[] taint_in_taint_out
    |> BackwardState.Tree.join existing
  in
  List.fold ~init:BackwardState.Tree.bottom ~f:to_tito models


let get_sanitize { models; _ } =
  let get_sanitize
      existing
      { Model.WithCallTarget.model = { Model.sanitizers = { global = sanitize; _ }; _ }; _ }
    =
    Sanitize.join sanitize existing
  in
  List.fold ~init:Sanitize.empty ~f:get_sanitize models


let get_modes { models; _ } =
  let get_modes existing { Model.WithCallTarget.model = { Model.modes; _ }; _ } =
    Model.ModeSet.join modes existing
  in
  List.fold ~init:Model.ModeSet.empty ~f:get_modes models


let is_sanitized { models; _ } =
  let is_sanitized_model
      { Model.WithCallTarget.model = { Model.sanitizers = { global = sanitize; _ }; _ }; _ }
    =
    Sanitize.is_all sanitize
  in
  List.exists ~f:is_sanitized_model models
