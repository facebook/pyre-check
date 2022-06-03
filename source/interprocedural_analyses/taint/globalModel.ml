(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis
open Expression
open Pyre
open Domains
module CallGraph = Interprocedural.CallGraph

type t = {
  models: Model.WithCallTarget.t list;
  resolution: Resolution.t;
  location: Location.WithModule.t;
  interval: Interprocedural.ClassIntervalSet.t;
}

let get_global_targets ~call_graph ~expression =
  match Node.value expression with
  | Expression.Name (Name.Identifier identifier) ->
      CallGraph.DefineCallGraph.resolve_identifier
        call_graph
        ~location:(Node.location expression)
        ~identifier
      >>| (fun { global_targets } -> global_targets)
      |> Option.value ~default:[]
  | Expression.Name (Name.Attribute { attribute; _ }) ->
      CallGraph.DefineCallGraph.resolve_attribute_access
        call_graph
        ~location:(Node.location expression)
        ~attribute
      >>| (fun { global_targets; _ } -> global_targets)
      |> Option.value ~default:[]
  | _ -> []


let from_expression ~resolution ~call_graph ~get_callee_model ~qualifier ~expression ~interval =
  let fetch_model ({ CallGraph.CallTarget.target; _ } as call_target) =
    let model =
      CallModel.at_callsite ~resolution ~get_callee_model ~call_target:target ~arguments:[]
    in
    { Model.WithCallTarget.model; call_target }
  in
  let models = get_global_targets ~call_graph ~expression |> List.map ~f:fetch_model in
  let location = Node.location expression |> Location.with_module ~module_reference:qualifier in
  { models; resolution; location; interval }


let global_root =
  AccessPath.Root.PositionalParameter { position = 0; name = "$global"; positional_only = false }


let get_source { models; resolution; location; interval } =
  let to_source
      existing
      {
        Model.WithCallTarget.call_target = { target; _ };
        model = { Model.forward = { Model.Forward.source_taint }; _ };
        _;
      }
    =
    ForwardState.read ~root:AccessPath.Root.LocalResult ~path:[] source_taint
    |> ForwardState.Tree.apply_call
         ~resolution
         ~location
         ~callee:(Some target)
         ~arguments:[]
         ~port:AccessPath.Root.LocalResult
         ~is_self_call:false
         ~caller_class_interval:interval
         ~receiver_class_interval:Interprocedural.ClassIntervalSet.top
    |> ForwardState.Tree.join existing
  in
  List.fold ~init:ForwardState.Tree.bottom ~f:to_source models


let get_sinks { models; resolution; location; interval } =
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
           ~resolution
           ~location
           ~callee:(Some target)
           ~arguments:[]
           ~port:AccessPath.Root.LocalResult
           ~is_self_call:false
           ~caller_class_interval:interval
           ~receiver_class_interval:Interprocedural.ClassIntervalSet.top
    in
    { Issue.SinkTreeWithHandle.sink_tree; handle = Issue.SinkHandle.make_global ~call_target }
  in
  List.map ~f:to_sink_tree_with_identifier models |> Issue.SinkTreeWithHandle.filter_bottom


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
    match sanitize with
    | {
     sources = Some SanitizeSources.All;
     sinks = Some SanitizeSinks.All;
     tito = Some SanitizeTito.All;
    } ->
        true
    | _ -> false
  in
  List.exists ~f:is_sanitized_model models
