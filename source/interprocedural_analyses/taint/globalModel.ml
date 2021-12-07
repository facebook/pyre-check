(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Expression
open Pyre
open Domains

type t = {
  models: Model.WithTarget.t list;
  location: Location.WithModule.t;
}

let get_global_targets ~call_graph ~expression =
  match Node.value expression with
  | Expression.Name (Name.Identifier identifier) ->
      Interprocedural.CallGraph.DefineCallGraph.resolve_identifier
        call_graph
        ~location:(Node.location expression)
        ~identifier
      >>| (fun { global_targets } -> global_targets)
      |> Option.value ~default:[]
  | Expression.Name (Name.Attribute { attribute; _ }) ->
      Interprocedural.CallGraph.DefineCallGraph.resolve_attribute_access
        call_graph
        ~location:(Node.location expression)
        ~attribute
      >>| (fun { global_targets; _ } -> global_targets)
      |> Option.value ~default:[]
  | _ -> []


let from_expression ~resolution ~call_graph ~qualifier ~expression =
  let fetch_model target =
    let model = CallModel.at_callsite ~resolution ~call_target:target ~arguments:[] in
    { Model.WithTarget.model; target }
  in
  let models = get_global_targets ~call_graph ~expression |> List.map ~f:fetch_model in
  let location = Node.location expression |> Location.with_module ~qualifier in
  { models; location }


let global_root =
  AccessPath.Root.PositionalParameter { position = 0; name = "$global"; positional_only = false }


let get_source { models; location } =
  let to_source
      existing
      { Model.WithTarget.target; model = { Model.forward = { Model.Forward.source_taint }; _ }; _ }
    =
    ForwardState.read ~root:AccessPath.Root.LocalResult ~path:[] source_taint
    |> ForwardState.Tree.apply_call location ~callees:[target] ~port:AccessPath.Root.LocalResult
    |> ForwardState.Tree.join existing
  in
  List.fold ~init:ForwardState.Tree.bottom ~f:to_source models


let get_sink { models; location } =
  let to_sink
      existing
      {
        Model.WithTarget.target;
        model = { Model.backward = { Model.Backward.sink_taint; _ }; _ };
        _;
      }
    =
    BackwardState.read ~root:global_root ~path:[] sink_taint
    |> BackwardState.Tree.apply_call location ~callees:[target] ~port:AccessPath.Root.LocalResult
    |> BackwardState.Tree.join existing
  in
  List.fold ~init:BackwardState.Tree.bottom ~f:to_sink models


let get_tito { models; _ } =
  let to_tito
      existing
      {
        Model.WithTarget.model = { Model.backward = { Model.Backward.taint_in_taint_out; _ }; _ };
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
      { Model.WithTarget.model = { Model.sanitizers = { global = sanitize; _ }; _ }; _ }
    =
    Sanitize.join sanitize existing
  in
  List.fold ~init:Sanitize.empty ~f:get_sanitize models


let get_modes { models; _ } =
  let get_modes existing { Model.WithTarget.model = { Model.modes; _ }; _ } =
    Model.ModeSet.join modes existing
  in
  List.fold ~init:Model.ModeSet.empty ~f:get_modes models


let is_sanitized { models; _ } =
  let is_sanitized_model
      { Model.WithTarget.model = { Model.sanitizers = { global = sanitize; _ }; _ }; _ }
    =
    match sanitize with
    | {
     sources = Some Sanitize.AllSources;
     sinks = Some Sanitize.AllSinks;
     tito = Some Sanitize.AllTito;
    } ->
        true
    | _ -> false
  in
  List.exists ~f:is_sanitized_model models
