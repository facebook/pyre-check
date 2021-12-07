(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Interprocedural
open Domains

let at_callsite ~resolution ~call_target ~arguments =
  let call_target = (call_target :> Target.t) in
  match Interprocedural.FixpointState.get_model call_target with
  | None -> Model.obscure_model
  | Some model ->
      let expand_via_value_of
          {
            Model.forward = { source_taint };
            backward = { sink_taint; taint_in_taint_out };
            sanitizers;
            modes;
          }
        =
        let expand frame =
          let transform via_feature frame =
            let match_all_arguments_to_parameter parameter =
              AccessPath.match_actuals_to_formals arguments [parameter]
              |> List.filter_map ~f:(fun (argument, matches) ->
                     if not (List.is_empty matches) then
                       Some argument
                     else
                       None)
            in
            let match_argument_to_parameter parameter =
              match match_all_arguments_to_parameter parameter with
              | [] -> None
              | argument :: _ -> Some argument.value
            in
            match via_feature with
            | Features.ViaFeature.ViaValueOf { parameter; tag } ->
                let arguments = match_all_arguments_to_parameter parameter in
                Frame.add_breadcrumb
                  (Features.ViaFeature.via_value_of_breadcrumb ?tag ~arguments)
                  frame
            | Features.ViaFeature.ViaTypeOf { parameter; tag } ->
                let breadcrumb =
                  match call_target with
                  | `Object object_target ->
                      Features.ViaFeature.via_type_of_breadcrumb_for_object
                        ?tag
                        ~resolution
                        ~object_target
                  | _ ->
                      Features.ViaFeature.via_type_of_breadcrumb
                        ?tag
                        ~resolution
                        ~argument:(match_argument_to_parameter parameter)
                in
                Frame.add_breadcrumb breadcrumb frame
          in
          Frame.fold Features.ViaFeatureSet.Element ~f:transform ~init:frame frame
        in
        let source_taint = ForwardState.transform Frame.Self Map ~f:expand source_taint in
        let sink_taint = BackwardState.transform Frame.Self Map ~f:expand sink_taint in
        let taint_in_taint_out =
          BackwardState.transform Frame.Self Map ~f:expand taint_in_taint_out
        in
        {
          Model.forward = { source_taint };
          backward = { sink_taint; taint_in_taint_out };
          sanitizers;
          modes;
        }
      in
      let taint_model =
        Interprocedural.AnalysisResult.get_model TaintResult.kind model
        |> Option.value ~default:Model.empty_model
        |> expand_via_value_of
      in
      let taint_model =
        if model.is_obscure then
          { taint_model with modes = Model.ModeSet.add Obscure taint_model.modes }
        else
          taint_model
      in
      taint_model


module ArgumentMatches = struct
  type t = {
    argument: Expression.t;
    sink_matches: AccessPath.argument_match list;
    tito_matches: AccessPath.argument_match list;
    sanitize_matches: AccessPath.argument_match list;
  }
end

let match_actuals_to_formals ~model:{ Model.backward; sanitizers; _ } ~arguments =
  let sink_argument_matches =
    BackwardState.roots backward.sink_taint
    |> AccessPath.match_actuals_to_formals arguments
    |> List.map ~f:(fun (argument, argument_match) ->
           argument.Expression.Call.Argument.value, argument_match)
  in
  let tito_argument_matches =
    BackwardState.roots backward.taint_in_taint_out
    |> AccessPath.match_actuals_to_formals arguments
    |> List.map ~f:(fun (argument, argument_match) ->
           argument.Expression.Call.Argument.value, argument_match)
  in
  let sanitize_argument_matches =
    SanitizeRootMap.roots sanitizers.roots
    |> AccessPath.match_actuals_to_formals arguments
    |> List.map ~f:(fun (argument, argument_match) ->
           argument.Expression.Call.Argument.value, argument_match)
  in
  List.zip_exn tito_argument_matches sanitize_argument_matches
  |> List.zip_exn sink_argument_matches
  |> List.map ~f:(fun ((argument, sink_matches), ((_, tito_matches), (_, sanitize_matches))) ->
         { ArgumentMatches.argument; sink_matches; tito_matches; sanitize_matches })
