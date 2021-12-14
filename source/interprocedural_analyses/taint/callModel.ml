(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
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
                Frame.add_propagated_breadcrumb
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
                Frame.add_propagated_breadcrumb breadcrumb frame
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


module TaintInTaintOutMap = struct
  type t = (Sinks.t, BackwardState.Tree.t) Map.Poly.t

  let empty = Map.Poly.empty

  let join left right =
    let join ~key:_ = function
      | `Left left -> Some left
      | `Right right -> Some right
      | `Both (left, right) -> Some (BackwardState.Tree.join left right)
    in
    Map.Poly.merge left right ~f:join


  let get map ~kind = Map.Poly.find map kind

  let set map ~kind ~tito_tree = Map.Poly.set map ~key:kind ~data:tito_tree

  let fold map ~init ~f = Map.Poly.fold map ~init ~f:(fun ~key ~data -> f ~kind:key ~tito_tree:data)
end

let taint_in_taint_out_mapping
    ~transform_non_leaves
    ~model:{ Model.backward; modes; _ }
    ~tito_matches
  =
  let combine_tito sofar { AccessPath.root; actual_path; formal_path } =
    BackwardState.read ~transform_non_leaves ~root ~path:formal_path backward.taint_in_taint_out
    |> BackwardState.Tree.prepend actual_path
    |> BackwardState.Tree.partition Domains.BackwardTaint.kind By ~f:Fn.id
    |> TaintInTaintOutMap.join sofar
  in
  let mapping = List.fold tito_matches ~f:combine_tito ~init:TaintInTaintOutMap.empty in
  let mapping =
    if Model.ModeSet.contains Obscure modes then
      let breadcrumbs =
        TaintInTaintOutMap.get mapping ~kind:Sinks.LocalReturn
        >>| BackwardState.Tree.accumulated_breadcrumbs
        |> Option.value ~default:Features.BreadcrumbSet.empty
        |> Features.BreadcrumbSet.add (Features.obscure_model ())
      in
      let return_tito =
        Domains.local_return_frame
        |> Frame.update Frame.Slots.Breadcrumb breadcrumbs
        |> BackwardTaint.singleton Sinks.LocalReturn
        |> BackwardState.Tree.create_leaf
      in
      TaintInTaintOutMap.set mapping ~kind:Sinks.LocalReturn ~tito_tree:return_tito
    else
      mapping
  in
  mapping


let return_paths ~kind ~tito_taint =
  match Sinks.discard_transforms kind with
  | Sinks.LocalReturn ->
      BackwardTaint.fold Features.ReturnAccessPathSet.Element tito_taint ~f:List.cons ~init:[]
  | _ ->
      (* No special handling of paths for side effects *)
      [[]]


let sink_tree_of_argument
    ~transform_non_leaves
    ~model:{ Model.backward; _ }
    ~location
    ~call_target
    ~sink_matches
  =
  let combine_sink_taint taint_tree { AccessPath.root; actual_path; formal_path } =
    BackwardState.read ~transform_non_leaves ~root ~path:[] backward.sink_taint
    |> BackwardState.Tree.apply_call location ~callees:[call_target] ~port:root
    |> BackwardState.Tree.read ~transform_non_leaves formal_path
    |> BackwardState.Tree.prepend actual_path
    |> BackwardState.Tree.join taint_tree
  in
  List.fold sink_matches ~f:combine_sink_taint ~init:BackwardState.Tree.empty


let sanitize_of_argument ~model:{ Model.sanitizers; _ } ~sanitize_matches =
  List.map
    ~f:(fun { AccessPath.root; _ } -> SanitizeRootMap.get root sanitizers.roots)
    sanitize_matches
  |> List.fold ~f:Sanitize.join ~init:Sanitize.empty
  |> Sanitize.join sanitizers.global
  |> Sanitize.join sanitizers.parameters
