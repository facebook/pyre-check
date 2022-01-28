(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
      let expand_model_via_features
          { Model.forward; backward = { sink_taint; taint_in_taint_out }; sanitizers; modes }
        =
        let expand_frame_via_features frame =
          let breadcrumbs =
            Frame.get Frame.Slots.ViaFeature frame
            |> Features.expand_via_features ~resolution ~callees:[call_target] ~arguments
          in
          Frame.add_propagated_breadcrumbs breadcrumbs frame
        in
        (* Note that we only need to do this for taint-in-taint-out, since
         * via-features expansion is done in `apply_call` for sources and sinks. *)
        let taint_in_taint_out =
          BackwardState.transform Frame.Self Map ~f:expand_frame_via_features taint_in_taint_out
        in
        { Model.forward; backward = { sink_taint; taint_in_taint_out }; sanitizers; modes }
      in
      let taint_model =
        Interprocedural.AnalysisResult.get_model TaintResult.kind model
        |> Option.value ~default:Model.empty_model
        |> expand_model_via_features
      in
      let taint_model =
        if model.is_obscure then
          { taint_model with Model.modes = Model.ModeSet.add Obscure taint_model.modes }
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
        >>| BackwardState.Tree.joined_breadcrumbs
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
    ~resolution
    ~transform_non_leaves
    ~model:{ Model.backward; _ }
    ~location
    ~call_target
    ~arguments
    ~sink_matches
  =
  let combine_sink_taint taint_tree { AccessPath.root; actual_path; formal_path } =
    BackwardState.read ~transform_non_leaves ~root ~path:[] backward.sink_taint
    |> BackwardState.Tree.apply_call
         ~resolution
         ~location
         ~callees:[call_target]
         ~arguments
         ~port:root
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
