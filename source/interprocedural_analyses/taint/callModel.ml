(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* CallModel: implements utility functions used to perform the forward or backward
 * taint analysis of a function or method call, which requires fetching the
 * model of the callee.
 *)

open Core
open Pyre
open Ast
open Interprocedural
open Domains

let at_callsite ~resolution ~get_callee_model ~call_target ~arguments =
  match get_callee_model call_target with
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
      expand_model_via_features model


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
    Sanitize.RootMap.roots sanitizers.roots
    |> AccessPath.match_actuals_to_formals arguments
    |> List.map ~f:(fun (argument, argument_match) ->
           argument.Expression.Call.Argument.value, argument_match)
  in
  List.zip_exn tito_argument_matches sanitize_argument_matches
  |> List.zip_exn sink_argument_matches
  |> List.map ~f:(fun ((argument, sink_matches), ((_, tito_matches), (_, sanitize_matches))) ->
         { ArgumentMatches.argument; sink_matches; tito_matches; sanitize_matches })


let tito_sanitize_of_argument ~model:{ Model.sanitizers; _ } ~sanitize_matches =
  let to_tito_sanitize { Sanitize.sources; sinks; tito } =
    SanitizeTransformSet.join tito { SanitizeTransformSet.sources; sinks }
  in
  List.map
    ~f:(fun { AccessPath.root; _ } ->
      Sanitize.RootMap.get root sanitizers.roots |> to_tito_sanitize)
    sanitize_matches
  |> List.fold ~f:SanitizeTransformSet.join ~init:SanitizeTransformSet.bottom
  |> SanitizeTransformSet.join
       (Sanitize.RootMap.get AccessPath.Root.LocalResult sanitizers.roots |> to_tito_sanitize)
  |> SanitizeTransformSet.join (to_tito_sanitize sanitizers.global)
  |> SanitizeTransformSet.join (to_tito_sanitize sanitizers.parameters)


(* A mapping from a taint-in-taint-out kind (e.g, `Sinks.LocalReturn`, `Sinks.ParameterUpdate` or
   `Sinks.AddFeatureToArgument`) to a tito taint (including features, return paths, depth) and the
   roots in the tito model whose trees contain this sink. *)
module TaintInTaintOutMap = struct
  module TreeRootsPair = struct
    type t = {
      tree: BackwardState.Tree.t;
      roots: AccessPath.Root.Set.t;
    }

    let join { tree = left_tree; roots = left_roots } { tree = right_tree; roots = right_roots } =
      {
        tree = BackwardState.Tree.join left_tree right_tree;
        roots = AccessPath.Root.Set.union left_roots right_roots;
      }
  end

  type t = (Sinks.t, TreeRootsPair.t) Map.Poly.t

  let empty = Map.Poly.empty

  let join left right =
    let join ~key:_ = function
      | `Left left -> Some left
      | `Right right -> Some right
      | `Both (left, right) -> Some (TreeRootsPair.join left right)
    in
    Map.Poly.merge left right ~f:join


  let get map ~kind = Map.Poly.find map kind

  let get_tree map ~kind =
    match get map ~kind with
    | Some { TreeRootsPair.tree; _ } -> Some tree
    | None -> None


  let set_tree map ~kind ~tito_tree =
    let tito_information =
      match get map ~kind with
      | Some pair -> { pair with TreeRootsPair.tree = tito_tree }
      | None -> { TreeRootsPair.roots = AccessPath.Root.Set.empty; tree = tito_tree }
    in
    Map.Poly.set map ~key:kind ~data:tito_information


  let remove map ~kind = Map.Poly.remove map kind

  let fold map ~init ~f = Map.Poly.fold map ~init ~f:(fun ~key ~data -> f ~kind:key ~pair:data)

  let filter map ~f = Map.Poly.filter_keys map ~f

  let map map ~f = Map.Poly.map map ~f
end

let taint_in_taint_out_mapping
    ~transform_non_leaves
    ~taint_configuration
    ~ignore_local_return
    ~model:({ Model.backward; modes; _ } as model)
    ~tito_matches
    ~sanitize_matches
  =
  let combine_tito sofar { AccessPath.root; actual_path; formal_path } =
    let mapping_for_path =
      BackwardState.read ~transform_non_leaves ~root ~path:formal_path backward.taint_in_taint_out
      |> BackwardState.Tree.prepend actual_path
      |> BackwardState.Tree.partition Domains.BackwardTaint.kind ByFilter ~f:(function
             | Sinks.Attach -> None
             | kind -> Some kind)
    in
    let mapping_for_path =
      if ignore_local_return then
        TaintInTaintOutMap.filter mapping_for_path ~f:(function
            | Sinks.LocalReturn
            | Sinks.Transform { base = Sinks.LocalReturn; _ } ->
                false
            | _ -> true)
      else
        mapping_for_path
    in
    TaintInTaintOutMap.map mapping_for_path ~f:(fun tree ->
        { TaintInTaintOutMap.TreeRootsPair.tree; roots = AccessPath.Root.Set.singleton root })
    |> TaintInTaintOutMap.join sofar
  in
  let mapping = List.fold tito_matches ~f:combine_tito ~init:TaintInTaintOutMap.empty in
  let mapping =
    if Model.ModeSet.contains Obscure modes && not ignore_local_return then
      (* Turn source- and sink- specific tito sanitizers into a tito taint with
       * sanitize taint transforms for obscure models. *)
      let obscure_sanitize = tito_sanitize_of_argument ~model ~sanitize_matches in
      let obscure_breadcrumbs =
        TaintInTaintOutMap.get_tree mapping ~kind:Sinks.LocalReturn
        >>| BackwardState.Tree.joined_breadcrumbs
        |> Option.value ~default:Features.BreadcrumbSet.empty
        |> Features.BreadcrumbSet.add (Features.obscure_model ())
      in
      let mapping = TaintInTaintOutMap.remove mapping ~kind:Sinks.LocalReturn in
      if SanitizeTransformSet.is_all obscure_sanitize then
        mapping
      else if SanitizeTransformSet.is_empty obscure_sanitize then
        let return_tito =
          Domains.local_return_frame ~output_path:[] ~collapse_depth:0
          |> Frame.update Frame.Slots.Breadcrumb obscure_breadcrumbs
          |> BackwardTaint.singleton CallInfo.declaration Sinks.LocalReturn
          |> BackwardState.Tree.create_leaf
        in
        TaintInTaintOutMap.set_tree mapping ~kind:Sinks.LocalReturn ~tito_tree:return_tito
      else
        let tito_kind =
          Option.value_exn
            (TaintTransformOperation.Sink.apply_sanitize_transforms
               ~taint_configuration
               obscure_sanitize
               TaintTransformOperation.InsertLocation.Front
               Sinks.LocalReturn)
        in
        let return_tito =
          Domains.local_return_frame ~output_path:[] ~collapse_depth:0
          |> Frame.update Frame.Slots.Breadcrumb obscure_breadcrumbs
          |> BackwardTaint.singleton CallInfo.Tito tito_kind
          |> BackwardState.Tree.create_leaf
        in
        TaintInTaintOutMap.set_tree mapping ~kind:tito_kind ~tito_tree:return_tito
    else
      mapping
  in
  mapping


let return_paths_and_collapse_depths ~kind ~tito_taint =
  match Sinks.discard_transforms kind with
  | Sinks.LocalReturn
  | Sinks.ParameterUpdate _ ->
      let paths =
        BackwardTaint.fold Features.ReturnAccessPathTree.Path tito_taint ~f:List.cons ~init:[]
      in
      let () =
        if List.is_empty paths then
          failwith "unexpected empty return path set"
      in
      paths
  | _ -> Format.asprintf "unexpected kind for tito: %a" Sinks.pp kind |> failwith


let sink_trees_of_argument
    ~resolution
    ~transform_non_leaves
    ~model:{ Model.backward; _ }
    ~location
    ~call_target:({ CallGraph.CallTarget.target; _ } as call_target)
    ~arguments
    ~sink_matches
    ~is_self_call
    ~caller_class_interval
    ~receiver_class_interval
  =
  let to_sink_tree_with_identifier { AccessPath.root; actual_path; formal_path } =
    let sink_tree =
      BackwardState.read ~transform_non_leaves ~root ~path:[] backward.sink_taint
      |> BackwardState.Tree.apply_call
           ~resolution
           ~location
           ~callee:(Some target)
           ~arguments
           ~port:root
           ~is_self_call
           ~caller_class_interval
           ~receiver_class_interval
      |> BackwardState.Tree.read ~transform_non_leaves formal_path
      |> BackwardState.Tree.prepend actual_path
    in
    { Issue.SinkTreeWithHandle.sink_tree; handle = Issue.SinkHandle.make_call ~call_target ~root }
  in
  List.map sink_matches ~f:to_sink_tree_with_identifier |> Issue.SinkTreeWithHandle.filter_bottom


let type_breadcrumbs_of_calls targets =
  List.fold targets ~init:Features.BreadcrumbSet.bottom ~f:(fun so_far call_target ->
      match call_target.CallGraph.CallTarget.return_type with
      | None -> so_far
      | Some return_type ->
          return_type |> Features.type_breadcrumbs |> Features.BreadcrumbSet.join so_far)


module ExtraTraceForTransforms = struct
  (* Collect sink taints that will be used as first hops of extra traces, i.e., whose call info
     matches the given callee roots and whose taint match the given named transforms *)
  let first_hops ~named_transforms ~tito_roots ~sink_taint =
    let match_call_info = function
      | CallInfo.CallSite { port; _ } -> AccessPath.Root.Set.mem port tito_roots
      | CallInfo.Origin _ -> false (* Skip origins because there is no subtrace to show *)
      | CallInfo.Declaration _
      | CallInfo.Tito ->
          false
    in
    let accumulate_extra_trace_first_hop call_info sink_kind so_far =
      match Sinks.discard_transforms sink_kind with
      | Sinks.ExtraTraceSink
        when List.equal TaintTransform.equal (Sinks.get_named_transforms sink_kind) named_transforms
        ->
          let extra_trace = { ExtraTraceFirstHop.call_info; leaf_kind = Sink sink_kind } in
          ExtraTraceFirstHop.Set.add extra_trace so_far
      | _ -> so_far
    in
    sink_taint
    |> BackwardTaint.transform BackwardTaint.call_info Filter ~f:match_call_info
    |> BackwardTaint.reduce
         BackwardTaint.kind
         ~using:(Context (BackwardTaint.call_info, Acc))
         ~f:accumulate_extra_trace_first_hop
         ~init:ExtraTraceFirstHop.Set.bottom


  let from_sink_trees ~argument_access_path ~named_transforms ~tito_roots ~sink_trees =
    let accumulate_extra_traces_from_sink_path (path, tip) so_far =
      let is_prefix =
        Abstract.TreeDomain.Label.is_prefix ~prefix:path argument_access_path
        || Abstract.TreeDomain.Label.is_prefix ~prefix:argument_access_path path
      in
      if not is_prefix then
        so_far
      else
        let extra_traces = first_hops ~tito_roots ~named_transforms ~sink_taint:tip in
        ExtraTraceFirstHop.Set.join extra_traces so_far
    in
    let accumulate_extra_traces so_far { Issue.SinkTreeWithHandle.sink_tree; _ } =
      BackwardState.Tree.fold
        BackwardState.Tree.Path
        ~f:accumulate_extra_traces_from_sink_path
        ~init:so_far
        sink_tree
      |> ExtraTraceFirstHop.Set.join so_far
    in
    List.fold sink_trees ~init:ExtraTraceFirstHop.Set.bottom ~f:accumulate_extra_traces


  (* ExtraTraceSink is used to show taint transforms. Hence, if a function does not have a tito
     behavior on an access path, then this access path will not have any taint transform and hence
     we can remove ExtraTraceSink on the same access path *)
  let prune ~sink_tree ~tito_tree =
    let remove_sink (sink_path, sink_tip) =
      let find_tito (tito_path, _) so_far =
        so_far
        || Abstract.TreeDomain.Label.is_prefix ~prefix:tito_path sink_path
        || Abstract.TreeDomain.Label.is_prefix ~prefix:sink_path tito_path
      in
      let exist_tito =
        BackwardState.Tree.fold BackwardState.Tree.Path ~f:find_tito ~init:false tito_tree
      in
      if exist_tito then
        sink_path, sink_tip
      else
        sink_path, BackwardTaint.bottom
    in
    BackwardState.Tree.transform BackwardState.Tree.Path Map ~f:remove_sink sink_tree
end

let transform_tito_depth_breadcrumb tito_taint =
  let length = BackwardTaint.fold TraceLength.Self ~f:min ~init:Int.max_value tito_taint in
  Features.Breadcrumb.TransformTitoDepth (length + 1) |> Features.BreadcrumbInterned.intern
