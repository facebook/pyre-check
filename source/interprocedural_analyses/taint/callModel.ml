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
open Expression

let at_callsite
    ~pyre_in_context
    ~type_of_expression_shared_memory
    ~get_callee_model
    ~call_target
    ~arguments
  =
  match get_callee_model call_target with
  | None -> Model.obscure_model
  | Some model ->
      let expand_model_via_features
          ({ Model.backward = { sink_taint; taint_in_taint_out }; _ } as model)
        =
        let expand_frame_via_features frame =
          let breadcrumbs =
            Frame.get Frame.Slots.ViaFeature frame
            |> Features.expand_via_features
                 ~pyre_in_context
                 ~type_of_expression_shared_memory
                 ~callee:call_target
                 ~arguments
            |> Features.BreadcrumbMayAlwaysSet.of_set
          in
          Frame.add_propagated_breadcrumbs breadcrumbs frame
        in
        (* Note that we only need to do this for taint-in-taint-out, since
         * via-features expansion is done in `apply_call` for sources and sinks. *)
        let taint_in_taint_out =
          BackwardState.transform Frame.Self Map ~f:expand_frame_via_features taint_in_taint_out
        in
        { model with backward = { sink_taint; taint_in_taint_out } }
      in
      expand_model_via_features model


module ArgumentMatches = struct
  type t = {
    argument: Expression.t;
    generation_source_matches: AccessPath.argument_match list;
    sink_matches: AccessPath.argument_match list;
    tito_matches: AccessPath.argument_match list;
    sanitize_matches: AccessPath.argument_match list;
  }
  [@@deriving show]
end

let match_captures ~model ~captures_taint ~location =
  let sink_capture_roots =
    model.Model.backward.sink_taint
    |> BackwardState.roots
    |> List.filter ~f:AccessPath.Root.is_captured_variable
    |> AccessPath.Root.Set.of_list
  in
  let tito_capture_roots =
    model.Model.backward.taint_in_taint_out
    |> BackwardState.roots
    |> List.filter ~f:AccessPath.Root.is_captured_variable
    |> AccessPath.Root.Set.of_list
  in
  let make_taint_with_argument_match capture_root =
    let name =
      match capture_root with
      | AccessPath.Root.CapturedVariable { name; _ } -> name
      | _ -> failwith "unreachable"
    in
    let expression =
      (* captured variables are not present at call site, so location of call expression instead of
         location of assignment that introduces taint is used *)
      Node.create ~location (Expression.Name (Name.Identifier name))
    in
    let taint = ForwardState.read ~root:(AccessPath.Root.Variable name) ~path:[] captures_taint in
    let argument_match =
      {
        ArgumentMatches.argument = expression;
        generation_source_matches = [];
        sink_matches =
          (if AccessPath.Root.Set.mem capture_root sink_capture_roots then
             [{ AccessPath.root = capture_root; actual_path = []; formal_path = [] }]
          else
            []);
        tito_matches =
          (if AccessPath.Root.Set.mem capture_root tito_capture_roots then
             [{ AccessPath.root = capture_root; actual_path = []; formal_path = [] }]
          else
            []);
        sanitize_matches = [];
      }
    in
    taint, argument_match
  in
  AccessPath.Root.Set.union sink_capture_roots tito_capture_roots
  |> AccessPath.Root.Set.elements
  |> List.map ~f:make_taint_with_argument_match
  |> List.unzip


let captures_as_arguments =
  List.map ~f:(fun capture ->
      { Call.Argument.name = None; value = capture.ArgumentMatches.argument })


let match_actuals_to_formals ~model:{ Model.forward; backward; sanitizers; _ } ~arguments =
  let generation_argument_matches =
    ForwardState.roots forward.generations
    |> AccessPath.match_actuals_to_formals arguments
    |> List.map ~f:(fun (argument, argument_match) -> argument.Call.Argument.value, argument_match)
  in
  let sink_argument_matches =
    BackwardState.roots backward.sink_taint
    |> AccessPath.match_actuals_to_formals arguments
    |> List.map ~f:(fun (argument, argument_match) -> argument.Call.Argument.value, argument_match)
  in
  let tito_argument_matches =
    BackwardState.roots backward.taint_in_taint_out
    |> AccessPath.match_actuals_to_formals arguments
    |> List.map ~f:(fun (argument, argument_match) -> argument.Call.Argument.value, argument_match)
  in
  let sanitize_argument_matches =
    Sanitize.RootMap.roots sanitizers.roots
    |> AccessPath.match_actuals_to_formals arguments
    |> List.map ~f:(fun (argument, argument_match) -> argument.Call.Argument.value, argument_match)
  in
  sanitize_argument_matches
  |> List.zip_exn tito_argument_matches
  |> List.zip_exn sink_argument_matches
  |> List.zip_exn generation_argument_matches
  |> List.map
       ~f:(fun
            ( (argument, generation_source_matches),
              ((_, sink_matches), ((_, tito_matches), (_, sanitize_matches))) )
          ->
         {
           ArgumentMatches.argument;
           generation_source_matches;
           sink_matches;
           tito_matches;
           sanitize_matches;
         })


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

let treat_tito_return_as_self_update target =
  match Target.get_regular target with
  | Target.Regular.Method { method_name = "__init__"; _ }
  | Target.Regular.Override { method_name = "__init__"; _ }
  | Target.Regular.Method { method_name = "__setitem__"; _ }
  | Target.Regular.Override { method_name = "__setitem__"; _ }
  | Target.Regular.Method { kind = Pyre1PropertySetter | PyreflyPropertySetter; _ }
  | Target.Regular.Override { kind = Pyre1PropertySetter | PyreflyPropertySetter; _ } ->
      true
  | _ -> false


let taint_in_taint_out_mapping_for_argument
    ~transform_non_leaves
    ~taint_configuration
    ~ignore_local_return
    ~model:({ Model.backward; modes; _ } as model)
    ~callable
    ~tito_matches
    ~sanitize_matches
  =
  let create_tito_tree obscure_breadcrumbs output_kind =
    Domains.local_return_frame ~output_path:[] ~collapse_depth:0
    |> Frame.update Frame.Slots.PropagatedBreadcrumb obscure_breadcrumbs
    |> BackwardTaint.singleton (CallInfo.tito ()) output_kind
    |> BackwardState.Tree.create_leaf
  in
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
  let treat_obscure_as_self_update = treat_tito_return_as_self_update callable in
  let mapping =
    if
      Model.ModeSet.contains Obscure modes
      && (treat_obscure_as_self_update || not ignore_local_return)
    then
      (* Turn source- and sink- specific tito sanitizers into a tito taint with
       * sanitize taint transforms for obscure models. *)
      let output_kind =
        if treat_obscure_as_self_update then
          Sinks.ParameterUpdate
            (AccessPath.Root.PositionalParameter
               { position = 0; name = "self"; positional_only = false })
        else
          Sinks.LocalReturn
      in
      let obscure_sanitize = tito_sanitize_of_argument ~model ~sanitize_matches in
      let obscure_breadcrumbs =
        TaintInTaintOutMap.get_tree mapping ~kind:output_kind
        >>| BackwardState.Tree.joined_breadcrumbs
        |> Option.value ~default:Features.BreadcrumbMayAlwaysSet.empty
        |> Features.BreadcrumbMayAlwaysSet.add (Features.obscure_model ())
      in
      let mapping = TaintInTaintOutMap.remove mapping ~kind:output_kind in
      if SanitizeTransformSet.is_all obscure_sanitize then
        mapping
      else if SanitizeTransformSet.is_empty obscure_sanitize then
        let tito = create_tito_tree obscure_breadcrumbs output_kind in
        TaintInTaintOutMap.set_tree mapping ~kind:output_kind ~tito_tree:tito
      else
        let output_kind =
          Option.value_exn
            (TaintTransformOperation.Sink.apply_sanitize_transforms
               ~taint_configuration
               obscure_sanitize
               TaintTransformOperation.InsertLocation.Front
               output_kind)
        in
        let tito = create_tito_tree obscure_breadcrumbs output_kind in
        TaintInTaintOutMap.set_tree mapping ~kind:output_kind ~tito_tree:tito
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


let tito_intervals tito_taint =
  let collect_intervals call_info so_far =
    match call_info with
    | CallInfo.Tito { class_intervals } ->
        ClassIntervalSet.join class_intervals.ClassIntervals.caller_interval so_far
    | _ -> so_far
  in
  BackwardTaint.fold
    BackwardTaint.call_info
    ~f:collect_intervals
    ~init:ClassIntervalSet.empty
    tito_taint


let sink_trees_of_argument
    ~pyre_in_context
    ~type_of_expression_shared_memory
    ~transform_non_leaves
    ~model:{ Model.backward; _ }
    ~call_site
    ~location
    ~call_target:({ CallGraph.CallTarget.target; _ } as call_target)
    ~arguments
    ~sink_matches
    ~is_class_method
    ~is_static_method
    ~call_info_intervals
  =
  let to_sink_tree_with_identifier { AccessPath.root; actual_path; formal_path } =
    let sink_tree =
      BackwardState.read ~transform_non_leaves ~root ~path:[] backward.sink_taint
      |> BackwardState.Tree.apply_call
           ~pyre_in_context
           ~type_of_expression_shared_memory
           ~call_site
           ~location
           ~callee:target
           ~arguments
           ~port:root
           ~is_class_method
           ~is_static_method
           ~call_info_intervals
      |> BackwardState.Tree.read ~transform_non_leaves formal_path
      |> BackwardState.Tree.prepend actual_path
    in
    {
      SinkTreeWithHandle.sink_tree;
      handle = IssueHandle.Sink.make_call ~call_target ~root;
      port = root;
    }
  in
  sink_matches
  |> List.map ~f:to_sink_tree_with_identifier
  |> Domains.SinkTreeWithHandle.filter_bottom


let source_tree_of_argument
    ~pyre_in_context
    ~type_of_expression_shared_memory
    ~model:{ Model.forward; _ }
    ~call_site
    ~location
    ~call_target:{ CallGraph.CallTarget.target; _ }
    ~arguments
    ~is_class_method
    ~is_static_method
    ~call_info_intervals
    ~generation_source_match:{ AccessPath.root; actual_path; formal_path }
  =
  ForwardState.read ~root ~path:[] forward.generations
  |> ForwardState.Tree.apply_call
       ~pyre_in_context
       ~type_of_expression_shared_memory
       ~call_site
       ~location
       ~callee:target
       ~arguments
       ~port:root
       ~is_class_method
       ~is_static_method
       ~call_info_intervals
  |> ForwardState.Tree.read formal_path
  |> ForwardState.Tree.prepend actual_path


let type_breadcrumbs_of_calls targets =
  List.fold targets ~init:Features.BreadcrumbSet.empty ~f:(fun so_far call_target ->
      match call_target.CallGraph.CallTarget.return_type with
      | None -> so_far
      | Some return_type ->
          return_type |> Features.type_breadcrumbs |> Features.BreadcrumbSet.union so_far)


module ExtraTraceForTransforms = struct
  (* Collect sink taints that will be used as first hops of extra traces, i.e., whose call info
     matches the given callee roots and whose taint match the given named transforms *)
  let first_hops ~named_transforms ~tito_roots ~sink_taint =
    let match_call_info call_info =
      if CallInfo.show_as_extra_trace call_info then
        match call_info with
        | CallInfo.CallSite { port; _ } -> AccessPath.Root.Set.mem port tito_roots
        | _ -> true
      else
        false
    in
    let accumulate_extra_trace_first_hop call_info sink_kind so_far =
      match Sinks.discard_transforms sink_kind with
      | Sinks.ExtraTraceSink
        when List.equal TaintTransform.equal (Sinks.get_named_transforms sink_kind) named_transforms
        ->
          let message = Sinks.get_named_transforms sink_kind |> TaintTransforms.show_transforms in
          let extra_trace =
            { ExtraTraceFirstHop.call_info; leaf_kind = Sink sink_kind; message = Some message }
          in
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
        AccessPath.Path.is_prefix ~prefix:path argument_access_path
        || AccessPath.Path.is_prefix ~prefix:argument_access_path path
      in
      if not is_prefix then
        so_far
      else
        let extra_traces = first_hops ~tito_roots ~named_transforms ~sink_taint:tip in
        ExtraTraceFirstHop.Set.join extra_traces so_far
    in
    let accumulate_extra_traces so_far { Domains.SinkTreeWithHandle.sink_tree; _ } =
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
        || AccessPath.Path.is_prefix ~prefix:tito_path sink_path
        || AccessPath.Path.is_prefix ~prefix:sink_path tito_path
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


module StringFormatCall = struct
  type string_literal = {
    value: string;
    location: Location.t;
  }

  (* Represent what to analyze when creating strings from string formatting operations, such as
     `str.__add__`, `str.__mod__`, `e.format`, and f-strings. *)
  type t = {
    (* Any expression used in the string formatting. *)
    nested_expressions: Ast.Expression.Expression.t list;
    (* Any string literal used in the string formatting. *)
    string_literal: string_literal;
    (* If a triggered flow exists on any expression used in the string formatting, this is the
       responsible call site. *)
    call_target: CallGraph.CallTarget.t;
    (* Location of the string formatting operation. *)
    location: Location.t;
  }

  let implicit_string_literal_sources
      ~pyre_in_context
      ~type_of_expression_shared_memory
      ~implicit_sources
      { value; location }
    =
    let literal_string_regular_expressions = implicit_sources.TaintConfiguration.literal_strings in
    if String.is_empty value || List.is_empty literal_string_regular_expressions then
      ForwardTaint.bottom
    else
      let add_matching_source_kind so_far { TaintConfiguration.pattern; source_kind = kind } =
        if Re2.matches pattern value then
          ForwardTaint.singleton CallInfo.declaration kind Frame.initial
          |> ForwardTaint.apply_call
               ~pyre_in_context
               ~type_of_expression_shared_memory
               ~call_site:(CallSite.create location)
               ~location
               ~callee:Target.ArtificialTargets.str_literal
               ~arguments:[]
               ~port:AccessPath.Root.LocalResult
               ~path:[]
               ~is_class_method:false
               ~is_static_method:false
               ~call_info_intervals:Domains.ClassIntervals.top
          |> ForwardTaint.join so_far
        else
          so_far
      in
      List.fold
        literal_string_regular_expressions
        ~f:add_matching_source_kind
        ~init:ForwardTaint.bottom


  let implicit_string_literal_sinks
      ~pyre_in_context
      ~type_of_expression_shared_memory
      ~implicit_sinks
      { value; location }
    =
    let literal_string_regular_expressions =
      implicit_sinks.TaintConfiguration.literal_string_sinks
    in
    if String.is_empty value || List.is_empty literal_string_regular_expressions then
      BackwardTaint.bottom
    else
      let add_matching_sink_kind so_far { TaintConfiguration.pattern; sink_kind } =
        if Re2.matches pattern value then
          BackwardTaint.singleton CallInfo.declaration sink_kind Frame.initial
          |> BackwardTaint.apply_call
               ~pyre_in_context
               ~type_of_expression_shared_memory
               ~call_site:(CallSite.create location)
               ~location
               ~callee:Target.ArtificialTargets.str_literal
               ~arguments:[]
               ~port:AccessPath.Root.LocalResult
               ~path:[]
               ~is_class_method:false
               ~is_static_method:false
               ~call_info_intervals:Domains.ClassIntervals.top
          |> BackwardTaint.join so_far
        else
          so_far
      in
      List.fold
        literal_string_regular_expressions
        ~f:add_matching_sink_kind
        ~init:BackwardTaint.bottom


  let declared_partial_sink_tree { TaintConfiguration.Heap.string_combine_partial_sinks; _ } =
    let create_sink_taint sink = BackwardTaint.singleton CallInfo.declaration sink Frame.initial in
    TaintConfiguration.StringOperationPartialSinks.get_partial_sinks string_combine_partial_sinks
    |> List.map ~f:create_sink_taint
    |> List.reduce ~f:BackwardTaint.join
    |> Option.value ~default:BackwardTaint.bottom
    |> BackwardState.Tree.create_leaf


  let apply_call ~callee ~pyre_in_context ~type_of_expression_shared_memory ~call_site ~location =
    BackwardState.Tree.apply_call
      ~pyre_in_context
      ~type_of_expression_shared_memory
      ~call_site
      ~location
      ~callee
      ~arguments:[]
      ~port:AccessPath.Root.sink_port_in_string_combine_functions
      ~is_class_method:false
      ~is_static_method:false
      ~call_info_intervals:Domains.ClassIntervals.top


  module CallTarget = struct
    let create ~call_targets ~default_target =
      call_targets
      |> List.min_elt ~compare:CallGraph.CallTarget.compare
      |> Option.value ~default:default_target


    let from_function_name string_function_name =
      CallGraph.CallTarget.create
        (match string_function_name with
        | "__add__" -> Target.ArtificialTargets.str_add
        | "__mod__" -> Target.ArtificialTargets.str_mod
        | "format" -> Target.ArtificialTargets.str_format
        | _ -> failwith "Expect either `__add__` or `__mod__` or `format`")


    let from_format_string ~call_graph_of_define ~location =
      let call_targets =
        match
          CallGraph.DefineCallGraph.resolve_format_string_artificial call_graph_of_define ~location
        with
        | Some { CallGraph.FormatStringArtificialCallees.targets } -> targets
        | None -> []
      in
      create
        ~call_targets
        ~default_target:(CallGraph.CallTarget.create Target.ArtificialTargets.format_string)
  end
end

let arguments_for_string_format arguments =
  let string_literals =
    List.map arguments ~f:(function
        | { Node.value = Expression.Constant (Constant.String { StringLiteral.value; _ }); _ } ->
            value
        | _ -> "{}")
  in
  let non_literal_arguments =
    List.filter arguments ~f:(function
        | { Node.value = Expression.Constant (Constant.String _); _ } -> false
        | _ -> true)
  in
  let string_literal = String.concat ~sep:"" string_literals in
  string_literal, non_literal_arguments


(* At a call site, extract the returned sink from `sink_model` of `callee` *)
let return_sink ~pyre_in_context ~type_of_expression_shared_memory ~location ~callee ~sink_model =
  let taint =
    BackwardState.read ~root:AccessPath.Root.LocalResult ~path:[] sink_model
    |> BackwardState.Tree.apply_call
         ~pyre_in_context
         ~type_of_expression_shared_memory
         ~call_site:(CallSite.create location)
         ~location
         ~callee
           (* When the source and sink meet at the return statement, we want the leaf callable to be
              non-empty, to provide more information about the flow. *)
         ~arguments:[]
         ~port:AccessPath.Root.LocalResult
         ~is_class_method:false
         ~is_static_method:false
         ~call_info_intervals:Domains.ClassIntervals.top
  in
  let breadcrumbs_to_attach, via_features_to_attach =
    BackwardState.extract_features_to_attach
      ~root:AccessPath.Root.LocalResult
      ~attach_to_kind:Sinks.Attach
      sink_model
  in
  taint
  |> BackwardState.Tree.add_local_breadcrumbs breadcrumbs_to_attach
  |> BackwardState.Tree.add_via_features via_features_to_attach


module ImplicitArgument = struct
  module Forward = struct
    type t =
      | CalleeBase of ForwardState.Tree.t
      | Callee of ForwardState.Tree.t
      | None

    let from_call_target
        ~is_implicit_new
        ?(callee_base_taint = Base.Option.None)
        ?(callee_taint = Base.Option.None)
        call_target
      =
      match CallGraph.ImplicitArgument.implicit_argument ~is_implicit_new call_target with
      | CallGraph.ImplicitArgument.Callee ->
          Callee (Option.value ~default:ForwardState.Tree.bottom callee_taint)
      | CallGraph.ImplicitArgument.CalleeBase ->
          CalleeBase (Option.value ~default:ForwardState.Tree.bottom callee_base_taint)
      | CallGraph.ImplicitArgument.None -> None
  end

  module Backward = struct
    type t = {
      (* Taint on the base of the callee (such as `obj` in `obj.method`) *)
      callee_base: BackwardState.Tree.t;
      (* Taint on the entire callee *)
      callee: BackwardState.Tree.t;
    }

    let empty = { callee_base = BackwardState.Tree.bottom; callee = BackwardState.Tree.bottom }

    let for_callee taint = { callee_base = BackwardState.Tree.bottom; callee = taint }

    let for_callee_base taint = { callee_base = taint; callee = BackwardState.Tree.bottom }

    let join
        { callee_base = left_callee_base; callee = left_callee }
        { callee_base = right_callee_base; callee = right_callee }
      =
      {
        callee_base = BackwardState.Tree.join left_callee_base right_callee_base;
        callee = BackwardState.Tree.join left_callee right_callee;
      }
  end
end

module Callee = struct
  type t = {
    (* Treat a callee expression at a call site as a `Name.t`, when applicable. *)
    name: Name.t option;
    location: Location.t;
  }

  let from_callee_expression { Node.value; location } =
    {
      name =
        (match value with
        | Expression.Name name -> Some name
        | _ -> None);
      location;
    }


  let from_stringify_call_target
      ~base
      ~stringify_origin
      ~location
      { CallGraph.CallTarget.target; _ }
    =
    let callee_name_from_method_name method_name =
      Some (Name.Attribute { base; attribute = method_name; origin = stringify_origin })
    in
    let name =
      match Target.get_regular target with
      | Target.Regular.Method { method_name; _ } -> callee_name_from_method_name method_name
      | Override { method_name; _ } -> callee_name_from_method_name method_name
      | Function { name; _ } -> Some (Name.Identifier name)
      | Object _ -> failwith "callees should be either methods or functions"
    in
    { name; location }


  (* We do not expect such expressions to affect taint analysis, such as attaching breadcrumbs or
     having global taint. *)
  let placeholder_expression = Expression.Constant Constant.NoneLiteral

  let as_argument { name; location } =
    let callee =
      match name with
      | Some name -> Expression.Name name
      | None -> placeholder_expression
    in
    { Call.Argument.name = None; value = Node.create ~location callee }


  let get_base = function
    | { name = Some (Name.Attribute { base; _ }); _ } -> base
    | { location; _ } -> Node.create ~location placeholder_expression


  (* TODO(T114580705): Better precision when deciding if an expression is `self` *)
  let is_self_call { name; _ } =
    match name with
    | Some
        (Name.Attribute
          { Name.Attribute.base = { Node.value = Name (Name.Identifier identifier); _ }; _ }) ->
        String.equal (Identifier.sanitized identifier) "self"
    | _ -> false


  (* TODO(T114580705): Better precision when deciding if an expression is `cls` *)
  let is_cls_call { name; _ } =
    match name with
    | Some
        (Name.Attribute
          { Name.Attribute.base = { Node.value = Name (Name.Identifier identifier); _ }; _ }) ->
        String.equal (Identifier.sanitized identifier) "cls"
    | _ -> false
end
