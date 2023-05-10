(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* ForwardAnalysis: implements a forward taint analysis on a function body.
 * This is used to infer the source part of a model, by propagating sources down
 * through the statements of the body. It also checks for sources matching sinks,
 * triggering issues.
 *
 * For instance, on the given function, we would infer the following taint
 * states, starting from the arguments:
 * ```
 * def foo(prompt: bool, request: requests.Request):
 *   # {}
 *   if prompt:
 *     # {}
 *     a = input()
 *     # {a -> UserControlled}
 *   else:
 *     a = request.header('username')
 *     # {a -> Header}
 *
 *   # {a -> {UserControlled, Header}}
 *   sink(a) # potential issue
 *   # {a -> {UserControlled, Header}}
 *   return a
 *   # {a -> {UserControlled, Header}}
 * ```
 *
 * We would find the potential issue and also infer that `foo` can return a
 * `UserControlled` source or `Header` source.
 *)

open Core
open Analysis
open Ast
open Expression
open Pyre
open Domains
module CallGraph = Interprocedural.CallGraph
module CallResolution = Interprocedural.CallResolution

module type FUNCTION_CONTEXT = sig
  val qualifier : Reference.t

  val definition : Statement.Define.t Node.t

  val debug : bool

  val profiler : TaintProfiler.t

  val environment : TypeEnvironment.ReadOnly.t

  val taint_configuration : TaintConfiguration.Heap.t

  val string_combine_partial_sink_tree : BackwardState.Tree.t

  val class_interval_graph : Interprocedural.ClassIntervalSetGraph.SharedMemory.t

  val global_constants : Interprocedural.GlobalConstants.SharedMemory.t

  val call_graph_of_define : CallGraph.DefineCallGraph.t

  val get_callee_model : Interprocedural.Target.t -> Model.t option

  val existing_model : Model.t

  val triggered_sinks_to_propagate : Issue.TriggeredSinkLocationMap.t

  val caller_class_interval : Interprocedural.ClassIntervalSet.t
end

let ( |>> ) (taint, state) f = f taint, state

module State (FunctionContext : FUNCTION_CONTEXT) = struct
  type t = { taint: ForwardState.t }

  let bottom = { taint = ForwardState.bottom }

  let pp formatter { taint } = ForwardState.pp formatter taint

  let show = Format.asprintf "%a" pp

  let less_or_equal ~left:{ taint = left } ~right:{ taint = right } =
    ForwardState.less_or_equal ~left ~right


  let join { taint = left } { taint = right; _ } =
    let taint = ForwardState.join left right in
    { taint }


  let widen ~previous:{ taint = prev; _ } ~next:{ taint = next } ~iteration =
    let taint = ForwardState.widen ~iteration ~prev ~next in
    { taint }


  let profiler = FunctionContext.profiler

  let class_interval_graph = FunctionContext.class_interval_graph

  let log format =
    if FunctionContext.debug then
      Log.dump format
    else
      Log.log ~section:`Taint format


  let get_call_callees ~location ~call =
    let callees =
      match
        CallGraph.DefineCallGraph.resolve_call FunctionContext.call_graph_of_define ~location ~call
      with
      | Some callees -> callees
      | None ->
          (* This is most likely a bug that should be fixed. *)
          Format.asprintf
            "Could not find callees for `%a` at `%a:%a` in the call graph."
            Expression.pp
            (Node.create_with_default_location (Expression.Call call) |> Ast.Expression.delocalize)
            Reference.pp
            FunctionContext.qualifier
            Location.pp
            location
          |> failwith
    in
    log
      "Resolved callees for call `%a` at %a:@,%a"
      Expression.pp
      (Node.create_with_default_location (Expression.Call call))
      Location.pp
      location
      CallGraph.CallCallees.pp
      callees;
    callees


  let get_attribute_access_callees ~location ~attribute =
    let callees =
      CallGraph.DefineCallGraph.resolve_attribute_access
        FunctionContext.call_graph_of_define
        ~location
        ~attribute
    in
    let () =
      match callees with
      | Some callees ->
          log
            "Resolved attribute access callees for `%s` at %a:@,%a"
            attribute
            Location.pp
            location
            CallGraph.AttributeAccessCallees.pp
            callees
      | _ -> ()
    in
    callees


  let get_string_format_callees ~location =
    CallGraph.DefineCallGraph.resolve_string_format FunctionContext.call_graph_of_define ~location


  let global_resolution = TypeEnvironment.ReadOnly.global_resolution FunctionContext.environment

  let local_annotations =
    TypeEnvironment.ReadOnly.get_local_annotations
      FunctionContext.environment
      (Node.value FunctionContext.definition |> Statement.Define.name)


  let is_constructor () =
    let { Node.value = { Statement.Define.signature = { name; _ }; _ }; _ } =
      FunctionContext.definition
    in
    match Reference.last name with
    | "__init__" -> true
    | _ -> false


  let candidates = Issue.Candidates.create ()

  let check_flow ~location ~sink_handle ~source_tree ~sink_tree =
    let () =
      if
        (not (ForwardState.Tree.is_bottom source_tree))
        && not (BackwardState.Tree.is_bottom sink_tree)
      then
        log
          "Sources flowing into sinks at `%a`@,With sources: %a@,With sinks: %a"
          Location.WithModule.pp
          location
          ForwardState.Tree.pp
          source_tree
          BackwardState.Tree.pp
          sink_tree
    in
    Issue.Candidates.check_flow candidates ~location ~sink_handle ~source_tree ~sink_tree


  let check_flow_to_global ~location ~source_tree global_model =
    let location = Location.with_module ~module_reference:FunctionContext.qualifier location in
    let check { SinkTreeWithHandle.sink_tree; handle } =
      check_flow ~location ~sink_handle:handle ~source_tree ~sink_tree
    in
    GlobalModel.get_sinks global_model |> List.iter ~f:check


  let check_triggered_flows ~triggered_sinks_for_call ~sink_handle ~location ~source_tree ~sink_tree
    =
    Issue.Candidates.check_triggered_flows
      candidates
      ~triggered_sinks_for_call
      ~sink_handle
      ~location
      ~source_tree
      ~sink_tree
      ~define:FunctionContext.definition
      ~taint_configuration:FunctionContext.taint_configuration


  let generate_issues () =
    Issue.Candidates.generate_issues
      candidates
      ~taint_configuration:FunctionContext.taint_configuration
      ~define:FunctionContext.definition


  let gather_triggered_sinks_to_propagate ~triggered_sinks ~path (root, taint_tree) taint_so_far =
    let update_kind_frame (kind, frame) =
      match Sinks.extract_partial_sink kind with
      | Some sink -> (
          match Issue.TriggeredSinkHashMap.find triggered_sinks sink with
          | Some issue_handles ->
              let existing_issue_handles = Frame.get Frame.Slots.MultiSourceIssueHandle frame in
              let frame =
                Frame.update
                  Frame.Slots.MultiSourceIssueHandle
                  (IssueHandleSet.join existing_issue_handles issue_handles)
                  frame
              in
              Some (Sinks.TriggeredPartialSink sink, frame)
          | None -> None)
      | None -> None
    in
    let taint_tree =
      BackwardState.Tree.transform
        BackwardTaint.kind_frame
        FilterMap
        taint_tree
        ~f:update_kind_frame
    in
    BackwardState.assign ~weak:true ~root ~path taint_tree taint_so_far


  (* Store all triggered sinks seen in `triggered_sinks` to propagate them up in the backward
     analysis. *)
  let store_triggered_sinks_to_propagate_for_call ~location ~triggered_sinks sink_taint =
    if not (Issue.TriggeredSinkHashMap.is_empty triggered_sinks) then
      let triggered_sinks =
        BackwardState.fold
          BackwardState.KeyValue
          sink_taint
          ~init:BackwardState.bottom
          ~f:(gather_triggered_sinks_to_propagate ~path:[] ~triggered_sinks)
      in
      Issue.TriggeredSinkLocationMap.add
        FunctionContext.triggered_sinks_to_propagate
        ~location
        ~taint:triggered_sinks


  let store_triggered_sinks_to_propagate_for_string_combine
      ~location
      ~triggered_sinks
      ~sink_tree
      nested_expressions
    =
    if not (Issue.TriggeredSinkHashMap.is_empty triggered_sinks) then
      let accumulate so_far nested_expression =
        match AccessPath.of_expression nested_expression with
        | Some access_path ->
            gather_triggered_sinks_to_propagate
              ~path:access_path.path
              ~triggered_sinks
              (access_path.root, sink_tree)
              so_far
        | None -> (* TODO(T146550300): False negative *) so_far
      in
      let triggered_sinks = List.fold nested_expressions ~f:accumulate ~init:BackwardState.bottom in
      Issue.TriggeredSinkLocationMap.add
        FunctionContext.triggered_sinks_to_propagate
        ~location
        ~taint:triggered_sinks


  module StringFormatCall = struct
    type string_literal = {
      value: string;
      location: Location.t;
    }

    (* Represent what to analyze when creating strings from string formatting operations, such as
       `str.__add__`, `str.__mod__`, `e.format`, and f-strings. *)
    type t = {
      (* Any expression used in the string formatting. *)
      nested_expressions: Expression.t list;
      (* Any string literal used in the string formatting. *)
      string_literal: string_literal;
      (* If a triggered flow exists on any expression used in the string formatting, this is the
         responsible call site. *)
      call_target_for_string_combine_rules: CallGraph.CallTarget.t option;
      (* Location of the string formatting operation. *)
      location: Location.t;
    }

    let create_call_target_for_string_combine ~call_targets ~default_target =
      call_targets
      |> List.min_elt ~compare:CallGraph.CallTarget.compare
      |> Option.value ~default:(CallGraph.CallTarget.create default_target)


    let implicit_string_literal_sources { value; location } =
      if String.equal "" value then
        ForwardTaint.bottom
      else
        let value_location =
          Location.with_module ~module_reference:FunctionContext.qualifier location
        in
        let literal_string_regular_expressions =
          FunctionContext.taint_configuration.implicit_sources.literal_strings
        in
        let add_matching_source_kind sofar { TaintConfiguration.pattern; source_kind = kind } =
          if Re2.matches pattern value then
            ForwardTaint.singleton (CallInfo.Origin value_location) kind Frame.initial
            |> ForwardTaint.join sofar
          else
            sofar
        in
        List.fold
          literal_string_regular_expressions
          ~init:ForwardTaint.bottom
          ~f:add_matching_source_kind


    let check_flow_implicit_string_literal_sinks ~string_literal:{ value; location } taint =
      (* We try to be a bit clever about bailing out early and not computing the matches. *)
      let literal_string_sinks =
        FunctionContext.taint_configuration.implicit_sinks.literal_string_sinks
      in
      if
        (not (String.equal "" value))
        && (not (List.is_empty literal_string_sinks))
        && not (ForwardTaint.is_bottom taint)
      then
        let value_location_with_module =
          Location.with_module ~module_reference:FunctionContext.qualifier location
        in
        List.iter literal_string_sinks ~f:(fun { TaintConfiguration.sink_kind; pattern } ->
            if Re2.matches pattern value then
              let sink_tree =
                BackwardTaint.singleton
                  (CallInfo.Origin value_location_with_module)
                  sink_kind
                  Frame.initial
                |> BackwardState.Tree.create_leaf
              in
              check_flow
                ~location:value_location_with_module
                ~sink_handle:(IssueHandle.Sink.LiteralStringSink sink_kind)
                ~source_tree:(ForwardState.Tree.create_leaf taint)
                ~sink_tree)


    (* Check triggered flows into partial sinks that are introduced onto expressions that are used
       in the string operations. *)
    let check_triggered_flows
        ~triggered_sinks
        ~parameter_index
        ~call_target_for_string_combine_rules
        ~location
        ~string_combine_partial_sink_tree
        source_tree
      =
      match call_target_for_string_combine_rules with
      | Some { CallGraph.CallTarget.target; index; _ } ->
          let location =
            Location.with_module ~module_reference:FunctionContext.qualifier location
          in
          let sink_handle =
            IssueHandle.Sink.StringFormat { callee = target; index; parameter_index }
          in
          check_triggered_flows
            ~triggered_sinks_for_call:triggered_sinks
            ~sink_handle
            ~location
            ~source_tree
            ~sink_tree:string_combine_partial_sink_tree
      | None -> ()
  end

  let return_sink ~resolution ~return_location =
    let taint =
      BackwardState.read
        ~root:AccessPath.Root.LocalResult
        ~path:[]
        FunctionContext.existing_model.Model.backward.sink_taint
      |> BackwardState.Tree.apply_call
           ~resolution
           ~location:return_location
           ~callee:None
           ~arguments:[]
           ~port:AccessPath.Root.LocalResult
           ~is_self_call:false
           ~caller_class_interval:Interprocedural.ClassIntervalSet.top
           ~receiver_class_interval:Interprocedural.ClassIntervalSet.top
    in
    let breadcrumbs_to_attach, via_features_to_attach =
      BackwardState.extract_features_to_attach
        ~root:AccessPath.Root.LocalResult
        ~attach_to_kind:Sinks.Attach
        FunctionContext.existing_model.Model.backward.sink_taint
    in
    taint
    |> BackwardState.Tree.add_local_breadcrumbs breadcrumbs_to_attach
    |> BackwardState.Tree.add_via_features via_features_to_attach


  let store_taint ?(weak = false) ~root ~path taint { taint = state_taint } =
    { taint = ForwardState.assign ~weak ~root ~path taint state_taint }


  let store_taint_option ?(weak = false) access_path taint state =
    match access_path with
    | Some { AccessPath.root; path } -> store_taint ~weak ~root ~path taint state
    | None -> state


  (* A mapping from a taint-in-taint-out kind (e.g, `Sinks.LocalReturn`, `Sinks.ParameterUpdate` or
     `Sinks.AddFeatureToArgument`) to a source taint that must be propagated. *)
  module TaintInTaintOutEffects = struct
    type t = (Sinks.t, ForwardState.Tree.t) Map.Poly.t

    let empty = (Map.Poly.empty : t) (* use t to silence a warning. *)

    let update map ~kind ~f = Map.Poly.update map kind ~f

    let add map ~kind ~taint =
      update map ~kind ~f:(function
          | None -> taint
          | Some previous -> ForwardState.Tree.join previous taint)


    let get map ~kind = Map.Poly.find map kind |> Option.value ~default:ForwardState.Tree.empty

    let fold map ~init ~f = Map.Poly.fold map ~init ~f:(fun ~key ~data -> f ~kind:key ~taint:data)
  end

  let add_extra_traces_for_transforms
      ~argument_access_path
      ~named_transforms
      ~sink_trees
      ~tito_roots
      taint
    =
    let extra_traces =
      CallModel.ExtraTraceForTransforms.from_sink_trees
        ~argument_access_path
        ~named_transforms
        ~tito_roots
        ~sink_trees
    in
    ForwardState.Tree.transform
      ForwardTaint.Self
      Map
      ~f:(ForwardTaint.add_extra_traces ~extra_traces)
      taint


  let apply_call_target
      ?(apply_tito = true)
      ~resolution
      ~is_result_used
      ~triggered_sinks_for_call
      ~call_location
      ~self
      ~self_taint
      ~callee
      ~callee_taint
      ~arguments
      ~arguments_taint
      ~state:initial_state
      ({
         CallGraph.CallTarget.target;
         implicit_self;
         implicit_dunder_call;
         index = _;
         return_type;
         receiver_type;
       } as call_target)
    =
    (* Add implicit self. *)
    let arguments, arguments_taint =
      if implicit_self && not implicit_dunder_call then
        ( { Call.Argument.name = None; value = Option.value_exn self } :: arguments,
          Option.value_exn self_taint :: arguments_taint )
      else if implicit_self && implicit_dunder_call then
        ( { Call.Argument.name = None; value = callee } :: arguments,
          Option.value_exn callee_taint :: arguments_taint )
      else
        arguments, arguments_taint
    in
    let ({ Model.forward; backward; _ } as taint_model) =
      TaintProfiler.track_model_fetch
        ~profiler
        ~analysis:TaintProfiler.Forward
        ~call_target:target
        ~f:(fun () ->
          CallModel.at_callsite
            ~resolution
            ~get_callee_model:FunctionContext.get_callee_model
            ~call_target:target
            ~arguments)
    in
    log
      "Forward analysis of call to `%a` with arguments (%a)@,Call site model:@,%a"
      Interprocedural.Target.pp_pretty
      target
      Ast.Expression.pp_expression_argument_list
      arguments
      Model.pp
      taint_model;
    let is_self_call = Ast.Expression.is_self_call ~callee in
    let receiver_class_interval =
      Interprocedural.ClassIntervalSetGraph.SharedMemory.of_type class_interval_graph receiver_type
    in
    let convert_tito_path_to_taint
        ~argument
        ~argument_taint
        ~tito_roots
        ~sink_trees
        ~kind
        (argument_access_path, tito_taint)
        accumulated_tito
      =
      let breadcrumbs =
        BackwardTaint.joined_breadcrumbs tito_taint |> Features.BreadcrumbSet.add (Features.tito ())
      in
      let taint_to_propagate =
        ForwardState.Tree.read argument_access_path argument_taint
        |> ForwardState.Tree.transform
             Features.TitoPositionSet.Element
             Add
             ~f:argument.Node.location
        |> ForwardState.Tree.add_local_breadcrumbs breadcrumbs
      in
      let taint_to_propagate =
        match kind with
        | Sinks.Transform { local = transforms; global; _ } when TaintTransforms.is_empty global ->
            (* Apply source- and sink- specific tito sanitizers. *)
            let taint_to_propagate =
              ForwardState.Tree.apply_transforms
                ~taint_configuration:FunctionContext.taint_configuration
                transforms
                TaintTransformOperation.InsertLocation.Front
                TaintTransforms.Order.Backward
                taint_to_propagate
            in
            let named_transforms = TaintTransforms.discard_sanitize_transforms transforms in
            if List.is_empty named_transforms then
              taint_to_propagate
            else
              let breadcrumb = CallModel.transform_tito_depth_breadcrumb tito_taint in
              let taint_to_propagate =
                ForwardState.Tree.add_local_breadcrumb
                  ~add_on_tito:false
                  breadcrumb
                  taint_to_propagate
              in
              add_extra_traces_for_transforms
                ~argument_access_path
                ~named_transforms
                ~sink_trees
                ~tito_roots
                taint_to_propagate
        | Sinks.Transform _ -> failwith "unexpected non-empty `global` transforms in tito"
        | _ -> taint_to_propagate
      in
      CallModel.return_paths_and_collapse_depths ~kind ~tito_taint
      |> List.fold
           ~f:(fun taint (return_path, collapse_depth) ->
             (if Features.CollapseDepth.should_collapse collapse_depth then
                ForwardState.Tree.collapse_to
                  ~breadcrumbs:(Features.tito_broadening_set ())
                  ~depth:collapse_depth
                  taint_to_propagate
             else
               taint_to_propagate)
             |> ForwardState.Tree.prepend return_path
             |> ForwardState.Tree.join taint)
           ~init:accumulated_tito
    in
    let convert_tito_tree_to_taint
        ~argument
        ~argument_taint
        ~sink_trees
        ~kind
        ~pair:{ CallModel.TaintInTaintOutMap.TreeRootsPair.tree = tito_tree; roots = tito_roots }
        tito_effects
      =
      let tito_tree =
        BackwardState.Tree.fold
          BackwardState.Tree.Path
          tito_tree
          ~init:ForwardState.Tree.empty
          ~f:(convert_tito_path_to_taint ~argument ~argument_taint ~tito_roots ~sink_trees ~kind)
      in
      TaintInTaintOutEffects.add tito_effects ~kind:(Sinks.discard_transforms kind) ~taint:tito_tree
    in
    let compute_argument_tito_effect
        (tito_effects, state)
        ( argument_taint,
          { CallModel.ArgumentMatches.argument; sink_matches; tito_matches; sanitize_matches } )
      =
      let location =
        Location.with_module ~module_reference:FunctionContext.qualifier argument.Node.location
      in
      let sink_trees =
        CallModel.sink_trees_of_argument
          ~resolution
          ~transform_non_leaves:(fun _ tree -> tree)
          ~model:taint_model
          ~location
          ~call_target
          ~arguments
          ~sink_matches
          ~is_self_call
          ~caller_class_interval:FunctionContext.caller_class_interval
          ~receiver_class_interval
      in
      let tito_effects =
        if apply_tito then
          CallModel.taint_in_taint_out_mapping
            ~transform_non_leaves:(fun _ tito -> tito)
            ~taint_configuration:FunctionContext.taint_configuration
            ~ignore_local_return:(not is_result_used)
            ~model:taint_model
            ~tito_matches
            ~sanitize_matches
          |> CallModel.TaintInTaintOutMap.fold
               ~init:tito_effects
               ~f:(convert_tito_tree_to_taint ~argument ~argument_taint ~sink_trees)
        else
          TaintInTaintOutEffects.empty
      in

      (* Add features to arguments. *)
      let state =
        match AccessPath.of_expression argument with
        | Some { AccessPath.root; path } ->
            let breadcrumbs_to_add =
              List.fold
                sink_trees
                ~f:(fun sofar { SinkTreeWithHandle.sink_tree; _ } ->
                  sink_tree
                  |> BackwardState.Tree.filter_by_kind ~kind:Sinks.AddFeatureToArgument
                  |> BackwardTaint.joined_breadcrumbs
                  |> Features.BreadcrumbSet.add_set ~to_add:sofar)
                ~init:Features.BreadcrumbSet.bottom
            in
            if Features.BreadcrumbSet.is_bottom breadcrumbs_to_add then
              state
            else
              let taint =
                ForwardState.read state.taint ~root ~path
                |> ForwardState.Tree.add_local_breadcrumbs breadcrumbs_to_add
              in
              store_taint ~root ~path taint state
        | None -> state
      in

      let () =
        List.iter sink_trees ~f:(fun { SinkTreeWithHandle.sink_tree; handle } ->
            (* Check for issues. *)
            check_flow ~location ~sink_handle:handle ~source_tree:argument_taint ~sink_tree;
            (* Check for issues for combined source rules. *)
            check_triggered_flows
              ~triggered_sinks_for_call
              ~sink_handle:handle
              ~location
              ~source_tree:argument_taint
              ~sink_tree;
            ())
      in
      tito_effects, state
    in
    let tito_effects, state =
      CallModel.match_actuals_to_formals ~model:taint_model ~arguments
      |> List.zip_exn arguments_taint
      |> List.fold
           ~f:compute_argument_tito_effect
           ~init:(TaintInTaintOutEffects.empty, initial_state)
    in
    let result_taint =
      if is_result_used then
        ForwardState.read ~root:AccessPath.Root.LocalResult ~path:[] forward.source_taint
        |> ForwardState.Tree.apply_call
             ~resolution
             ~location:
               (Location.with_module ~module_reference:FunctionContext.qualifier call_location)
             ~callee:(Some target)
             ~arguments
             ~port:AccessPath.Root.LocalResult
             ~is_self_call
             ~caller_class_interval:FunctionContext.caller_class_interval
             ~receiver_class_interval
      else
        ForwardState.Tree.empty
    in
    let tito_taint = TaintInTaintOutEffects.get tito_effects ~kind:Sinks.LocalReturn in
    let () =
      (* Need to be called after calling `check_triggered_flows` *)
      store_triggered_sinks_to_propagate_for_call
        ~location:call_location
        ~triggered_sinks:triggered_sinks_for_call
        backward.sink_taint
    in
    let apply_tito_side_effects tito_effects state =
      (* We also have to consider the cases when the updated parameter has a global model, in which
         case we need to capture the flow. *)
      let apply_argument_effect ~argument:{ Call.Argument.value = argument; _ } ~source_tree state =
        GlobalModel.from_expression
          ~resolution
          ~call_graph:FunctionContext.call_graph_of_define
          ~get_callee_model:FunctionContext.get_callee_model
          ~qualifier:FunctionContext.qualifier
          ~expression:argument
          ~interval:FunctionContext.caller_class_interval
        |> check_flow_to_global ~location:argument.Node.location ~source_tree;
        let access_path = AccessPath.of_expression argument in
        log
          "Propagating taint to argument `%a`: %a"
          Expression.pp
          argument
          ForwardState.Tree.pp
          source_tree;
        store_taint_option ~weak:true access_path source_tree state
      in
      let for_each_target ~kind:target ~taint state =
        match target with
        | Sinks.LocalReturn -> state (* This is regular tito which was computed above *)
        | ParameterUpdate n -> (
            (* Side effect on argument n *)
            match List.nth arguments n with
            | None -> state
            | Some argument -> apply_argument_effect ~argument ~source_tree:taint state)
        | _ -> Format.asprintf "unexpected kind for tito: %a" Sinks.pp target |> failwith
      in
      TaintInTaintOutEffects.fold tito_effects ~f:for_each_target ~init:state
    in
    let returned_taint = ForwardState.Tree.join result_taint tito_taint in
    let returned_taint =
      ForwardState.Tree.add_local_breadcrumbs
        (Features.type_breadcrumbs (Option.value_exn return_type))
        returned_taint
    in
    let state = apply_tito_side_effects tito_effects state in
    returned_taint, state


  let apply_obscure_call
      ~apply_tito
      ~callee
      ~callee_taint
      ~arguments
      ~arguments_taint
      ~state:initial_state
    =
    log
      "Forward analysis of obscure call to `%a` with arguments (%a)"
      Expression.pp
      callee
      Ast.Expression.pp_expression_argument_list
      arguments;
    let callee_taint =
      Option.value_exn callee_taint
      |> ForwardState.Tree.collapse ~breadcrumbs:(Features.tito_broadening_set ())
      |> ForwardTaint.transform Features.TitoPositionSet.Element Add ~f:callee.Node.location
    in
    let analyze_argument taint_accumulator ({ Call.Argument.value = argument; _ }, argument_taint) =
      let argument_taint =
        match argument.Node.value with
        | Starred (Starred.Once _)
        | Starred (Starred.Twice _) ->
            ForwardState.Tree.read [Abstract.TreeDomain.Label.AnyIndex] argument_taint
        | _ -> argument_taint
      in
      argument_taint
      |> ForwardState.Tree.collapse ~breadcrumbs:(Features.tito_broadening_set ())
      |> ForwardTaint.transform Features.TitoPositionSet.Element Add ~f:argument.Node.location
      |> ForwardTaint.join taint_accumulator
    in
    let taint =
      if apply_tito then
        List.zip_exn arguments arguments_taint
        |> List.fold ~f:analyze_argument ~init:callee_taint
        |> ForwardTaint.add_local_breadcrumb (Features.obscure_unknown_callee ())
        |> ForwardState.Tree.create_leaf
      else
        ForwardState.Tree.empty
    in
    taint, initial_state


  let apply_constructor_targets
      ~resolution
      ~is_result_used
      ~triggered_sinks_for_call
      ~call_location
      ~callee
      ~callee_taint
      ~arguments
      ~arguments_taint
      ~new_targets
      ~init_targets
      ~state:initial_state
    =
    let is_object_new = CallGraph.CallCallees.is_object_new new_targets in
    let is_object_init = CallGraph.CallCallees.is_object_init init_targets in

    (* If both `is_object_new` and `is_object_init` are true, this is probably a stub
     * class (e.g, `class X: ...`), in which case, we treat it as an obscure call. *)

    (* Call `__new__`. Add the `cls` implicit argument. *)
    let new_return_taint, state =
      if is_object_new then
        ForwardState.Tree.bottom, initial_state
      else
        List.map new_targets ~f:(fun target ->
            apply_call_target
              ~resolution
              ~is_result_used:true
              ~triggered_sinks_for_call
              ~call_location
              ~self:(Some callee)
              ~self_taint:callee_taint
              ~callee
              ~callee_taint:(Some ForwardState.Tree.bottom)
              ~arguments
              ~arguments_taint
              ~state:initial_state
              target)
        |> List.fold
             ~init:(ForwardState.Tree.empty, bottom)
             ~f:(fun (taint, state) (new_taint, new_state) ->
               ForwardState.Tree.join taint new_taint, join state new_state)
    in

    (* Call `__init__`. Add the `self` implicit argument. *)
    let taint, state =
      if is_object_init && not is_object_new then
        new_return_taint, state
      else
        let call_expression =
          Expression.Call { Call.callee; arguments } |> Node.create ~location:call_location
        in
        List.map init_targets ~f:(fun target ->
            apply_call_target
              ~resolution
              ~is_result_used
              ~triggered_sinks_for_call
              ~call_location
              ~self:(Some call_expression)
              ~self_taint:(Some new_return_taint)
              ~callee
              ~callee_taint:(Some ForwardState.Tree.bottom)
              ~arguments
              ~arguments_taint
              ~state
              target)
        |> List.fold
             ~init:(ForwardState.Tree.empty, bottom)
             ~f:(fun (taint, state) (new_taint, new_state) ->
               ForwardState.Tree.join taint new_taint, join state new_state)
    in

    taint, state


  let apply_callees_with_arguments_taint
      ?(apply_tito = true)
      ~resolution
      ~is_result_used
      ~callee
      ~call_location
      ~arguments
      ~self_taint
      ~callee_taint
      ~arguments_taint
      ~state:initial_state
      {
        CallGraph.CallCallees.call_targets;
        new_targets;
        init_targets;
        higher_order_parameters = _;
        unresolved;
      }
    =
    (* Set of sinks of combined source rules triggered (i.e, a source flowed to
     * a partial sink) for the current call expression. *)
    let triggered_sinks_for_call = Issue.TriggeredSinkHashMap.create () in

    (* Extract the implicit self, if any *)
    let self =
      match callee.Node.value with
      | Expression.Name (Name.Attribute { base; _ }) -> Some base
      | _ ->
          (* Default to a benign self if we don't understand/retain information of what self is. *)
          Expression.Constant Constant.NoneLiteral
          |> Node.create ~location:callee.Node.location
          |> Option.some
    in

    (* Apply regular call targets. *)
    let taint, state =
      List.map
        call_targets
        ~f:
          (apply_call_target
             ~apply_tito
             ~resolution
             ~is_result_used
             ~triggered_sinks_for_call
             ~call_location
             ~self
             ~self_taint
             ~callee
             ~callee_taint
             ~arguments
             ~arguments_taint
             ~state:initial_state)
      |> List.fold
           ~init:(ForwardState.Tree.empty, bottom)
           ~f:(fun (taint, state) (new_taint, new_state) ->
             ForwardState.Tree.join taint new_taint, join state new_state)
    in

    (* Apply an obscure call if the call was not fully resolved. *)
    let taint, state =
      if unresolved then
        let obscure_taint, new_state =
          apply_obscure_call
            ~apply_tito
            ~callee
            ~callee_taint
            ~arguments
            ~arguments_taint
            ~state:initial_state
        in
        ForwardState.Tree.join taint obscure_taint, join state new_state
      else
        taint, state
    in

    (* Apply constructor calls, if any. *)
    let taint, state =
      match new_targets, init_targets with
      | [], [] -> taint, state
      | _ ->
          let new_taint, new_state =
            apply_constructor_targets
              ~resolution
              ~is_result_used
              ~triggered_sinks_for_call
              ~call_location
              ~callee
              ~callee_taint
              ~arguments
              ~arguments_taint
              ~new_targets
              ~init_targets
              ~state:initial_state
          in
          ForwardState.Tree.join taint new_taint, join state new_state
    in

    let taint =
      (* Add index breadcrumb if appropriate. *)
      match Node.value callee, arguments with
      | Expression.Name (Name.Attribute { attribute = "get"; _ }), index :: _ ->
          let label = AccessPath.get_index index.value in
          ForwardState.Tree.add_local_first_index label taint
      | _ -> taint
    in

    taint, state


  type analyze_attribute_access_result = {
    base_taint: ForwardState.Tree.t;
    attribute_taint: ForwardState.Tree.t;
    state: t;
  }

  let join_analyze_attribute_access_result
      { base_taint = left_base_taint; attribute_taint = left_attribute_taint; state = left_state }
      {
        base_taint = right_base_taint;
        attribute_taint = right_attribute_taint;
        state = right_state;
      }
    =
    {
      base_taint = ForwardState.Tree.join left_base_taint right_base_taint;
      attribute_taint = ForwardState.Tree.join left_attribute_taint right_attribute_taint;
      state = join left_state right_state;
    }


  type analyze_callee_result = {
    self_taint: ForwardState.Tree.t option;
    callee_taint: ForwardState.Tree.t option;
    state: t;
  }

  let rec analyze_callee ~resolution ~is_property_call ~state ~callee =
    match callee.Node.value with
    | Expression.Name (Name.Attribute { base; attribute; special }) ->
        (* If we are already analyzing a call of a property, then ignore properties
         * to avoid infinite recursion. *)
        let resolve_properties = not is_property_call in
        let { base_taint; attribute_taint; state } =
          analyze_attribute_access
            ~resolution
            ~state
            ~is_attribute_used:true
            ~location:callee.Node.location
            ~resolve_properties
            ~base
            ~attribute
            ~special
        in
        { self_taint = Some base_taint; callee_taint = Some attribute_taint; state }
    | _ ->
        let taint, state =
          analyze_expression ~resolution ~state ~is_result_used:true ~expression:callee
        in
        { self_taint = Some ForwardState.Tree.bottom; callee_taint = Some taint; state }


  (* Lazy version of `analyze_callee` which only analyze what we need for a call site. *)
  and analyze_callee_for_callees ~resolution ~is_property_call ~state ~callee callees =
    (* Special case: `x.foo()` where foo is a property returning a callable. *)
    let callee_is_property =
      match is_property_call, callee.Node.value with
      | false, Expression.Name (Name.Attribute { attribute; _ }) ->
          get_attribute_access_callees ~location:callee.Node.location ~attribute |> Option.is_some
      | _ -> false
    in
    match callees, callee_is_property with
    | _, true
    | { CallGraph.CallCallees.unresolved = true; _ }, _
    | { CallGraph.CallCallees.new_targets = _ :: _; _ }, _
    | { CallGraph.CallCallees.init_targets = _ :: _; _ }, _ ->
        (* We need both the taint on self and on the whole callee. *)
        analyze_callee ~resolution ~is_property_call ~state ~callee
    | { CallGraph.CallCallees.call_targets; _ }, _
      when List.exists
             ~f:(fun { CallGraph.CallTarget.implicit_self; implicit_dunder_call; _ } ->
               implicit_self && implicit_dunder_call)
             call_targets ->
        (* We need the taint on the whole callee. *)
        analyze_callee ~resolution ~is_property_call ~state ~callee
    | { CallGraph.CallCallees.call_targets; _ }, _
      when List.exists
             ~f:(fun { CallGraph.CallTarget.implicit_self; _ } -> implicit_self)
             call_targets ->
        (* We only need the taint of the receiver. *)
        let taint, state =
          match callee.Node.value with
          | Expression.Name (Name.Attribute { base; _ }) ->
              analyze_expression ~resolution ~state ~is_result_used:true ~expression:base
          | _ -> ForwardState.Tree.bottom, state
        in
        { self_taint = Some taint; callee_taint = None; state }
    | _ ->
        (* We can ignore the callee entirely. *)
        { self_taint = None; callee_taint = None; state }


  and analyze_arguments ~resolution ~state ~arguments =
    let compute_argument_taint (arguments_taint, state) argument =
      let taint, state =
        analyze_unstarred_expression
          ~resolution
          ~is_result_used:true
          argument.Call.Argument.value
          state
      in
      taint :: arguments_taint, state
    in
    (* Explicitly analyze arguments from left to right. *)
    let arguments_taint, state = List.fold ~init:([], state) ~f:compute_argument_taint arguments in
    List.rev arguments_taint, state


  and analyze_dictionary_entry
      ~resolution
      ~is_result_used
      (taint, state)
      { Dictionary.Entry.key; value }
    =
    let field_name = AccessPath.get_index key in
    let key_taint, state =
      analyze_expression ~resolution ~state ~is_result_used ~expression:key
      |>> ForwardState.Tree.prepend [AccessPath.dictionary_keys]
    in
    analyze_expression ~resolution ~state ~is_result_used ~expression:value
    |>> ForwardState.Tree.prepend [field_name]
    |>> ForwardState.Tree.join taint
    |>> ForwardState.Tree.join key_taint


  and analyze_list_element ~resolution ~is_result_used position (taint, state) expression =
    let index_name = Abstract.TreeDomain.Label.Index (string_of_int position) in
    analyze_expression ~resolution ~state ~is_result_used ~expression
    |>> ForwardState.Tree.prepend [index_name]
    |>> ForwardState.Tree.join taint


  and analyze_set_element ~resolution ~is_result_used (taint, state) expression =
    let value_taint, state =
      analyze_expression ~resolution ~state ~is_result_used ~expression
      |>> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]
    in
    ForwardState.Tree.join taint value_taint, state


  and analyze_comprehension_generators ~resolution ~state generators =
    let add_binding (state, resolution) ({ Comprehension.Generator.conditions; _ } as generator) =
      let ({ Statement.Assign.target; value; _ } as assignment) =
        Statement.Statement.generator_assignment generator
      in
      let assign_value_taint, state =
        analyze_expression ~resolution ~state ~is_result_used:true ~expression:value
      in
      let state =
        analyze_assignment ~resolution target assign_value_taint assign_value_taint state
      in
      (* Since generators create variables that Pyre sees as scoped within the generator, handle
         them by adding the generator's bindings to the resolution. *)
      let resolution = Resolution.resolve_assignment resolution assignment in
      (* Analyzing the conditions might have issues and side effects. *)
      let analyze_condition state condiiton =
        analyze_expression ~resolution ~state ~is_result_used:false ~expression:condiiton |> snd
      in
      List.fold conditions ~init:state ~f:analyze_condition, resolution
    in
    List.fold ~f:add_binding generators ~init:(state, resolution)


  and analyze_comprehension
      ~resolution
      ~state
      ~is_result_used
      { Comprehension.element; generators; _ }
    =
    let bound_state, resolution = analyze_comprehension_generators ~resolution ~state generators in
    analyze_expression ~resolution ~state:bound_state ~is_result_used ~expression:element
    |>> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]


  and analyze_dictionary_comprehension
      ~resolution
      ~state
      ~is_result_used
      { Comprehension.element = { Dictionary.Entry.key; value }; generators; _ }
    =
    let state, resolution = analyze_comprehension_generators ~resolution ~state generators in
    let value_taint, state =
      analyze_expression ~resolution ~state ~is_result_used ~expression:value
      |>> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]
    in
    let key_taint, state =
      analyze_expression ~resolution ~state ~is_result_used ~expression:key
      |>> ForwardState.Tree.prepend [AccessPath.dictionary_keys]
    in
    ForwardState.Tree.join key_taint value_taint, state


  (* Skip through * and **. Used at call sites where * and ** are handled explicitly *)
  and analyze_unstarred_expression ~resolution ~is_result_used expression state =
    match expression.Node.value with
    | Starred (Starred.Once expression)
    | Starred (Starred.Twice expression) ->
        analyze_expression ~resolution ~state ~is_result_used ~expression
    | _ -> analyze_expression ~resolution ~state ~is_result_used ~expression


  and analyze_arguments_with_higher_order_parameters
      ~resolution
      ~arguments
      ~state
      ~higher_order_parameters
    =
    (* If we have functions `fn1`, `fn2`, `fn3` getting passed into `hof`, we use the following strategy:
     * hof(q, fn1, x, fn2, y, fn3) gets translated into the following block: (analyzed forwards)
     * if rand():
     *   $all = {q, x, y}
     *   $result_fn1 = fn1( *all, **all)
     *   $result_fn2 = fn2( *all, **all)
     *   $result_fn3 = fn3( *all, **all)
     * else:
     *   $result_fn1 = fn1
     *   $result_fn2 = fn2
     *   $result_fn3 = fn3
     * hof(q, $result_fn1, x, $result_fn2, y, $result_fn3)
     *)
    let higher_order_parameters =
      higher_order_parameters
      |> CallGraph.HigherOrderParameterMap.to_list
      |> List.filter_map
           ~f:(fun ({ CallGraph.HigherOrderParameter.index; _ } as higher_order_parameter) ->
             match List.nth arguments index with
             | Some { Call.Argument.value = argument; _ } -> Some (higher_order_parameter, argument)
             | None -> None)
    in

    (* Analyze all function arguments once (ignoring the higher order call for now). *)
    let function_callee_taints, state =
      let analyze_function_arguments
          (function_callee_taints, state)
          ({ CallGraph.HigherOrderParameter.index; _ }, function_argument)
        =
        let { self_taint; callee_taint; state } =
          analyze_callee ~resolution ~is_property_call:false ~state ~callee:function_argument
        in
        let callee_taint = Option.value_exn callee_taint in
        (index, self_taint, callee_taint) :: function_callee_taints, state
      in
      List.fold ~init:([], state) ~f:analyze_function_arguments higher_order_parameters
    in

    (* Analyze all non-function arguments once. *)
    let non_function_arguments_taint, state =
      let function_argument_indices =
        List.fold
          ~init:Int.Set.empty
          ~f:(fun indices ({ CallGraph.HigherOrderParameter.index; _ }, _) ->
            Int.Set.add indices index)
          higher_order_parameters
      in
      let non_function_arguments =
        List.filter_mapi
          ~f:(fun index argument ->
            Option.some_if (not (Int.Set.mem function_argument_indices index)) (index, argument))
          arguments
      in
      let compute_argument_taint (arguments_taint, state) (index, argument) =
        let taint, state =
          analyze_unstarred_expression
            ~resolution
            ~is_result_used:true
            argument.Call.Argument.value
            state
        in
        (index, taint) :: arguments_taint, state
      in
      List.fold ~init:([], state) ~f:compute_argument_taint non_function_arguments
    in

    (* Simulate if branch. *)
    let function_arguments_taints, if_branch_state =
      (* Simulate `$all = {q, x, y}`. *)
      let all_argument_taint =
        List.fold
          non_function_arguments_taint
          ~f:(fun taint (_, argument_taint) -> ForwardState.Tree.join taint argument_taint)
          ~init:ForwardState.Tree.empty
        |> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]
      in

      let analyze_function_call
          (function_call_taints, state)
          ( { CallGraph.HigherOrderParameter.index; call_targets; unresolved },
            ({ Node.location = argument_location; _ } as argument) )
        =
        (* Simulate `$result_fn = fn( *all, **all)`. *)
        let _, self_taint, callee_taint =
          List.find_exn
            ~f:(fun (argument_index, _, _) -> Int.equal argument_index index)
            function_callee_taints
        in
        let all_argument =
          Expression.Name (Name.Identifier "$all") |> Node.create ~location:argument_location
        in
        let arguments =
          [
            {
              Call.Argument.value =
                Expression.Starred (Starred.Once all_argument)
                |> Node.create ~location:argument_location;
              name = None;
            };
            {
              Call.Argument.value =
                Expression.Starred (Starred.Twice all_argument)
                |> Node.create ~location:argument_location;
              name = None;
            };
          ]
        in
        let arguments_taint = [all_argument_taint; all_argument_taint] in
        let taint, state =
          apply_callees_with_arguments_taint
            ~resolution
            ~is_result_used:true
            ~callee:argument
            ~call_location:argument_location
            ~arguments
            ~self_taint
            ~callee_taint:(Some callee_taint)
            ~arguments_taint
            ~state
            (CallGraph.CallCallees.create ~call_targets ~unresolved ())
        in
        let taint = ForwardState.Tree.add_local_breadcrumb (Features.lambda ()) taint in
        (* Join result_fn taint from both if and else branches. *)
        let taint = ForwardState.Tree.join taint callee_taint in
        (index, taint) :: function_call_taints, state
      in
      List.fold ~init:([], state) ~f:analyze_function_call higher_order_parameters
    in

    (* Join state from both if and else branches. *)
    let state = join state if_branch_state in

    (* Return the arguments taint.
     * The caller will simulate `hof(q, $result_fn1, x, $result_fn2, y, $result_fn3)`. *)
    let index_map_to_ordered_list map =
      let compare_by_index (left_index, _) (right_index, _) = Int.compare left_index right_index in
      map |> List.sort ~compare:compare_by_index |> List.map ~f:snd
    in
    let arguments_taint =
      List.rev_append non_function_arguments_taint function_arguments_taints
      |> index_map_to_ordered_list
    in
    arguments_taint, state


  and apply_callees
      ?(apply_tito = true)
      ~resolution
      ~is_result_used
      ~is_property
      ~callee
      ~call_location
      ~arguments
      ~state
      callees
    =
    let { self_taint; callee_taint; state } =
      analyze_callee_for_callees ~resolution ~is_property_call:is_property ~state ~callee callees
    in

    let arguments_taint, state =
      if CallGraph.HigherOrderParameterMap.is_empty callees.higher_order_parameters then
        analyze_arguments ~resolution ~state ~arguments
      else
        analyze_arguments_with_higher_order_parameters
          ~resolution
          ~arguments
          ~state
          ~higher_order_parameters:callees.higher_order_parameters
    in

    apply_callees_with_arguments_taint
      ~apply_tito
      ~resolution
      ~is_result_used
      ~callee
      ~call_location
      ~arguments
      ~self_taint
      ~callee_taint
      ~arguments_taint
      ~state
      callees


  and analyze_getitem_call_target
      ~resolution
      ~is_result_used
      ~index
      ~index_number
      ~state
      ~location
      ~base
      ~taint_and_state_after_index_access
      call_target
    =
    let analyze_getitem receiver_type =
      let named_tuple_attributes =
        Analysis.NamedTuple.field_names ~global_resolution receiver_type
      in
      match named_tuple_attributes, index_number with
      | Some named_tuple_attributes, Some index_number ->
          List.nth named_tuple_attributes index_number
          (* Access an attribute of a named tuple via valid indices *)
          >>| (fun attribute ->
                let { attribute_taint = taint; state; _ } =
                  analyze_attribute_access
                    ~resolution
                    ~state
                    ~is_attribute_used:is_result_used
                    ~resolve_properties:false
                    ~location
                    ~base
                    ~attribute
                    ~special:false
                in
                let taint =
                  taint
                  |> ForwardState.Tree.add_local_first_field attribute
                  |> ForwardState.Tree.add_local_first_index index
                in
                taint, state)
          (* Access an attribute of a named tuple via invalid indices *)
          |> Option.value ~default:(ForwardState.Tree.bottom, bottom)
      | Some _, None ->
          (* Access an attribute of a named tuple via unknown indices *)
          Lazy.force taint_and_state_after_index_access
      | None, _ ->
          (* Not access a named tuple *)
          Lazy.force taint_and_state_after_index_access
    in
    match call_target with
    | {
        CallGraph.CallTarget.target = Method { method_name = "__getitem__"; _ };
        receiver_type = Some receiver_type;
        _;
      }
    | {
        CallGraph.CallTarget.target = Override { method_name = "__getitem__"; _ };
        receiver_type = Some receiver_type;
        _;
      } ->
        (* Potentially access a named tuple *)
        analyze_getitem receiver_type
    | _ ->
        (* Not access a named tuple *)
        Lazy.force taint_and_state_after_index_access


  and analyze_call ~resolution ~location ~state ~is_result_used ~callee ~arguments =
    let { Call.callee; arguments } =
      CallGraph.redirect_special_calls ~resolution { Call.callee; arguments }
    in
    let callees = get_call_callees ~location ~call:{ Call.callee; arguments } in

    (* To properly treat attribute assignments in the constructor, we treat
     * `__init__` calls as an assignment `self = self.__init__()`. *)
    let should_assign_return_to_parameter =
      match Node.value callee, FunctionContext.definition with
      | ( Expression.Name (Name.Attribute { base; attribute = "__init__"; _ }),
          {
            Node.value =
              {
                Statement.Define.signature =
                  {
                    Statement.Define.Signature.parameters =
                      { Node.value = { Parameter.name = self_parameter; _ }; _ } :: _;
                    _;
                  };
                _;
              };
            _;
          } )
        when is_constructor ()
             && Interprocedural.CallResolution.is_super
                  ~resolution
                  ~define:FunctionContext.definition
                  base ->
          Some self_parameter
      | _ -> None
    in
    let is_result_used = is_result_used || Option.is_some should_assign_return_to_parameter in
    let add_type_breadcrumbs taint =
      let type_breadcrumbs = CallModel.type_breadcrumbs_of_calls callees.call_targets in
      taint |> ForwardState.Tree.add_local_breadcrumbs type_breadcrumbs
    in

    let taint, state =
      match { Call.callee; arguments } with
      | {
       callee = { Node.value = Name (Name.Attribute { base; attribute = "__getitem__"; _ }); _ };
       arguments =
         [
           {
             Call.Argument.value = { Node.value = argument_expression; _ } as argument_value;
             name = None;
           };
         ];
      } ->
          let _, state =
            analyze_expression ~resolution ~state ~is_result_used:false ~expression:argument_value
          in
          let index = AccessPath.get_index argument_value in
          let taint_and_state_after_index_access =
            lazy
              (analyze_expression ~resolution ~state ~is_result_used ~expression:base
              |>> ForwardState.Tree.read [index]
              |>> ForwardState.Tree.add_local_first_index index)
          in
          if List.is_empty callees.call_targets then
            (* This call may be unresolved, because for example the receiver type is unknown *)
            Lazy.force taint_and_state_after_index_access |>> add_type_breadcrumbs
          else
            let index_number =
              match argument_expression with
              | Expression.Constant (Constant.Integer i) -> Some i
              | _ -> None
            in
            List.fold
              callees.call_targets
              ~init:(ForwardState.Tree.empty, bottom)
              ~f:(fun (taint_so_far, state_so_far) call_target ->
                let taint, state =
                  analyze_getitem_call_target
                    ~resolution
                    ~is_result_used
                    ~index
                    ~index_number
                    ~state
                    ~location
                    ~base
                    ~taint_and_state_after_index_access
                    call_target
                in
                let taint =
                  let type_breadcrumbs = CallModel.type_breadcrumbs_of_calls [call_target] in
                  ForwardState.Tree.add_local_breadcrumbs type_breadcrumbs taint
                in
                ForwardState.Tree.join taint taint_so_far, join state state_so_far)
      (* Special case `__iter__` and `__next__` as being a random index access (this pattern is the
         desugaring of `for element in x`). *)
      | {
       callee = { Node.value = Name (Name.Attribute { base; attribute = "__next__"; _ }); _ };
       arguments = [];
      } ->
          analyze_expression ~resolution ~state ~is_result_used ~expression:base
          |>> add_type_breadcrumbs
      | {
       callee =
         { Node.value = Name (Name.Attribute { base; attribute = "__iter__"; special = true }); _ };
       arguments = [];
      } ->
          let taint, state =
            analyze_expression ~resolution ~state ~is_result_used ~expression:base
          in
          let label =
            (* For dictionaries, the default iterator is keys. *)
            if CallGraph.CallCallees.is_mapping_method callees then
              AccessPath.dictionary_keys
            else
              Abstract.TreeDomain.Label.AnyIndex
          in
          ForwardState.Tree.read [label] taint, state
      (* x[0] = value is converted to x.__setitem__(0, value). in parsing. *)
      | {
       callee =
         { Node.value = Name (Name.Attribute { base; attribute = "__setitem__"; _ }); _ } as callee;
       arguments =
         [{ Call.Argument.value = index; name = None }; { Call.Argument.value; name = None }] as
         arguments;
      } ->
          let is_dict_setitem = CallGraph.CallCallees.is_mapping_method callees in
          let is_sequence_setitem = CallGraph.CallCallees.is_sequence_method callees in
          let use_custom_tito =
            is_dict_setitem
            || is_sequence_setitem
            || not (CallGraph.CallCallees.is_partially_resolved callees)
          in
          let taint_from_analyze_callee, state_from_analyze_callee =
            (* Process the custom model for `__setitem__`. We treat `e.__setitem__(k, v)` as `e =
               e.__setitem__(k, v)` where method `__setitem__` returns the updated self. Due to
               modeling with the assignment, the user-provided models of `__setitem__` will be
               ignored, if they are inconsistent with treating `__setitem__` as returning an updated
               self. In the case that the call target is a dict or list, only propagate sources and
               sinks, and ignore tito propagation. *)
            let taint, state =
              apply_callees
                ~apply_tito:(not use_custom_tito)
                ~resolution
                ~is_result_used:true
                ~is_property:false
                ~callee
                ~call_location:location
                ~arguments
                ~state
                callees
            in
            let state = analyze_assignment ~resolution base taint taint state in
            taint, state
          in
          if use_custom_tito then
            (* Use the hardcoded behavior of `__setitem__` for any subtype of dict or list, and for
               unresolved calls. This is incorrect, but can lead to higher SNR, because we assume in
               most cases, we run into an expression whose type is exactly `dict`, rather than a
               (strict) subtype of `dict` that overrides `__setitem__`. *)
            let state =
              let value_taint, state =
                analyze_expression ~resolution ~state ~is_result_used:true ~expression:value
              in
              analyze_assignment
                ~resolution
                ~fields:[AccessPath.get_index index]
                base
                value_taint
                value_taint
                state
            in
            let state_from_key_value =
              let key_taint, state =
                analyze_expression ~resolution ~state ~is_result_used:true ~expression:index
              in
              if not is_sequence_setitem then
                (* Smash the taint of ALL keys into one place, i.e., a special field of the
                   dictionary: `d[**keys] = index`. *)
                analyze_assignment
                  ~resolution
                  ~fields:[AccessPath.dictionary_keys]
                  ~weak:true
                  base
                  key_taint
                  key_taint
                  state
              else
                state
            in
            let state = join state_from_key_value state_from_analyze_callee in
            (* Since `dict.__setitem__` returns None, we return no taint here. *)
            ForwardState.Tree.empty, state
          else
            taint_from_analyze_callee, state_from_analyze_callee
      (* We special object.__setattr__, which is sometimes used in order to work around dataclasses
         being frozen post-initialization. *)
      | {
       callee =
         {
           Node.value =
             Name
               (Name.Attribute
                 {
                   base = { Node.value = Name (Name.Identifier "object"); _ };
                   attribute = "__setattr__";
                   _;
                 });
           _;
         };
       arguments =
         [
           { Call.Argument.value = self; name = None };
           {
             Call.Argument.value =
               {
                 Node.value =
                   Expression.Constant (Constant.String { StringLiteral.value = attribute; _ });
                 _;
               };
             name = None;
           };
           { Call.Argument.value = assigned_value; name = None };
         ];
      } ->
          let taint, state =
            analyze_expression ~resolution ~state ~is_result_used:true ~expression:assigned_value
          in
          let state =
            analyze_assignment
              ~resolution
              (Expression.Name (Name.Attribute { base = self; attribute; special = true })
              |> Node.create ~location)
              taint
              taint
              state
          in
          taint, state
      (* `getattr(a, "field", default)` should evaluate to the join of `a.field` and `default`. *)
      | {
       callee = { Node.value = Name (Name.Identifier "getattr"); _ };
       arguments =
         [
           { Call.Argument.value = base; name = None };
           {
             Call.Argument.value =
               {
                 Node.value =
                   Expression.Constant (Constant.String { StringLiteral.value = attribute; _ });
                 _;
               };
             name = None;
           };
           { Call.Argument.value = default; name = _ };
         ];
      } ->
          let attribute_expression =
            Expression.Name (Name.Attribute { base; attribute; special = false })
            |> Node.create ~location
          in
          let attribute_taint, state =
            analyze_expression ~resolution ~state ~is_result_used ~expression:attribute_expression
          in
          let default_taint, state =
            analyze_expression ~resolution ~state ~is_result_used ~expression:default
          in
          ForwardState.Tree.join attribute_taint default_taint, state
      (* `zip(a, b, ...)` creates a taint object which, when iterated on, has first index equal to
         a[*]'s taint, second index with b[*]'s taint, etc. *)
      | { callee = { Node.value = Name (Name.Identifier "zip"); _ }; arguments = lists } ->
          let add_list_to_taint index (taint, state) { Call.Argument.value; _ } =
            let index_name = Abstract.TreeDomain.Label.Index (string_of_int index) in
            analyze_expression ~resolution ~state ~is_result_used ~expression:value
            |>> ForwardState.Tree.read [Abstract.TreeDomain.Label.AnyIndex]
            |>> ForwardState.Tree.prepend [index_name]
            |>> ForwardState.Tree.join taint
          in
          List.foldi lists ~init:(ForwardState.Tree.bottom, state) ~f:add_list_to_taint
          |>> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]
      | {
       Call.callee =
         {
           Node.value =
             Name
               (Name.Attribute
                 { base = { Node.value = Expression.Name name; _ }; attribute = "gather"; _ });
           _;
         };
       arguments;
      }
        when String.equal "asyncio" (Name.last name) ->
          analyze_expression
            ~resolution
            ~state
            ~is_result_used
            ~expression:
              {
                Node.location = Node.location callee;
                value =
                  Expression.Tuple
                    (List.map arguments ~f:(fun argument -> argument.Call.Argument.value));
              }
      (* dictionary .keys(), .values() and .items() functions are special, as they require handling
         of dictionary_keys taint. *)
      | {
       callee = { Node.value = Name (Name.Attribute { base; attribute = "values"; _ }); _ };
       arguments = [];
      }
        when CallGraph.CallCallees.is_mapping_method callees ->
          analyze_expression ~resolution ~state ~is_result_used ~expression:base
          |>> ForwardState.Tree.read [Abstract.TreeDomain.Label.AnyIndex]
          |>> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]
      | {
       callee = { Node.value = Name (Name.Attribute { base; attribute = "keys"; _ }); _ };
       arguments = [];
      }
        when CallGraph.CallCallees.is_mapping_method callees ->
          analyze_expression ~resolution ~state ~is_result_used ~expression:base
          |>> ForwardState.Tree.read [AccessPath.dictionary_keys]
          |>> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]
      | {
       callee =
         {
           Node.value =
             Name
               (Name.Attribute
                 {
                   base = { Node.value = Name (Name.Identifier identifier); _ } as base;
                   attribute = "update";
                   _;
                 });
           _;
         };
       arguments =
         [
           {
             Call.Argument.value =
               { Node.value = Expression.Dictionary { Dictionary.entries; keywords = [] }; _ };
             name = None;
           };
         ];
      }
        when CallGraph.CallCallees.is_mapping_method callees
             && Option.is_some (Dictionary.string_literal_keys entries) ->
          let entries = Option.value_exn (Dictionary.string_literal_keys entries) in
          let taint =
            ForwardState.read ~root:(AccessPath.Root.Variable identifier) ~path:[] state.taint
          in
          let override_taint_from_update (taint, state) (key, value) =
            let value_taint, state =
              analyze_expression ~resolution ~state ~is_result_used:true ~expression:value
            in
            let value_taint =
              ForwardState.Tree.transform
                Features.TitoPositionSet.Element
                Add
                ~f:value.Node.location
                value_taint
            in
            let new_taint =
              ForwardState.Tree.assign
                ~weak:false
                ~tree:taint
                [Abstract.TreeDomain.Label.Index key]
                ~subtree:value_taint
            in
            new_taint, state
          in
          let taint, state = List.fold entries ~init:(taint, state) ~f:override_taint_from_update in
          GlobalModel.from_expression
            ~resolution
            ~call_graph:FunctionContext.call_graph_of_define
            ~get_callee_model:FunctionContext.get_callee_model
            ~qualifier:FunctionContext.qualifier
            ~expression:base
            ~interval:FunctionContext.caller_class_interval
          |> check_flow_to_global ~location:base.Node.location ~source_tree:taint;
          let state =
            store_taint ~root:(AccessPath.Root.Variable identifier) ~path:[] taint state
          in
          taint, state
      | {
       callee =
         {
           Node.value =
             Name
               (Name.Attribute
                 {
                   base = { Node.value = Name (Name.Identifier identifier); _ };
                   attribute = "pop";
                   _;
                 });
           _;
         };
       arguments =
         [
           {
             Call.Argument.value =
               { Node.value = Expression.Constant (Constant.String { StringLiteral.value; _ }); _ };
             name = None;
           };
         ];
      }
        when CallGraph.CallCallees.is_mapping_method callees ->
          let taint =
            ForwardState.read ~root:(AccessPath.Root.Variable identifier) ~path:[] state.taint
          in
          let new_taint =
            ForwardState.Tree.assign
              ~weak:false
              ~tree:taint
              [Abstract.TreeDomain.Label.Index value]
              ~subtree:ForwardState.Tree.bottom
          in
          let new_state =
            store_taint ~root:(AccessPath.Root.Variable identifier) ~path:[] new_taint state
          in
          let key_taint =
            ForwardState.Tree.read [Abstract.TreeDomain.Label.Index value] taint
            |> add_type_breadcrumbs
          in
          key_taint, new_state
      | {
       callee = { Node.value = Name (Name.Attribute { base; attribute = "items"; _ }); _ };
       arguments = [];
      }
        when CallGraph.CallCallees.is_mapping_method callees ->
          let taint, state =
            analyze_expression ~resolution ~state ~is_result_used ~expression:base
          in
          let taint =
            let key_taint = ForwardState.Tree.read [AccessPath.dictionary_keys] taint in
            let value_taint = ForwardState.Tree.read [Abstract.TreeDomain.Label.AnyIndex] taint in
            ForwardState.Tree.join
              (ForwardState.Tree.prepend [Abstract.TreeDomain.Label.create_int_index 0] key_taint)
              (ForwardState.Tree.prepend [Abstract.TreeDomain.Label.create_int_index 1] value_taint)
            |> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]
          in
          taint, state
      (* `locals()` is a dictionary from all local names -> values. *)
      | { callee = { Node.value = Name (Name.Identifier "locals"); _ }; arguments = [] } ->
          let add_root_taint locals_taint root =
            let path_of_root =
              match root with
              | AccessPath.Root.Variable variable ->
                  [Abstract.TreeDomain.Label.Index (Identifier.sanitized variable)]
              | NamedParameter { name }
              | PositionalParameter { name; _ } ->
                  [Abstract.TreeDomain.Label.Index (Identifier.sanitized name)]
              | _ -> [Abstract.TreeDomain.Label.AnyIndex]
            in
            let root_taint =
              ForwardState.read ~root ~path:[] state.taint |> ForwardState.Tree.prepend path_of_root
            in
            ForwardState.Tree.join locals_taint root_taint
          in
          let taint =
            List.fold
              (ForwardState.roots state.taint)
              ~init:ForwardState.Tree.bottom
              ~f:add_root_taint
          in
          taint, state
      (* Special case `"{}".format(s)` and `"%s" % (s,)` for Literal String Sinks *)
      | {
          callee =
            {
              Node.value =
                Name
                  (Name.Attribute
                    {
                      base =
                        {
                          Node.value = Constant (Constant.String { StringLiteral.value; _ });
                          location = value_location;
                        };
                      attribute = "__mod__" as function_name;
                      _;
                    });
              _;
            };
          arguments;
        }
      | {
          callee =
            {
              Node.value =
                Name
                  (Name.Attribute
                    {
                      base =
                        {
                          Node.value = Constant (Constant.String { StringLiteral.value; _ });
                          location = value_location;
                        };
                      attribute = "format" as function_name;
                      _;
                    });
              _;
            };
          arguments;
        } ->
          let nested_expressions =
            List.map ~f:(fun call_argument -> call_argument.value) arguments
          in
          let default_target =
            match function_name with
            | "__mod__" -> Interprocedural.Target.StringCombineArtificialTargets.str_mod
            | "format" -> Interprocedural.Target.StringCombineArtificialTargets.str_format
            | _ -> failwith "Expect either `__mod__` or `format`"
          in
          let call_target_for_string_combine_rules =
            Some
              (StringFormatCall.create_call_target_for_string_combine
                 ~call_targets:callees.call_targets
                 ~default_target)
          in
          analyze_joined_string
            ~resolution
            ~state
            ~breadcrumbs:(Features.BreadcrumbSet.singleton (Features.format_string ()))
            {
              StringFormatCall.nested_expressions;
              string_literal = { value; location = value_location };
              call_target_for_string_combine_rules;
              location;
            }
      (* Special case `"str" + s` and `s + "str"` for Literal String Sinks *)
      | {
       callee =
         { Node.value = Name (Name.Attribute { base = expression; attribute = "__add__"; _ }); _ };
       arguments =
         [
           {
             Call.Argument.value =
               {
                 Node.value = Expression.Constant (Constant.String { StringLiteral.value; _ });
                 location = value_location;
               };
             name = None;
           };
         ];
      } ->
          let call_target_for_string_combine_rules =
            Some
              (StringFormatCall.create_call_target_for_string_combine
                 ~call_targets:callees.call_targets
                 ~default_target:Interprocedural.Target.StringCombineArtificialTargets.str_add)
          in
          analyze_joined_string
            ~resolution
            ~state
            ~breadcrumbs:
              (Features.BreadcrumbSet.singleton (Features.string_concat_left_hand_side ()))
            {
              StringFormatCall.nested_expressions = [expression];
              string_literal = { value; location = value_location };
              call_target_for_string_combine_rules;
              location;
            }
      | {
       callee =
         {
           Node.value =
             Name
               (Name.Attribute
                 {
                   base =
                     {
                       Node.value = Constant (Constant.String { StringLiteral.value; _ });
                       location = value_location;
                     };
                   attribute = "__add__";
                   _;
                 });
           _;
         };
       arguments = [{ Call.Argument.value = expression; name = None }];
      } ->
          let call_target_for_string_combine_rules =
            Some
              (StringFormatCall.create_call_target_for_string_combine
                 ~call_targets:callees.call_targets
                 ~default_target:Interprocedural.Target.StringCombineArtificialTargets.str_add)
          in
          analyze_joined_string
            ~resolution
            ~state
            ~breadcrumbs:
              (Features.BreadcrumbSet.singleton (Features.string_concat_right_hand_side ()))
            {
              StringFormatCall.nested_expressions = [expression];
              string_literal = { value; location = value_location };
              call_target_for_string_combine_rules;
              location;
            }
      | {
       callee =
         {
           Node.value =
             Name
               (Name.Attribute
                 { base; attribute = ("__add__" | "__mod__" | "format") as function_name; _ });
           _;
         };
       arguments;
      }
        when CallGraph.CallCallees.is_string_method callees ->
          let breadcrumbs =
            match function_name with
            | "__mod__"
            | "format" ->
                Features.BreadcrumbSet.singleton (Features.format_string ())
            | _ -> Features.BreadcrumbSet.empty
          in
          let default_target =
            match function_name with
            | "__add__" -> Interprocedural.Target.StringCombineArtificialTargets.str_add
            | "__mod__" -> Interprocedural.Target.StringCombineArtificialTargets.str_mod
            | "format" -> Interprocedural.Target.StringCombineArtificialTargets.str_format
            | _ -> failwith "Expect either `__add__` or `__mod__` or `format`"
          in
          let call_target_for_string_combine_rules =
            Some
              (StringFormatCall.create_call_target_for_string_combine
                 ~call_targets:callees.call_targets
                 ~default_target)
          in
          let nested_expressions =
            arguments
            |> List.map ~f:(fun call_argument -> call_argument.Call.Argument.value)
            |> List.cons base
          in
          let string_literal, nested_expressions =
            CallModel.arguments_for_string_format nested_expressions
          in
          analyze_joined_string
            ~resolution
            ~state
            ~breadcrumbs
            {
              StringFormatCall.nested_expressions;
              string_literal = { StringFormatCall.value = string_literal; location };
              call_target_for_string_combine_rules;
              location;
            }
      | {
       callee = { Node.value = Expression.Name (Name.Identifier "reveal_taint"); _ };
       arguments = [{ Call.Argument.value = expression; _ }];
      } ->
          let taint, _ = analyze_expression ~resolution ~state ~is_result_used:true ~expression in
          let location =
            Node.location callee |> Location.with_module ~module_reference:FunctionContext.qualifier
          in
          Log.dump
            "%a: Revealed forward taint for `%s`: %s"
            Location.WithModule.pp
            location
            (Transform.sanitize_expression expression |> Expression.show)
            (ForwardState.Tree.show taint);
          ForwardState.Tree.bottom, state
      | {
       callee = { Node.value = Expression.Name (Name.Identifier "reveal_type"); _ };
       arguments = [{ Call.Argument.value = expression; _ }];
      } ->
          let location =
            Node.location callee |> Location.with_module ~module_reference:FunctionContext.qualifier
          in
          Log.dump
            "%a: Revealed type for %s: %s"
            Location.WithModule.pp
            location
            (Transform.sanitize_expression expression |> Expression.show)
            (CallResolution.resolve_ignoring_untracked ~resolution expression |> Type.show);
          ForwardState.Tree.bottom, state
      | _ ->
          apply_callees
            ~resolution
            ~is_result_used
            ~is_property:false
            ~call_location:location
            ~callee
            ~arguments
            ~state
            callees
    in

    let taint, state =
      match should_assign_return_to_parameter with
      | Some self_parameter ->
          let self = AccessPath.Root.Variable self_parameter in
          let self_taint =
            ForwardState.read ~root:self ~path:[] state.taint |> ForwardState.Tree.join taint
          in
          taint, { taint = ForwardState.assign ~root:self ~path:[] self_taint state.taint }
      | None -> taint, state
    in

    if not FunctionContext.taint_configuration.lineage_analysis then
      taint, state
    else
      let analyze_expression_unwrap ~resolution ~state ~expression =
        let taint, state =
          analyze_expression ~resolution ~state:{ taint = state } ~is_result_used:true ~expression
        in
        taint, state.taint
      in
      let taint, forward_state =
        LineageAnalysis.forward_analyze_call
          ~analyze_expression:analyze_expression_unwrap
          ~resolution
          ~callee
          ~arguments
          ~taint
          ~state:state.taint
      in
      taint, { taint = forward_state }


  and analyze_attribute_access
      ~resolution
      ~state
      ~is_attribute_used
      ~location
      ~resolve_properties
      ~base
      ~attribute
      ~special
    =
    let expression =
      Expression.Name (Name.Attribute { base; attribute; special }) |> Node.create ~location
    in
    let base_taint, state =
      analyze_expression ~resolution ~state ~is_result_used:true ~expression:base
    in
    let attribute_access_callees =
      if resolve_properties then get_attribute_access_callees ~location ~attribute else None
    in

    let property_call_result =
      match attribute_access_callees with
      | Some { property_targets = _ :: _ as property_targets; _ } ->
          let taint, state =
            apply_callees_with_arguments_taint
              ~resolution
              ~is_result_used:is_attribute_used
              ~callee:expression
              ~call_location:location
              ~arguments:[]
              ~self_taint:(Some base_taint)
              ~callee_taint:None
              ~arguments_taint:[]
              ~state
              (CallGraph.CallCallees.create ~call_targets:property_targets ())
          in
          { base_taint = ForwardState.Tree.bottom; attribute_taint = taint; state }
      | _ ->
          {
            base_taint = ForwardState.Tree.bottom;
            attribute_taint = ForwardState.Tree.bottom;
            state = bottom;
          }
    in

    let regular_attribute_result =
      match attribute_access_callees with
      | Some { is_attribute = true; _ }
      | None ->
          let global_model =
            GlobalModel.from_expression
              ~resolution
              ~call_graph:FunctionContext.call_graph_of_define
              ~get_callee_model:FunctionContext.get_callee_model
              ~qualifier:FunctionContext.qualifier
              ~expression
              ~interval:FunctionContext.caller_class_interval
          in
          let attribute_taint = GlobalModel.get_source global_model in
          let add_tito_features taint =
            let attribute_breadcrumbs =
              global_model |> GlobalModel.get_tito |> BackwardState.Tree.joined_breadcrumbs
            in
            ForwardState.Tree.add_local_breadcrumbs attribute_breadcrumbs taint
          in
          let apply_attribute_sanitizers taint =
            let sanitizers =
              let sanitizer = GlobalModel.get_sanitize global_model in
              { SanitizeTransformSet.sources = sanitizer.sources; sinks = sanitizer.sinks }
            in
            let taint =
              ForwardState.Tree.apply_sanitize_transforms
                ~taint_configuration:FunctionContext.taint_configuration
                sanitizers
                TaintTransformOperation.InsertLocation.Front
                taint
            in
            taint
          in
          let attribute_taint =
            if is_attribute_used then
              base_taint
              |> add_tito_features
              |> ForwardState.Tree.read [Abstract.TreeDomain.Label.Index attribute]
              |> ForwardState.Tree.add_local_first_field attribute
              (* This should be applied before the join with the attribute taint, so inferred taint
               * is sanitized, but user-specified taint on the attribute is still propagated. *)
              |> apply_attribute_sanitizers
              |> ForwardState.Tree.join attribute_taint
            else
              ForwardState.Tree.bottom
          in
          { base_taint; attribute_taint; state }
      | _ ->
          {
            base_taint = ForwardState.Tree.bottom;
            attribute_taint = ForwardState.Tree.bottom;
            state = bottom;
          }
    in

    join_analyze_attribute_access_result property_call_result regular_attribute_result


  (* If there exists a triggered flow, `call_target_for_string_combine_rules` is the responsible
     callsite. We assume there is a call to `call_target_for_string_combine_rules` whose arguments
     are `string_literal` followed by `nested_expressions`. *)
  and analyze_joined_string
      ~resolution
      ~state
      ~breadcrumbs
      {
        StringFormatCall.nested_expressions;
        string_literal = { location = string_literal_location; _ } as string_literal;
        call_target_for_string_combine_rules;
        location;
      }
    =
    let callee =
      match call_target_for_string_combine_rules with
      | Some call_target -> Some call_target.target
      | None -> None
    in
    let string_combine_partial_sink_tree =
      BackwardState.Tree.apply_call
        ~resolution
        ~location:(Location.with_module ~module_reference:FunctionContext.qualifier location)
        ~callee
        ~arguments:[]
        ~port:AccessPath.Root.LocalResult
        ~is_self_call:false
        ~caller_class_interval:Interprocedural.ClassIntervalSet.top
        ~receiver_class_interval:Interprocedural.ClassIntervalSet.top
        FunctionContext.string_combine_partial_sink_tree
    in
    let string_literal_taint = StringFormatCall.implicit_string_literal_sources string_literal in
    let triggered_sinks = Issue.TriggeredSinkHashMap.create () in
    StringFormatCall.check_triggered_flows
      ~triggered_sinks
      ~parameter_index:0
      ~call_target_for_string_combine_rules
      ~location:string_literal_location
      ~string_combine_partial_sink_tree
      (ForwardState.Tree.create_leaf string_literal_taint);

    let analyze_stringify_callee
        (taint_to_join, state_to_join)
        ~call_target
        ~call_location
        ~base
        ~base_taint
        ~base_state
      =
      let callees = CallGraph.CallCallees.create ~call_targets:[call_target] () in
      let callee =
        let callee_from_method_name method_name =
          {
            Node.value =
              Expression.Name (Name.Attribute { base; attribute = method_name; special = false });
            location = call_location;
          }
        in
        match call_target.target with
        | Interprocedural.Target.Method { method_name; _ } -> callee_from_method_name method_name
        | Override { method_name; _ } -> callee_from_method_name method_name
        | Function { name; _ } ->
            { Node.value = Name (Name.Identifier name); location = call_location }
        | Object _ -> failwith "callees should be either methods or functions"
      in
      let new_taint, new_state =
        apply_callees_with_arguments_taint
          ~resolution
          ~is_result_used:true
          ~call_location
          ~arguments:[]
          ~self_taint:(Some base_taint)
          ~callee_taint:None
          ~arguments_taint:[]
          ~state:base_state
          ~callee
          callees
      in
      ForwardState.Tree.join taint_to_join new_taint, join state_to_join new_state
    in
    let analyze_nested_expression
        index
        (taint, state)
        ({ Node.location = expression_location; _ } as expression)
      =
      let base_taint, base_state =
        analyze_expression ~resolution ~state ~is_result_used:true ~expression
        |>> ForwardState.Tree.transform Features.TitoPositionSet.Element Add ~f:expression_location
        |>> ForwardState.Tree.add_local_breadcrumb (Features.tito ())
      in
      let expression_taint, state =
        match get_string_format_callees ~location:expression_location with
        | Some { CallGraph.StringFormatCallees.stringify_targets = _ :: _ as stringify_targets; _ }
          ->
            List.fold
              stringify_targets
              ~init:(ForwardState.Tree.empty, { taint = ForwardState.empty })
              ~f:(fun (taint_to_join, state_to_join) call_target ->
                analyze_stringify_callee
                  (taint_to_join, state_to_join)
                  ~call_target
                  ~call_location:expression_location
                  ~base:expression
                  ~base_taint
                  ~base_state)
        | _ ->
            (* When there is no stringify target, treat as TITO. Meanwhile, we do not analyze
               f_string_targets because they are artificial targets that are introduced only for
               distinguishing f-strings at different locations from the same function. Such targets
               are only used in the issue sink handles, to distinguish the locations of the
               issues. *)
            base_taint, base_state
      in
      let expression_taint =
        ForwardState.Tree.collapse ~breadcrumbs:(Features.tito_broadening_set ()) expression_taint
      in
      StringFormatCall.check_triggered_flows
        ~triggered_sinks
        ~parameter_index:(index + 1)
        ~call_target_for_string_combine_rules
        ~location:expression_location
        ~string_combine_partial_sink_tree
        (ForwardState.Tree.create_leaf expression_taint);
      ForwardTaint.join expression_taint taint, state
    in
    let taint, state =
      List.foldi nested_expressions ~f:analyze_nested_expression ~init:(ForwardTaint.bottom, state)
      |>> ForwardTaint.add_local_breadcrumbs breadcrumbs
      |>> ForwardTaint.join string_literal_taint
    in
    store_triggered_sinks_to_propagate_for_string_combine
      ~location
      ~triggered_sinks
      ~sink_tree:string_combine_partial_sink_tree
      nested_expressions;
    (* Compute flows to literal string sinks if applicable. *)
    StringFormatCall.check_flow_implicit_string_literal_sinks ~string_literal taint;
    ForwardState.Tree.create_leaf taint, state


  and analyze_expression
      ~resolution
      ~state
      ~is_result_used
      ~expression:({ Node.location; _ } as expression)
    =
    let taint, state =
      match expression.Node.value with
      | Await expression -> analyze_expression ~resolution ~state ~is_result_used ~expression
      | BooleanOperator { left; operator = _; right } ->
          let left_taint, state =
            analyze_expression ~resolution ~state ~is_result_used ~expression:left
          in
          let right_taint, state =
            analyze_expression ~resolution ~state ~is_result_used ~expression:right
          in
          ForwardState.Tree.join left_taint right_taint, state
      | ComparisonOperator ({ left; operator = _; right } as comparison) -> (
          match ComparisonOperator.override ~location comparison with
          | Some override ->
              analyze_expression ~resolution ~state ~is_result_used ~expression:override
          | None ->
              let left_taint, state =
                analyze_expression ~resolution ~state ~is_result_used ~expression:left
              in
              let right_taint, state =
                analyze_expression ~resolution ~state ~is_result_used ~expression:right
              in
              let taint =
                ForwardState.Tree.join left_taint right_taint
                |> ForwardState.Tree.add_local_breadcrumbs (Features.type_bool_scalar_set ())
              in
              taint, state)
      | Call { callee; arguments } ->
          analyze_call ~resolution ~location ~state ~is_result_used ~callee ~arguments
      | Constant (Constant.String { StringLiteral.value; _ }) ->
          analyze_joined_string
            ~resolution
            ~state
            ~breadcrumbs:Features.BreadcrumbSet.empty
            {
              StringFormatCall.nested_expressions = [];
              string_literal = { value; location };
              call_target_for_string_combine_rules = None;
              location;
            }
      | Constant _ -> ForwardState.Tree.empty, state
      | Dictionary { Dictionary.entries; keywords } ->
          let taint, state =
            List.fold
              entries
              ~f:(analyze_dictionary_entry ~resolution ~is_result_used)
              ~init:(ForwardState.Tree.empty, state)
          in
          let analyze_dictionary_keywords (taint, state) keywords =
            let new_taint, state =
              analyze_expression ~resolution ~state ~is_result_used ~expression:keywords
            in
            ForwardState.Tree.join new_taint taint, state
          in
          List.fold keywords ~f:analyze_dictionary_keywords ~init:(taint, state)
      | DictionaryComprehension comprehension ->
          analyze_dictionary_comprehension ~resolution ~state ~is_result_used comprehension
      | Generator comprehension ->
          analyze_comprehension ~resolution ~state ~is_result_used comprehension
      | Lambda { parameters = _; body } ->
          (* Ignore parameter bindings and pretend body is inlined *)
          analyze_expression ~resolution ~state ~is_result_used ~expression:body
      | List list ->
          List.foldi
            list
            ~f:(analyze_list_element ~resolution ~is_result_used)
            ~init:(ForwardState.Tree.empty, state)
      | ListComprehension comprehension ->
          analyze_comprehension ~resolution ~state ~is_result_used comprehension
      | Name (Name.Identifier identifier) ->
          let global_model_taint =
            GlobalModel.from_expression
              ~resolution
              ~call_graph:FunctionContext.call_graph_of_define
              ~get_callee_model:FunctionContext.get_callee_model
              ~qualifier:FunctionContext.qualifier
              ~expression
              ~interval:FunctionContext.caller_class_interval
            |> GlobalModel.get_source
          in

          let global_taint, state =
            let as_reference = Reference.create identifier in
            let global_string =
              Interprocedural.GlobalConstants.SharedMemory.get
                FunctionContext.global_constants
                as_reference
            in
            (* Reanalyze expression with global identifier replaced by assigned string *)
            match global_string with
            | Some global_string ->
                analyze_joined_string
                  ~resolution
                  ~state
                  ~breadcrumbs:Features.BreadcrumbSet.empty
                  {
                    StringFormatCall.nested_expressions = [];
                    string_literal = { value = global_string.value; location };
                    call_target_for_string_combine_rules = None;
                    location;
                  }
            | None -> ForwardState.Tree.empty, state
          in

          let local_taint =
            let root = AccessPath.Root.Variable identifier in
            ForwardState.read ~root ~path:[] state.taint
            |> ForwardState.Tree.add_local_type_breadcrumbs ~resolution ~expression
          in
          ( local_taint
            |> ForwardState.Tree.join global_taint
            |> ForwardState.Tree.join global_model_taint,
            state )
      (* __dict__ reveals an object's underlying data structure, so we should analyze the base under
         the existing taint instead of adding the index to the taint. *)
      | Name (Name.Attribute { base; attribute = "__dict__"; _ }) ->
          analyze_expression ~resolution ~state ~is_result_used ~expression:base
      | Name (Name.Attribute { base; attribute; special }) ->
          let { attribute_taint; state; _ } =
            analyze_attribute_access
              ~resolution
              ~state
              ~is_attribute_used:is_result_used
              ~resolve_properties:true
              ~location
              ~base
              ~attribute
              ~special
          in
          attribute_taint, state
      | Set set ->
          List.fold
            ~f:(analyze_set_element ~resolution ~is_result_used)
            set
            ~init:(ForwardState.Tree.empty, state)
      | SetComprehension comprehension ->
          analyze_comprehension ~resolution ~state ~is_result_used comprehension
      | Starred (Starred.Once expression)
      | Starred (Starred.Twice expression) ->
          analyze_expression ~resolution ~state ~is_result_used ~expression
          |>> ForwardState.Tree.read [Abstract.TreeDomain.Label.AnyIndex]
      | FormatString substrings ->
          let substrings =
            List.map substrings ~f:(function
                | Substring.Format expression -> expression
                | Substring.Literal { Node.value; location } ->
                    Expression.Constant (Constant.String { StringLiteral.value; kind = String })
                    |> Node.create ~location)
          in
          let string_literal, nested_expressions =
            CallModel.arguments_for_string_format substrings
          in
          let call_target_for_string_combine_rules =
            let call_targets =
              match get_string_format_callees ~location with
              | Some { CallGraph.StringFormatCallees.f_string_targets; _ } -> f_string_targets
              | None -> []
            in
            StringFormatCall.create_call_target_for_string_combine
              ~call_targets
              ~default_target:Interprocedural.Target.StringCombineArtificialTargets.format_string
          in
          analyze_joined_string
            ~resolution
            ~state
            ~breadcrumbs:(Features.BreadcrumbSet.singleton (Features.format_string ()))
            {
              StringFormatCall.nested_expressions;
              string_literal = { value = string_literal; location };
              call_target_for_string_combine_rules = Some call_target_for_string_combine_rules;
              location;
            }
      | Ternary { target; test; alternative } ->
          let state = analyze_condition ~resolution test state in
          let taint_then, state_then =
            analyze_expression ~resolution ~state ~is_result_used ~expression:target
          in
          let taint_else, state_else =
            analyze_expression ~resolution ~state ~is_result_used ~expression:alternative
          in
          ForwardState.Tree.join taint_then taint_else, join state_then state_else
      | Tuple expressions ->
          List.foldi
            ~f:(analyze_list_element ~resolution ~is_result_used)
            expressions
            ~init:(ForwardState.Tree.empty, state)
      | UnaryOperator { operator = _; operand } ->
          analyze_expression ~resolution ~state ~is_result_used ~expression:operand
      | WalrusOperator { target; value } ->
          let value_taint, state =
            analyze_expression ~resolution ~state ~is_result_used:true ~expression:value
          in
          let state = analyze_assignment ~resolution target value_taint value_taint state in
          value_taint, state
      | Yield None -> ForwardState.Tree.empty, state
      | Yield (Some expression)
      | YieldFrom expression ->
          let taint, state =
            analyze_expression ~resolution ~state ~is_result_used:true ~expression
          in
          taint, store_taint ~root:AccessPath.Root.LocalResult ~path:[] taint state
    in
    log "Forward taint of expression `%a`: %a" Expression.pp expression ForwardState.Tree.pp taint;
    taint, state


  and analyze_assignment
      ~resolution
      ?(weak = false)
      ?(fields = [])
      ({ Node.location; value } as target)
      taint
      surrounding_taint
      state
    =
    let taint = ForwardState.Tree.add_local_type_breadcrumbs ~resolution ~expression:target taint in
    match value with
    | Starred (Once target | Twice target) ->
        (* This is approximate. Unless we can get the tuple type on the right to tell how many total
           elements there will be, we just pick up the entire collection. *)
        analyze_assignment ~resolution ~weak target surrounding_taint surrounding_taint state
    | List targets
    | Tuple targets ->
        let analyze_target_element i state target =
          let index = Abstract.TreeDomain.Label.Index (string_of_int i) in
          let indexed_taint = ForwardState.Tree.read [index] taint in
          analyze_assignment ~resolution ~weak target indexed_taint taint state
        in
        List.foldi targets ~f:analyze_target_element ~init:state
    (* Assignments of the form a[1][2] = 3 translate to a.__getitem__(1).__setitem__(2, 3).*)
    | Call
        {
          callee = { Node.value = Name (Name.Attribute { base; attribute = "__getitem__"; _ }); _ };
          arguments = [{ Call.Argument.value = argument_value; _ }];
        } ->
        let index = AccessPath.get_index argument_value in
        analyze_assignment
          ~resolution
            (* Fields should only get propagated for getitem/setitem chains. These fields are not
             * reversed, as `a[0][1]` is parsed as `(a[0])[1]`, and `[1]` will appear as the
             * first attribute. *)
          ~weak
          ~fields:(index :: fields)
          base
          taint
          surrounding_taint
          state
    | _ ->
        (* Check flows to tainted globals/attributes. *)
        let source_tree = taint in
        GlobalModel.from_expression
          ~resolution
          ~call_graph:FunctionContext.call_graph_of_define
          ~get_callee_model:FunctionContext.get_callee_model
          ~qualifier:FunctionContext.qualifier
          ~expression:target
          ~interval:FunctionContext.caller_class_interval
        |> check_flow_to_global ~location ~source_tree;

        (* Propagate taint. *)
        let access_path = AccessPath.of_expression target >>| AccessPath.extend ~path:fields in
        store_taint_option ~weak access_path taint state


  and analyze_condition ~resolution expression state =
    let { Node.location; _ } = expression in
    let location = Location.with_module ~module_reference:FunctionContext.qualifier location in
    let taint, state = analyze_expression ~resolution ~state ~is_result_used:true ~expression in
    (* There maybe configured sinks for conditionals, so test them here. *)
    let () =
      FunctionContext.taint_configuration.implicit_sinks.conditional_test
      |> List.iter ~f:(fun sink_kind ->
             let sink_tree =
               BackwardTaint.singleton (CallInfo.Origin location) sink_kind Frame.initial
               |> BackwardState.Tree.create_leaf
             in
             check_flow
               ~location
               ~sink_handle:(IssueHandle.Sink.ConditionalTestSink sink_kind)
               ~source_tree:taint
               ~sink_tree)
    in
    state


  let analyze_definition ~define:_ state = state

  let analyze_statement ~resolution { Node.value = statement; location } state =
    match statement with
    | Statement.Statement.Assign
        { value = { Node.value = Expression.Constant Constant.Ellipsis; _ }; _ } ->
        state
    | Assign { value = { Node.value = Expression.Constant Constant.NoneLiteral; _ }; target; _ }
      -> (
        match AccessPath.of_expression target with
        | Some { AccessPath.root; path } ->
            (* We need to take some care to ensure we clear existing taint, without adding new
               taint. *)
            let taint = ForwardState.read ~root ~path state.taint in
            if not (ForwardState.Tree.is_bottom taint) then
              { taint = ForwardState.assign ~root ~path ForwardState.Tree.bottom state.taint }
            else
              state
        | _ -> state)
    | Assign { target = { Node.location; value = target_value } as target; value; _ } -> (
        let target_global_model =
          GlobalModel.from_expression
            ~resolution
            ~call_graph:FunctionContext.call_graph_of_define
            ~get_callee_model:FunctionContext.get_callee_model
            ~qualifier:FunctionContext.qualifier
            ~expression:target
            ~interval:FunctionContext.caller_class_interval
        in
        if GlobalModel.is_sanitized target_global_model then
          analyze_expression ~resolution ~state ~is_result_used:false ~expression:value |> snd
        else
          match target_value with
          | Expression.Name (Name.Attribute { base; attribute; _ }) ->
              let attribute_access_callees = get_attribute_access_callees ~location ~attribute in

              let property_call_state =
                match attribute_access_callees with
                | Some { property_targets = _ :: _ as property_targets; _ } ->
                    (* Treat `a.property = x` as `a = a.property(x)` *)
                    let taint, state =
                      apply_callees
                        ~resolution
                        ~is_result_used:true
                        ~is_property:true
                        ~callee:target
                        ~call_location:location
                        ~arguments:[{ Call.Argument.name = None; value }]
                        ~state
                        (CallGraph.CallCallees.create ~call_targets:property_targets ())
                    in
                    store_taint_option (AccessPath.of_expression base) taint state
                | _ -> bottom
              in

              let regular_attribute_state =
                match attribute_access_callees with
                | Some { is_attribute = true; _ }
                | None ->
                    let taint, state =
                      analyze_expression ~resolution ~state ~is_result_used:true ~expression:value
                    in
                    analyze_assignment ~resolution target taint taint state
                | _ -> bottom
              in

              join property_call_state regular_attribute_state
          | _ ->
              let taint, state =
                analyze_expression ~resolution ~state ~is_result_used:true ~expression:value
              in
              analyze_assignment ~resolution target taint taint state)
    | Assert { test; _ } -> analyze_condition ~resolution test state
    | Break
    | Class _
    | Continue ->
        state
    | Define define -> analyze_definition ~define state
    | Delete expressions ->
        let process_expression state expression =
          match AccessPath.of_expression expression with
          | Some { AccessPath.root; path } ->
              { taint = ForwardState.assign ~root ~path ForwardState.Tree.bottom state.taint }
          | _ -> state
        in
        List.fold expressions ~init:state ~f:process_expression
    | Expression expression ->
        let _, state = analyze_expression ~resolution ~state ~is_result_used:false ~expression in
        state
    | For _
    | Global _
    | If _
    | Import _
    | Match _
    | Nonlocal _
    | Pass
    | Raise { expression = None; _ } ->
        state
    | Raise { expression = Some expression; _ } ->
        let _, state = analyze_expression ~resolution ~state ~is_result_used:false ~expression in
        state
    | Return { expression = Some expression; _ } ->
        let taint, state = analyze_expression ~resolution ~state ~is_result_used:true ~expression in
        let location = Location.with_module ~module_reference:FunctionContext.qualifier location in
        check_flow
          ~location
          ~sink_handle:IssueHandle.Sink.Return
          ~source_tree:taint
          ~sink_tree:(return_sink ~resolution ~return_location:location);
        store_taint ~root:AccessPath.Root.LocalResult ~path:[] taint state
    | Return { expression = None; _ }
    | Try _
    | With _
    | While _ ->
        state


  let create ~existing_model parameters =
    (* Use primed sources to populate initial state of parameters *)
    let forward_primed_taint = existing_model.Model.forward.source_taint in
    let resolution =
      TypeCheck.resolution
        global_resolution
        (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
        (module TypeCheck.DummyContext)
    in
    let prime_parameter
        state
        (parameter_root, name, { Node.location; value = { Parameter.value; _ } })
      =
      let prime =
        let location = Location.with_module ~module_reference:FunctionContext.qualifier location in
        ForwardState.read ~root:parameter_root ~path:[] forward_primed_taint
        |> ForwardState.Tree.apply_call
             ~resolution
             ~location
             ~callee:(Some (Interprocedural.Target.create FunctionContext.definition))
             ~arguments:[]
             ~port:parameter_root
             ~is_self_call:false
             ~caller_class_interval:FunctionContext.caller_class_interval
             ~receiver_class_interval:Interprocedural.ClassIntervalSet.top
      in
      let default_value_taint, state =
        match value with
        | None -> ForwardState.Tree.bottom, state
        | Some expression -> analyze_expression ~resolution ~state ~is_result_used:true ~expression
      in
      let root = AccessPath.Root.Variable name in
      let taint =
        ForwardState.assign
          ~root
          ~path:[]
          (ForwardState.Tree.join prime default_value_taint)
          state.taint
      in
      { taint }
    in
    List.fold parameters ~init:bottom ~f:prime_parameter


  let forward ~statement_key state ~statement =
    TaintProfiler.track_statement_analysis
      ~profiler
      ~analysis:TaintProfiler.Forward
      ~statement
      ~f:(fun () ->
        log
          "Forward analysis of statement: `%a`@,With forward state: %a"
          Statement.pp
          statement
          pp
          state;
        let resolution =
          let { Node.value = { Statement.Define.signature = { parent; _ }; _ }; _ } =
            FunctionContext.definition
          in
          TypeCheck.resolution_with_key
            ~global_resolution
            ~local_annotations
            ~parent
            ~statement_key
            (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
            (module TypeCheck.DummyContext)
        in
        analyze_statement ~resolution statement state)


  let backward ~statement_key:_ _ ~statement:_ = failwith "Don't call me"
end

let extract_source_model
    ~define
    ~resolution
    ~taint_configuration:
      {
        TaintConfiguration.Heap.analysis_model_constraints =
          { maximum_model_source_tree_width; maximum_trace_length; _ };
        _;
      }
    ~breadcrumbs_to_attach
    ~via_features_to_attach
    exit_taint
  =
  let { Statement.Define.signature = { return_annotation; name; parameters; _ }; _ } = define in
  let return_type_breadcrumbs =
    return_annotation
    >>| GlobalResolution.parse_annotation resolution
    |> Features.type_breadcrumbs_from_annotation ~resolution
  in

  let simplify tree =
    let tree =
      match maximum_trace_length with
      | Some maximum_trace_length ->
          ForwardState.Tree.prune_maximum_length maximum_trace_length tree
      | _ -> tree
    in
    tree
    |> ForwardState.Tree.shape
         ~mold_with_return_access_paths:false
         ~breadcrumbs:(Features.widen_broadening_set ())
    |> ForwardState.Tree.add_local_breadcrumbs return_type_breadcrumbs
    |> ForwardState.Tree.limit_to
         ~breadcrumbs:(Features.widen_broadening_set ())
         ~width:maximum_model_source_tree_width
  in
  let return_taint =
    let return_variable =
      (* Our handling of property setters is counterintuitive.
       * We treat `a.property = x` as `a = a.property(x)`.
       *
       * This is because the property setter callable can arbitrarily mutate self in
       * its body, meaning that we can't do a naive join of the taint returned by the property
       * setter and the pre-existing taint (as we wouldn't know what taint to delete, etc. Marking
       * self as implicitly returned here allows this handling to work and simulates runtime
       * behavior accurately. *)
      if String.equal (Reference.last name) "__init__" || Statement.Define.is_property_setter define
      then
        match parameters with
        | { Node.value = { Parameter.name = self_parameter; _ }; _ } :: _ ->
            AccessPath.Root.Variable self_parameter
        | [] -> AccessPath.Root.LocalResult
      else
        AccessPath.Root.LocalResult
    in
    ForwardState.read ~root:return_variable ~path:[] exit_taint |> simplify
  in

  ForwardState.assign ~root:AccessPath.Root.LocalResult ~path:[] return_taint ForwardState.bottom
  |> ForwardState.add_local_breadcrumbs breadcrumbs_to_attach
  |> ForwardState.add_via_features via_features_to_attach


let run
    ?(profiler = TaintProfiler.none)
    ~taint_configuration
    ~string_combine_partial_sink_tree
    ~environment
    ~class_interval_graph
    ~global_constants
    ~qualifier
    ~callable
    ~define
    ~cfg
    ~call_graph_of_define
    ~get_callee_model
    ~existing_model
    ()
  =
  let { Node.value = { Statement.Define.signature = { parameters; _ }; _ }; _ } = define in
  let module FunctionContext = struct
    let qualifier = qualifier

    let definition = define

    let debug = Statement.Define.dump define.value

    let profiler = profiler

    let environment = environment

    let taint_configuration = taint_configuration

    let class_interval_graph = class_interval_graph

    let global_constants = global_constants

    let call_graph_of_define = call_graph_of_define

    let get_callee_model = get_callee_model

    let existing_model = existing_model

    let triggered_sinks_to_propagate = Issue.TriggeredSinkLocationMap.create ()

    let caller_class_interval =
      Interprocedural.ClassIntervalSetGraph.SharedMemory.of_definition
        class_interval_graph
        definition


    let string_combine_partial_sink_tree = string_combine_partial_sink_tree
  end
  in
  let module State = State (FunctionContext) in
  let module Fixpoint = Analysis.Fixpoint.Make (State) in
  if FunctionContext.debug || Statement.Define.dump_call_graph define.value then
    Log.dump
      "Call graph of `%a`:@,%a"
      Reference.pp
      (Statement.Define.name define.Node.value)
      CallGraph.DefineCallGraph.pp
      call_graph_of_define;
  State.log "Forward analysis of callable: `%a`" Interprocedural.Target.pp_pretty callable;
  let timer = Timer.start () in
  let initial =
    TaintProfiler.track_duration ~profiler ~name:"Forward analysis - initial state" ~f:(fun () ->
        let normalized_parameters = AccessPath.Root.normalize_parameters parameters in
        State.create ~existing_model normalized_parameters)
  in
  let () = State.log "Processing CFG:@.%a" Cfg.pp cfg in
  let exit_state =
    TaintProfiler.track_duration ~profiler ~name:"Forward analysis - fixpoint" ~f:(fun () ->
        Interprocedural.Metrics.with_alarm
          ~max_time_in_seconds:60
          ~event_name:"forward taint analysis"
          ~callable
          (fun () -> Fixpoint.forward ~cfg ~initial |> Fixpoint.exit)
          ())
  in
  let () =
    match exit_state with
    | Some exit_state -> State.log "Exit state:@,%a" State.pp exit_state
    | None -> State.log "No exit state found"
  in
  let resolution = TypeEnvironment.ReadOnly.global_resolution environment in
  let extract_model { State.taint; _ } =
    let breadcrumbs_to_attach, via_features_to_attach =
      ForwardState.extract_features_to_attach
        ~root:AccessPath.Root.LocalResult
        ~attach_to_kind:Sources.Attach
        existing_model.forward.source_taint
    in
    let source_taint =
      TaintProfiler.track_duration ~profiler ~name:"Forward analysis - extract model" ~f:(fun () ->
          extract_source_model
            ~define:define.value
            ~resolution
            ~taint_configuration:FunctionContext.taint_configuration
            ~breadcrumbs_to_attach
            ~via_features_to_attach
            taint)
    in
    let model = Model.Forward.{ source_taint } in
    let () = State.log "Forward Model:@,%a" Model.Forward.pp model in
    model
  in
  let issues = State.generate_issues () in
  let model = exit_state >>| extract_model |> Option.value ~default:Model.Forward.empty in
  Statistics.performance
    ~randomly_log_every:1000
    ~always_log_time_threshold:1.0 (* Seconds *)
    ~name:"Forward analysis"
    ~section:`Taint
    ~normals:["callable", Interprocedural.Target.show_pretty callable]
    ~timer
    ();
  model, issues, FunctionContext.triggered_sinks_to_propagate
