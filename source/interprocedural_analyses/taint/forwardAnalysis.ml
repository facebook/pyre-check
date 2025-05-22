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
open Ast
open Expression
open Pyre
open Domains
module CallGraph = Interprocedural.CallGraph
module CallResolution = Interprocedural.CallResolution
module AccessPath = Interprocedural.AccessPath
module PyrePysaEnvironment = Analysis.PyrePysaEnvironment
module PyrePysaLogic = Analysis.PyrePysaLogic

module type FUNCTION_CONTEXT = sig
  val qualifier : Reference.t

  val define_name : Reference.t

  val definition : Statement.Define.t Node.t

  val callable : Interprocedural.Target.t

  val debug : bool

  val profiler : TaintProfiler.t

  val pyre_api : PyrePysaEnvironment.ReadOnly.t

  val callables_to_definitions_map : Interprocedural.Target.CallablesSharedMemory.ReadOnly.t

  val taint_configuration : TaintConfiguration.Heap.t

  val string_combine_partial_sink_tree : BackwardState.Tree.t

  val class_interval_graph : Interprocedural.ClassIntervalSetGraph.SharedMemory.t

  val global_constants : Interprocedural.GlobalConstants.SharedMemory.ReadOnly.t

  val call_graph_of_define : CallGraph.DefineCallGraph.t

  val get_callee_model : Interprocedural.Target.t -> Model.t option

  val existing_model : Model.t

  val triggered_sinks_to_propagate : Issue.TriggeredSinkForBackward.t

  val caller_class_interval : Interprocedural.ClassIntervalSet.t

  val type_of_expression_shared_memory : Interprocedural.TypeOfExpressionSharedMemory.t
end

let ( |>> ) (taint, state) f = f taint, state

module State (FunctionContext : FUNCTION_CONTEXT) = struct
  type t = { taint: ForwardState.t }

  let pyre_api = FunctionContext.pyre_api

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
            (Node.create_with_default_location (Expression.Call call)
            |> Ast.Expression.delocalize ~create_origin:(fun ~expression:_ _ -> None))
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


  let self_variable =
    if Interprocedural.Target.is_method FunctionContext.callable then
      let { Node.value = { Statement.Define.signature = { parameters; _ }; _ }; _ } =
        FunctionContext.definition
      in
      match AccessPath.normalize_parameters parameters with
      | { root = AccessPath.Root.PositionalParameter { position = 0; _ }; qualified_name; _ } :: _
        ->
          Some (AccessPath.Root.Variable qualified_name)
      | _ -> None
    else
      None


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
    let check { SinkTreeWithHandle.sink_tree; handle; _ } =
      check_flow ~location ~sink_handle:handle ~source_tree ~sink_tree
    in
    GlobalModel.get_sinks
      ~type_of_expression_shared_memory:FunctionContext.type_of_expression_shared_memory
      ~caller:FunctionContext.callable
      global_model
    |> List.iter ~f:check


  let check_triggered_flows ~triggered_sinks_for_call ~sink_handle ~location ~source_tree ~sink_tree
    =
    Issue.Candidates.check_triggered_flows
      candidates
      ~triggered_sinks_for_call
      ~sink_handle
      ~location
      ~source_tree
      ~sink_tree
      ~taint_configuration:FunctionContext.taint_configuration


  let generate_issues () =
    Issue.Candidates.generate_issues
      candidates
      ~taint_configuration:FunctionContext.taint_configuration
      ~callable:FunctionContext.callable
      ~define:FunctionContext.definition


  (* Store all triggered sinks seen in `triggered_sinks` to propagate them up in the backward
     analysis. *)
  let store_triggered_sinks_to_propagate_for_call ~call_site ~triggered_sinks =
    if not (Issue.TriggeredSinkForCall.is_empty triggered_sinks) then
      Issue.TriggeredSinkForBackward.add
        ~call_site
        ~triggered_sinks_for_call:triggered_sinks
        FunctionContext.triggered_sinks_to_propagate


  module StringFormatCall = struct
    let check_flow_implicit_string_literal_sinks
        ~pyre_in_context
        ~string_literal:{ CallModel.StringFormatCall.value; location }
        ~call_target
        taint
      =
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
                BackwardTaint.singleton CallInfo.declaration sink_kind Frame.initial
                |> BackwardTaint.apply_call
                     ~pyre_in_context
                     ~type_of_expression_shared_memory:
                       FunctionContext.type_of_expression_shared_memory
                     ~caller:FunctionContext.callable
                     ~call_site:(CallSite.create location)
                     ~location
                     ~callee:call_target.CallGraph.CallTarget.target
                     ~arguments:[]
                     ~port:AccessPath.Root.LocalResult
                     ~path:[]
                     ~is_class_method:false
                     ~is_static_method:false
                     ~call_info_intervals:Domains.ClassIntervals.top
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
        ~call_target:{ CallGraph.CallTarget.target; index; _ }
        ~location
        ~string_combine_partial_sink_tree
        source_tree
      =
      let location = Location.with_module ~module_reference:FunctionContext.qualifier location in
      let sink_handle = IssueHandle.Sink.StringFormat { callee = target; index; parameter_index } in
      check_triggered_flows
        ~triggered_sinks_for_call:triggered_sinks
        ~sink_handle
        ~location
        ~source_tree
        ~sink_tree:string_combine_partial_sink_tree
  end

  let store_taint ?(weak = false) ~root ~path taint { taint = state_taint } =
    { taint = ForwardState.assign ~weak ~root ~path taint state_taint }


  let store_taint_option ?(weak = false) access_path taint state =
    match access_path with
    | Some { AccessPath.root; path } -> store_taint ~weak ~root ~path taint state
    | None -> state


  (* A mapping from a taint-in-taint-out kind (`Sinks.LocalReturn` or `Sinks.ParameterUpdate`) to
     sources that must be propagated. *)
  module CallEffects = struct
    type t = (Sinks.t, ForwardState.Tree.t) Map.Poly.t

    let empty = (Map.Poly.empty : t) (* use t to silence a warning. *)

    let update map ~kind ~f = Map.Poly.update map kind ~f

    let add map ~kind ~taint =
      if not (ForwardState.Tree.is_empty taint) then
        update map ~kind ~f:(function
            | None -> taint
            | Some previous -> ForwardState.Tree.join previous taint)
      else
        map


    let get map ~kind = Map.Poly.find map kind |> Option.value ~default:ForwardState.Tree.empty

    let fold map ~init ~f = Map.Poly.fold map ~init ~f:(fun ~key ~data -> f ~kind:key ~taint:data)

    let get_for_self map =
      fold map ~init:ForwardState.Tree.bottom ~f:(fun ~kind ~taint sofar ->
          match kind with
          | Sinks.ParameterUpdate (AccessPath.Root.PositionalParameter { position = 0; _ }) ->
              ForwardState.Tree.join sofar taint
          | _ -> sofar)
  end

  let add_extra_traces_for_tito_transforms
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
    if not (ExtraTraceFirstHop.Set.is_bottom extra_traces) then
      ForwardState.Tree.transform
        ForwardTaint.Self
        Map
        ~f:(ForwardTaint.add_extra_traces ~extra_traces)
        taint
    else
      (* If there are no extra traces, this means we are trying to apply a tito that is a false
         positive. In that case, we should stop propagating the taint.

         This can happen when the tito should be on a specific path, but was collapsed to the root
         due to over-approximations, while the sink was not collapsed, and ended up not being
         propagated. See for instance integration test `transform_tito_with_missing_extra_sink`. *)
      ForwardState.Tree.bottom


  let apply_call_target
      ?(apply_tito = true)
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~is_result_used
      ~implicit_returns_self
      ~triggered_sinks_for_call
      ~call_location
      ~self
      ~callee
      ~implicit_argument_taint
      ~arguments
      ~arguments_taint
      ~state:initial_state
      ({
         CallGraph.CallTarget.target;
         index = _;
         return_type;
         receiver_class;
         is_class_method;
         is_static_method;
         _;
       } as call_target)
    =
    (* Add implicit argument. *)
    let arguments, arguments_taint =
      match implicit_argument_taint with
      | CallModel.ImplicitArgument.Forward.CalleeBase taint ->
          ( { Call.Argument.name = None; value = Option.value_exn self } :: arguments,
            taint :: arguments_taint )
      | CallModel.ImplicitArgument.Forward.Callee taint ->
          { Call.Argument.name = None; value = callee } :: arguments, taint :: arguments_taint
      | CallModel.ImplicitArgument.Forward.None -> arguments, arguments_taint
    in
    let ({ Model.forward; _ } as taint_model) =
      TaintProfiler.track_model_fetch ~profiler ~analysis:Forward ~call_target:target ~f:(fun () ->
          CallModel.at_callsite
            ~pyre_in_context
            ~type_of_expression_shared_memory:FunctionContext.type_of_expression_shared_memory
            ~caller:FunctionContext.callable
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
    let call_info_intervals =
      {
        Domains.ClassIntervals.is_self_call = Ast.Expression.is_self_call ~callee;
        is_cls_call = Ast.Expression.is_cls_call ~callee;
        caller_interval = FunctionContext.caller_class_interval;
        receiver_interval =
          receiver_class
          >>| Interprocedural.ClassIntervalSetGraph.SharedMemory.of_class class_interval_graph
          |> Option.value ~default:Interprocedural.ClassIntervalSet.top;
      }
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
        |> ForwardState.Tree.apply_class_intervals_for_tito
             ~is_class_method
             ~is_static_method
             ~call_info_intervals
             ~tito_intervals:(CallModel.tito_intervals tito_taint)
             ~callee:call_target.CallGraph.CallTarget.target
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
                ForwardState.Tree.transform_non_tito
                  Features.LocalKindSpecificBreadcrumbSet.Self
                  Map
                  ~f:(Features.BreadcrumbSet.add breadcrumb)
                  taint_to_propagate
              in
              add_extra_traces_for_tito_transforms
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
        call_effects
      =
      let tito_tree =
        BackwardState.Tree.fold
          BackwardState.Tree.Path
          tito_tree
          ~init:ForwardState.Tree.empty
          ~f:(convert_tito_path_to_taint ~argument ~argument_taint ~tito_roots ~sink_trees ~kind)
      in
      CallEffects.add call_effects ~kind:(Sinks.discard_transforms kind) ~taint:tito_tree
    in
    let call_site = CallSite.create call_location in
    let analyze_argument_effect
        (call_effects, state)
        ( argument_taint,
          {
            CallModel.ArgumentMatches.argument;
            generation_source_matches;
            sink_matches;
            tito_matches;
            sanitize_matches;
          } )
      =
      let track_apply_call_step step f =
        TaintProfiler.track_apply_call_step
          ~profiler
          ~analysis:Forward
          ~step
          ~call_target:(Some target)
          ~location:call_location
          ~argument:(Some argument)
          ~f
      in
      let sink_trees =
        track_apply_call_step ApplyCallForArgumentSinks (fun () ->
            CallModel.sink_trees_of_argument
              ~pyre_in_context
              ~type_of_expression_shared_memory:FunctionContext.type_of_expression_shared_memory
              ~caller:FunctionContext.callable
              ~transform_non_leaves:(fun _ tree -> tree)
              ~model:taint_model
              ~call_site
              ~location:argument.Node.location
              ~call_target
              ~arguments
              ~sink_matches
              ~is_class_method
              ~is_static_method
              ~call_info_intervals)
      in
      let call_effects =
        let taint_in_taint_out_map =
          track_apply_call_step BuildTaintInTaintOutMapping (fun () ->
              if apply_tito then
                CallModel.taint_in_taint_out_mapping_for_argument
                  ~transform_non_leaves:(fun _ tito -> tito)
                  ~taint_configuration:FunctionContext.taint_configuration
                  ~ignore_local_return:(not is_result_used)
                  ~model:taint_model
                  ~callable:target
                  ~tito_matches
                  ~sanitize_matches
              else
                CallModel.TaintInTaintOutMap.empty)
        in
        track_apply_call_step ApplyTitoForArgument (fun () ->
            CallModel.TaintInTaintOutMap.fold
              ~init:call_effects
              ~f:(convert_tito_tree_to_taint ~argument ~argument_taint ~sink_trees)
              taint_in_taint_out_map)
      in

      (* Add features to arguments. *)
      let state =
        match AccessPath.of_expression ~self_variable argument with
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
        track_apply_call_step CheckIssuesForArgument (fun () ->
            List.iter sink_trees ~f:(fun { SinkTreeWithHandle.sink_tree; handle; _ } ->
                let location =
                  Location.with_module
                    ~module_reference:FunctionContext.qualifier
                    argument.Node.location
                in
                (* Check for issues. *)
                check_flow ~location ~sink_handle:handle ~source_tree:argument_taint ~sink_tree;
                (* Check for issues for combined source rules. *)
                check_triggered_flows
                  ~triggered_sinks_for_call
                  ~sink_handle:handle
                  ~location
                  ~source_tree:argument_taint
                  ~sink_tree;
                ()))
      in

      (* Propagate generations *)
      let call_effects =
        track_apply_call_step ApplyCallForArgumentSources (fun () ->
            let add_generation_effect
                call_effects
                ({ AccessPath.root; _ } as generation_source_match)
              =
              let source_tree =
                CallModel.source_tree_of_argument
                  ~pyre_in_context
                  ~type_of_expression_shared_memory:FunctionContext.type_of_expression_shared_memory
                  ~caller:FunctionContext.callable
                  ~model:taint_model
                  ~call_site
                  ~location:argument.Node.location
                  ~call_target
                  ~arguments
                  ~is_class_method
                  ~is_static_method
                  ~call_info_intervals
                  ~generation_source_match
              in
              CallEffects.add ~kind:(Sinks.ParameterUpdate root) ~taint:source_tree call_effects
            in
            List.fold ~init:call_effects ~f:add_generation_effect generation_source_matches)
      in

      call_effects, state
    in
    let arguments_matches = CallModel.match_actuals_to_formals ~model:taint_model ~arguments in
    let call_effects, state =
      let captures_taint, captured_arguments_matches =
        CallModel.match_captures
          ~model:taint_model
          ~captures_taint:initial_state.taint
          ~location:call_location
      in
      arguments_matches @ captured_arguments_matches
      |> List.zip_exn (arguments_taint @ captures_taint)
      |> List.fold ~f:analyze_argument_effect ~init:(CallEffects.empty, initial_state)
    in

    (* Compute return taint *)
    let result_generation_taint =
      TaintProfiler.track_apply_call_step
        ~profiler
        ~analysis:Forward
        ~step:ApplyCallForReturn
        ~call_target:(Some target)
        ~location:call_location
        ~argument:None
        ~f:(fun () ->
          if is_result_used then
            ForwardState.read ~root:AccessPath.Root.LocalResult ~path:[] forward.generations
            |> ForwardState.Tree.apply_call
                 ~pyre_in_context
                 ~type_of_expression_shared_memory:FunctionContext.type_of_expression_shared_memory
                 ~caller:FunctionContext.callable
                 ~call_site
                 ~location:call_location
                 ~callee:target
                 ~arguments
                 ~port:AccessPath.Root.LocalResult
                 ~is_class_method
                 ~is_static_method
                 ~call_info_intervals
          else
            ForwardState.Tree.empty)
    in
    let result_tito_taint = CallEffects.get call_effects ~kind:Sinks.LocalReturn in
    let result_taint = ForwardState.Tree.join result_generation_taint result_tito_taint in
    let result_taint =
      if implicit_returns_self && is_result_used then
        (* For implicit `__init__` calls, e.g `Foo()`, we should consider the return value as
           tainted. *)
        result_taint
        |> ForwardState.Tree.join (CallEffects.get_for_self call_effects)
        |> ForwardState.Tree.join
             (List.hd arguments_taint |> Option.value ~default:ForwardState.Tree.bottom)
      else
        result_taint
    in
    let result_taint =
      ForwardState.Tree.add_local_breadcrumbs
        (Features.type_breadcrumbs (Option.value_exn return_type))
        result_taint
    in
    let () =
      (* Need to be called after calling `check_triggered_flows` *)
      store_triggered_sinks_to_propagate_for_call
        ~call_site
        ~triggered_sinks:triggered_sinks_for_call
    in

    (* Update state *)
    let apply_call_effects call_effects state =
      (* We also have to consider the cases when the updated parameter has a global model, in which
         case we need to capture the flow. *)
      let apply_effect_for_argument_expression
          ~argument:{ Call.Argument.value = argument; _ }
          ~source_tree
          state
        =
        GlobalModel.from_expression
          ~pyre_in_context
          ~type_of_expression_shared_memory:FunctionContext.type_of_expression_shared_memory
          ~caller:FunctionContext.callable
          ~call_graph:FunctionContext.call_graph_of_define
          ~get_callee_model:FunctionContext.get_callee_model
          ~expression:argument
          ~interval:FunctionContext.caller_class_interval
        |> check_flow_to_global ~location:argument.Node.location ~source_tree;
        let access_path = AccessPath.of_expression ~self_variable argument in
        log
          "Propagating taint to argument `%a`: %a"
          Expression.pp
          argument
          ForwardState.Tree.pp
          source_tree;
        store_taint_option ~weak:true access_path source_tree state
      in
      let apply_effect_for_formal_argument ~source_tree ~formal_argument state =
        AccessPath.match_actuals_to_one_formal arguments formal_argument
        |> List.fold ~init:state ~f:(fun state (actual_argument, _) ->
               apply_effect_for_argument_expression ~argument:actual_argument ~source_tree state)
      in
      let for_each_target ~kind:target ~taint state =
        match target with
        | Sinks.LocalReturn ->
            (* This is regular tito which was computed above *)
            state
        | ParameterUpdate root ->
            (* Side effect on argument *)
            apply_effect_for_formal_argument ~source_tree:taint ~formal_argument:root state
        | _ -> Format.asprintf "unexpected kind for tito: %a" Sinks.pp target |> failwith
      in
      CallEffects.fold call_effects ~f:for_each_target ~init:state
    in
    let apply_captured_variable_side_effects state =
      let propagate_captured_variables state root =
        match root with
        | AccessPath.Root.CapturedVariable { name = variable } ->
            let nonlocal_reference = Reference.delocalize (Reference.create variable) in
            let define_reference =
              PyrePysaLogic.qualified_name_of_define
                ~module_name:FunctionContext.qualifier
                FunctionContext.definition.value
            in
            let is_prefix = Reference.is_prefix ~prefix:define_reference nonlocal_reference in
            (* Treat any function call, even those that wrap a closure write, as a closure write *)
            let state =
              (* TODO(T169657906): Programatically decide between weak and strong storing of
                 taint *)
              store_taint
                ~weak:true
                ~root:(AccessPath.Root.captured_variable_to_variable root)
                ~path:[]
                (ForwardState.read ~root ~path:[] forward.generations)
                state
            in
            (* Propagate captured variable taint up until the function where the nonlocal variable
               is initialized *)
            if not is_prefix then
              store_taint
                ~weak:true
                ~root
                ~path:[]
                (ForwardState.read ~root ~path:[] forward.generations)
                state
            else
              state
        | _ -> state
      in
      List.fold ~init:state ~f:propagate_captured_variables (ForwardState.roots forward.generations)
    in
    let state =
      TaintProfiler.track_apply_call_step
        ~profiler
        ~analysis:Forward
        ~step:ApplyCallEffects
        ~call_target:(Some target)
        ~location:call_location
        ~argument:None
        ~f:(fun () ->
          state |> apply_call_effects call_effects |> apply_captured_variable_side_effects)
    in
    result_taint, state


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
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~is_result_used
      ~triggered_sinks_for_call
      ~call_location
      ~callee
      ~callee_taint
      ~arguments
      ~arguments_taint
      ~origin
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
              ~pyre_in_context
              ~is_result_used:true
              ~implicit_returns_self:false
              ~triggered_sinks_for_call
              ~call_location
              ~self:(Some callee)
              ~implicit_argument_taint:
                (CallModel.ImplicitArgument.Forward.from_call_target
                   ~is_implicit_new:true
                   ~callee_base_taint:callee_taint
                   target)
              ~callee
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
          Expression.Call
            {
              Call.callee;
              arguments;
              origin =
                Some (Origin.create ?base:origin ~location:call_location Origin.ImplicitInitCall);
            }
          |> Node.create ~location:call_location
        in
        List.map init_targets ~f:(fun target ->
            apply_call_target
              ~pyre_in_context
              ~is_result_used
              ~implicit_returns_self:true
              ~triggered_sinks_for_call
              ~call_location
              ~self:(Some call_expression)
              ~callee
              ~implicit_argument_taint:
                (CallModel.ImplicitArgument.Forward.from_call_target
                   ~is_implicit_new:false
                   ~callee_base_taint:(Some new_return_taint)
                   target)
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
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~is_result_used
      ~callee
      ~call_location
      ~arguments
      ~origin
      ~self_taint
      ~callee_taint
      ~arguments_taint
      ~state:initial_state
      {
        CallGraph.CallCallees.call_targets;
        new_targets;
        init_targets;
        decorated_targets = _;
        higher_order_parameters = _;
        unresolved;
        recognized_call = _;
      }
    =
    (* Set of sinks of combined source rules triggered (i.e, a source flowed to
     * a partial sink) for the current call expression. *)
    let triggered_sinks_for_call = Issue.TriggeredSinkForCall.create () in

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
      List.map call_targets ~f:(function call_target ->
          apply_call_target
            ~apply_tito
            ~pyre_in_context
            ~is_result_used
            ~implicit_returns_self:false
            ~triggered_sinks_for_call
            ~call_location
            ~self
            ~callee
            ~implicit_argument_taint:
              (CallModel.ImplicitArgument.Forward.from_call_target
                 ~is_implicit_new:false
                 ~callee_base_taint:self_taint
                 ~callee_taint
                 call_target)
            ~arguments
            ~arguments_taint
            ~state:initial_state
            call_target)
      |> List.fold
           ~init:(ForwardState.Tree.empty, bottom)
           ~f:(fun (taint, state) (new_taint, new_state) ->
             ForwardState.Tree.join taint new_taint, join state new_state)
    in

    (* Apply an obscure call if the call was not fully resolved. *)
    let taint, state =
      if CallGraph.Unresolved.is_unresolved unresolved then
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
              ~pyre_in_context
              ~is_result_used
              ~triggered_sinks_for_call
              ~call_location
              ~callee
              ~callee_taint
              ~arguments
              ~arguments_taint
              ~origin
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
    (* Taint of the entire attribute access expression. *)
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

  let rec analyze_callee
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~is_property_call
      ~state
      ~callee
    =
    match callee.Node.value with
    | Expression.Name (Name.Attribute { base; attribute; origin }) ->
        (* If we are already analyzing a call of a property, then ignore properties
         * to avoid infinite recursion. *)
        let resolve_properties = not is_property_call in
        let { base_taint; attribute_taint; state } =
          analyze_attribute_access
            ~pyre_in_context
            ~state
            ~is_attribute_used:true
            ~location:callee.Node.location
            ~resolve_properties
            ~base
            ~attribute
            ~origin
        in
        { self_taint = Some base_taint; callee_taint = Some attribute_taint; state }
    | _ ->
        let taint, state =
          analyze_expression ~pyre_in_context ~state ~is_result_used:true ~expression:callee
        in
        { self_taint = Some ForwardState.Tree.bottom; callee_taint = Some taint; state }


  (* Lazy version of `analyze_callee` which only analyze what we need for a call site. *)
  and analyze_callee_for_callees
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~is_property_call
      ~state
      ~callee
      callees
    =
    (* Special case: `x.foo()` where foo is a property returning a callable. *)
    let callee_is_property =
      match is_property_call, callee.Node.value with
      | false, Expression.Name (Name.Attribute { attribute; _ }) ->
          get_attribute_access_callees ~location:callee.Node.location ~attribute |> Option.is_some
      | _ -> false
    in
    let taint_on_self_and_callee () =
      (* We need both the taint on self and on the whole callee. *)
      analyze_callee ~pyre_in_context ~is_property_call ~state ~callee
    in
    match callees, callee_is_property with
    | _, true -> taint_on_self_and_callee ()
    | { CallGraph.CallCallees.unresolved; _ }, _ when CallGraph.Unresolved.is_unresolved unresolved
      ->
        taint_on_self_and_callee ()
    | { CallGraph.CallCallees.new_targets = _ :: _; _ }, _
    | { CallGraph.CallCallees.init_targets = _ :: _; _ }, _ ->
        taint_on_self_and_callee ()
    | { CallGraph.CallCallees.call_targets; _ }, _
      when List.exists
             ~f:(fun { CallGraph.CallTarget.implicit_receiver; implicit_dunder_call; _ } ->
               implicit_receiver && implicit_dunder_call)
             call_targets ->
        taint_on_self_and_callee ()
    | { CallGraph.CallCallees.call_targets; _ }, _
      when List.exists
             ~f:(fun { CallGraph.CallTarget.implicit_receiver; _ } -> implicit_receiver)
             call_targets ->
        (* We only need the taint of the receiver. *)
        let taint, state =
          match callee.Node.value with
          | Expression.Name (Name.Attribute { base; _ }) ->
              analyze_expression ~pyre_in_context ~state ~is_result_used:true ~expression:base
          | _ -> ForwardState.Tree.bottom, state
        in
        { self_taint = Some taint; callee_taint = None; state }
    | _ ->
        (* We can ignore the callee entirely. *)
        { self_taint = None; callee_taint = None; state }


  and analyze_arguments ~(pyre_in_context : PyrePysaEnvironment.InContext.t) ~state ~arguments =
    let compute_argument_taint (arguments_taint, state) argument =
      let taint, state =
        analyze_unstarred_expression
          ~pyre_in_context
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
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~is_result_used
      (taint, state)
      entry
    =
    let open Dictionary.Entry in
    match entry with
    | KeyValue { key; value } ->
        let field_name = AccessPath.get_index key in
        let key_taint, state =
          analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:key
          |>> ForwardState.Tree.prepend [AccessPath.dictionary_keys]
        in
        analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:value
        |>> ForwardState.Tree.prepend [field_name]
        |>> ForwardState.Tree.join taint
        |>> ForwardState.Tree.join key_taint
    | Splat s ->
        let new_taint, state =
          analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:s
        in
        ForwardState.Tree.join new_taint taint, state


  and analyze_list_element
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~is_result_used
      position
      (taint, state)
      expression
    =
    let index_name = Abstract.TreeDomain.Label.Index (string_of_int position) in
    analyze_expression ~pyre_in_context ~state ~is_result_used ~expression
    |>> ForwardState.Tree.prepend [index_name]
    |>> ForwardState.Tree.join taint


  and analyze_set_element
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~is_result_used
      (taint, state)
      expression
    =
    let value_taint, state =
      analyze_expression ~pyre_in_context ~state ~is_result_used ~expression
      |>> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]
    in
    ForwardState.Tree.join taint value_taint, state


  and analyze_comprehension_generators
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~state
      generators
    =
    let add_binding
        (state, pyre_in_context)
        ({ Comprehension.Generator.conditions; _ } as generator)
      =
      let ({ Statement.Assign.target; value; _ } as assignment) =
        Statement.Statement.generator_assignment generator
      in
      let assign_value_taint, state =
        match value with
        | Some value ->
            analyze_expression ~pyre_in_context ~state ~is_result_used:true ~expression:value
        | None -> ForwardState.Tree.empty, state
      in
      let state =
        analyze_assignment ~pyre_in_context target assign_value_taint assign_value_taint state
      in
      (* Since generators create variables that Pyre sees as scoped within the generator, handle
         them by adding the generator's bindings to the resolution. *)
      (* Analyzing the conditions might have issues and side effects. *)
      let analyze_condition state condiiton =
        analyze_expression ~pyre_in_context ~state ~is_result_used:false ~expression:condiiton
        |> snd
      in
      let pyre_in_context =
        PyrePysaEnvironment.InContext.resolve_assignment pyre_in_context assignment
      in
      List.fold conditions ~init:state ~f:analyze_condition, pyre_in_context
    in
    List.fold ~f:add_binding generators ~init:(state, pyre_in_context)


  and analyze_comprehension
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~state
      ~is_result_used
      { Comprehension.element; generators; _ }
    =
    let bound_state, pyre_in_context =
      analyze_comprehension_generators ~pyre_in_context ~state generators
    in
    analyze_expression ~pyre_in_context ~state:bound_state ~is_result_used ~expression:element
    |>> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]


  and analyze_dictionary_comprehension
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~state
      ~is_result_used
      { Comprehension.element = Dictionary.Entry.KeyValue.{ key; value }; generators; _ }
    =
    let state, pyre_in_context =
      analyze_comprehension_generators ~pyre_in_context ~state generators
    in
    let value_taint, state =
      analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:value
      |>> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]
    in
    let key_taint, state =
      analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:key
      |>> ForwardState.Tree.prepend [AccessPath.dictionary_keys]
    in
    ForwardState.Tree.join key_taint value_taint, state


  (* Skip through * and **. Used at call sites where * and ** are handled explicitly *)
  and analyze_unstarred_expression
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~is_result_used
      expression
      state
    =
    match expression.Node.value with
    | Starred (Starred.Once expression)
    | Starred (Starred.Twice expression) ->
        analyze_expression ~pyre_in_context ~state ~is_result_used ~expression
    | _ -> analyze_expression ~pyre_in_context ~state ~is_result_used ~expression


  and analyze_arguments_with_higher_order_parameters
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~arguments
      ~origin
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
          analyze_callee ~pyre_in_context ~is_property_call:false ~state ~callee:function_argument
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
          ~f:(fun indices ({ CallGraph.HigherOrderParameter.index; _ }, _) -> Set.add indices index)
          higher_order_parameters
      in
      let non_function_arguments =
        List.filter_mapi
          ~f:(fun index argument ->
            Option.some_if (not (Set.mem function_argument_indices index)) (index, argument))
          arguments
      in
      let compute_argument_taint (arguments_taint, state) (index, argument) =
        let taint, state =
          analyze_unstarred_expression
            ~pyre_in_context
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
        |> ForwardState.Tree.add_local_breadcrumb (Features.higher_order_parameter ())
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
            ~pyre_in_context
            ~is_result_used:true
            ~callee:argument
            ~call_location:argument_location
            ~arguments
            ~origin:
              (Some
                 (Origin.create
                    ?base:origin
                    ~location:argument_location
                    (Origin.PysaHigherOrderParameter index)))
            ~self_taint
            ~callee_taint:(Some callee_taint)
            ~arguments_taint
            ~state
            (CallGraph.CallCallees.create ~call_targets ~unresolved ())
        in
        let taint =
          ForwardState.Tree.add_local_breadcrumb (Features.higher_order_parameter ()) taint
        in
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
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~is_result_used
      ~is_property
      ~callee
      ~call_location
      ~arguments
      ~origin
      ~state
      callees
    =
    let { self_taint; callee_taint; state } =
      analyze_callee_for_callees
        ~pyre_in_context
        ~is_property_call:is_property
        ~state
        ~callee
        callees
    in

    let arguments_taint, state =
      if CallGraph.HigherOrderParameterMap.is_empty callees.higher_order_parameters then
        analyze_arguments ~pyre_in_context ~state ~arguments
      else
        analyze_arguments_with_higher_order_parameters
          ~pyre_in_context
          ~arguments
          ~origin
          ~state
          ~higher_order_parameters:callees.higher_order_parameters
    in

    apply_callees_with_arguments_taint
      ~apply_tito
      ~pyre_in_context
      ~is_result_used
      ~callee
      ~call_location
      ~arguments
      ~origin
      ~self_taint
      ~callee_taint
      ~arguments_taint
      ~state
      callees


  and analyze_getitem_call_target
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~is_result_used
      ~index
      ~index_number
      ~state
      ~location
      ~base
      ~origin
      ~taint_and_state_after_index_access
      call_target
    =
    let analyze_getitem receiver_class =
      let named_tuple_attributes =
        PyrePysaEnvironment.ReadOnly.named_tuple_attributes pyre_api receiver_class
      in
      match named_tuple_attributes, index_number with
      | Some named_tuple_attributes, Some index_number ->
          List.nth named_tuple_attributes index_number
          (* Access an attribute of a named tuple via valid indices *)
          >>| (fun attribute ->
                let { attribute_taint = taint; state; _ } =
                  analyze_attribute_access
                    ~pyre_in_context
                    ~state
                    ~is_attribute_used:is_result_used
                    ~resolve_properties:false
                    ~location
                    ~base
                    ~attribute
                    ~origin
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
    match
      ( Interprocedural.Target.get_regular call_target.CallGraph.CallTarget.target,
        call_target.CallGraph.CallTarget.receiver_class )
    with
    | Interprocedural.Target.Regular.Method { method_name = "__getitem__"; _ }, Some receiver_class
    | Override { method_name = "__getitem__"; _ }, Some receiver_class ->
        (* Potentially access a named tuple *)
        analyze_getitem receiver_class
    | _ ->
        (* Not access a named tuple *)
        Lazy.force taint_and_state_after_index_access


  and analyze_call
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~location
      ~state
      ~is_result_used
      ~callee
      ~arguments
      ~origin
    =
    let callees = get_call_callees ~location ~call:{ Call.callee; arguments; origin } in

    let add_type_breadcrumbs taint =
      let type_breadcrumbs = CallModel.type_breadcrumbs_of_calls callees.call_targets in
      taint |> ForwardState.Tree.add_local_breadcrumbs type_breadcrumbs
    in

    match { Call.callee; arguments; origin } with
    | {
     callee = { Node.value = Name (Name.Attribute { base; attribute = "__getitem__"; _ }); _ };
     arguments =
       [
         {
           Call.Argument.value = { Node.value = argument_expression; _ } as argument_value;
           name = None;
         };
       ];
     origin;
    } ->
        let _, state =
          analyze_expression
            ~pyre_in_context
            ~state
            ~is_result_used:false
            ~expression:argument_value
        in
        let index = AccessPath.get_index argument_value in
        let taint_and_state_after_index_access =
          lazy
            (analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:base
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
                  ~pyre_in_context
                  ~is_result_used
                  ~index
                  ~index_number
                  ~state
                  ~location
                  ~base
                  ~origin
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
     origin = _;
    } ->
        analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:base
        |>> add_type_breadcrumbs
    | {
     callee =
       { Node.value = Name (Name.Attribute { base; attribute = "__iter__"; origin = Some _ }); _ };
     arguments = [];
     origin = _;
    } ->
        let taint, state =
          analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:base
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
     origin;
    } ->
        let is_dict_setitem = CallGraph.CallCallees.is_mapping_method callees in
        let is_sequence_setitem = CallGraph.CallCallees.is_sequence_method callees in
        let use_custom_tito =
          is_dict_setitem
          || is_sequence_setitem
          || not (CallGraph.CallCallees.is_partially_resolved callees)
        in
        let state =
          (* Process the custom model for `__setitem__`. Ignore tito if we assume this is a regular
             dict.__setitem__ call. *)
          apply_callees
            ~apply_tito:(not use_custom_tito)
            ~pyre_in_context
            ~is_result_used:false
            ~is_property:false
            ~callee
            ~call_location:location
            ~arguments
            ~origin
            ~state
            callees
          |> snd
        in
        let state =
          if use_custom_tito then
            (* Use the hardcoded behavior of `__setitem__` for any subtype of dict or list, and for
               unresolved calls. This is incorrect, but can lead to higher SNR, because we assume in
               most cases, we run into an expression whose type is exactly `dict`, rather than a
               (strict) subtype of `dict` that overrides `__setitem__`. *)
            let state =
              let value_taint, state =
                analyze_expression ~pyre_in_context ~state ~is_result_used:true ~expression:value
              in
              analyze_assignment
                ~pyre_in_context
                ~fields:[AccessPath.get_index index]
                base
                value_taint
                value_taint
                state
            in
            let state =
              let key_taint, state =
                analyze_expression ~pyre_in_context ~state ~is_result_used:true ~expression:index
              in
              if not is_sequence_setitem then
                (* Smash the taint of ALL keys into one place, i.e., a special field of the
                   dictionary: `d[**keys] = index`. *)
                analyze_assignment
                  ~pyre_in_context
                  ~fields:[AccessPath.dictionary_keys]
                  ~weak:true
                  base
                  key_taint
                  key_taint
                  state
              else
                state
            in
            state
          else
            state
        in
        (* Since `dict.__setitem__` returns None, we return no taint here. *)
        ForwardState.Tree.empty, state
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
     origin = call_origin;
    } ->
        let taint, state =
          analyze_expression ~pyre_in_context ~state ~is_result_used:true ~expression:assigned_value
        in
        let state =
          analyze_assignment
            ~pyre_in_context
            (Expression.Name
               (Name.Attribute
                  {
                    base = self;
                    attribute;
                    origin =
                      Some (Origin.create ?base:call_origin ~location Origin.SetAttrConstantLiteral);
                  })
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
     origin = call_origin;
    } ->
        let attribute_expression =
          Expression.Name
            (Name.Attribute
               {
                 base;
                 attribute;
                 origin =
                   Some (Origin.create ?base:call_origin ~location Origin.GetAttrConstantLiteral);
               })
          |> Node.create ~location
        in
        let attribute_taint, state =
          analyze_expression
            ~pyre_in_context
            ~state
            ~is_result_used
            ~expression:attribute_expression
        in
        let default_taint, state =
          analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:default
        in
        ForwardState.Tree.join attribute_taint default_taint, state
    (* `zip(a, b, ...)` creates a taint object which, when iterated on, has first index equal to
       a[*]'s taint, second index with b[*]'s taint, etc. *)
    | { callee = { Node.value = Name (Name.Identifier "zip"); _ }; arguments = lists; origin = _ }
      ->
        let add_list_to_taint index (taint, state) { Call.Argument.value; _ } =
          let index_name = Abstract.TreeDomain.Label.Index (string_of_int index) in
          analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:value
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
     origin = _;
    }
      when String.equal "asyncio" (Name.last name) ->
        analyze_expression
          ~pyre_in_context
          ~state
          ~is_result_used
          ~expression:
            {
              Node.location = Node.location callee;
              value =
                Expression.Tuple
                  (List.map arguments ~f:(fun argument -> argument.Call.Argument.value));
            }
    (* dictionary .keys(), .values() and .items() functions are special, as they require handling of
       dictionary_keys taint. *)
    | {
     callee = { Node.value = Name (Name.Attribute { base; attribute = "values"; _ }); _ };
     arguments = [];
     origin = _;
    }
      when CallGraph.CallCallees.is_mapping_method callees ->
        analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:base
        |>> ForwardState.Tree.read [Abstract.TreeDomain.Label.AnyIndex]
        |>> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]
    | {
     callee = { Node.value = Name (Name.Attribute { base; attribute = "keys"; _ }); _ };
     arguments = [];
     origin = _;
    }
      when CallGraph.CallCallees.is_mapping_method callees ->
        analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:base
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
       [{ Call.Argument.value = { Node.value = Expression.Dictionary entries; _ }; name = None }];
     origin = _;
    }
      when CallGraph.CallCallees.is_mapping_method callees
           && Option.is_some (Dictionary.string_literal_keys entries) ->
        let entries = Option.value_exn (Dictionary.string_literal_keys entries) in
        let taint =
          ForwardState.read ~root:(AccessPath.Root.Variable identifier) ~path:[] state.taint
        in
        let override_taint_from_update (taint, state) (key, value) =
          let value_taint, state =
            analyze_expression ~pyre_in_context ~state ~is_result_used:true ~expression:value
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
          ~pyre_in_context
          ~type_of_expression_shared_memory:FunctionContext.type_of_expression_shared_memory
          ~caller:FunctionContext.callable
          ~call_graph:FunctionContext.call_graph_of_define
          ~get_callee_model:FunctionContext.get_callee_model
          ~expression:base
          ~interval:FunctionContext.caller_class_interval
        |> check_flow_to_global ~location:base.Node.location ~source_tree:taint;
        let state = store_taint ~root:(AccessPath.Root.Variable identifier) ~path:[] taint state in
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
     origin = _;
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
     origin = _;
    }
      when CallGraph.CallCallees.is_mapping_method callees ->
        let taint, state =
          analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:base
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
    | {
     callee = { Node.value = Name (Name.Attribute { base; attribute = "get"; _ }); _ };
     arguments =
       {
         Call.Argument.value =
           {
             Node.value = Expression.Constant (Constant.String { StringLiteral.value = index; _ });
             _;
           };
         name = None;
       }
       :: (([] | [_]) as optional_arguments);
     origin = _;
    }
      when CallGraph.CallCallees.is_mapping_method callees ->
        let index = Abstract.TreeDomain.Label.Index index in
        let taint, state =
          analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:base
          |>> ForwardState.Tree.read [index]
          |>> ForwardState.Tree.add_local_first_index index
          |>> ForwardState.Tree.transform Features.TitoPositionSet.Element Add ~f:base.Node.location
        in
        let taint, state =
          match optional_arguments with
          | [{ Call.Argument.value = default_expression; _ }] ->
              let default_taint, state =
                analyze_expression
                  ~pyre_in_context
                  ~state
                  ~is_result_used
                  ~expression:default_expression
                |>> ForwardState.Tree.transform
                      Features.TitoPositionSet.Element
                      Add
                      ~f:default_expression.Node.location
              in
              ForwardState.Tree.join taint default_taint, state
          | [] -> taint, state
          | _ -> failwith "unreachable"
        in
        let taint = add_type_breadcrumbs taint in
        taint, state
    (* `locals()` is a dictionary from all local names -> values. *)
    | { callee = { Node.value = Name (Name.Identifier "locals"); _ }; arguments = []; origin = _ }
      ->
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
                    origin = _;
                  });
            _;
          };
        arguments;
        origin = _;
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
        origin = _;
      } ->
        let nested_expressions = List.map ~f:(fun call_argument -> call_argument.value) arguments in
        let call_target =
          CallModel.StringFormatCall.CallTarget.create
            ~call_targets:callees.call_targets
            ~default_target:(CallModel.StringFormatCall.CallTarget.from_function_name function_name)
        in
        analyze_joined_string
          ~pyre_in_context
          ~state
          ~breadcrumbs:(Features.BreadcrumbSet.singleton (Features.format_string ()))
          {
            CallModel.StringFormatCall.nested_expressions;
            string_literal = { value; location = value_location };
            call_target;
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
     origin = _;
    } ->
        let call_target =
          CallModel.StringFormatCall.CallTarget.create
            ~call_targets:callees.call_targets
            ~default_target:
              (CallGraph.CallTarget.create Interprocedural.Target.ArtificialTargets.str_add)
        in
        analyze_joined_string
          ~pyre_in_context
          ~state
          ~breadcrumbs:(Features.BreadcrumbSet.singleton (Features.string_concat_left_hand_side ()))
          {
            CallModel.StringFormatCall.nested_expressions = [expression];
            string_literal = { value; location = value_location };
            call_target;
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
                 origin = _;
               });
         _;
       };
     arguments = [{ Call.Argument.value = expression; name = None }];
     origin = _;
    } ->
        let call_target =
          CallModel.StringFormatCall.CallTarget.create
            ~call_targets:callees.call_targets
            ~default_target:
              (CallGraph.CallTarget.create Interprocedural.Target.ArtificialTargets.str_add)
        in
        analyze_joined_string
          ~pyre_in_context
          ~state
          ~breadcrumbs:
            (Features.BreadcrumbSet.singleton (Features.string_concat_right_hand_side ()))
          {
            CallModel.StringFormatCall.nested_expressions = [expression];
            string_literal = { value; location = value_location };
            call_target;
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
     origin = _;
    }
      when CallGraph.CallCallees.is_string_method callees ->
        let breadcrumbs =
          match function_name with
          | "__mod__"
          | "format" ->
              Features.BreadcrumbSet.singleton (Features.format_string ())
          | _ -> Features.BreadcrumbSet.empty
        in
        let call_target =
          CallModel.StringFormatCall.CallTarget.create
            ~call_targets:callees.call_targets
            ~default_target:(CallModel.StringFormatCall.CallTarget.from_function_name function_name)
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
          ~pyre_in_context
          ~state
          ~breadcrumbs
          {
            CallModel.StringFormatCall.nested_expressions;
            string_literal = { CallModel.StringFormatCall.value = string_literal; location };
            call_target;
            location;
          }
    | { Call.callee = { Node.value = Name (Name.Identifier "super"); _ }; arguments; origin = _ }
      -> (
        match arguments with
        | [_; Call.Argument.{ value = object_; _ }] ->
            analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:object_
        | _ -> (
            (* Use implicit self *)
            match self_variable with
            | Some root -> ForwardState.read ~root ~path:[] state.taint, state
            | None -> ForwardState.Tree.empty, state))
    | {
     callee = { Node.value = Expression.Name (Name.Identifier "reveal_taint"); _ };
     arguments = [{ Call.Argument.value = expression; _ }];
     origin = _;
    } ->
        let taint, _ =
          analyze_expression ~pyre_in_context ~state ~is_result_used:true ~expression
        in
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
     origin = _;
    } ->
        let location =
          Node.location callee |> Location.with_module ~module_reference:FunctionContext.qualifier
        in
        Log.dump
          "%a: Revealed type for %s: %s"
          Location.WithModule.pp
          location
          (Transform.sanitize_expression expression |> Expression.show)
          (CallResolution.resolve_ignoring_untracked ~pyre_in_context expression |> Type.show);
        ForwardState.Tree.bottom, state
    | _ ->
        apply_callees
          ~pyre_in_context
          ~is_result_used
          ~is_property:false
          ~call_location:location
          ~callee
          ~arguments
          ~origin
          ~state
          callees


  and analyze_attribute_access
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~state
      ~is_attribute_used
      ~location
      ~resolve_properties
      ~base
      ~attribute
      ~origin
    =
    let expression =
      Expression.Name (Name.Attribute { base; attribute; origin }) |> Node.create ~location
    in
    let base_taint, state =
      analyze_expression ~pyre_in_context ~state ~is_result_used:true ~expression:base
    in
    let attribute_access_callees =
      if resolve_properties then get_attribute_access_callees ~location ~attribute else None
    in

    let property_call_result =
      match attribute_access_callees with
      | Some { property_targets = _ :: _ as property_targets; _ } ->
          let taint, state =
            apply_callees_with_arguments_taint
              ~pyre_in_context
              ~is_result_used:is_attribute_used
              ~callee:expression
              ~call_location:location
              ~arguments:[]
              ~origin
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
              ~pyre_in_context
              ~type_of_expression_shared_memory:FunctionContext.type_of_expression_shared_memory
              ~caller:FunctionContext.callable
              ~call_graph:FunctionContext.call_graph_of_define
              ~get_callee_model:FunctionContext.get_callee_model
              ~expression
              ~interval:FunctionContext.caller_class_interval
          in
          let attribute_taint =
            GlobalModel.get_source
              ~type_of_expression_shared_memory:FunctionContext.type_of_expression_shared_memory
              ~caller:FunctionContext.callable
              global_model
          in
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


  (* If there exists a triggered flow, `call_target` is the responsible callsite. We assume there is
     a call to `call_target` whose arguments are `string_literal` followed by
     `nested_expressions`. *)
  and analyze_joined_string
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~state
      ~breadcrumbs
      {
        CallModel.StringFormatCall.nested_expressions;
        string_literal = { location = string_literal_location; _ } as string_literal;
        call_target;
        location = call_location;
      }
    =
    let call_site = CallSite.create call_location in
    (* This is the backward model of the callee -- each actual argument has this taint tree. *)
    let string_combine_partial_sink_tree =
      CallModel.StringFormatCall.apply_call
        ~callee:call_target.CallGraph.CallTarget.target
        ~pyre_in_context
        ~type_of_expression_shared_memory:FunctionContext.type_of_expression_shared_memory
        ~caller:FunctionContext.callable
        ~call_site
        ~location:call_location
        FunctionContext.string_combine_partial_sink_tree
    in
    let string_literal_taint =
      CallModel.StringFormatCall.implicit_string_literal_sources
        ~pyre_in_context
        ~type_of_expression_shared_memory:FunctionContext.type_of_expression_shared_memory
        ~caller:FunctionContext.callable
        ~implicit_sources:FunctionContext.taint_configuration.implicit_sources
        string_literal
    in
    let triggered_sinks = Issue.TriggeredSinkForCall.create () in
    StringFormatCall.check_triggered_flows
      ~triggered_sinks
      ~parameter_index:0
      ~call_target
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
      let stringify_origin =
        Some
          (Origin.create
             ?base:(Ast.Expression.origin base)
             ~location:call_location
             Origin.FormatStringImplicitStr)
      in
      let callee =
        let callee_from_method_name method_name =
          {
            Node.value =
              Expression.Name
                (Name.Attribute { base; attribute = method_name; origin = stringify_origin });
            location = call_location;
          }
        in
        match Interprocedural.Target.get_regular call_target.target with
        | Interprocedural.Target.Regular.Method { method_name; _ } ->
            callee_from_method_name method_name
        | Override { method_name; _ } -> callee_from_method_name method_name
        | Function { name; _ } ->
            { Node.value = Name (Name.Identifier name); location = call_location }
        | Object _ -> failwith "callees should be either methods or functions"
      in
      let new_taint, new_state =
        apply_callees_with_arguments_taint
          ~pyre_in_context
          ~is_result_used:true
          ~call_location
          ~arguments:[]
          ~origin:stringify_origin
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
        analyze_expression ~pyre_in_context ~state ~is_result_used:true ~expression
        |>> ForwardState.Tree.transform Features.TitoPositionSet.Element Add ~f:expression_location
        |>> ForwardState.Tree.add_local_breadcrumb (Features.tito ())
      in
      let expression_taint, state =
        match
          CallModel.StringFormatCall.get_string_format_callees
            ~call_graph_of_define:FunctionContext.call_graph_of_define
            ~location:expression_location
        with
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
        ~call_target
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
    store_triggered_sinks_to_propagate_for_call ~call_site ~triggered_sinks;
    (* Compute flows to literal string sinks if applicable. *)
    StringFormatCall.check_flow_implicit_string_literal_sinks
      ~pyre_in_context
      ~string_literal
      ~call_target
      taint;
    ForwardState.Tree.create_leaf taint, state


  and analyze_expression
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~state
      ~is_result_used
      ~expression:({ Node.value; location } as expression)
    =
    let analyze_expression_inner () =
      let value =
        CallGraph.redirect_expressions
          ~pyre_in_context
          ~callables_to_definitions_map:FunctionContext.callables_to_definitions_map
          ~location
          value
      in
      match value with
      | Await { Await.operand; origin = _ } ->
          analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:operand
      | BooleanOperator { left; operator = _; right; origin = _ } ->
          let left_taint, state =
            analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:left
          in
          let right_taint, state =
            analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:right
          in
          ForwardState.Tree.join left_taint right_taint, state
      | ComparisonOperator { left; operator = _; right; origin = _ } ->
          let left_taint, state =
            analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:left
          in
          let right_taint, state =
            analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:right
          in
          let taint =
            ForwardState.Tree.join left_taint right_taint
            |> ForwardState.Tree.add_local_breadcrumbs (Features.type_bool_scalar_set ())
          in
          taint, state
      | Call { callee; arguments; origin } ->
          analyze_call ~pyre_in_context ~location ~state ~is_result_used ~callee ~arguments ~origin
      | Constant (Constant.String { StringLiteral.value; _ }) ->
          let call_target =
            CallGraph.CallTarget.create Interprocedural.Target.ArtificialTargets.str_literal
          in
          analyze_joined_string
            ~pyre_in_context
            ~state
            ~breadcrumbs:Features.BreadcrumbSet.empty
            {
              CallModel.StringFormatCall.nested_expressions = [];
              string_literal = { value; location };
              call_target;
              location;
            }
      | Constant _ -> ForwardState.Tree.empty, state
      | Dictionary entries ->
          List.fold
            entries
            ~f:(analyze_dictionary_entry ~pyre_in_context ~is_result_used)
            ~init:(ForwardState.Tree.empty, state)
      | DictionaryComprehension comprehension ->
          analyze_dictionary_comprehension ~pyre_in_context ~state ~is_result_used comprehension
      | Generator comprehension ->
          analyze_comprehension ~pyre_in_context ~state ~is_result_used comprehension
      | Lambda { parameters = _; body } ->
          (* Ignore parameter bindings and pretend body is inlined *)
          analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:body
      | List list ->
          List.foldi
            list
            ~f:(analyze_list_element ~pyre_in_context ~is_result_used)
            ~init:(ForwardState.Tree.empty, state)
      | ListComprehension comprehension ->
          analyze_comprehension ~pyre_in_context ~state ~is_result_used comprehension
      | Name (Name.Identifier identifier) ->
          let global_model_taint =
            GlobalModel.from_expression
              ~pyre_in_context
              ~type_of_expression_shared_memory:FunctionContext.type_of_expression_shared_memory
              ~caller:FunctionContext.callable
              ~call_graph:FunctionContext.call_graph_of_define
              ~get_callee_model:FunctionContext.get_callee_model
              ~expression:{ Node.value; location }
              ~interval:FunctionContext.caller_class_interval
            |> GlobalModel.get_source
                 ~type_of_expression_shared_memory:FunctionContext.type_of_expression_shared_memory
                 ~caller:FunctionContext.callable
          in

          let global_taint, state =
            let as_reference = identifier |> Reference.create |> Reference.delocalize in
            let global_string =
              Interprocedural.GlobalConstants.SharedMemory.ReadOnly.get
                FunctionContext.global_constants
                ~cache:true
                as_reference
            in
            (* Reanalyze expression with global identifier replaced by assigned string *)
            match global_string with
            | Some global_string ->
                let call_target =
                  CallGraph.CallTarget.create Interprocedural.Target.ArtificialTargets.str_literal
                in
                analyze_joined_string
                  ~pyre_in_context
                  ~state
                  ~breadcrumbs:Features.BreadcrumbSet.empty
                  {
                    CallModel.StringFormatCall.nested_expressions = [];
                    string_literal = { value = global_string.value; location };
                    call_target;
                    location;
                  }
            | None -> ForwardState.Tree.empty, state
          in

          let local_taint =
            let root = AccessPath.Root.Variable identifier in
            ForwardState.read ~root ~path:[] state.taint
            |> ForwardState.Tree.add_local_type_breadcrumbs
                 ~pyre_in_context
                 ~type_of_expression_shared_memory:FunctionContext.type_of_expression_shared_memory
                 ~callable:FunctionContext.callable
                 ~expression:{ Node.value; location }
          in
          ( local_taint
            |> ForwardState.Tree.join global_taint
            |> ForwardState.Tree.join global_model_taint,
            state )
      (* __dict__ reveals an object's underlying data structure, so we should analyze the base under
         the existing taint instead of adding the index to the taint. *)
      | Name (Name.Attribute { base; attribute = "__dict__"; _ }) ->
          analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:base
      | Name (Name.Attribute { base; attribute; origin }) ->
          let { attribute_taint; state; _ } =
            analyze_attribute_access
              ~pyre_in_context
              ~state
              ~is_attribute_used:is_result_used
              ~resolve_properties:true
              ~location
              ~base
              ~attribute
              ~origin
          in
          attribute_taint, state
      | Set set ->
          List.fold
            ~f:(analyze_set_element ~pyre_in_context ~is_result_used)
            set
            ~init:(ForwardState.Tree.empty, state)
      | SetComprehension comprehension ->
          analyze_comprehension ~pyre_in_context ~state ~is_result_used comprehension
      | Starred (Starred.Once expression)
      | Starred (Starred.Twice expression) ->
          analyze_expression ~pyre_in_context ~state ~is_result_used ~expression
          |>> ForwardState.Tree.read [Abstract.TreeDomain.Label.AnyIndex]
      | Slice _ ->
          failwith "Slice nodes should always be rewritten by `CallGraph.redirect_expressions`"
      | Subscript _ ->
          failwith "Subscripts nodes should always be rewritten by `CallGraph.redirect_expressions`"
      | BinaryOperator _ ->
          failwith
            "BinaryOperator nodes should always be rewritten by `CallGraph.redirect_expressions`"
      | FormatString substrings ->
          let substrings =
            List.concat_map substrings ~f:(function
                | Substring.Format { value; format_spec = None } -> [value]
                | Substring.Format { value; format_spec = Some format_spec } -> [value; format_spec]
                | Substring.Literal { Node.value; location } ->
                    [
                      Expression.Constant (Constant.String { StringLiteral.value; kind = String })
                      |> Node.create ~location;
                    ])
          in
          let string_literal, nested_expressions =
            CallModel.arguments_for_string_format substrings
          in
          let call_target =
            CallModel.StringFormatCall.CallTarget.from_format_string
              ~call_graph_of_define:FunctionContext.call_graph_of_define
              ~location
          in
          analyze_joined_string
            ~pyre_in_context
            ~state
            ~breadcrumbs:(Features.BreadcrumbSet.singleton (Features.format_string ()))
            {
              CallModel.StringFormatCall.nested_expressions;
              string_literal = { value = string_literal; location };
              call_target;
              location;
            }
      | Ternary { target; test; alternative } ->
          let state = analyze_condition ~pyre_in_context test state in
          let taint_then, state_then =
            analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:target
          in
          let taint_else, state_else =
            analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:alternative
          in
          ForwardState.Tree.join taint_then taint_else, join state_then state_else
      | Tuple expressions ->
          List.foldi
            ~f:(analyze_list_element ~pyre_in_context ~is_result_used)
            expressions
            ~init:(ForwardState.Tree.empty, state)
      | UnaryOperator { operator = _; operand; origin = _ } ->
          analyze_expression ~pyre_in_context ~state ~is_result_used ~expression:operand
      | WalrusOperator { target; value; origin = _ } ->
          let value_taint, state =
            analyze_expression ~pyre_in_context ~state ~is_result_used:true ~expression:value
          in
          let state = analyze_assignment ~pyre_in_context target value_taint value_taint state in
          value_taint, state
      | Yield None -> ForwardState.Tree.empty, state
      | Yield (Some expression)
      | YieldFrom expression ->
          let taint, state =
            analyze_expression ~pyre_in_context ~state ~is_result_used:true ~expression
          in
          taint, store_taint ~root:AccessPath.Root.LocalResult ~path:[] taint state
    in
    let taint, state =
      TaintProfiler.track_expression_analysis
        ~profiler
        ~analysis:Forward
        ~expression
        ~f:analyze_expression_inner
    in
    log
      "Forward taint of expression `%a`: %a"
      Expression.pp_expression
      value
      ForwardState.Tree.pp
      taint;
    taint, state


  and analyze_assignment
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ?(weak = false)
      ?(fields = [])
      ({ Node.location; value } as target)
      taint
      surrounding_taint
      state
    =
    let taint =
      ForwardState.Tree.add_local_type_breadcrumbs
        ~pyre_in_context
        ~type_of_expression_shared_memory:FunctionContext.type_of_expression_shared_memory
        ~callable:FunctionContext.callable
        ~expression:target
        taint
    in
    match value with
    | Starred (Once target | Twice target) ->
        (* This is approximate. Unless we can get the tuple type on the right to tell how many total
           elements there will be, we just pick up the entire collection. *)
        analyze_assignment ~pyre_in_context ~weak target surrounding_taint surrounding_taint state
    | List targets
    | Tuple targets ->
        let analyze_target_element i state target =
          let index = Abstract.TreeDomain.Label.Index (string_of_int i) in
          let indexed_taint = ForwardState.Tree.read [index] taint in
          analyze_assignment ~pyre_in_context ~weak target indexed_taint taint state
        in
        List.foldi targets ~f:analyze_target_element ~init:state
    (* Assignments of the form a[1][2] = 3 translate to a.__getitem__(1).__setitem__(2, 3).*)
    | Call
        {
          callee = { Node.value = Name (Name.Attribute { base; attribute = "__getitem__"; _ }); _ };
          arguments = [{ Call.Argument.value = argument_value; _ }];
          origin = _;
        } ->
        let index = AccessPath.get_index argument_value in
        analyze_assignment
          ~pyre_in_context
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
          ~pyre_in_context
          ~type_of_expression_shared_memory:FunctionContext.type_of_expression_shared_memory
          ~caller:FunctionContext.callable
          ~call_graph:FunctionContext.call_graph_of_define
          ~get_callee_model:FunctionContext.get_callee_model
          ~expression:target
          ~interval:FunctionContext.caller_class_interval
        |> check_flow_to_global ~location ~source_tree;
        (* Check flows to nonlocals. *)
        let state =
          let nonlocal_target_identifier =
            match Node.value target with
            | Expression.Name (Name.Identifier identifier) ->
                FunctionContext.call_graph_of_define
                |> CallGraph.DefineCallGraph.resolve_identifier
                     ~location:(Node.location target)
                     ~identifier
                >>| (fun { nonlocal_targets; _ } -> nonlocal_targets)
                >>| Fn.non List.is_empty
                >>| (fun has_nonlocal_targets -> Option.some_if has_nonlocal_targets identifier)
                |> Option.value ~default:None
            (* TODO(T168869049): Handle class attribute writes *)
            | _ -> None
          in

          match nonlocal_target_identifier with
          | None -> state
          | Some identifier ->
              (* Propagate taint for nonlocal assignment. *)
              store_taint
                ~weak
                ~root:(AccessPath.Root.CapturedVariable { name = identifier })
                ~path:fields
                (ForwardState.Tree.add_local_breadcrumb (Features.captured_variable ()) taint)
                state
        in
        (* Propagate taint for assignment. *)
        let access_path =
          AccessPath.of_expression ~self_variable target >>| AccessPath.extend ~path:fields
        in
        store_taint_option ~weak access_path taint state


  and analyze_condition ~(pyre_in_context : PyrePysaEnvironment.InContext.t) expression state =
    let { Node.location; _ } = expression in
    let call_site = CallSite.create location in
    let location = Location.with_module ~module_reference:FunctionContext.qualifier location in
    let taint, state =
      analyze_expression ~pyre_in_context ~state ~is_result_used:true ~expression
    in
    (* There maybe configured sinks for conditionals, so test them here. *)
    let () =
      FunctionContext.taint_configuration.implicit_sinks.conditional_test
      |> List.iter ~f:(fun sink_kind ->
             let sink_tree =
               BackwardTaint.singleton CallInfo.declaration sink_kind Frame.initial
               |> BackwardTaint.apply_call
                    ~pyre_in_context
                    ~type_of_expression_shared_memory:
                      FunctionContext.type_of_expression_shared_memory
                    ~caller:FunctionContext.callable
                    ~call_site
                    ~location:(Location.strip_module location)
                    ~callee:Interprocedural.Target.ArtificialTargets.condition
                    ~arguments:[]
                    ~port:
                      (AccessPath.Root.PositionalParameter
                         { position = 0; name = "condition"; positional_only = false })
                    ~path:[]
                    ~is_class_method:false
                    ~is_static_method:false
                    ~call_info_intervals:Domains.ClassIntervals.top
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

  let analyze_statement ~pyre_in_context ({ Node.location; _ } as statement) state =
    let statement = CallGraph.redirect_assignments statement in
    match Node.value statement with
    | Statement.Statement.Assign
        { value = Some { Node.value = Expression.Constant Constant.Ellipsis; _ }; _ } ->
        state
    | Assign { value = None; _ } -> state
    | Assign
        { value = Some { Node.value = Expression.Constant Constant.NoneLiteral; _ }; target; _ }
      -> (
        match AccessPath.of_expression ~self_variable target with
        | Some { AccessPath.root; path } ->
            (* We need to take some care to ensure we clear existing taint, without adding new
               taint. *)
            let taint = ForwardState.read ~root ~path state.taint in
            if not (ForwardState.Tree.is_bottom taint) then
              { taint = ForwardState.assign ~root ~path ForwardState.Tree.bottom state.taint }
            else
              state
        | _ -> state)
    | Assign { target = { Node.location; value = target_value } as target; value = Some value; _ }
      -> (
        let target_global_model =
          GlobalModel.from_expression
            ~pyre_in_context
            ~type_of_expression_shared_memory:FunctionContext.type_of_expression_shared_memory
            ~caller:FunctionContext.callable
            ~call_graph:FunctionContext.call_graph_of_define
            ~get_callee_model:FunctionContext.get_callee_model
            ~expression:target
            ~interval:FunctionContext.caller_class_interval
        in
        if GlobalModel.is_sanitized target_global_model then
          analyze_expression ~pyre_in_context ~state ~is_result_used:false ~expression:value |> snd
        else
          match target_value with
          | Expression.Name (Name.Attribute { attribute; _ }) ->
              let attribute_access_callees = get_attribute_access_callees ~location ~attribute in

              let property_call_state =
                match attribute_access_callees with
                | Some { property_targets = _ :: _ as property_targets; _ } ->
                    (* a.property = x *)
                    let _, state =
                      apply_callees
                        ~pyre_in_context
                        ~is_result_used:false
                        ~is_property:true
                        ~callee:target
                        ~call_location:location
                        ~arguments:[{ Call.Argument.name = None; value }]
                        ~origin:None
                        ~state
                        (CallGraph.CallCallees.create ~call_targets:property_targets ())
                    in
                    state
                | _ -> bottom
              in

              let regular_attribute_state =
                match attribute_access_callees with
                | Some { is_attribute = true; _ }
                | None ->
                    let taint, state =
                      analyze_expression
                        ~pyre_in_context
                        ~state
                        ~is_result_used:true
                        ~expression:value
                    in
                    analyze_assignment ~pyre_in_context target taint taint state
                | _ -> bottom
              in

              join property_call_state regular_attribute_state
          | _ ->
              let taint, state =
                analyze_expression ~pyre_in_context ~state ~is_result_used:true ~expression:value
              in
              analyze_assignment ~pyre_in_context target taint taint state)
    | Assert { test; _ } -> analyze_condition ~pyre_in_context test state
    | Define define -> analyze_definition ~define state
    | Delete expressions ->
        let process_expression state expression =
          match AccessPath.of_expression ~self_variable expression with
          | Some { AccessPath.root; path } ->
              { taint = ForwardState.assign ~root ~path ForwardState.Tree.bottom state.taint }
          | _ -> state
        in
        List.fold expressions ~init:state ~f:process_expression
    | Expression expression ->
        let _, state =
          analyze_expression ~pyre_in_context ~state ~is_result_used:false ~expression
        in
        state
    | Raise { expression = None; _ } -> state
    | Raise { expression = Some expression; _ } ->
        let _, state =
          analyze_expression ~pyre_in_context ~state ~is_result_used:false ~expression
        in
        state
    | Return { expression = Some expression; _ } ->
        let taint, state =
          analyze_expression ~pyre_in_context ~state ~is_result_used:true ~expression
        in
        check_flow
          ~location:(Location.with_module ~module_reference:FunctionContext.qualifier location)
          ~sink_handle:IssueHandle.Sink.Return
          ~source_tree:taint
          ~sink_tree:
            (CallModel.return_sink
               ~pyre_in_context
               ~type_of_expression_shared_memory:FunctionContext.type_of_expression_shared_memory
               ~caller:FunctionContext.callable
               ~location
               ~callee:FunctionContext.callable
               ~sink_model:FunctionContext.existing_model.Model.backward.sink_taint);
        store_taint ~root:AccessPath.Root.LocalResult ~path:[] taint state
    | Return { expression = None; _ } -> state
    | Break
    | Class _
    | Continue
    | Global _
    | Import _
    | Nonlocal _
    | Pass ->
        state
    | TypeAlias _ (* TODO(T196994965): handle Type Alias *) -> state
    | Try _ ->
        (* Try statements are lowered down in `Cfg.create`, but they are preserved in the final Cfg.
           They should be ignored. *)
        state
    | For _
    | If _
    | Match _
    | With _
    | While _ ->
        failwith "For/If/Match/With/While nodes should always be rewritten by `Cfg.create`"
    | AugmentedAssign _ ->
        failwith
          "AugmentedAssign nodes should always be rewritten by `CallGraph.redirect_assignments`"


  let create ~existing_model parameters define_location =
    (* Use primed sources to populate initial state of parameters *)
    let parameter_sources = existing_model.Model.parameter_sources.parameter_sources in
    let pyre_in_context = PyrePysaEnvironment.InContext.create_at_global_scope pyre_api in
    let apply_call ~location ~root =
      ForwardState.read ~root ~path:[] parameter_sources
      |> ForwardState.Tree.apply_call
           ~pyre_in_context
           ~type_of_expression_shared_memory:FunctionContext.type_of_expression_shared_memory
           ~caller:FunctionContext.callable
           ~call_site:(CallSite.create define_location)
           ~location
           ~callee:FunctionContext.callable
             (* Provide leaf callable names when sources originate from parameters. *)
           ~arguments:[]
           ~port:root
           ~is_class_method:false
           ~is_static_method:false
           ~call_info_intervals:
             {
               Domains.ClassIntervals.top with
               caller_interval = FunctionContext.caller_class_interval;
             }
    in
    let prime_parameter
        state
        {
          AccessPath.NormalizedParameter.root = parameter_root;
          qualified_name;
          original = { Node.location; value = { Parameter.value; _ } };
        }
      =
      let prime = apply_call ~location ~root:parameter_root in
      let default_value_taint, state =
        match value with
        | None -> ForwardState.Tree.bottom, state
        | Some expression ->
            analyze_expression ~pyre_in_context ~state ~is_result_used:true ~expression
      in
      store_taint
        ~root:(AccessPath.Root.Variable qualified_name)
        ~path:[]
        (ForwardState.Tree.join prime default_value_taint)
        state
    in
    let add_captured_variables_paramater_sources state =
      let store_captured_variable_taint state root =
        if AccessPath.Root.is_captured_variable root then
          (* The origin for captured variables taint is at the inner function boundry due to there
             being no explicit parameter to mark as location *)
          (* TODO(T184561320): Pull in location of `nonlocal` statement if present *)
          let taint = apply_call ~location:define_location ~root in
          store_taint
            ~root:(AccessPath.Root.captured_variable_to_variable root)
            ~path:[]
            taint
            state
        else
          state
      in
      List.fold ~init:state ~f:store_captured_variable_taint (ForwardState.roots parameter_sources)
    in

    parameters
    |> List.fold ~init:bottom ~f:prime_parameter
    |> add_captured_variables_paramater_sources


  let forward ~statement_key state ~statement =
    TaintProfiler.track_statement_analysis ~profiler ~analysis:Forward ~statement ~f:(fun () ->
        log
          "Forward analysis of statement: `%a`@,With forward state: %a"
          Statement.pp
          statement
          pp
          state;
        let pyre_in_context =
          PyrePysaEnvironment.InContext.create_at_statement_key
            pyre_api
            ~define_name:FunctionContext.define_name
            ~define:FunctionContext.definition
            ~statement_key
        in
        analyze_statement ~pyre_in_context statement state)


  let backward ~statement_key:_ _ ~statement:_ = failwith "Don't call me"
end

let extract_source_model
    ~define
    ~pyre_api
    ~taint_configuration:
      {
        TaintConfiguration.Heap.analysis_model_constraints =
          { maximum_model_source_tree_width; maximum_trace_length; _ };
        source_sink_filter;
        _;
      }
    ~callable
    ~existing_forward
    ~apply_broadening
    exit_taint
  =
  let { Statement.Define.signature = { return_annotation; parameters; _ }; _ } = define in
  let normalized_parameters = AccessPath.normalize_parameters parameters in
  let simplify tree =
    let tree =
      (* We limit by maximum_trace_length - 1, since the distance will be incremented by one when
         the taint is propagated. *)
      let decrement x = x - 1 in
      ForwardState.Tree.prune_maximum_length
        ~global_maximum:(maximum_trace_length >>| decrement)
        ~maximum_per_kind:(fun source ->
          source |> SourceSinkFilter.maximum_source_distance source_sink_filter >>| decrement)
        tree
    in
    if apply_broadening then
      tree
      |> ForwardState.Tree.shape
           ~mold_with_return_access_paths:false
           ~breadcrumbs:(Features.model_source_shaping_set ())
      |> ForwardState.Tree.limit_to
           ~breadcrumbs:(Features.model_source_broadening_set ())
           ~width:maximum_model_source_tree_width
    else
      tree
  in
  let extract_model_from_variable ~variable ~port ~annotation state =
    let breadcrumbs_to_attach, via_features_to_attach =
      ForwardState.extract_features_to_attach
        ~root:port
        ~attach_to_kind:Sources.Attach
        existing_forward.Model.Forward.generations
    in
    let type_breadcrumbs =
      annotation
      >>| PyrePysaEnvironment.ReadOnly.parse_annotation pyre_api
      |> Features.type_breadcrumbs_from_annotation ~pyre_api
    in
    let taint =
      ForwardState.read ~root:variable ~path:[] exit_taint
      |> simplify
      |> ForwardState.Tree.add_local_breadcrumbs breadcrumbs_to_attach
      |> ForwardState.Tree.add_via_features via_features_to_attach
      |> ForwardState.Tree.add_local_breadcrumbs type_breadcrumbs
    in
    ForwardState.assign ~root:port ~path:[] taint state
  in
  let model =
    match normalized_parameters with
    | {
        root = self_parameter;
        original =
          { Node.value = { Parameter.name = self_variable; annotation = self_annotation; _ }; _ };
        _;
      }
      :: _
      when Interprocedural.Target.is_method callable ->
        ForwardState.bottom
        |> extract_model_from_variable
             ~variable:(AccessPath.Root.Variable self_variable)
             ~port:self_parameter
             ~annotation:self_annotation
        |> extract_model_from_variable
             ~variable:AccessPath.Root.LocalResult
             ~port:AccessPath.Root.LocalResult
             ~annotation:return_annotation
    | _ ->
        extract_model_from_variable
          ~variable:AccessPath.Root.LocalResult
          ~port:AccessPath.Root.LocalResult
          ~annotation:return_annotation
          ForwardState.bottom
  in
  let model =
    ForwardState.roots exit_taint
    |> List.filter ~f:AccessPath.Root.is_captured_variable
    |> List.fold ~init:model ~f:(fun model root ->
           let captured_variable_taint = ForwardState.read ~root ~path:[] exit_taint |> simplify in
           ForwardState.assign ~root ~path:[] captured_variable_taint model)
  in
  model


let run
    ?(profiler = TaintProfiler.disabled)
    ~taint_configuration
    ~string_combine_partial_sink_tree
    ~pyre_api
    ~callables_to_definitions_map
    ~class_interval_graph
    ~global_constants
    ~type_of_expression_shared_memory
    ~qualifier
    ~callable
    ~define
    ~cfg
    ~call_graph_of_define
    ~get_callee_model
    ~existing_model
    ()
  =
  let {
    Node.value = { Statement.Define.signature = { parameters; _ }; _ };
    location = define_location;
  }
    =
    define
  in
  let define_name =
    PyrePysaLogic.qualified_name_of_define ~module_name:qualifier (Node.value define)
  in
  let module FunctionContext = struct
    let qualifier = qualifier

    let define_name = define_name

    let definition = define

    let callable = callable

    let debug = Statement.Define.dump define.value

    let profiler = profiler

    let pyre_api = pyre_api

    let callables_to_definitions_map = callables_to_definitions_map

    let taint_configuration = taint_configuration

    let class_interval_graph = class_interval_graph

    let global_constants = global_constants

    let call_graph_of_define = call_graph_of_define

    let get_callee_model = get_callee_model

    let existing_model = existing_model

    let triggered_sinks_to_propagate = Issue.TriggeredSinkForBackward.create ()

    let caller_class_interval =
      Interprocedural.ClassIntervalSetGraph.SharedMemory.of_definition
        class_interval_graph
        define_name
        (Node.value definition)


    let string_combine_partial_sink_tree = string_combine_partial_sink_tree

    let type_of_expression_shared_memory = type_of_expression_shared_memory
  end
  in
  let module State = State (FunctionContext) in
  let module Fixpoint = PyrePysaLogic.Fixpoint.Make (State) in
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
        let normalized_parameters = AccessPath.normalize_parameters parameters in
        State.create ~existing_model normalized_parameters define_location)
  in
  let () = State.log "Processing CFG:@.%a" PyrePysaLogic.Cfg.pp cfg in
  let exit_state =
    TaintProfiler.track_duration ~profiler ~name:"Forward analysis - fixpoint" ~f:(fun () ->
        Alarm.with_alarm
          ~max_time_in_seconds:60
          ~event_name:"forward taint analysis"
          ~callable:(Interprocedural.Target.show_pretty callable)
          (fun () -> Fixpoint.forward ~cfg ~initial |> Fixpoint.exit)
          ())
  in
  let () =
    match exit_state with
    | Some exit_state -> State.log "Exit state:@,%a" State.pp exit_state
    | None -> State.log "No exit state found"
  in
  let extract_model { State.taint; _ } =
    let apply_broadening =
      not (Model.ModeSet.contains Model.Mode.SkipModelBroadening existing_model.modes)
    in
    let generations =
      TaintProfiler.track_duration ~profiler ~name:"Forward analysis - extract model" ~f:(fun () ->
          extract_source_model
            ~define:define.value
            ~pyre_api
            ~taint_configuration:FunctionContext.taint_configuration
            ~callable
            ~existing_forward:existing_model.forward
            ~apply_broadening
            taint)
    in
    let model = Model.Forward.{ generations } in
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
