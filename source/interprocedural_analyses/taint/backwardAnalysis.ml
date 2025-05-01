(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* BackwardAnalysis: implements a backward taint analysis on a function body.
 * This is used to infer the sink and taint-in-taint-out part of a model, by
 * propagating sinks up through the statements of the body.
 *
 * For instance, on the given function, we would infer the following taint
 * states, starting from the return statement:
 * ```
 * def foo(a, b):
 *   # {x -> SQL, a -> SQL, y -> LocalReturn, b -> LocalReturn }
 *   x = str(a)
 *   # {x -> SQL, y -> LocalReturn, b -> LocalReturn}
 *   sql(x)
 *   # {y -> LocalReturn, b -> LocalReturn}
 *   y = int(b)
 *   # {y -> LocalReturn}
 *   return y
 * ```
 *
 * We would infer that `a` leads to the sink `SQL`, and that calling `foo` with
 * a tainted `b` leads to the return value being tainted (which we call
 * taint-in-taint-out, tito for short).
 *)

open Core
open Ast
open Expression
open Pyre
open Domains
module CallGraph = Interprocedural.CallGraph
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

  val triggered_sinks : Issue.TriggeredSinkForBackward.t

  val caller_class_interval : Interprocedural.ClassIntervalSet.t
end

module State (FunctionContext : FUNCTION_CONTEXT) = struct
  type t = { taint: BackwardState.t }

  let pyre_api = FunctionContext.pyre_api

  let bottom = { taint = BackwardState.bottom }

  let pp formatter { taint } = BackwardState.pp formatter taint

  let show = Format.asprintf "%a" pp

  let less_or_equal ~left:{ taint = left; _ } ~right:{ taint = right; _ } =
    BackwardState.less_or_equal ~left ~right


  let join { taint = left } { taint = right; _ } =
    let taint = BackwardState.join left right in
    { taint }


  let widen ~previous:{ taint = prev; _ } ~next:{ taint = next; _ } ~iteration =
    let taint = BackwardState.widen ~iteration ~prev ~next in
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
          (* That can happen if that statement is not reachable by the forward analysis. *)
          Log.warning
            "Could not find callees for `%a` at `%a:%a` in the call graph. This is most likely \
             dead code."
            Expression.pp
            (Node.create_with_default_location (Expression.Call call) |> Ast.Expression.delocalize)
            Reference.pp
            FunctionContext.qualifier
            Location.pp
            location;
          CallGraph.CallCallees.unresolved
            ~reason:CallGraph.Unresolved.NoRecordInCallGraph
            ~message:(lazy "No record in call graph")
            ()
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


  let is_constructor =
    match Reference.last FunctionContext.define_name with
    | "__init__" -> true
    | _ -> false


  let is_setitem = String.equal (Reference.last FunctionContext.define_name) "__setitem__"

  let is_instance_or_class_method =
    let { Node.value = { Statement.Define.signature; _ }; _ } = FunctionContext.definition in
    Statement.Define.Signature.is_method signature
    && not (Statement.Define.Signature.is_static_method signature)


  let self_variable, self_parameter =
    if Interprocedural.Target.is_method FunctionContext.callable then
      let { Node.value = { Statement.Define.signature = { parameters; _ }; _ }; _ } =
        FunctionContext.definition
      in
      match AccessPath.normalize_parameters parameters with
      | {
          root = AccessPath.Root.PositionalParameter { position = 0; _ } as root;
          qualified_name;
          _;
        }
        :: _ ->
          Some (AccessPath.Root.Variable qualified_name), Some root
      | _ -> None, None
    else
      None, None


  (* This is where we can observe access paths reaching into LocalReturn and record the extraneous
     paths for more precise tito. *)
  let initial_taint =
    let {
      TaintConfiguration.Heap.infer_self_tito = configuration_infer_self_tito;
      TaintConfiguration.Heap.infer_argument_tito = configuration_infer_argument_tito;
      analysis_model_constraints = { maximum_tito_collapse_depth; _ };
      _;
    }
      =
      FunctionContext.taint_configuration
    in
    let mode_infer_self_tito =
      Model.ModeSet.contains Model.Mode.InferSelfTito FunctionContext.existing_model.Model.modes
    in
    let mode_infer_argument_tito =
      Model.ModeSet.contains Model.Mode.InferArgumentTito FunctionContext.existing_model.Model.modes
    in
    let make_tito_leaf kind =
      BackwardState.Tree.create_leaf
        (Domains.BackwardTaint.singleton
           (CallInfo.tito ())
           kind
           (Domains.local_return_frame ~output_path:[] ~collapse_depth:maximum_tito_collapse_depth))
    in
    let taint =
      match self_variable, self_parameter with
      | Some self_variable, Some self_parameter
        when is_instance_or_class_method
             && (configuration_infer_self_tito
                || mode_infer_self_tito
                || is_constructor
                || is_setitem
                || Statement.Define.is_property_setter (Node.value FunctionContext.definition)) ->
          (* Infer tito from arguments to self. *)
          BackwardState.assign
            ~root:self_variable
            ~path:[]
            (make_tito_leaf (Sinks.ParameterUpdate self_parameter))
            BackwardState.bottom
      | _ -> BackwardState.bottom
    in
    let taint =
      if configuration_infer_argument_tito || mode_infer_argument_tito then
        (* Infer tito between arguments. *)
        let { Node.value = { Statement.Define.signature = { parameters; _ }; _ }; _ } =
          FunctionContext.definition
        in
        parameters
        |> AccessPath.normalize_parameters
        |> List.fold
             ~init:taint
             ~f:(fun taint { AccessPath.NormalizedParameter.root; qualified_name; _ } ->
               BackwardState.assign
                 ~root:(AccessPath.Root.Variable qualified_name)
                 ~path:[]
                 (make_tito_leaf (Sinks.ParameterUpdate root))
                 taint)
      else
        taint
    in
    BackwardState.assign
      ~root:AccessPath.Root.LocalResult
      ~path:[]
      (make_tito_leaf Sinks.LocalReturn)
      taint


  let transform_non_leaves new_path taint =
    let infer_output_path sink paths =
      match Sinks.discard_transforms sink with
      | Sinks.LocalReturn
      | Sinks.ParameterUpdate _ ->
          let open Features.ReturnAccessPathTree in
          fold
            Path
            ~f:(fun (current_path, collapse_depth) paths ->
              let to_take = min collapse_depth (List.length new_path) in
              let new_path = List.take new_path to_take in
              let new_collapse_depth = collapse_depth - to_take in
              create_leaf new_collapse_depth
              |> prepend new_path
              |> prepend current_path
              |> join paths)
            ~init:bottom
            paths
          |> limit_depth
      | _ -> paths
    in
    BackwardTaint.transform_tito
      Features.ReturnAccessPathTree.Self
      (Context (BackwardTaint.kind, Map))
      ~f:infer_output_path
      taint


  let read_tree = BackwardState.Tree.read ~transform_non_leaves

  let get_taint access_path { taint; _ } =
    match access_path with
    | None -> BackwardState.Tree.empty
    | Some { AccessPath.root; path } -> BackwardState.read ~transform_non_leaves ~root ~path taint


  let store_taint ?(weak = false) ~root ~path taint { taint = state_taint } =
    { taint = BackwardState.assign ~weak ~root ~path taint state_taint }


  let analyze_definition ~define:_ state = state

  let globals_to_constants = function
    | { Node.value = Expression.Name (Name.Identifier identifier); _ } as value -> (
        let as_reference = identifier |> Reference.create |> Reference.delocalize in
        let global_string =
          Interprocedural.GlobalConstants.SharedMemory.ReadOnly.get
            FunctionContext.global_constants
            ~cache:true
            as_reference
        in
        match global_string with
        | Some global_string ->
            global_string
            |> (fun string_literal -> Expression.Constant (Constant.String string_literal))
            |> Node.create ~location:value.location
        | _ -> value)
    | value -> value


  type call_target_result = {
    arguments_taint: BackwardState.Tree.t list;
    implicit_argument_taint: CallModel.ImplicitArgument.Backward.t;
    captures_taint: BackwardState.Tree.t list;
    captures: CallModel.ArgumentMatches.t list;
    state: t;
  }

  let join_call_target_results
      {
        arguments_taint = left_arguments_taint;
        implicit_argument_taint = left_implicit_argument_taint;
        captures_taint = left_captures_taint;
        captures = left_captures;
        state = left_state;
      }
      {
        arguments_taint = right_arguments_taint;
        implicit_argument_taint = right_implicit_argument_taint;
        captures_taint = right_captures_taint;
        captures = right_captures;
        state = right_state;
      }
    =
    let arguments_taint =
      List.map2_exn left_arguments_taint right_arguments_taint ~f:BackwardState.Tree.join
    in
    let captures_taint =
      if List.length left_captures_taint > List.length right_captures_taint then
        left_captures_taint
      else
        right_captures_taint
    in
    let implicit_argument_taint =
      CallModel.ImplicitArgument.Backward.join
        left_implicit_argument_taint
        right_implicit_argument_taint
    in
    let state = join left_state right_state in
    let captures =
      if List.length left_captures > List.length right_captures then
        left_captures
      else
        right_captures
    in
    { arguments_taint; implicit_argument_taint; captures_taint; captures; state }


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
      BackwardState.Tree.transform
        BackwardTaint.Self
        Map
        ~f:(BackwardTaint.add_extra_traces ~extra_traces)
        taint
    else (* See reasoning in `add_extra_traces_for_tito_transforms` of forwardAnalysis.ml *)
      BackwardState.Tree.bottom


  let apply_call_target
      ?(apply_tito = true)
      ~pyre_in_context
      ~call_location
      ~self
      ~callee
      ~arguments
      ~state:initial_state
      ~call_taint
      ~is_implicit_new
      ~implicit_returns_self
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
    let implicit_argument =
      CallGraph.ImplicitArgument.implicit_argument ~is_implicit_new call_target
    in
    let arguments =
      match implicit_argument with
      | CalleeBase -> { Call.Argument.name = None; value = Option.value_exn self } :: arguments
      | Callee -> { Call.Argument.name = None; value = callee } :: arguments
      | None -> arguments
    in
    let taint_model =
      TaintProfiler.track_model_fetch ~profiler ~analysis:Backward ~call_target:target ~f:(fun () ->
          CallModel.at_callsite
            ~pyre_in_context
            ~get_callee_model:FunctionContext.get_callee_model
            ~call_target:target
            ~arguments)
    in
    log
      "Backward analysis of call to `%a` with arguments (%a)@,Call site model:@,%a"
      Interprocedural.Target.pp_pretty
      target
      Ast.Expression.pp_expression_argument_list
      arguments
      Model.pp
      taint_model;
    let call_taint =
      BackwardState.Tree.add_local_breadcrumbs
        (Features.type_breadcrumbs (Option.value_exn return_type))
        call_taint
    in
    let get_taint_from_argument_expression
        ~pyre_in_context
        ~root
        ~argument:{ Call.Argument.value = argument; _ }
      =
      let global_sink =
        GlobalModel.from_expression
          ~pyre_in_context
          ~call_graph:FunctionContext.call_graph_of_define
          ~get_callee_model:FunctionContext.get_callee_model
          ~expression:argument
          ~interval:FunctionContext.caller_class_interval
        |> GlobalModel.get_sinks
        |> SinkTreeWithHandle.join
      in
      let taint_from_state =
        let access_path = AccessPath.of_expression ~self_variable argument in
        get_taint access_path initial_state
      in
      let implicit_self_taint =
        match root with
        | AccessPath.Root.PositionalParameter { position = 0; _ } when implicit_returns_self ->
            call_taint
        | _ -> BackwardState.Tree.bottom
      in
      taint_from_state
      |> BackwardState.Tree.join global_sink
      |> BackwardState.Tree.join implicit_self_taint
    in
    let get_taint_of_formal_argument ~pyre_in_context ~formal_argument =
      AccessPath.match_actuals_to_one_formal arguments formal_argument
      |> List.fold ~init:BackwardState.Tree.empty ~f:(fun sofar (actual_argument, _) ->
             get_taint_from_argument_expression
               ~pyre_in_context
               ~root:formal_argument
               ~argument:actual_argument
             |> BackwardState.Tree.join sofar)
    in
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
        ~sink_trees
        ~tito_roots
        ~kind
        (tito_path, tito_taint)
        argument_taint
      =
      (* TODO(T201555212): Distinguish `LocalTaint` (e.g., breadcrumbs) from different tito
         intervals. One example false positive is function `issue_precise_tito_intervals` in
         `class_interval.py`. *)
      let breadcrumbs = BackwardTaint.joined_breadcrumbs tito_taint in
      let tito_depth =
        BackwardTaint.fold TraceLength.Self tito_taint ~f:TraceLength.join ~init:TraceLength.bottom
      in
      let taint_to_propagate =
        (match Sinks.discard_transforms kind with
        | Sinks.LocalReturn -> call_taint
        | Sinks.ParameterUpdate root ->
            get_taint_of_formal_argument ~pyre_in_context ~formal_argument:root
        | Sinks.Attach ->
            (* Attach nodes shouldn't affect analysis. *)
            BackwardState.Tree.empty
        | _ -> Format.asprintf "unexpected kind for tito: %a" Sinks.pp kind |> failwith)
        |> BackwardState.Tree.apply_class_intervals_for_tito
             ~is_class_method
             ~is_static_method
             ~call_info_intervals
             ~tito_intervals:(CallModel.tito_intervals tito_taint)
             ~callee:call_target.CallGraph.CallTarget.target
      in
      let taint_to_propagate =
        match kind with
        | Sinks.Transform { local = transforms; global; _ } when TaintTransforms.is_empty global ->
            (* Apply tito transforms and source- and sink-specific sanitizers. *)
            let taint_to_propagate =
              BackwardState.Tree.apply_transforms
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
                BackwardState.Tree.transform_non_tito
                  Features.LocalKindSpecificBreadcrumbSet.Self
                  Map
                  ~f:(Features.BreadcrumbSet.add breadcrumb)
                  taint_to_propagate
              in
              add_extra_traces_for_tito_transforms
                ~argument_access_path:tito_path
                ~named_transforms
                ~sink_trees
                ~tito_roots
                taint_to_propagate
        | Sinks.Transform _ -> failwith "unexpected non-empty `global` transforms in tito"
        | _ -> taint_to_propagate
      in
      let transform_existing_tito ~callee_collapse_depth kind frame =
        match Sinks.discard_transforms kind with
        | Sinks.LocalReturn
        | Sinks.ParameterUpdate _ ->
            frame
            |> Frame.transform TraceLength.Self Map ~f:(fun depth -> max depth (1 + tito_depth))
            |> Frame.transform Features.CollapseDepth.Self Map ~f:(fun collapse_depth ->
                   min collapse_depth callee_collapse_depth)
        | _ -> frame
      in
      CallModel.return_paths_and_collapse_depths ~kind ~tito_taint
      |> List.fold
           ~f:(fun taint (return_path, collapse_depth) ->
             let taint_to_propagate = read_tree return_path taint_to_propagate in
             (if Features.CollapseDepth.should_collapse collapse_depth then
                BackwardState.Tree.collapse_to
                  ~breadcrumbs:(Features.tito_broadening_set ())
                  ~depth:collapse_depth
                  taint_to_propagate
             else
               taint_to_propagate)
             |> BackwardState.Tree.add_local_breadcrumbs breadcrumbs
             |> BackwardState.Tree.transform_tito
                  Frame.Self
                  (Context (BackwardTaint.kind, Map))
                  ~f:(transform_existing_tito ~callee_collapse_depth:collapse_depth)
             |> BackwardState.Tree.prepend tito_path
             |> BackwardState.Tree.join taint)
           ~init:argument_taint
    in
    let convert_tito_tree_to_taint
        ~argument
        ~sink_trees
        ~kind
        ~pair:{ CallModel.TaintInTaintOutMap.TreeRootsPair.tree = tito_tree; roots = tito_roots }
        taint_tree
      =
      BackwardState.Tree.fold
        BackwardState.Tree.Path
        tito_tree
        ~init:BackwardState.Tree.bottom
        ~f:(convert_tito_path_to_taint ~sink_trees ~tito_roots ~kind)
      |> BackwardState.Tree.transform Features.TitoPositionSet.Element Add ~f:argument.Node.location
      |> BackwardState.Tree.add_local_breadcrumb (Features.tito ())
      |> BackwardState.Tree.join taint_tree
    in
    let call_site = CallSite.create call_location in
    let analyze_argument
        (arguments_taint, state)
        {
          CallModel.ArgumentMatches.argument;
          generation_source_matches = _;
          sink_matches;
          tito_matches;
          sanitize_matches;
        }
      =
      let track_apply_call_step step f =
        TaintProfiler.track_apply_call_step
          ~profiler
          ~analysis:Backward
          ~step
          ~call_target:(Some target)
          ~location:call_location
          ~argument:(Some argument)
          ~f
      in
      let convert_partial_sinks_into_triggered
          ({ SinkTreeWithHandle.sink_tree; _ } as sink_tree_with_handle)
        =
        let sink_tree =
          Issue.TriggeredSinkForBackward.convert_partial_sinks_into_triggered
            ~call_site
            ~argument_location:argument.Node.location
            ~argument_sink:sink_tree
            FunctionContext.triggered_sinks
        in
        { sink_tree_with_handle with sink_tree }
      in
      let sink_trees =
        track_apply_call_step ApplyCallForArgumentSinks (fun () ->
            CallModel.sink_trees_of_argument
              ~pyre_in_context
              ~transform_non_leaves
              ~model:taint_model
              ~call_site
              ~location:argument.Node.location
              ~call_target
              ~arguments
              ~sink_matches
              ~is_class_method
              ~is_static_method
              ~call_info_intervals
            |> List.map ~f:convert_partial_sinks_into_triggered)
      in
      let taint_in_taint_out =
        let taint_in_taint_out_map =
          track_apply_call_step BuildTaintInTaintOutMapping (fun () ->
              if apply_tito then
                CallModel.taint_in_taint_out_mapping_for_argument
                  ~transform_non_leaves
                  ~taint_configuration:FunctionContext.taint_configuration
                  ~ignore_local_return:(BackwardState.Tree.is_bottom call_taint)
                  ~model:taint_model
                  ~callable:target
                  ~tito_matches
                  ~sanitize_matches
              else
                CallModel.TaintInTaintOutMap.empty)
        in
        track_apply_call_step ApplyTitoForArgument (fun () ->
            CallModel.TaintInTaintOutMap.fold
              ~init:BackwardState.Tree.empty
              ~f:(convert_tito_tree_to_taint ~argument ~sink_trees)
              taint_in_taint_out_map)
      in
      let sink_taint = SinkTreeWithHandle.join sink_trees in
      let taint = BackwardState.Tree.join sink_taint taint_in_taint_out in
      let state =
        match AccessPath.of_expression ~self_variable argument with
        | Some { AccessPath.root; path } ->
            let breadcrumbs_to_add =
              BackwardState.Tree.filter_by_kind ~kind:Sinks.AddFeatureToArgument sink_taint
              |> BackwardTaint.joined_breadcrumbs
            in
            if Features.BreadcrumbSet.is_bottom breadcrumbs_to_add then
              state
            else
              let taint =
                BackwardState.read state.taint ~root ~path
                |> BackwardState.Tree.add_local_breadcrumbs breadcrumbs_to_add
              in
              { taint = BackwardState.assign ~root ~path taint state.taint }
        | None -> state
      in
      taint :: arguments_taint, state
    in
    let analyze_argument_matches argument_matches initial_state =
      argument_matches |> List.rev |> List.fold ~f:analyze_argument ~init:([], initial_state)
    in
    let _, captures =
      CallModel.match_captures
        ~model:taint_model
        ~captures_taint:ForwardState.empty
        ~location:call_location
    in
    let captures_taint, _ = analyze_argument_matches captures initial_state in

    let arguments_taint, state =
      analyze_argument_matches
        (CallModel.match_actuals_to_formals ~model:taint_model ~arguments)
        initial_state
    in
    (* Extract the taint for implicit arguments. *)
    let implicit_argument_taint, arguments_taint =
      match implicit_argument with
      | CalleeBase -> (
          match arguments_taint with
          | self_taint :: arguments_taint ->
              CallModel.ImplicitArgument.Backward.CalleeBase self_taint, arguments_taint
          | _ -> failwith "missing taint for self argument")
      | Callee -> (
          match arguments_taint with
          | callee_taint :: arguments_taint ->
              CallModel.ImplicitArgument.Backward.Callee callee_taint, arguments_taint
          | _ -> failwith "missing taint for callee argument")
      | None -> CallModel.ImplicitArgument.Backward.None, arguments_taint
    in
    { arguments_taint; implicit_argument_taint; captures_taint; captures; state }


  let apply_obscure_call ~apply_tito ~callee ~arguments ~state:initial_state ~call_taint =
    log
      "Backward analysis of obscure call to `%a` with arguments (%a)"
      Expression.pp
      callee
      Ast.Expression.pp_expression_argument_list
      arguments;
    let obscure_taint =
      if apply_tito then
        BackwardState.Tree.collapse ~breadcrumbs:(Features.tito_broadening_set ()) call_taint
        |> BackwardTaint.add_local_breadcrumb (Features.obscure_unknown_callee ())
        |> BackwardTaint.transform_tito
             Features.CollapseDepth.Self
             Map
             ~f:Features.CollapseDepth.approximate
        |> BackwardState.Tree.create_leaf
      else
        BackwardState.Tree.empty
    in
    let compute_argument_taint { Call.Argument.value = argument; _ } =
      let taint = obscure_taint in
      let taint =
        match argument.Node.value with
        | Starred (Starred.Once _)
        | Starred (Starred.Twice _) ->
            BackwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex] taint
        | _ -> taint
      in
      let taint =
        BackwardState.Tree.transform
          Features.TitoPositionSet.Element
          Add
          ~f:argument.Node.location
          taint
      in
      taint
    in
    let arguments_taint = List.map ~f:compute_argument_taint arguments in
    {
      arguments_taint;
      implicit_argument_taint = CallModel.ImplicitArgument.Backward.Callee obscure_taint;
      captures_taint = [];
      captures = [];
      state = initial_state;
    }


  let apply_constructor_targets
      ~pyre_in_context
      ~call_location
      ~callee
      ~arguments
      ~new_targets
      ~init_targets
      ~state:initial_state
      ~call_taint
    =
    let is_object_new = CallGraph.CallCallees.is_object_new new_targets in
    let is_object_init = CallGraph.CallCallees.is_object_init init_targets in

    (* If both `is_object_new` and `is_object_init` are true, this is probably a stub
     * class (e.g, `class X: ...`), in which case, we treat it as an obscure call. *)

    (* Call `__init__`. Add the `self` implicit argument. *)
    let {
      arguments_taint = init_arguments_taint;
      implicit_argument_taint;
      captures_taint = _;
      captures = _;
      state;
    }
      =
      if is_object_init && not is_object_new then
        {
          arguments_taint = List.map arguments ~f:(fun _ -> BackwardState.Tree.bottom);
          implicit_argument_taint = CallModel.ImplicitArgument.Backward.CalleeBase call_taint;
          captures_taint = [];
          captures = [];
          state = initial_state;
        }
      else
        let call_expression =
          Expression.Call
            {
              Call.callee;
              arguments;
              origin = Some { Node.location = call_location; value = Origin.ImplicitInitCall };
            }
          |> Node.create ~location:call_location
        in
        List.map init_targets ~f:(fun target ->
            apply_call_target
              ~pyre_in_context
              ~call_location
              ~self:(Some call_expression)
              ~callee
              ~arguments
              ~state:initial_state
              ~call_taint
              ~is_implicit_new:false
              ~implicit_returns_self:true
              target)
        |> List.fold
             ~f:join_call_target_results
             ~init:
               {
                 arguments_taint = List.map arguments ~f:(fun _ -> BackwardState.Tree.bottom);
                 implicit_argument_taint = CallModel.ImplicitArgument.Backward.None;
                 captures_taint = [];
                 captures = [];
                 state = bottom;
               }
    in
    let base_taint =
      match implicit_argument_taint with
      | CalleeBase taint -> taint
      | None -> BackwardState.Tree.bottom
      | Callee _ ->
          (* T122799408: This is a rare case, which is handled with a simple workaround. See
             function `dunder_call_partial_constructor` in
             `source/interprocedural_analyses/taint/test/integration/partial.py`. *)
          BackwardState.Tree.bottom
    in

    (* Call `__new__`. *)
    let call_target_result =
      if is_object_new then
        {
          arguments_taint = init_arguments_taint;
          implicit_argument_taint = CallModel.ImplicitArgument.Backward.None;
          captures_taint = [];
          captures = [];
          state;
        }
      else (* Add the `cls` implicit argument. *)
        let {
          arguments_taint = new_arguments_taint;
          implicit_argument_taint;
          captures_taint = _;
          captures = _;
          state;
        }
          =
          List.map new_targets ~f:(fun target ->
              apply_call_target
                ~pyre_in_context
                ~call_location
                ~self:(Some callee)
                ~callee
                ~arguments
                ~state
                ~call_taint:base_taint
                ~is_implicit_new:true
                ~implicit_returns_self:false
                target)
          |> List.fold
               ~f:join_call_target_results
               ~init:
                 {
                   arguments_taint = List.map arguments ~f:(fun _ -> BackwardState.Tree.bottom);
                   implicit_argument_taint = CallModel.ImplicitArgument.Backward.None;
                   captures_taint = [];
                   captures = [];
                   state = bottom;
                 }
        in
        let callee_taint =
          match implicit_argument_taint with
          | CallModel.ImplicitArgument.Backward.CalleeBase taint -> taint
          | Callee _
          | None ->
              failwith "Expect implicit argument `CalleeBase` from calling `__new__`"
        in
        {
          arguments_taint =
            List.map2_exn init_arguments_taint new_arguments_taint ~f:BackwardState.Tree.join;
          implicit_argument_taint = CallModel.ImplicitArgument.Backward.Callee callee_taint;
          captures_taint = [];
          captures = [];
          state;
        }
    in

    call_target_result


  let apply_callees_and_return_arguments_taint
      ?(apply_tito = true)
      ~pyre_in_context
      ~callee
      ~call_location
      ~arguments
      ~state:initial_state
      ~call_taint
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
    let call_taint =
      (* Add index breadcrumb if appropriate. *)
      match callee.Node.value, arguments with
      | Expression.Name (Name.Attribute { attribute = "get"; _ }), index :: _ ->
          let label = AccessPath.get_index index.Call.Argument.value in
          BackwardState.Tree.add_local_first_index label call_taint
      | _ -> call_taint
    in

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
    let call_target_result =
      List.map
        call_targets
        ~f:
          (apply_call_target
             ~apply_tito
             ~pyre_in_context
             ~call_location
             ~self
             ~callee
             ~arguments
             ~state:initial_state
             ~call_taint
             ~is_implicit_new:false
             ~implicit_returns_self:false)
      |> List.fold
           ~f:join_call_target_results
           ~init:
             {
               arguments_taint = List.map arguments ~f:(fun _ -> BackwardState.Tree.bottom);
               implicit_argument_taint = CallModel.ImplicitArgument.Backward.None;
               captures_taint = [];
               captures = [];
               state = bottom;
             }
    in

    (* Apply an obscure call if the call was not fully resolved. *)
    let call_target_result =
      if CallGraph.Unresolved.is_unresolved unresolved then
        apply_obscure_call ~apply_tito ~callee ~arguments ~state:initial_state ~call_taint
        |> join_call_target_results call_target_result
      else
        call_target_result
    in

    (* Apply constructor calls, if any. *)
    let call_target_result =
      match new_targets, init_targets with
      | [], [] -> call_target_result
      | _ ->
          apply_constructor_targets
            ~pyre_in_context
            ~call_location
            ~callee
            ~arguments
            ~new_targets
            ~init_targets
            ~state:initial_state
            ~call_taint
          |> join_call_target_results call_target_result
    in

    call_target_result


  let rec analyze_arguments ~pyre_in_context ~arguments ~arguments_taint ~state =
    (* Explicitly analyze arguments from right to left (opposite of forward analysis). *)
    List.zip_exn arguments arguments_taint
    |> List.rev
    |> List.fold
         ~init:state
         ~f:(fun state ({ Call.Argument.value = argument; _ }, argument_taint) ->
           analyze_unstarred_expression ~pyre_in_context argument_taint argument state)


  and analyze_callee
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~is_property_call
      ~callee
      ~implicit_argument_taint
      ~state
    =
    (* Special case: `x.foo()` where foo is a property returning a callable. *)
    let analyze ~base_taint ~callee_taint =
      match callee.Node.value with
      | Expression.Name (Name.Attribute { base; attribute; origin }) ->
          (* If we are already analyzing a call of a property, then ignore properties
           * to avoid infinite recursion. *)
          let resolve_properties = not is_property_call in
          analyze_attribute_access
            ~pyre_in_context
            ~location:callee.Node.location
            ~resolve_properties
            ~base
            ~attribute
            ~origin
            ~base_taint
            ~attribute_taint:callee_taint
            ~state
      | _ -> analyze_expression ~pyre_in_context ~taint:callee_taint ~state ~expression:callee
    in
    let callee_is_property =
      match is_property_call, callee.Node.value with
      | false, Expression.Name (Name.Attribute { attribute; _ }) ->
          get_attribute_access_callees ~location:callee.Node.location ~attribute |> Option.is_some
      | _ -> false
    in
    if callee_is_property then
      let base_taint, callee_taint =
        match implicit_argument_taint with
        | CallModel.ImplicitArgument.Backward.Callee taint -> BackwardState.Tree.bottom, taint
        | CalleeBase taint -> taint, BackwardState.Tree.bottom
        | None -> BackwardState.Tree.bottom, BackwardState.Tree.bottom
      in
      analyze ~base_taint ~callee_taint
    else
      match implicit_argument_taint with
      | CallModel.ImplicitArgument.Backward.Callee callee_taint ->
          analyze ~base_taint:BackwardState.Tree.bottom ~callee_taint
      | CalleeBase taint -> (
          match callee.Node.value with
          | Expression.Name (Name.Attribute { base; _ }) ->
              analyze_expression ~pyre_in_context ~taint ~state ~expression:base
          | _ -> state)
      | None -> state


  and analyze_attribute_access
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~location
      ~resolve_properties
      ~base
      ~attribute
      ~origin
      ~base_taint:initial_base_taint
      ~attribute_taint
      ~state
    =
    let expression =
      Expression.Name (Name.Attribute { base; attribute; origin }) |> Node.create ~location
    in
    let attribute_access_callees =
      if resolve_properties then get_attribute_access_callees ~location ~attribute else None
    in

    let base_taint_property_call, state_property_call =
      match attribute_access_callees with
      | Some { property_targets = _ :: _ as property_targets; _ } ->
          let {
            arguments_taint = _;
            implicit_argument_taint;
            captures_taint = _;
            captures = _;
            state;
          }
            =
            apply_callees_and_return_arguments_taint
              ~pyre_in_context
              ~callee:expression
              ~call_location:location
              ~arguments:[]
              ~state
              ~call_taint:attribute_taint
              (CallGraph.CallCallees.create ~call_targets:property_targets ())
          in
          let base_taint =
            match implicit_argument_taint with
            | CallModel.ImplicitArgument.Backward.CalleeBase taint -> taint
            | _ -> failwith "Expect `CalleeBase` for attribute access"
          in
          base_taint, state
      | _ -> BackwardState.Tree.bottom, bottom
    in

    let base_taint_attribute, state_attribute =
      match attribute_access_callees with
      | Some { is_attribute = true; _ }
      | None ->
          let global_model =
            GlobalModel.from_expression
              ~pyre_in_context
              ~call_graph:FunctionContext.call_graph_of_define
              ~get_callee_model:FunctionContext.get_callee_model
              ~expression
              ~interval:FunctionContext.caller_class_interval
          in
          let add_tito_features taint =
            let attribute_breadcrumbs =
              global_model |> GlobalModel.get_tito |> BackwardState.Tree.joined_breadcrumbs
            in
            BackwardState.Tree.add_local_breadcrumbs attribute_breadcrumbs taint
          in

          let apply_attribute_sanitizers taint =
            let sanitizer = GlobalModel.get_sanitize global_model in
            let taint =
              let sanitizers =
                { SanitizeTransformSet.sources = sanitizer.sources; sinks = sanitizer.sinks }
              in
              BackwardState.Tree.apply_sanitize_transforms
                ~taint_configuration:FunctionContext.taint_configuration
                sanitizers
                TaintTransformOperation.InsertLocation.Front
                taint
            in
            taint
          in

          let base_taint =
            attribute_taint
            |> add_tito_features
            |> BackwardState.Tree.prepend [Abstract.TreeDomain.Label.Index attribute]
            |> apply_attribute_sanitizers
          in
          base_taint, state
      | _ -> BackwardState.Tree.bottom, bottom
    in

    let base_taint =
      initial_base_taint
      |> BackwardState.Tree.join base_taint_property_call
      |> BackwardState.Tree.join base_taint_attribute
    in
    let state = join state_property_call state_attribute in
    analyze_expression ~pyre_in_context ~taint:base_taint ~state ~expression:base


  and analyze_arguments_with_higher_order_parameters
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~arguments
      ~arguments_taint
      ~state
      ~higher_order_parameters
    =
    (* If we have functions `fn1`, `fn2`, `fn3` getting passed into `hof`, we use the following strategy:
     * hof(q, fn1, x, fn2, y, fn3) gets translated into (analyzed backwards)
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
    let arguments_and_taints = List.zip_exn arguments arguments_taint in

    let higher_order_parameters =
      higher_order_parameters
      |> CallGraph.HigherOrderParameterMap.to_list
      |> List.filter_map
           ~f:(fun ({ CallGraph.HigherOrderParameter.index; _ } as higher_order_parameter) ->
             match List.nth arguments_and_taints index with
             | Some ({ Call.Argument.value = argument; _ }, taint) ->
                 Some (higher_order_parameter, argument, taint)
             | None -> None)
    in

    let non_function_arguments_taint =
      let function_argument_indices =
        List.fold
          ~init:Int.Set.empty
          ~f:(fun indices ({ CallGraph.HigherOrderParameter.index; _ }, _, _) ->
            Set.add indices index)
          higher_order_parameters
      in
      List.filteri arguments_and_taints ~f:(fun index _ ->
          not (Set.mem function_argument_indices index))
    in

    (* Simulate if branch. *)
    let all_taint, if_branch_state =
      let analyze_function_call
          (all_taint, state)
          ( { CallGraph.HigherOrderParameter.call_targets; index = _; unresolved },
            ({ Node.location = argument_location; _ } as argument),
            argument_taint )
        =
        (* Simulate $result = fn( *all, **all) *)
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
        let { arguments_taint; implicit_argument_taint; captures_taint; captures; state } =
          apply_callees_and_return_arguments_taint
            ~pyre_in_context
            ~callee:argument
            ~call_location:argument_location
            ~arguments
            ~call_taint:argument_taint
            ~state
            (CallGraph.CallCallees.create ~call_targets ~unresolved ())
        in
        let state =
          analyze_callee
            ~pyre_in_context
            ~is_property_call:false
            ~callee:argument
            ~implicit_argument_taint
            ~state
        in
        let state =
          List.fold
            ~init:state
            ~f:(fun state (capture, capture_taint) ->
              analyze_expression
                ~pyre_in_context
                ~taint:capture_taint
                ~state
                ~expression:capture.value)
            (List.zip_exn (CallModel.captures_as_arguments captures) captures_taint)
        in
        let all_taint =
          arguments_taint
          |> List.fold ~f:BackwardState.Tree.join ~init:BackwardState.Tree.bottom
          |> read_tree [Abstract.TreeDomain.Label.AnyIndex]
          |> BackwardState.Tree.add_local_breadcrumb (Features.higher_order_parameter ())
          |> BackwardState.Tree.join all_taint
        in
        all_taint, state
      in
      List.fold
        ~init:(BackwardState.Tree.bottom, state)
        ~f:analyze_function_call
        higher_order_parameters
    in

    (* Simulate else branch. *)
    let else_branch_state =
      let analyze_function_expression state (_, argument, argument_taint) =
        analyze_expression ~pyre_in_context ~taint:argument_taint ~state ~expression:argument
      in
      List.fold ~init:state ~f:analyze_function_expression higher_order_parameters
    in

    (* Join both branches. *)
    let state = join else_branch_state if_branch_state in

    (* Analyze arguments. *)
    List.fold
      non_function_arguments_taint
      ~init:state
      ~f:(fun state ({ Call.Argument.value = argument; _ }, argument_taint) ->
        let argument_taint = BackwardState.Tree.join argument_taint all_taint in
        analyze_unstarred_expression ~pyre_in_context argument_taint argument state)


  and apply_callees
      ?(apply_tito = true)
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~is_property
      ~callee
      ~call_location
      ~arguments
      ~state:initial_state
      ~call_taint
      callees
    =
    let { arguments_taint; implicit_argument_taint; captures_taint; captures; state } =
      apply_callees_and_return_arguments_taint
        ~apply_tito
        ~pyre_in_context
        ~callee
        ~call_location
        ~arguments
        ~state:initial_state
        ~call_taint
        callees
    in
    let state =
      if CallGraph.HigherOrderParameterMap.is_empty callees.higher_order_parameters then
        analyze_arguments
          ~pyre_in_context
          ~arguments:(arguments @ CallModel.captures_as_arguments captures)
          ~arguments_taint:(arguments_taint @ captures_taint)
          ~state
      else
        analyze_arguments_with_higher_order_parameters
          ~pyre_in_context
          ~arguments:(arguments @ CallModel.captures_as_arguments captures)
          ~arguments_taint:(arguments_taint @ captures_taint)
          ~state
          ~higher_order_parameters:callees.higher_order_parameters
    in

    let state =
      analyze_callee
        ~pyre_in_context
        ~is_property_call:is_property
        ~callee
        ~implicit_argument_taint
        ~state
    in
    state


  and analyze_dictionary_entry
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      taint
      state
      entry
    =
    let open Dictionary.Entry in
    match entry with
    | KeyValue { key; value } ->
        let key_taint = read_tree [AccessPath.dictionary_keys] taint in
        let state = analyze_expression ~pyre_in_context ~taint:key_taint ~state ~expression:key in
        let field_name = AccessPath.get_index key in
        let value_taint = read_tree [field_name] taint in
        analyze_expression ~pyre_in_context ~taint:value_taint ~state ~expression:value
    | Splat s -> analyze_expression ~pyre_in_context ~taint ~state ~expression:s


  and analyze_reverse_list_element
      ~total
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      taint
      reverse_position
      state
      expression
    =
    let position = total - reverse_position - 1 in
    let index_name = Abstract.TreeDomain.Label.Index (string_of_int position) in
    let value_taint = read_tree [index_name] taint in
    analyze_expression ~pyre_in_context ~taint:value_taint ~state ~expression


  and analyze_generators ~(pyre_in_context : PyrePysaEnvironment.InContext.t) ~state generators =
    let handle_generator state ({ Comprehension.Generator.conditions; _ } as generator) =
      let state =
        List.fold conditions ~init:state ~f:(fun state condition ->
            analyze_expression
              ~pyre_in_context
              ~taint:BackwardState.Tree.empty
              ~state
              ~expression:condition)
      in
      let { Statement.Assign.target; value; _ } =
        Statement.Statement.generator_assignment generator
      in
      match value with
      | Some value -> analyze_assignment ~pyre_in_context ~target ~value state
      | None -> state
    in
    List.fold ~f:handle_generator generators ~init:state


  and analyze_comprehension
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      taint
      { Comprehension.element; generators; _ }
      state
    =
    let pyre_in_context =
      PyrePysaEnvironment.InContext.resolve_generators pyre_in_context generators
    in
    let element_taint = read_tree [Abstract.TreeDomain.Label.AnyIndex] taint in
    let state =
      analyze_expression ~pyre_in_context ~taint:element_taint ~state ~expression:element
    in
    analyze_generators ~pyre_in_context ~state generators


  (* Skip through * and **. Used at call sites where * and ** are handled explicitly *)
  and analyze_unstarred_expression
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      taint
      expression
      state
    =
    match expression.Node.value with
    | Starred (Starred.Once expression)
    | Starred (Starred.Twice expression) ->
        analyze_expression ~pyre_in_context ~taint ~state ~expression
    | _ -> analyze_expression ~pyre_in_context ~taint ~state ~expression


  and analyze_getitem_call_target
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~index_number
      ~location
      ~base
      ~taint
      ~state
      ~state_before_index_access
      call_target
    =
    let analyze_getitem receiver_class =
      let named_tuple_attributes =
        PyrePysaEnvironment.ReadOnly.named_tuple_attributes pyre_api receiver_class
      in
      match named_tuple_attributes, index_number with
      | Some named_tuple_attributes, Some index_number ->
          List.nth named_tuple_attributes index_number
          (* Access an attribute of a named tuple via indices *)
          >>| (fun attribute ->
                analyze_attribute_access
                  ~pyre_in_context
                  ~location
                  ~resolve_properties:false
                  ~base
                  ~attribute
                  ~origin:None
                  ~base_taint:BackwardState.Tree.bottom
                  ~attribute_taint:taint
                  ~state)
          (* Access an attribute of a named tuple via invalid indices *)
          |> Option.value ~default:bottom
      | Some _, None ->
          (* Access a named tuple with unknown indices *)
          Lazy.force state_before_index_access
      | None, _ ->
          (* Not access a named tuple *)
          Lazy.force state_before_index_access
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
        Lazy.force state_before_index_access


  and analyze_call ~pyre_in_context ~location ~taint ~state ~callee ~arguments ~origin =
    let callees = get_call_callees ~location ~call:{ Call.callee; arguments; origin } in

    let add_type_breadcrumbs taint =
      let type_breadcrumbs = CallModel.type_breadcrumbs_of_calls callees.call_targets in
      BackwardState.Tree.add_local_breadcrumbs type_breadcrumbs taint
    in

    match { Call.callee; arguments; origin } with
    | {
     callee =
       { Node.value = Name (Name.Attribute { base; attribute = "__setitem__"; _ }); _ } as callee;
     arguments =
       [{ Call.Argument.value = index; name = None }; { Call.Argument.value; name = None }] as
       arguments;
     origin = _;
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
            ~is_property:false
            ~call_location:location
            ~state
            ~callee
            ~arguments
            ~call_taint:taint
            callees
        in
        if use_custom_tito then
          (* Use the hardcoded behavior of `__setitem__` for any subtype of dict or list, and for
             unresolved calls. This is incorrect, but can lead to higher SNR, because we assume in
             most cases, we run into an expression whose type is exactly `dict`, rather than a
             (strict) subtype of `dict` that overrides `__setitem__`. *)
          let state =
            if not is_sequence_setitem then
              (* Since we smash the taint of ALL keys, we do a weak update here to avoid removing
                 the taint in `**keys`. That is, we join the state before analyzing the assignment
                 to `**keys` and the state afterwards. *)
              analyze_assignment
                ~weak:true
                ~pyre_in_context
                ~fields:[AccessPath.dictionary_keys]
                ~target:base
                ~value:index
                state
            else
              analyze_expression
                ~pyre_in_context
                ~taint:BackwardState.Tree.bottom
                ~state
                ~expression:index
          in
          analyze_assignment
            ~pyre_in_context
            ~fields:[AccessPath.get_index index]
            ~target:base
            ~value
            state
        else
          state
    | {
     callee = { Node.value = Name (Name.Attribute { base; attribute = "__getitem__"; _ }); _ };
     arguments =
       [
         {
           Call.Argument.value = { Node.value = argument_expression; _ } as argument_value;
           name = None;
         };
       ];
     origin = _;
    } ->
        let taint = add_type_breadcrumbs taint in
        let index = AccessPath.get_index argument_value in
        let state_before_index_access =
          lazy
            (let taint =
               BackwardState.Tree.prepend [index] taint
               |> BackwardState.Tree.add_local_first_index index
             in
             analyze_expression ~pyre_in_context ~taint ~state ~expression:base)
        in
        let state =
          if List.is_empty callees.call_targets then
            (* This call may be unresolved, because for example the receiver type is unknown *)
            Lazy.force state_before_index_access
          else
            let index_number =
              match argument_expression with
              | Expression.Constant (Constant.Integer i) -> Some i
              | _ -> None
            in
            List.fold callees.call_targets ~init:bottom ~f:(fun state_so_far call_target ->
                analyze_getitem_call_target
                  ~index_number
                  ~pyre_in_context
                  ~location
                  ~base
                  ~taint
                  ~state
                  ~state_before_index_access
                  call_target
                |> join state_so_far)
        in
        analyze_expression
          ~pyre_in_context
          ~taint:BackwardState.Tree.bottom
          ~state
          ~expression:argument_value
    (* Special case `__iter__` and `__next__` as being a random index access (this pattern is the
       desugaring of `for element in x`). *)
    | {
     callee = { Node.value = Name (Name.Attribute { base; attribute = "__next__"; _ }); _ };
     arguments = [];
     origin = _;
    } ->
        let taint = add_type_breadcrumbs taint in
        analyze_expression ~pyre_in_context ~taint ~state ~expression:base
    | {
     callee =
       { Node.value = Name (Name.Attribute { base; attribute = "__iter__"; origin = Some _ }); _ };
     arguments = [];
     origin = _;
    } ->
        let label =
          (* For dictionaries, the default iterator is keys. *)
          if CallGraph.CallCallees.is_mapping_method callees then
            AccessPath.dictionary_keys
          else
            Abstract.TreeDomain.Label.AnyIndex
        in
        let taint = BackwardState.Tree.prepend [label] taint in
        analyze_expression ~pyre_in_context ~taint ~state ~expression:base
    (* We special-case object.__setattr__, which is sometimes used in order to work around
       dataclasses being frozen post-initialization. *)
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
                 Expression.Constant (Constant.String { value = attribute; kind = String });
               _;
             };
           name = None;
         };
         { Call.Argument.value; name = None };
       ];
     origin = _;
    } ->
        analyze_assignment
          ~pyre_in_context
          ~target:
            (Expression.Name
               (Name.Attribute
                  {
                    base = self;
                    attribute;
                    origin = Some { Node.location; value = Origin.SetAttrConstantLiteral };
                  })
            |> Node.create ~location)
          ~value
          state
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
     origin = _;
    } ->
        let attribute_expression =
          Expression.Name
            (Name.Attribute
               {
                 base;
                 attribute;
                 origin = Some { Node.location; value = Origin.GetAttrConstantLiteral };
               })
          |> Node.create ~location
        in
        let state =
          analyze_expression ~pyre_in_context ~state ~expression:attribute_expression ~taint
        in
        analyze_expression ~pyre_in_context ~state ~expression:default ~taint
    (* `zip(a, b, ...)` creates a taint object whose first index has a's taint, second index has b's
       taint, etc. *)
    | { callee = { Node.value = Name (Name.Identifier "zip"); _ }; arguments = lists; origin = _ }
      ->
        let taint = BackwardState.Tree.read [Abstract.TreeDomain.Label.AnyIndex] taint in
        let analyze_zipped_list index state { Call.Argument.value; _ } =
          let index_name = Abstract.TreeDomain.Label.Index (string_of_int index) in
          let taint =
            BackwardState.Tree.read [index_name] taint
            |> BackwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]
          in
          analyze_expression ~pyre_in_context ~state ~taint ~expression:value
        in
        List.foldi lists ~init:state ~f:analyze_zipped_list
    (* dictionary .keys(), .values() and .items() functions are special, as they require handling of
       DictionaryKeys taint. *)
    | {
     callee = { Node.value = Name (Name.Attribute { base; attribute = "values"; _ }); _ };
     arguments = [];
     origin = _;
    }
      when CallGraph.CallCallees.is_mapping_method callees ->
        let taint =
          taint
          |> BackwardState.Tree.read [Abstract.TreeDomain.Label.AnyIndex]
          |> BackwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]
        in
        analyze_expression ~pyre_in_context ~taint ~state ~expression:base
    | {
     callee = { Node.value = Name (Name.Attribute { base; attribute = "keys"; _ }); _ };
     arguments = [];
     origin = _;
    }
      when CallGraph.CallCallees.is_mapping_method callees ->
        let taint =
          taint
          |> BackwardState.Tree.read [Abstract.TreeDomain.Label.AnyIndex]
          |> BackwardState.Tree.prepend [AccessPath.dictionary_keys]
        in
        analyze_expression ~pyre_in_context ~taint ~state ~expression:base
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
        let access_path =
          Some { AccessPath.root = AccessPath.Root.Variable identifier; path = [] }
        in
        let dict_taint =
          let global_taint =
            GlobalModel.from_expression
              ~pyre_in_context
              ~call_graph:FunctionContext.call_graph_of_define
              ~get_callee_model:FunctionContext.get_callee_model
              ~expression:base
              ~interval:FunctionContext.caller_class_interval
            |> GlobalModel.get_sinks
            |> SinkTreeWithHandle.join
          in
          BackwardState.Tree.join global_taint (get_taint access_path state)
        in
        let override_taint_from_update (taint, state) (key, value) =
          let path = [Abstract.TreeDomain.Label.Index key] in
          let value_taint =
            BackwardState.Tree.read ~transform_non_leaves path dict_taint
            |> BackwardState.Tree.transform
                 Features.TitoPositionSet.Element
                 Add
                 ~f:value.Node.location
          in
          (* update backwards is overwriting the old key taint with bottom *)
          let taint =
            BackwardState.Tree.assign ~tree:taint path ~subtree:BackwardState.Tree.bottom
          in
          let state =
            analyze_expression ~pyre_in_context ~taint:value_taint ~state ~expression:value
          in
          taint, state
        in
        let taint, state =
          List.fold entries ~init:(dict_taint, state) ~f:override_taint_from_update
        in
        store_taint ~root:(AccessPath.Root.Variable identifier) ~path:[] taint state
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
        let access_path =
          Some { AccessPath.root = AccessPath.Root.Variable identifier; path = [] }
        in
        let old_taint = get_taint access_path state in
        let new_taint =
          BackwardState.Tree.assign
            ~tree:old_taint
            [Abstract.TreeDomain.Label.Index value]
            ~subtree:(add_type_breadcrumbs taint)
        in
        store_taint ~root:(AccessPath.Root.Variable identifier) ~path:[] new_taint state
    | {
     callee = { Node.value = Name (Name.Attribute { base; attribute = "items"; _ }); _ };
     arguments = [];
     origin = _;
    }
      when CallGraph.CallCallees.is_mapping_method callees ->
        (* When we're faced with an assign of the form `k, v = d.items().__iter__().__next__()`, the
           taint we analyze d.items() under will be {* -> {0 -> k, 1 -> v} }. We want to analyze d
           itself under the taint of `{* -> v, $keys -> k}`. *)
        let item_taint = BackwardState.Tree.read [Abstract.TreeDomain.Label.AnyIndex] taint in
        let key_taint =
          BackwardState.Tree.read [Abstract.TreeDomain.Label.create_int_index 0] item_taint
        in
        let value_taint =
          BackwardState.Tree.read [Abstract.TreeDomain.Label.create_int_index 1] item_taint
        in
        let taint =
          BackwardState.Tree.join
            (BackwardState.Tree.prepend [AccessPath.dictionary_keys] key_taint)
            (BackwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex] value_taint)
        in
        analyze_expression ~pyre_in_context ~taint ~state ~expression:base
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
        let taint = add_type_breadcrumbs taint in
        let state =
          match optional_arguments with
          | [{ Call.Argument.value = default_expression; _ }] ->
              let taint =
                BackwardState.Tree.transform
                  Features.TitoPositionSet.Element
                  Add
                  ~f:default_expression.Node.location
                  taint
              in
              analyze_expression ~pyre_in_context ~taint ~state ~expression:default_expression
          | [] -> state
          | _ -> failwith "unreachable"
        in
        let taint =
          taint
          |> BackwardState.Tree.prepend [index]
          |> BackwardState.Tree.add_local_first_index index
          |> BackwardState.Tree.transform Features.TitoPositionSet.Element Add ~f:base.Node.location
        in
        analyze_expression ~pyre_in_context ~taint ~state ~expression:base
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
          ~taint
          ~state
          ~expression:
            {
              Node.location;
              value =
                Expression.Tuple
                  (List.map arguments ~f:(fun argument -> argument.Call.Argument.value));
            }
    (* Special case `"{}".format(s)` for Literal String Sinks *)
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
        let arguments_formatted_string =
          List.map ~f:(fun call_argument -> call_argument.value) arguments
        in
        let call_target =
          CallModel.StringFormatCall.CallTarget.create
            ~call_targets:callees.call_targets
            ~default_target:(CallModel.StringFormatCall.CallTarget.from_function_name function_name)
        in
        analyze_joined_string
          ~pyre_in_context
          ~taint
          ~state
          ~breadcrumbs:(Features.BreadcrumbSet.singleton (Features.format_string ()))
          {
            CallModel.StringFormatCall.nested_expressions = arguments_formatted_string;
            string_literal = { value; location = value_location };
            call_target;
            location;
          }
    (* Special case `"str" + s` and `s + "str"` for Literal String Sinks *)
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
        let substrings =
          arguments
          |> List.map ~f:(fun argument -> argument.Call.Argument.value)
          |> List.cons base
          |> List.map ~f:globals_to_constants
        in
        let string_literal, substrings = CallModel.arguments_for_string_format substrings in
        let call_target =
          CallModel.StringFormatCall.CallTarget.create
            ~call_targets:callees.call_targets
            ~default_target:(CallModel.StringFormatCall.CallTarget.from_function_name function_name)
        in
        analyze_joined_string
          ~pyre_in_context
          ~taint
          ~state
          ~breadcrumbs
          {
            CallModel.StringFormatCall.nested_expressions = substrings;
            string_literal = { CallModel.StringFormatCall.value = string_literal; location };
            call_target;
            location;
          }
    | { Call.callee = { Node.value = Name (Name.Identifier "super"); _ }; arguments; origin = _ }
      -> (
        match arguments with
        | [_; Call.Argument.{ value = object_; _ }] ->
            analyze_expression ~pyre_in_context ~taint ~state ~expression:object_
        | _ -> (
            (* Use implicit self *)
            match self_variable with
            | Some root -> store_taint ~weak:true ~root ~path:[] taint state
            | None -> state))
    | {
     Call.callee = { Node.value = Name (Name.Identifier "reveal_taint"); _ };
     arguments = [{ Call.Argument.value = expression; _ }];
     origin = _;
    } ->
        begin
          match AccessPath.of_expression ~self_variable expression with
          | None ->
              Log.dump
                "%a: Revealed backward taint for `%s`: expression is too complex"
                Location.WithModule.pp
                (Location.with_module location ~module_reference:FunctionContext.qualifier)
                (Transform.sanitize_expression expression |> Expression.show)
          | access_path ->
              let taint = get_taint access_path state in
              Log.dump
                "%a: Revealed backward taint for `%s`: %s"
                Location.WithModule.pp
                (Location.with_module location ~module_reference:FunctionContext.qualifier)
                (Transform.sanitize_expression expression |> Expression.show)
                (BackwardState.Tree.show taint)
        end;
        state
    | _ ->
        apply_callees
          ~pyre_in_context
          ~is_property:false
          ~call_location:location
          ~state
          ~callee
          ~arguments
          ~call_taint:taint
          callees


  and analyze_binary_operator
      ~pyre_in_context
      ~location
      ~taint
      ~state
      ({ BinaryOperator.left; _ } as operator)
    =
    let implicit_call =
      BinaryOperator.lower_to_call ~location ~callee_location:left.Node.location operator
    in
    let dunder_method = BinaryOperator.binary_operator_method operator.operator in
    let callees = get_call_callees ~location ~call:implicit_call in
    match operator with
    (* Special case `"%s" % (s,)` for Literal String Sinks *)
    | {
     BinaryOperator.left =
       {
         Node.value = Constant (Constant.String { StringLiteral.value; _ });
         location = value_location;
       };
     right;
     operator = BinaryOperator.Mod;
    } ->
        let call_target =
          CallModel.StringFormatCall.CallTarget.create
            ~call_targets:callees.call_targets
            ~default_target:(CallModel.StringFormatCall.CallTarget.from_function_name dunder_method)
        in
        analyze_joined_string
          ~pyre_in_context
          ~taint
          ~state
          ~breadcrumbs:(Features.BreadcrumbSet.singleton (Features.format_string ()))
          {
            CallModel.StringFormatCall.nested_expressions = [right];
            string_literal = { value; location = value_location };
            call_target;
            location;
          }
    (* Special case `"str" + s` and `s + "str"` for Literal String Sinks *)
    | {
     BinaryOperator.left;
     right =
       {
         Node.value = Constant (Constant.String { StringLiteral.value; _ });
         location = value_location;
       };
     operator = BinaryOperator.Add;
    } ->
        let call_target =
          CallModel.StringFormatCall.CallTarget.create
            ~call_targets:callees.call_targets
            ~default_target:
              (CallGraph.CallTarget.create Interprocedural.Target.ArtificialTargets.str_add)
        in
        analyze_joined_string
          ~pyre_in_context
          ~taint
          ~state
          ~breadcrumbs:(Features.BreadcrumbSet.singleton (Features.string_concat_left_hand_side ()))
          {
            CallModel.StringFormatCall.nested_expressions = [left];
            string_literal = { value; location = value_location };
            call_target;
            location;
          }
    | {
     BinaryOperator.left =
       {
         Node.value = Constant (Constant.String { StringLiteral.value; _ });
         location = value_location;
       };
     right;
     operator = BinaryOperator.Add;
    } ->
        let call_target =
          CallModel.StringFormatCall.CallTarget.create
            ~call_targets:callees.call_targets
            ~default_target:
              (CallGraph.CallTarget.create Interprocedural.Target.ArtificialTargets.str_add)
        in
        analyze_joined_string
          ~pyre_in_context
          ~taint
          ~state
          ~breadcrumbs:
            (Features.BreadcrumbSet.singleton (Features.string_concat_right_hand_side ()))
          {
            CallModel.StringFormatCall.nested_expressions = [right];
            string_literal = { value; location = value_location };
            call_target;
            location;
          }
    | {
     BinaryOperator.left;
     right;
     operator = (BinaryOperator.Add | BinaryOperator.Mod) as operator;
    }
      when CallGraph.CallCallees.is_string_method callees ->
        let breadcrumbs =
          match operator with
          | BinaryOperator.Mod -> Features.BreadcrumbSet.singleton (Features.format_string ())
          | _ -> Features.BreadcrumbSet.empty
        in
        let substrings = List.map ~f:globals_to_constants [left; right] in
        let string_literal, substrings = CallModel.arguments_for_string_format substrings in
        let call_target =
          CallModel.StringFormatCall.CallTarget.create
            ~call_targets:callees.call_targets
            ~default_target:(CallModel.StringFormatCall.CallTarget.from_function_name dunder_method)
        in
        analyze_joined_string
          ~pyre_in_context
          ~taint
          ~state
          ~breadcrumbs
          {
            CallModel.StringFormatCall.nested_expressions = substrings;
            string_literal = { CallModel.StringFormatCall.value = string_literal; location };
            call_target;
            location;
          }
    | { BinaryOperator.left; right; _ } ->
        let { arguments_taint; implicit_argument_taint; state; _ } =
          apply_callees_and_return_arguments_taint
            ~apply_tito:true
            ~pyre_in_context
            ~callee:implicit_call.callee
            ~call_location:location
            ~arguments:implicit_call.arguments
            ~state
            ~call_taint:taint
            callees
        in
        let state =
          match arguments_taint with
          | right_taint :: _ ->
              analyze_expression ~pyre_in_context ~taint:right_taint ~state ~expression:right
          | _ -> failwith "unexpected"
        in
        let left_taint =
          match implicit_argument_taint with
          | CallModel.ImplicitArgument.Backward.Callee taint
          | CalleeBase taint ->
              taint
          | None -> BackwardState.Tree.bottom
        in
        analyze_expression ~pyre_in_context ~taint:left_taint ~state ~expression:left


  and analyze_joined_string
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~taint
      ~state
      ~breadcrumbs
      { CallModel.StringFormatCall.nested_expressions; string_literal; call_target; location }
    =
    let taint =
      BackwardState.Tree.transform_tito (* Treat as a call site for tito taint. *)
        Domains.TraceLength.Self
        Map
        ~f:TraceLength.increase
        taint
    in
    let call_site = CallSite.create location in
    (* This is the backward model of the callee -- each actual argument has this taint tree. *)
    let string_combine_partial_sink_tree =
      CallModel.StringFormatCall.apply_call
        ~callee:call_target.CallGraph.CallTarget.target
        ~pyre_in_context
        ~call_site
        ~location
        FunctionContext.string_combine_partial_sink_tree
    in
    let taint =
      string_literal
      |> CallModel.StringFormatCall.implicit_string_literal_sinks
           ~pyre_in_context
           ~implicit_sinks:FunctionContext.taint_configuration.implicit_sinks
      |> BackwardState.Tree.create_leaf
      |> BackwardState.Tree.join taint
      |> BackwardState.Tree.join string_combine_partial_sink_tree
      |> BackwardState.Tree.collapse ~breadcrumbs:(Features.tito_broadening_set ())
      |> BackwardTaint.add_local_breadcrumbs breadcrumbs
      |> BackwardState.Tree.create_leaf
    in
    let analyze_stringify_callee
        ~taint_to_join
        ~state_to_join
        ~call_target
        ~call_location
        ~base
        ~base_state
        taint
      =
      let {
        arguments_taint = _;
        implicit_argument_taint;
        captures_taint = _;
        captures = _;
        state = new_state;
      }
        =
        let callees = CallGraph.CallCallees.create ~call_targets:[call_target] () in
        let callee =
          let callee_from_method_name method_name =
            {
              Node.value =
                Expression.Name
                  (Name.Attribute
                     {
                       base;
                       attribute = method_name;
                       origin =
                         Some
                           { Node.location = call_location; value = Origin.FormatStringImplicitStr };
                     });
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
        apply_callees_and_return_arguments_taint
          ~pyre_in_context
          ~callee
          ~call_location
          ~arguments:[]
          ~state:base_state
          ~call_taint:taint
          callees
      in
      let new_taint =
        match implicit_argument_taint with
        | CallModel.ImplicitArgument.Backward.CalleeBase self_taint ->
            BackwardState.Tree.join taint_to_join self_taint
        | None -> taint_to_join
        | _ -> failwith "Expect `CalleeBase` or `None` for stringify callee"
      in
      new_taint, join state_to_join new_state
    in
    let analyze_nested_expression state ({ Node.location = expression_location; _ } as expression) =
      let taint =
        Issue.TriggeredSinkForBackward.convert_partial_sinks_into_triggered
          ~call_site
          ~argument_location:expression_location
          ~argument_sink:taint
          FunctionContext.triggered_sinks
      in
      let new_taint, new_state =
        match
          CallModel.StringFormatCall.get_string_format_callees
            ~call_graph_of_define:FunctionContext.call_graph_of_define
            ~location:expression_location
        with
        | Some { CallGraph.StringFormatCallees.stringify_targets = _ :: _ as stringify_targets; _ }
          ->
            List.fold
              stringify_targets
              ~init:(taint, state)
              ~f:(fun (taint_to_join, state_to_join) call_target ->
                analyze_stringify_callee
                  ~taint_to_join
                  ~state_to_join
                  ~call_target
                  ~call_location:expression_location
                  ~base:expression
                  ~base_state:state
                  taint)
        | _ -> taint, state
      in
      let new_taint =
        new_taint
        |> BackwardState.Tree.transform Features.TitoPositionSet.Element Add ~f:expression_location
        |> BackwardState.Tree.add_local_breadcrumb (Features.tito ())
      in
      analyze_expression ~pyre_in_context ~taint:new_taint ~state:new_state ~expression
    in
    List.fold (List.rev nested_expressions) ~f:analyze_nested_expression ~init:state


  and analyze_expression
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ~taint
      ~state
      ~expression:({ Node.value; location } as expression)
    =
    log
      "Backward analysis of expression: `%a` with backward taint: %a"
      Expression.pp_expression
      value
      BackwardState.Tree.pp
      taint;
    let analyze_expression_inner () =
      let value =
        CallGraph.redirect_expressions
          ~pyre_in_context
          ~callables_to_definitions_map:FunctionContext.callables_to_definitions_map
          ~location
          value
      in
      match value with
      | Await expression -> analyze_expression ~pyre_in_context ~taint ~state ~expression
      | BinaryOperator operator ->
          analyze_binary_operator ~pyre_in_context ~location ~taint ~state operator
      | BooleanOperator { left; operator = _; right } ->
          analyze_expression ~pyre_in_context ~taint ~state ~expression:right
          |> fun state -> analyze_expression ~pyre_in_context ~taint ~state ~expression:left
      | ComparisonOperator ({ left; operator = _; right } as comparison) -> (
          match
            ComparisonOperator.lower_to_expression
              ~location
              ~callee_location:left.Node.location
              comparison
          with
          | Some override -> analyze_expression ~pyre_in_context ~taint ~state ~expression:override
          | None ->
              let taint =
                BackwardState.Tree.add_local_breadcrumbs (Features.type_bool_scalar_set ()) taint
              in
              analyze_expression ~pyre_in_context ~taint ~state ~expression:right
              |> fun state -> analyze_expression ~pyre_in_context ~taint ~state ~expression:left)
      | Call { callee; arguments; origin } ->
          analyze_call ~pyre_in_context ~location ~taint ~state ~callee ~arguments ~origin
      | Constant _ -> state
      | Dictionary entries ->
          List.fold ~f:(analyze_dictionary_entry ~pyre_in_context taint) entries ~init:state
      | DictionaryComprehension { Comprehension.element = { key; value }; generators; _ } ->
          let pyre_in_context =
            PyrePysaEnvironment.InContext.resolve_generators pyre_in_context generators
          in
          let state =
            analyze_expression
              ~pyre_in_context
              ~taint:(read_tree [AccessPath.dictionary_keys] taint)
              ~state
              ~expression:key
          in
          let state =
            analyze_expression
              ~pyre_in_context
              ~taint:(read_tree [Abstract.TreeDomain.Label.AnyIndex] taint)
              ~state
              ~expression:value
          in
          analyze_generators ~pyre_in_context ~state generators
      | Generator comprehension -> analyze_comprehension ~pyre_in_context taint comprehension state
      | Lambda { parameters = _; body } ->
          (* Ignore parameter bindings and pretend body is inlined *)
          analyze_expression ~pyre_in_context ~taint ~state ~expression:body
      | List list ->
          let total = List.length list in
          List.rev list
          |> List.foldi ~f:(analyze_reverse_list_element ~total ~pyre_in_context taint) ~init:state
      | ListComprehension comprehension ->
          analyze_comprehension ~pyre_in_context taint comprehension state
      | Name (Name.Identifier identifier) ->
          let taint =
            BackwardState.Tree.add_local_type_breadcrumbs
              ~pyre_in_context
              ~expression:{ Node.value; location }
              taint
          in
          store_taint ~weak:true ~root:(AccessPath.Root.Variable identifier) ~path:[] taint state
      | Name (Name.Attribute { base; attribute = "__dict__"; _ }) ->
          analyze_expression ~pyre_in_context ~taint ~state ~expression:base
      | Name (Name.Attribute { base; attribute; origin }) ->
          analyze_attribute_access
            ~pyre_in_context
            ~location
            ~resolve_properties:true
            ~base
            ~attribute
            ~origin
            ~base_taint:BackwardState.Tree.bottom
            ~attribute_taint:taint
            ~state
      | Set set ->
          let element_taint = read_tree [Abstract.TreeDomain.Label.AnyIndex] taint in
          List.fold
            set
            ~f:(fun state expression ->
              analyze_expression ~pyre_in_context ~taint:element_taint ~state ~expression)
            ~init:state
      | SetComprehension comprehension ->
          analyze_comprehension ~pyre_in_context taint comprehension state
      | Starred (Starred.Once expression)
      | Starred (Starred.Twice expression) ->
          let taint = BackwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex] taint in
          analyze_expression ~pyre_in_context ~taint ~state ~expression
      | Slice _ ->
          (* This case should be unreachable, fail if we hit it *)
          failwith "Slice nodes should always be rewritten by `CallGraph.redirect_expressions`"
      | Subscript _ ->
          (* This case should be unreachable, fail if we hit it *)
          failwith "Subscripts nodes should always be rewritten by `CallGraph.redirect_expressions`"
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
          let string_literal, substrings = CallModel.arguments_for_string_format substrings in
          let call_target =
            CallModel.StringFormatCall.CallTarget.from_format_string
              ~call_graph_of_define:FunctionContext.call_graph_of_define
              ~location
          in
          analyze_joined_string
            ~pyre_in_context
            ~taint
            ~state
            ~breadcrumbs:(Features.BreadcrumbSet.singleton (Features.format_string ()))
            {
              CallModel.StringFormatCall.nested_expressions = substrings;
              string_literal = { value = string_literal; location };
              call_target;
              location;
            }
      | Ternary { target; test; alternative } ->
          let state_then = analyze_expression ~pyre_in_context ~taint ~state ~expression:target in
          let state_else =
            analyze_expression ~pyre_in_context ~taint ~state ~expression:alternative
          in
          join state_then state_else
          |> fun state ->
          analyze_expression
            ~pyre_in_context
            ~taint:BackwardState.Tree.empty
            ~state
            ~expression:test
      | Tuple list ->
          let total = List.length list in
          List.rev list
          |> List.foldi ~f:(analyze_reverse_list_element ~total ~pyre_in_context taint) ~init:state
      | UnaryOperator { operator = _; operand } ->
          analyze_expression ~pyre_in_context ~taint ~state ~expression:operand
      | WalrusOperator { target; value } ->
          let state = analyze_assignment ~pyre_in_context ~target ~value state in
          analyze_expression ~pyre_in_context ~taint ~state ~expression:value
      | Yield None -> state
      | Yield (Some expression)
      | YieldFrom expression ->
          let access_path = { AccessPath.root = AccessPath.Root.LocalResult; path = [] } in
          let return_taint = get_taint (Some access_path) state in
          analyze_expression ~pyre_in_context ~taint:return_taint ~state ~expression
    in
    TaintProfiler.track_expression_analysis
      ~profiler
      ~analysis:Backward
      ~expression
      ~f:analyze_expression_inner


  (* Returns the taint, and whether to collapse one level (due to star expression) *)
  and compute_assignment_taint ~(pyre_in_context : PyrePysaEnvironment.InContext.t) target state =
    match target.Node.value with
    | Expression.Starred (Once target | Twice target) ->
        (* This is approximate. Unless we can get the tuple type on the right to tell how many total
           elements there will be, we just pick up the entire collection. *)
        let taint, _ = compute_assignment_taint ~pyre_in_context target state in
        taint, true
    | List targets
    | Tuple targets ->
        let compute_tuple_target_taint position taint_accumulator target =
          let taint, collapse = compute_assignment_taint ~pyre_in_context target state in
          let index_taint =
            if collapse then
              taint
            else
              let index_name = Abstract.TreeDomain.Label.Index (string_of_int position) in
              BackwardState.Tree.prepend [index_name] taint
          in
          BackwardState.Tree.join index_taint taint_accumulator
        in
        let taint =
          List.foldi targets ~f:compute_tuple_target_taint ~init:BackwardState.Tree.empty
        in
        taint, false
    | Call
        {
          callee = { Node.value = Name (Name.Attribute { base; attribute = "__getitem__"; _ }); _ };
          arguments = [{ Call.Argument.value = index; _ }];
          origin = _;
        } ->
        let taint =
          compute_assignment_taint ~pyre_in_context base state
          |> fst
          |> BackwardState.Tree.read [AccessPath.get_index index]
        in
        taint, false
    | _ ->
        let taint =
          let local_taint =
            let access_path = AccessPath.of_expression ~self_variable target in
            get_taint access_path state
          in
          let global_taint =
            GlobalModel.from_expression
              ~pyre_in_context
              ~call_graph:FunctionContext.call_graph_of_define
              ~get_callee_model:FunctionContext.get_callee_model
              ~expression:target
              ~interval:FunctionContext.caller_class_interval
            |> GlobalModel.get_sinks
            |> SinkTreeWithHandle.join
          in
          BackwardState.Tree.join local_taint global_taint
        in
        taint, false


  and clear_target_taint ~fields state target =
    match Node.value target with
    | Expression.Tuple items -> List.fold items ~f:(clear_target_taint ~fields) ~init:state
    | _ -> (
        match AccessPath.of_expression ~self_variable target with
        | Some { root; path } ->
            {
              taint =
                BackwardState.assign
                  ~root
                  ~path:(path @ fields)
                  BackwardState.Tree.empty
                  state.taint;
            }
        | None -> state)


  and analyze_assignment
      ?(weak = false)
      ~(pyre_in_context : PyrePysaEnvironment.InContext.t)
      ?(fields = [])
      ~target
      ~value
      state
    =
    let taint =
      compute_assignment_taint ~pyre_in_context target state
      |> fst
      |> read_tree fields
      |> BackwardState.Tree.add_local_type_breadcrumbs ~pyre_in_context ~expression:target
    in
    let state =
      if weak then (* Weak updates do not remove the taint. *)
        state
      else
        clear_target_taint ~fields state target
    in
    analyze_expression ~pyre_in_context ~taint ~state ~expression:value


  let analyze_statement ~pyre_in_context state ({ Node.location; _ } as statement) =
    let statement = CallGraph.redirect_assignments statement in
    match Node.value statement with
    | Statement.Statement.Assign
        { value = Some { Node.value = Expression.Constant Constant.Ellipsis; _ }; _ } ->
        state
    | Assign { value = None; _ } -> state
    | Assign { target = { Node.location; value = target_value } as target; value = Some value; _ }
      -> (
        let target_global_model =
          GlobalModel.from_expression
            ~pyre_in_context
            ~call_graph:FunctionContext.call_graph_of_define
            ~get_callee_model:FunctionContext.get_callee_model
            ~expression:target
            ~interval:FunctionContext.caller_class_interval
        in
        if GlobalModel.is_sanitized target_global_model then
          analyze_expression
            ~pyre_in_context
            ~taint:BackwardState.Tree.bottom
            ~state
            ~expression:value
        else
          match target_value with
          | Expression.Name (Name.Attribute { attribute; _ }) ->
              let attribute_access_callees = get_attribute_access_callees ~location ~attribute in

              let property_call_state =
                match attribute_access_callees with
                | Some { property_targets = _ :: _ as property_targets; _ } ->
                    (* `a.property = x` *)
                    apply_callees
                      ~pyre_in_context
                      ~is_property:true
                      ~callee:target
                      ~call_location:location
                      ~arguments:[{ name = None; value }]
                      ~state
                      ~call_taint:BackwardState.Tree.empty
                      (CallGraph.CallCallees.create ~call_targets:property_targets ())
                | _ -> bottom
              in

              let attribute_state =
                match attribute_access_callees with
                | Some { is_attribute = true; _ }
                | None ->
                    analyze_assignment ~pyre_in_context ~target ~value state
                | _ -> bottom
              in

              join property_call_state attribute_state
          | _ -> analyze_assignment ~pyre_in_context ~target ~value state)
    | AugmentedAssign ({ target; value; _ } as assign) ->
        let target_taint = compute_assignment_taint ~pyre_in_context target state |> fst in
        let state = clear_target_taint ~fields:[] state target in

        let implicit_call =
          Statement.AugmentedAssign.lower_to_call
            ~location
            ~callee_location:target.Node.location
            assign
        in
        let callees = get_call_callees ~location ~call:implicit_call in
        let { arguments_taint; implicit_argument_taint; state; _ } =
          apply_callees_and_return_arguments_taint
            ~apply_tito:true
            ~pyre_in_context
            ~callee:implicit_call.callee
            ~call_location:location
            ~arguments:implicit_call.arguments
            ~state
            ~call_taint:target_taint
            callees
        in
        let value_taint =
          match arguments_taint with
          | value_taint :: _ -> value_taint
          | [] -> failwith "unexpected"
        in
        let state =
          analyze_expression ~pyre_in_context ~taint:value_taint ~state ~expression:value
        in
        let target_taint =
          match implicit_argument_taint with
          | CallModel.ImplicitArgument.Backward.Callee taint
          | CalleeBase taint ->
              taint
          | None -> BackwardState.Tree.bottom
        in
        analyze_expression ~pyre_in_context ~taint:target_taint ~state ~expression:target
    | Assert { test; _ } ->
        analyze_expression ~pyre_in_context ~taint:BackwardState.Tree.empty ~state ~expression:test
    | Break
    | Class _
    | Continue ->
        state
    | Define define -> analyze_definition ~define state
    | Delete expressions ->
        let process_expression state expression =
          match AccessPath.of_expression ~self_variable expression with
          | Some { AccessPath.root; path } ->
              { taint = BackwardState.assign ~root ~path BackwardState.Tree.bottom state.taint }
          | _ -> state
        in
        List.fold expressions ~init:state ~f:process_expression
    | Expression expression ->
        analyze_expression ~pyre_in_context ~taint:BackwardState.Tree.empty ~state ~expression
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
        analyze_expression ~pyre_in_context ~taint:BackwardState.Tree.empty ~state ~expression
    | Return { expression = Some expression; _ } ->
        let access_path = { AccessPath.root = AccessPath.Root.LocalResult; path = [] } in
        let return_taint = get_taint (Some access_path) state in
        let return_sink =
          CallModel.return_sink
            ~pyre_in_context
            ~location
            ~callee:FunctionContext.callable
            ~sink_model:FunctionContext.existing_model.Model.backward.sink_taint
          |> BackwardState.Tree.add_local_breadcrumb (Features.propagated_return_sink ())
        in
        analyze_expression
          ~pyre_in_context
          ~taint:(BackwardState.Tree.join return_taint return_sink)
          ~state
          ~expression
    | Return { expression = None; _ }
    | Try _
    | TypeAlias _ (* TODO(T196994965): handle Type Alias *)
    | With _
    | While _ ->
        state


  let backward ~statement_key state ~statement =
    TaintProfiler.track_statement_analysis ~profiler ~analysis:Backward ~statement ~f:(fun () ->
        log
          "Backward analysis of statement: `%a`@,With backward state: %a"
          Statement.pp
          statement
          pp
          state;
        let pyre_in_context =
          PyrePysaEnvironment.InContext.create_at_statement_key
            pyre_api
            ~define_name:FunctionContext.define_name
            ~define:(Ast.Node.value FunctionContext.definition)
            ~statement_key
        in
        analyze_statement ~pyre_in_context state statement)


  let forward ~statement_key:_ _ ~statement:_ = failwith "Don't call me"
end

module SinkPartition = struct
  type t = (Sinks.t, BackwardState.Tree.t) Map.Poly.t

  let read_and_partition_sinks ~root ~extract_sink sink_taint : t =
    sink_taint
    |> BackwardState.read ~root ~path:[]
    |> BackwardState.Tree.partition BackwardTaint.kind By ~f:extract_sink


  let merge_partitions : t -> BackwardState.Tree.t =
    Map.Poly.fold ~init:BackwardState.Tree.empty ~f:(fun ~key:_ ~data:taint sofar ->
        BackwardState.Tree.join taint sofar)
end

let get_normalized_parameters { Statement.Define.signature = { parameters; _ }; captures; _ } =
  let normalized_parameters =
    parameters
    |> AccessPath.normalize_parameters
    |> List.map ~f:(fun { AccessPath.NormalizedParameter.root; qualified_name; original } ->
           root, qualified_name, original.Node.value.Parameter.annotation)
  in
  let captures =
    List.map captures ~f:(fun capture ->
        AccessPath.Root.CapturedVariable { name = capture.name }, capture.name, None)
  in
  List.append normalized_parameters captures


(* Split the inferred entry state into externally visible taint_in_taint_out parts and
   sink_taint. *)
let extract_tito_and_sink_models
    ~normalized_parameters
    ~pyre_api
    ~is_constructor
    ~taint_configuration:
      {
        TaintConfiguration.Heap.analysis_model_constraints =
          {
            maximum_model_sink_tree_width;
            maximum_model_tito_tree_width;
            maximum_trace_length;
            maximum_tito_depth;
            maximum_tito_collapse_depth;
            _;
          };
        source_sink_filter;
        _;
      }
    ~existing_backward
    ~apply_broadening
    entry_taint
  =
  (* Simplify trees by keeping only essential structure and merging details back into that. *)
  let simplify ~shape_breadcrumbs ~limit_breadcrumbs ~maximum_tree_width tree =
    if apply_broadening then
      tree
      |> BackwardState.Tree.shape
           ~mold_with_return_access_paths:is_constructor
           ~breadcrumbs:shape_breadcrumbs
      |> BackwardState.Tree.limit_to ~breadcrumbs:limit_breadcrumbs ~width:maximum_tree_width
      |> BackwardState.Tree.transform_tito
           Features.ReturnAccessPathTree.Self
           Map
           ~f:Features.ReturnAccessPathTree.limit_width
    else
      tree
  in
  let add_type_breadcrumbs annotation tree =
    let type_breadcrumbs =
      annotation
      >>| PyrePysaEnvironment.ReadOnly.parse_annotation pyre_api
      |> Features.type_breadcrumbs_from_annotation ~pyre_api
    in
    BackwardState.Tree.add_local_breadcrumbs type_breadcrumbs tree
  in
  let split_and_simplify model (parameter, qualified_name, annotation) =
    let partition =
      SinkPartition.read_and_partition_sinks
        ~root:(AccessPath.Root.Variable qualified_name)
        ~extract_sink:Sinks.discard_transforms
        entry_taint
    in
    let taint_in_taint_out =
      let breadcrumbs_to_attach, via_features_to_attach =
        BackwardState.extract_features_to_attach
          ~root:parameter
          ~attach_to_kind:Sinks.Attach
          existing_backward.Model.Backward.taint_in_taint_out
      in
      let discard_trivial_tito partition =
        (* Remove trivial taint-in-taint-out Argument(x) -> Argument(x) *)
        Map.Poly.change partition (Sinks.ParameterUpdate parameter) ~f:(function
            | Some tito_taint ->
                (* Drop sanitizers. Since tito between arguments is implemented as a weak update, we
                   will always assume Argument(x) -> Argument(x) is a valid flow. Adding sanitizers
                   would create extra flows that are not interesting. *)
                let tito_taint =
                  BackwardState.Tree.transform
                    BackwardTaint.kind
                    Map
                    ~f:Sinks.discard_sanitize_transforms
                    tito_taint
                in
                let tito_for_comparison =
                  BackwardState.Tree.essential ~preserve_return_access_paths:true tito_taint
                in
                let trivial_tito =
                  Domains.local_return_frame
                    ~output_path:[]
                    ~collapse_depth:maximum_tito_collapse_depth
                  |> BackwardTaint.singleton (CallInfo.tito ()) (Sinks.ParameterUpdate parameter)
                  |> BackwardState.Tree.create_leaf
                  |> BackwardState.Tree.essential ~preserve_return_access_paths:true
                in
                let is_trivial_tito =
                  BackwardState.Tree.less_or_equal ~left:tito_for_comparison ~right:trivial_tito
                in
                Option.some_if (not is_trivial_tito) tito_taint
            | None -> None)
      in
      let candidate_tree =
        partition
        |> Map.Poly.filteri ~f:(fun ~key:kind ~data:_ ->
               match kind with
               | Sinks.LocalReturn
               | Sinks.ParameterUpdate _ ->
                   true
               | _ -> false)
        |> discard_trivial_tito
        |> SinkPartition.merge_partitions
        |> simplify
             ~shape_breadcrumbs:(Features.model_tito_shaping_set ())
             ~limit_breadcrumbs:(Features.model_tito_broadening_set ())
             ~maximum_tree_width:maximum_model_tito_tree_width
        |> add_type_breadcrumbs annotation
      in
      let candidate_tree =
        match maximum_tito_depth with
        | Some maximum_tito_depth ->
            BackwardState.Tree.prune_maximum_length
              ~global_maximum:(Some maximum_tito_depth)
              ~maximum_per_kind:(fun _ -> None)
              candidate_tree
        | _ -> candidate_tree
      in
      candidate_tree
      |> BackwardState.Tree.add_local_breadcrumbs breadcrumbs_to_attach
      |> BackwardState.Tree.add_via_features via_features_to_attach
    in
    let sink_taint =
      let simplify_sink_taint ~key:sink ~data:sink_tree accumulator =
        match sink with
        | Sinks.LocalReturn
        | Sinks.ParameterUpdate _
        | Sinks.Attach ->
            accumulator
        | _ ->
            let sink_tree =
              (* We limit by maximum_trace_length - 1, since the distance will be incremented by one
                 when the taint is propagated. *)
              let decrement x = x - 1 in
              BackwardState.Tree.prune_maximum_length
                ~global_maximum:(maximum_trace_length >>| decrement)
                ~maximum_per_kind:(fun sink ->
                  sink |> SourceSinkFilter.maximum_sink_distance source_sink_filter >>| decrement)
                sink_tree
            in
            let sink_tree =
              sink_tree
              |> simplify
                   ~shape_breadcrumbs:(Features.model_sink_shaping_set ())
                   ~limit_breadcrumbs:(Features.model_sink_broadening_set ())
                   ~maximum_tree_width:maximum_model_sink_tree_width
              |> add_type_breadcrumbs annotation
            in
            let sink_tree =
              match Sinks.discard_transforms sink with
              | Sinks.ExtraTraceSink ->
                  CallModel.ExtraTraceForTransforms.prune ~sink_tree ~tito_tree:taint_in_taint_out
              | _ -> sink_tree
            in
            BackwardState.Tree.join accumulator sink_tree
      in
      Map.Poly.fold ~init:BackwardState.Tree.empty ~f:simplify_sink_taint partition
    in
    let sink_taint =
      let breadcrumbs_to_attach, via_features_to_attach =
        BackwardState.extract_features_to_attach
          ~root:parameter
          ~attach_to_kind:Sinks.Attach
          existing_backward.Model.Backward.sink_taint
      in
      sink_taint
      |> BackwardState.Tree.add_local_breadcrumbs breadcrumbs_to_attach
      |> BackwardState.Tree.add_via_features via_features_to_attach
    in
    Model.Backward.
      {
        taint_in_taint_out =
          BackwardState.assign ~root:parameter ~path:[] taint_in_taint_out model.taint_in_taint_out;
        sink_taint = BackwardState.assign ~root:parameter ~path:[] sink_taint model.sink_taint;
      }
  in
  List.fold normalized_parameters ~f:split_and_simplify ~init:Model.Backward.empty


let remove_unmatched_partial_sinks
    ~normalized_parameters
    ({ Model.Backward.sink_taint; _ } as model)
  =
  let collect_call_info = BackwardState.Tree.fold BackwardTaint.call_info ~init:[] ~f:List.cons in
  (* A partial sink tree is matched if for one of its partial sink, there exists another parameter
     who has a partial sink tree where one of its partial sink comes from the same call site. *)
  let has_matching_partial_sink ~other_partial_sinks partial_sink_tree =
    let call_infos = collect_call_info partial_sink_tree in
    List.exists other_partial_sinks ~f:(fun other_partial_sink_tree ->
        other_partial_sink_tree
        |> collect_call_info
        |> List.exists ~f:(fun other_call_info ->
               List.exists call_infos ~f:(CallInfo.at_same_call_site other_call_info)))
  in
  let remove_unmatched_partial_sinks ~index ~parameter_with_sink_partitions =
    List.filter ~f:(fun partial_sink_tree ->
        List.existsi
          parameter_with_sink_partitions
          ~f:(fun other_index (_, other_partial_sinks, _) ->
            if Int.equal index other_index then
              (* A matched partial sink must be from a different parameter. *)
              false
            else
              has_matching_partial_sink ~other_partial_sinks partial_sink_tree))
  in
  let parameter_with_sink_partitions =
    List.map normalized_parameters ~f:(fun (parameter, _, _) ->
        let partial_sinks, other_sinks =
          sink_taint
          |> SinkPartition.read_and_partition_sinks ~root:parameter ~extract_sink:Fn.id
          |> Map.Poly.partitioni_tf ~f:(fun ~key ~data:_ ->
                 match key with
                 | Sinks.PartialSink _ -> true
                 | _ -> false)
        in
        parameter, Map.Poly.data partial_sinks, Map.Poly.data other_sinks)
  in
  let merge_trees =
    Algorithms.fold_balanced ~init:BackwardState.Tree.empty ~f:BackwardState.Tree.join
  in
  let sink_taint =
    List.foldi
      parameter_with_sink_partitions
      ~init:BackwardState.empty
      ~f:(fun index so_far (parameter, partial_sinks, other_sinks) ->
        let sink_tree =
          partial_sinks
          |> remove_unmatched_partial_sinks ~index ~parameter_with_sink_partitions
          |> merge_trees
          |> BackwardState.Tree.join (merge_trees other_sinks)
        in
        BackwardState.assign ~root:parameter ~path:[] sink_tree so_far)
  in
  { model with sink_taint }


let run
    ?(profiler = TaintProfiler.disabled)
    ~taint_configuration
    ~string_combine_partial_sink_tree
    ~pyre_api
    ~callables_to_definitions_map
    ~class_interval_graph
    ~global_constants
    ~qualifier
    ~callable
    ~define
    ~cfg
    ~call_graph_of_define
    ~get_callee_model
    ~existing_model
    ~triggered_sinks
    ~decorator_inlined
    ()
  =
  let timer = Timer.start () in
  (* Apply decorators to make sure we match parameters up correctly. Decorators are not applied in
     the forward analysis, because in case a decorator changes the parameters of the decorated
     function, the user-defined models of the function may no longer be applicable to the resultant
     function of the application (e.g., T132302522). *)
  let define =
    if decorator_inlined then
      PyrePysaEnvironment.ReadOnly.decorated_define pyre_api define
    else
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

    let triggered_sinks = triggered_sinks

    let caller_class_interval =
      Interprocedural.ClassIntervalSetGraph.SharedMemory.of_definition
        class_interval_graph
        define_name
        (Node.value definition)


    let string_combine_partial_sink_tree = string_combine_partial_sink_tree
  end
  in
  let module State = State (FunctionContext) in
  let module Fixpoint = PyrePysaLogic.Fixpoint.Make (State) in
  let initial = State.{ taint = initial_taint } in
  let () =
    State.log "Backward analysis of callable: `%a`" Interprocedural.Target.pp_pretty callable
  in
  let entry_state =
    (* TODO(T156333229): hide side effect work behind feature flag *)
    match define.value.signature.parameters, define.value.captures with
    | [], [] ->
        (* Without parameters or captures, the inferred model will always be empty. *)
        let () =
          State.log "Skipping backward analysis since the callable has no parameters or captures"
        in
        None
    | _ ->
        TaintProfiler.track_duration ~profiler ~name:"Backward analysis - fixpoint" ~f:(fun () ->
            Alarm.with_alarm
              ~max_time_in_seconds:60
              ~event_name:"backward taint analysis"
              ~callable:(Interprocedural.Target.show_pretty callable)
              (fun () -> Fixpoint.backward ~cfg ~initial |> Fixpoint.entry)
              ())
  in
  let () =
    match entry_state with
    | Some entry_state -> State.log "Entry state:@,%a" State.pp entry_state
    | None -> State.log "No entry state found"
  in
  let apply_broadening =
    not (Model.ModeSet.contains Model.Mode.SkipModelBroadening existing_model.Model.modes)
  in
  let normalized_parameters = get_normalized_parameters define.value in
  let extract_model State.{ taint; _ } =
    TaintProfiler.track_duration ~profiler ~name:"Backward analysis - extract model" ~f:(fun () ->
        extract_tito_and_sink_models
          ~normalized_parameters
          ~pyre_api
          ~is_constructor:State.is_constructor
          ~taint_configuration:FunctionContext.taint_configuration
          ~existing_backward:existing_model.Model.backward
          ~apply_broadening
          taint)
  in
  let log_model model =
    let () = State.log "Backward Model:@,%a" Model.Backward.pp model in
    model
  in
  Statistics.performance
    ~randomly_log_every:1000
    ~always_log_time_threshold:1.0 (* Seconds *)
    ~name:"Backward analysis"
    ~normals:["callable", Interprocedural.Target.show_pretty callable]
    ~section:`Taint
    ~timer
    ();

  entry_state
  >>| extract_model
  >>| remove_unmatched_partial_sinks ~normalized_parameters
  >>| log_model
  |> Option.value ~default:Model.Backward.empty
