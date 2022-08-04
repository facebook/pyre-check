(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

  val class_interval_graph : Interprocedural.ClassIntervalSetGraph.SharedMemory.t

  val call_graph_of_define : CallGraph.DefineCallGraph.t

  val get_callee_model : Interprocedural.Target.t -> Model.t option

  val triggered_sinks : ForwardAnalysis.triggered_sinks

  val caller_class_interval : Interprocedural.ClassIntervalSet.t
end

let ( |>> ) (taint, state) f = f taint, state

module State (FunctionContext : FUNCTION_CONTEXT) = struct
  type t = { taint: BackwardState.t }

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


  let get_format_string_callees ~location =
    CallGraph.DefineCallGraph.resolve_format_string FunctionContext.call_graph_of_define ~location


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


  let is_setitem () =
    let { Node.value = { Statement.Define.signature = { name; _ }; _ }; _ } =
      FunctionContext.definition
    in
    String.equal (Reference.last name) "__setitem__"


  let first_parameter () =
    let { Node.value = { Statement.Define.signature = { parameters; _ }; _ }; _ } =
      FunctionContext.definition
    in
    match parameters with
    | { Node.value = { Parameter.name; _ }; _ } :: _ -> Some (AccessPath.Root.Variable name)
    | _ -> None


  (* This is where we can observe access paths reaching into LocalReturn and record the extraneous
     paths for more precise tito. *)
  let initial_taint =
    (* We handle constructors, __setitem__ methods, and property setters specially and track
       effects. *)
    if
      is_constructor ()
      || is_setitem ()
      || Statement.Define.is_property_setter (Node.value FunctionContext.definition)
    then
      match first_parameter () with
      | Some root ->
          BackwardState.assign
            ~root
            ~path:[]
            (BackwardState.Tree.create_leaf Domains.local_return_taint)
            BackwardState.bottom
      | _ -> BackwardState.bottom
    else
      BackwardState.assign
        ~root:AccessPath.Root.LocalResult
        ~path:[]
        (BackwardState.Tree.create_leaf Domains.local_return_taint)
        BackwardState.bottom


  let transform_non_leaves path taint =
    let f prefix = prefix @ path in
    match path with
    | Abstract.TreeDomain.Label.AnyIndex :: _ -> taint
    | _ -> BackwardTaint.transform Features.ReturnAccessPathSet.Element Map ~f taint


  let read_tree = BackwardState.Tree.read ~transform_non_leaves

  let get_taint access_path { taint; _ } =
    match access_path with
    | None -> BackwardState.Tree.empty
    | Some { AccessPath.root; path } -> BackwardState.read ~transform_non_leaves ~root ~path taint


  let store_taint ?(weak = false) ~root ~path taint { taint = state_taint } =
    { taint = BackwardState.assign ~weak ~root ~path taint state_taint }


  let analyze_definition ~define:_ state = state

  type call_target_result = {
    arguments_taint: BackwardState.Tree.t list;
    self_taint: BackwardState.Tree.t option;
    callee_taint: BackwardState.Tree.t option;
    state: t;
  }

  let join_call_target_results
      {
        arguments_taint = left_arguments_taint;
        self_taint = left_self_taint;
        callee_taint = left_callee_taint;
        state = left_state;
      }
      {
        arguments_taint = right_arguments_taint;
        self_taint = right_self_taint;
        callee_taint = right_callee_taint;
        state = right_state;
      }
    =
    let arguments_taint =
      List.map2_exn left_arguments_taint right_arguments_taint ~f:BackwardState.Tree.join
    in
    let join_option left right =
      match left, right with
      | Some left, Some right -> Some (BackwardState.Tree.join left right)
      | Some left, None -> Some left
      | None, Some right -> Some right
      | None, None -> None
    in
    let self_taint = join_option left_self_taint right_self_taint in
    let callee_taint = join_option left_callee_taint right_callee_taint in
    let state = join left_state right_state in
    { arguments_taint; self_taint; callee_taint; state }


  let apply_call_target
      ~resolution
      ~call_location
      ~self
      ~callee
      ~arguments
      ~state:initial_state
      ~call_taint
      ({
         CallGraph.CallTarget.target;
         implicit_self;
         implicit_dunder_call;
         collapse_tito = _;
         index = _;
         return_type;
         receiver_type;
       } as call_target)
    =
    let arguments =
      if implicit_self && not implicit_dunder_call then
        { Call.Argument.name = None; value = Option.value_exn self } :: arguments
      else if implicit_self && implicit_dunder_call then
        { Call.Argument.name = None; value = callee } :: arguments
      else
        arguments
    in
    let triggered_taint =
      match Hashtbl.find FunctionContext.triggered_sinks call_location with
      | Some items ->
          List.fold
            items
            ~f:(fun state (root, sink) ->
              let new_taint =
                BackwardTaint.singleton sink Frame.initial |> BackwardState.Tree.create_leaf
              in
              BackwardState.assign ~root ~path:[] new_taint state)
            ~init:BackwardState.bottom
      | None -> BackwardState.bottom
    in
    let ({ Model.modes; _ } as taint_model) =
      TaintProfiler.track_model_fetch
        ~profiler
        ~analysis:TaintProfiler.Backward
        ~call_target:target
        ~f:(fun () ->
          CallModel.at_callsite
            ~resolution
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
    let taint_model =
      {
        taint_model with
        backward =
          {
            taint_model.backward with
            sink_taint = BackwardState.join taint_model.backward.sink_taint triggered_taint;
          };
      }
    in
    let call_taint =
      if Model.ModeSet.contains Obscure modes then
        BackwardState.Tree.add_local_breadcrumbs
          (Features.type_breadcrumbs (Option.value_exn return_type))
          call_taint
      else
        call_taint
    in
    let get_argument_taint ~resolution ~argument:{ Call.Argument.value = argument; _ } =
      let global_sink =
        GlobalModel.from_expression
          ~resolution
          ~call_graph:FunctionContext.call_graph_of_define
          ~get_callee_model:FunctionContext.get_callee_model
          ~qualifier:FunctionContext.qualifier
          ~expression:argument
          ~interval:FunctionContext.caller_class_interval
        |> GlobalModel.get_sinks
        |> Issue.SinkTreeWithHandle.join
      in
      let access_path = AccessPath.of_expression argument in
      get_taint access_path initial_state |> BackwardState.Tree.join global_sink
    in
    let convert_tito_path_to_taint ~kind (tito_path, tito_taint) argument_taint =
      let breadcrumbs = BackwardTaint.joined_breadcrumbs tito_taint in
      let tito_depth =
        BackwardTaint.fold TraceLength.Self tito_taint ~f:TraceLength.join ~init:TraceLength.bottom
      in
      let taint_to_propagate =
        match Sinks.discard_transforms kind with
        | Sinks.LocalReturn -> call_taint
        (* Attach nodes shouldn't affect analysis. *)
        | Sinks.Attach -> BackwardState.Tree.empty
        | Sinks.ParameterUpdate n -> (
            match List.nth arguments n with
            | None -> BackwardState.Tree.empty
            | Some argument -> get_argument_taint ~resolution ~argument)
        | _ -> failwith "unexpected tito sink"
      in
      let taint_to_propagate =
        match kind with
        | Sinks.Transform { local = transforms; global; _ } when TaintTransforms.is_empty global ->
            (* Apply tito transforms and source- and sink-specific sanitizers. *)
            taint_to_propagate
            |> BackwardState.Tree.apply_transforms transforms TaintTransforms.Order.Backward
            |> BackwardState.Tree.transform BackwardTaint.kind Filter ~f:Issue.sink_can_match_rule
        | Sinks.Transform _ -> failwith "unexpected non-empty `global` transforms in tito"
        | _ -> taint_to_propagate
      in
      let compute_tito_depth kind depth =
        match Sinks.discard_transforms kind with
        | Sinks.LocalReturn -> max depth (1 + tito_depth)
        | _ -> depth
      in
      CallModel.return_paths ~kind ~tito_taint
      |> List.fold
           ~f:(fun taint return_path ->
             read_tree return_path taint_to_propagate
             |> BackwardState.Tree.collapse
                  ~transform:(BackwardTaint.add_local_breadcrumbs (Features.tito_broadening_set ()))
             |> BackwardTaint.add_local_breadcrumbs breadcrumbs
             |> BackwardTaint.transform
                  TraceLength.Self
                  (Context (BackwardTaint.kind, Map))
                  ~f:compute_tito_depth
             |> BackwardState.Tree.create_leaf
             |> BackwardState.Tree.prepend tito_path
             |> BackwardState.Tree.join taint)
           ~init:argument_taint
    in
    let convert_tito_tree_to_taint ~argument ~kind ~tito_tree taint_tree =
      BackwardState.Tree.fold
        BackwardState.Tree.Path
        tito_tree
        ~init:BackwardState.Tree.bottom
        ~f:(convert_tito_path_to_taint ~kind)
      |> BackwardState.Tree.transform Features.TitoPositionSet.Element Add ~f:argument.Node.location
      |> BackwardState.Tree.add_local_breadcrumb (Features.tito ())
      |> BackwardState.Tree.join taint_tree
    in
    let is_self_call = Ast.Expression.is_self_call ~callee in
    let receiver_class_interval =
      Interprocedural.ClassIntervalSetGraph.SharedMemory.of_type class_interval_graph receiver_type
    in
    let analyze_argument
        (arguments_taint, state)
        { CallModel.ArgumentMatches.argument; sink_matches; tito_matches; sanitize_matches }
      =
      let location =
        Location.with_module ~module_reference:FunctionContext.qualifier argument.Node.location
      in
      let sink_taint =
        CallModel.sink_trees_of_argument
          ~resolution
          ~transform_non_leaves
          ~model:taint_model
          ~location
          ~call_target
          ~arguments
          ~sink_matches
          ~is_self_call
          ~caller_class_interval:FunctionContext.caller_class_interval
          ~receiver_class_interval
        |> Issue.SinkTreeWithHandle.join
      in
      let taint_in_taint_out =
        CallModel.taint_in_taint_out_mapping
          ~transform_non_leaves
          ~model:taint_model
          ~tito_matches
          ~sanitize_matches
        |> CallModel.TaintInTaintOutMap.fold
             ~init:BackwardState.Tree.empty
             ~f:(convert_tito_tree_to_taint ~argument)
      in
      let taint = BackwardState.Tree.join sink_taint taint_in_taint_out in
      let state =
        match AccessPath.of_expression argument with
        | Some { AccessPath.root; path } ->
            let breadcrumbs_to_add =
              BackwardState.Tree.filter_by_kind ~kind:Sinks.AddFeatureToArgument sink_taint
              |> BackwardTaint.accumulated_breadcrumbs
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
    let arguments_taint, state =
      CallModel.match_actuals_to_formals ~model:taint_model ~arguments
      |> List.rev
      |> List.fold ~f:analyze_argument ~init:([], initial_state)
    in
    (* Extract the taint for self. *)
    let self_taint, callee_taint, arguments_taint =
      if implicit_self && not implicit_dunder_call then
        match arguments_taint with
        | self_taint :: arguments_taint -> Some self_taint, None, arguments_taint
        | _ -> failwith "missing taint for self argument"
      else if implicit_self && implicit_dunder_call then
        match arguments_taint with
        | callee_taint :: arguments_taint -> None, Some callee_taint, arguments_taint
        | _ -> failwith "missing taint for callee argument"
      else
        None, None, arguments_taint
    in
    { arguments_taint; self_taint; callee_taint; state }


  let apply_obscure_call ~callee ~arguments ~state:initial_state ~call_taint =
    log
      "Backward analysis of obscure call to `%a` with arguments (%a)"
      Expression.pp
      callee
      Ast.Expression.pp_expression_argument_list
      arguments;
    let obscure_taint =
      BackwardState.Tree.collapse
        ~transform:(BackwardTaint.add_local_breadcrumbs (Features.tito_broadening_set ()))
        call_taint
      |> BackwardTaint.add_local_breadcrumb (Features.obscure_unknown_callee ())
      |> BackwardState.Tree.create_leaf
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
    { arguments_taint; self_taint = None; callee_taint = Some obscure_taint; state = initial_state }


  let apply_constructor_targets
      ~resolution
      ~call_location
      ~callee
      ~arguments
      ~new_targets
      ~init_targets
      ~state:initial_state
      ~call_taint
    =
    (* Call `__init__`. Add the `self` implicit argument. *)
    let { arguments_taint = init_arguments_taint; self_taint; callee_taint = _; state } =
      match init_targets with
      | [] ->
          {
            arguments_taint = List.map arguments ~f:(fun _ -> BackwardState.Tree.bottom);
            self_taint = Some call_taint;
            callee_taint = None;
            state = initial_state;
          }
      | init_targets ->
          let call_expression =
            Expression.Call { Call.callee; arguments } |> Node.create ~location:call_location
          in
          List.map init_targets ~f:(fun target ->
              apply_call_target
                ~resolution
                ~call_location
                ~self:(Some call_expression)
                ~callee
                ~arguments
                ~state:initial_state
                ~call_taint
                target)
          |> List.fold
               ~f:join_call_target_results
               ~init:
                 {
                   arguments_taint = List.map arguments ~f:(fun _ -> BackwardState.Tree.bottom);
                   self_taint = None;
                   callee_taint = None;
                   state = bottom;
                 }
    in
    let self_taint = Option.value self_taint ~default:BackwardState.Tree.bottom in

    (* Call `__new__`. *)
    let call_target_result =
      match new_targets with
      | []
      | [
          {
            CallGraph.CallTarget.target =
              Interprocedural.Target.Method
                { class_name = "object"; method_name = "__new__"; kind = Normal };
            _;
          };
        ] ->
          { arguments_taint = init_arguments_taint; self_taint = None; callee_taint = None; state }
      | new_targets ->
          (* Add the `cls` implicit argument. *)
          let {
            arguments_taint = new_arguments_taint;
            self_taint = callee_taint;
            callee_taint = _;
            state;
          }
            =
            List.map new_targets ~f:(fun target ->
                apply_call_target
                  ~resolution
                  ~call_location
                  ~self:(Some callee)
                  ~callee
                  ~arguments
                  ~state
                  ~call_taint:self_taint
                  target)
            |> List.fold
                 ~f:join_call_target_results
                 ~init:
                   {
                     arguments_taint = List.map arguments ~f:(fun _ -> BackwardState.Tree.bottom);
                     self_taint = None;
                     callee_taint = None;
                     state = bottom;
                   }
          in
          {
            arguments_taint =
              List.map2_exn init_arguments_taint new_arguments_taint ~f:BackwardState.Tree.join;
            self_taint = None;
            callee_taint;
            state;
          }
    in

    call_target_result


  let apply_callees_and_return_arguments_taint
      ~resolution
      ~callee
      ~call_location
      ~arguments
      ~state:initial_state
      ~call_taint
      {
        CallGraph.CallCallees.call_targets;
        new_targets;
        init_targets;
        higher_order_parameter = _;
        unresolved;
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

    let call_targets, unresolved, call_taint =
      (* Specially handle super.__init__ calls and explicit calls to superclass' `__init__` in
         constructors for tito. *)
      match Node.value callee with
      | Name (Name.Attribute { base; attribute; _ })
        when is_constructor ()
             && String.equal attribute "__init__"
             && Interprocedural.CallResolution.is_super
                  ~resolution
                  ~define:FunctionContext.definition
                  base ->
          (* If the super call is `object.__init__`, this is likely due to a lack of type
             information for that constructor - we treat that case as obscure to not lose argument
             taint for these calls. *)
          let call_targets, unresolved =
            match call_targets with
            | [
             {
               CallGraph.CallTarget.target =
                 Interprocedural.Target.Method
                   { class_name = "object"; method_name = "__init__"; kind = Normal };
               _;
             };
            ] ->
                [], true
            | _ -> call_targets, unresolved
          in
          let call_taint =
            BackwardState.Tree.create_leaf Domains.local_return_taint
            |> BackwardState.Tree.join call_taint
          in
          call_targets, unresolved, call_taint
      | _ -> call_targets, unresolved, call_taint
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
             ~resolution
             ~call_location
             ~self
             ~callee
             ~arguments
             ~state:initial_state
             ~call_taint)
      |> List.fold
           ~f:join_call_target_results
           ~init:
             {
               arguments_taint = List.map arguments ~f:(fun _ -> BackwardState.Tree.bottom);
               self_taint = None;
               callee_taint = None;
               state = bottom;
             }
    in

    (* Apply an obscure call if the call was not fully resolved. *)
    let call_target_result =
      if unresolved then
        apply_obscure_call ~callee ~arguments ~state:initial_state ~call_taint
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
            ~resolution
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


  let rec analyze_arguments ~resolution ~arguments ~arguments_taint ~state =
    (* Explicitly analyze arguments from right to left (opposite of forward analysis). *)
    List.zip_exn arguments arguments_taint
    |> List.rev
    |> List.fold
         ~init:state
         ~f:(fun state ({ Call.Argument.value = argument; _ }, argument_taint) ->
           analyze_unstarred_expression ~resolution argument_taint argument state)


  and analyze_callee ~resolution ~is_property_call ~callee ~self_taint ~callee_taint ~state =
    (* Special case: `x.foo()` where foo is a property returning a callable. *)
    let callee_is_property =
      match is_property_call, callee.Node.value with
      | false, Expression.Name (Name.Attribute { attribute; _ }) ->
          get_attribute_access_callees ~location:callee.Node.location ~attribute |> Option.is_some
      | _ -> false
    in
    match self_taint, callee_taint, callee_is_property with
    | _, _, true
    | _, Some _, _ -> (
        match callee.Node.value with
        | Expression.Name (Name.Attribute { base; attribute; special }) ->
            (* If we are already analyzing a call of a property, then ignore properties
             * to avoid infinite recursion. *)
            let resolve_properties = not is_property_call in
            analyze_attribute_access
              ~resolution
              ~location:callee.Node.location
              ~resolve_properties
              ~base
              ~attribute
              ~special
              ~base_taint:(Option.value self_taint ~default:BackwardState.Tree.bottom)
              ~attribute_taint:(Option.value callee_taint ~default:BackwardState.Tree.bottom)
              ~state
        | _ ->
            analyze_expression
              ~resolution
              ~taint:(Option.value callee_taint ~default:BackwardState.Tree.bottom)
              ~state
              ~expression:callee)
    | Some self_taint, None, _ -> (
        match callee.Node.value with
        | Expression.Name (Name.Attribute { base; _ }) ->
            analyze_expression ~resolution ~taint:self_taint ~state ~expression:base
        | _ -> state)
    | None, None, _ -> state


  and analyze_attribute_access
      ~resolution
      ~location
      ~resolve_properties
      ~base
      ~attribute
      ~special
      ~base_taint:initial_base_taint
      ~attribute_taint
      ~state
    =
    let expression =
      Expression.Name (Name.Attribute { base; attribute; special }) |> Node.create ~location
    in
    let attribute_access_callees =
      if resolve_properties then get_attribute_access_callees ~location ~attribute else None
    in

    let base_taint_property_call, state_property_call =
      match attribute_access_callees with
      | Some { property_targets = _ :: _ as property_targets; _ } ->
          let { arguments_taint = _; self_taint; callee_taint = _; state } =
            apply_callees_and_return_arguments_taint
              ~resolution
              ~callee:expression
              ~call_location:location
              ~arguments:[]
              ~state
              ~call_taint:attribute_taint
              (CallGraph.CallCallees.create ~call_targets:property_targets ())
          in
          let base_taint = Option.value_exn self_taint in
          base_taint, state
      | _ -> BackwardState.Tree.bottom, bottom
    in

    let base_taint_attribute, state_attribute =
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
          let add_tito_features taint =
            let attribute_breadcrumbs =
              global_model |> GlobalModel.get_tito |> BackwardState.Tree.accumulated_breadcrumbs
            in
            BackwardState.Tree.add_local_breadcrumbs attribute_breadcrumbs taint
          in

          let apply_attribute_sanitizers taint =
            let sanitizer = GlobalModel.get_sanitize global_model in
            let taint =
              match sanitizer.sinks with
              | Some All -> BackwardState.Tree.empty
              | Some (Specific sanitized_sinks) ->
                  let sanitized_sinks_transforms =
                    Sinks.Set.to_sanitize_transforms_exn sanitized_sinks
                    |> SanitizeTransformSet.from_sinks
                  in
                  BackwardState.Tree.apply_sanitize_transforms sanitized_sinks_transforms taint
              | _ -> taint
            in
            let taint =
              match sanitizer.sources with
              | Some (Specific sanitized_sources) ->
                  let sanitized_sources_transforms =
                    Sources.Set.to_sanitize_transforms_exn sanitized_sources
                    |> SanitizeTransformSet.from_sources
                  in
                  taint
                  |> BackwardState.Tree.apply_sanitize_transforms sanitized_sources_transforms
                  |> BackwardState.Tree.transform
                       BackwardTaint.kind
                       Filter
                       ~f:Issue.sink_can_match_rule
              | _ -> taint
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
    analyze_expression ~resolution ~taint:base_taint ~state ~expression:base


  and analyze_arguments_for_lambda_call
      ~resolution
      ~arguments
      ~arguments_taint
      ~state
      ~lambda_argument:
        { Call.Argument.value = { location = lambda_location; _ } as lambda_callee; _ }
      { CallGraph.HigherOrderParameter.index = lambda_index; call_targets }
    =
    (* If we have a lambda `fn` getting passed into `hof`, we use the following strategy:
     * hof(q, fn, x, y) gets translated into (analyzed backwards)
     * if rand():
     *   $all = {q, x, y}
     *   $result = fn( *all, **all)
     * else:
     *   $result = fn
     * hof(q, $result, x, y)
     *)
    let result_taint = List.nth_exn arguments_taint lambda_index in
    let non_lambda_arguments_taint =
      let arguments_taint = List.zip_exn arguments arguments_taint in
      List.take arguments_taint lambda_index @ List.drop arguments_taint (lambda_index + 1)
    in

    (* Simulate if branch. *)
    let if_branch_state, all_taint =
      (* Simulate $result = fn( *all, **all) *)
      let all_argument =
        Expression.Name (Name.Identifier "$all") |> Node.create ~location:lambda_location
      in
      let arguments =
        [
          {
            Call.Argument.value =
              Expression.Starred (Starred.Once all_argument)
              |> Node.create ~location:lambda_location;
            name = None;
          };
          {
            Call.Argument.value =
              Expression.Starred (Starred.Twice all_argument)
              |> Node.create ~location:lambda_location;
            name = None;
          };
        ]
      in
      let { arguments_taint; self_taint; callee_taint; state } =
        apply_callees_and_return_arguments_taint
          ~resolution
          ~callee:lambda_callee
          ~call_location:lambda_location
          ~arguments
          ~call_taint:result_taint
          ~state
          (CallGraph.CallCallees.create ~call_targets ())
      in
      let state =
        analyze_callee
          ~resolution
          ~is_property_call:false
          ~callee:lambda_callee
          ~self_taint
          ~callee_taint
          ~state
      in
      let all_taint =
        arguments_taint
        |> List.fold ~f:BackwardState.Tree.join ~init:BackwardState.Tree.bottom
        |> read_tree [Abstract.TreeDomain.Label.AnyIndex]
        |> BackwardState.Tree.add_local_breadcrumb (Features.lambda ())
      in
      state, all_taint
    in

    (* Simulate else branch. *)
    let else_branch_state =
      analyze_expression ~resolution ~taint:result_taint ~state ~expression:lambda_callee
    in

    (* Join both branches. *)
    let state = join else_branch_state if_branch_state in

    (* Analyze arguments. *)
    List.fold
      non_lambda_arguments_taint
      ~init:state
      ~f:(fun state ({ Call.Argument.value = argument; _ }, argument_taint) ->
        let argument_taint = BackwardState.Tree.join argument_taint all_taint in
        analyze_unstarred_expression ~resolution argument_taint argument state)


  and apply_callees
      ~resolution
      ~is_property
      ~callee
      ~call_location
      ~arguments
      ~state:initial_state
      ~call_taint
      callees
    =
    let { arguments_taint; self_taint; callee_taint; state } =
      apply_callees_and_return_arguments_taint
        ~resolution
        ~callee
        ~call_location
        ~arguments
        ~state:initial_state
        ~call_taint
        callees
    in

    let state =
      match callees with
      | {
       higher_order_parameter =
         Some ({ CallGraph.HigherOrderParameter.index; _ } as higher_order_parameter);
       _;
      } -> (
          match List.nth arguments index with
          | Some lambda_argument ->
              analyze_arguments_for_lambda_call
                ~resolution
                ~arguments
                ~arguments_taint
                ~state
                ~lambda_argument
                higher_order_parameter
          | _ -> analyze_arguments ~resolution ~arguments ~arguments_taint ~state)
      | _ -> analyze_arguments ~resolution ~arguments ~arguments_taint ~state
    in
    let state =
      analyze_callee
        ~resolution
        ~is_property_call:is_property
        ~callee
        ~self_taint
        ~callee_taint
        ~state
    in
    state


  and analyze_dictionary_entry ~resolution taint state { Dictionary.Entry.key; value } =
    let key_taint = read_tree [AccessPath.dictionary_keys] taint in
    let state = analyze_expression ~resolution ~taint:key_taint ~state ~expression:key in
    let field_name = AccessPath.get_index key in
    let value_taint = read_tree [field_name] taint in
    analyze_expression ~resolution ~taint:value_taint ~state ~expression:value


  and analyze_reverse_list_element ~total ~resolution taint reverse_position state expression =
    let position = total - reverse_position - 1 in
    let index_name = Abstract.TreeDomain.Label.Index (string_of_int position) in
    let value_taint = read_tree [index_name] taint in
    analyze_expression ~resolution ~taint:value_taint ~state ~expression


  and generator_resolution ~resolution generators =
    let resolve_generator resolution generator =
      Resolution.resolve_assignment resolution (Statement.Statement.generator_assignment generator)
    in
    List.fold generators ~init:resolution ~f:resolve_generator


  and analyze_generators ~resolution ~state generators =
    let handle_generator state ({ Comprehension.Generator.conditions; _ } as generator) =
      let state =
        List.fold conditions ~init:state ~f:(fun state condition ->
            analyze_expression
              ~resolution
              ~taint:BackwardState.Tree.empty
              ~state
              ~expression:condition)
      in
      let { Statement.Assign.target; value; _ } =
        Statement.Statement.generator_assignment generator
      in
      analyze_assignment ~resolution ~target ~value state
    in
    List.fold ~f:handle_generator generators ~init:state


  and analyze_comprehension ~resolution taint { Comprehension.element; generators; _ } state =
    let resolution = generator_resolution ~resolution generators in
    let element_taint = read_tree [Abstract.TreeDomain.Label.AnyIndex] taint in
    let state = analyze_expression ~resolution ~taint:element_taint ~state ~expression:element in
    analyze_generators ~resolution ~state generators


  (* Skip through * and **. Used at call sites where * and ** are handled explicitly *)
  and analyze_unstarred_expression ~resolution taint expression state =
    match expression.Node.value with
    | Starred (Starred.Once expression)
    | Starred (Starred.Twice expression) ->
        analyze_expression ~resolution ~taint ~state ~expression
    | _ -> analyze_expression ~resolution ~taint ~state ~expression


  and analyze_getitem_call_target
      ~resolution
      ~index_number
      ~location
      ~base
      ~taint
      ~state
      ~state_before_index_access
      call_target
    =
    let analyze_getitem receiver_type =
      let named_tuple_attributes =
        Analysis.NamedTuple.field_names ~global_resolution receiver_type
      in
      match named_tuple_attributes, index_number with
      | Some named_tuple_attributes, Some index_number ->
          List.nth named_tuple_attributes index_number
          (* Access an attribute of a named tuple via indices *)
          >>| (fun attribute ->
                analyze_attribute_access
                  ~resolution
                  ~location
                  ~resolve_properties:false
                  ~base
                  ~attribute
                  ~special:false
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
        Lazy.force state_before_index_access


  and analyze_call ~resolution ~location ~taint ~state ~callee ~arguments =
    match { Call.callee; arguments } with
    | {
     callee =
       { Node.value = Name (Name.Attribute { base; attribute = "__setitem__"; _ }); _ } as callee;
     arguments = [{ Call.Argument.value = index; _ }; { Call.Argument.value; _ }] as arguments;
    } ->
        let ({ CallGraph.CallCallees.call_targets; _ } as callees) =
          get_call_callees ~location ~call:{ Call.callee; arguments }
        in
        let is_dict_setitem =
          match call_targets with
          | [
              {
                CallGraph.CallTarget.target =
                  Method { class_name = "dict"; method_name = "__setitem__"; kind = Normal };
                _;
              };
            ]
          | [
              {
                CallGraph.CallTarget.target =
                  Override { class_name = "dict"; method_name = "__setitem__"; kind = Normal };
                _;
              };
            ] ->
              true
          | _ -> false
        in
        if is_dict_setitem || not (CallGraph.CallCallees.is_partially_resolved callees) then
          (* Use the hardcoded model of `__setitem__` for any subtype of dict or unresolved callees:
             `base[index] = value`. This is incorrect, but can lead to higher SNR, because we assume
             in most cases, we run into an expression whose type is exactly `dict`, rather than a
             (strict) subtype of `dict` that overrides `__setitem__`. *)
          let state =
            (* Since we smash the taint of ALL keys, we do a weak update here to avoid removing the
               taint in `**keys`. That is, we join the state before analyzing the assignment to
               `**keys` and the state afterwards. *)
            analyze_assignment
              ~weak:true
              ~resolution
              ~fields:[AccessPath.dictionary_keys]
              ~target:base
              ~value:index
              state
          in
          analyze_assignment
            ~resolution
            ~fields:[AccessPath.get_index index]
            ~target:base
            ~value
            state
        else
          (* Use the custom model of `__setitem__`. We treat `e.__setitem__(k, v)` as `e =
             e.__setitem__(k, v)` where method `__setitem__` returns the updated self. Due to
             modeling with the assignment, the user-provided models of `__setitem__` will be
             ignored, if they are inconsistent with treating `__setitem__` as returning an updated
             self. *)
          let taint = compute_assignment_taint ~resolution base state |> fst in
          apply_callees
            ~resolution
            ~is_property:false
            ~call_location:location
            ~state
            ~callee
            ~arguments
            ~call_taint:taint
            callees
    | {
     callee = { Node.value = Name (Name.Attribute { base; attribute = "__getitem__"; _ }); _ };
     arguments =
       [{ Call.Argument.value = { Node.value = argument_expression; _ } as argument_value; _ }];
    } ->
        let index = AccessPath.get_index argument_value in
        let state_before_index_access =
          lazy
            (let taint =
               BackwardState.Tree.prepend [index] taint
               |> BackwardState.Tree.add_local_first_index index
             in
             analyze_expression ~resolution ~taint ~state ~expression:base)
        in
        let { CallGraph.CallCallees.call_targets; _ } =
          get_call_callees ~location ~call:{ Call.callee; arguments }
        in
        let state =
          if List.is_empty call_targets then
            (* This call may be unresolved, because for example the receiver type is unknown *)
            Lazy.force state_before_index_access
          else
            let index_number =
              match argument_expression with
              | Expression.Constant (Constant.Integer i) -> Some i
              | _ -> None
            in
            List.fold call_targets ~init:bottom ~f:(fun state_so_far call_target ->
                analyze_getitem_call_target
                  ~index_number
                  ~resolution
                  ~location
                  ~base
                  ~taint
                  ~state
                  ~state_before_index_access
                  call_target
                |> join state_so_far)
        in
        analyze_expression
          ~resolution
          ~taint:BackwardState.Tree.bottom
          ~state
          ~expression:argument_value
    (* Special case x.__iter__().__next__() as being a random index access (this pattern is the
       desugaring of `for element in x`). Special case dictionary keys appropriately. *)
    | {
     callee =
       {
         Node.value =
           Name
             (Name.Attribute
               {
                 base =
                   {
                     Node.value =
                       Call
                         {
                           callee =
                             {
                               Node.value =
                                 Name (Name.Attribute { base; attribute = "__iter__"; _ });
                               _;
                             };
                           arguments = [];
                         };
                     _;
                   };
                 attribute = "__next__";
                 _;
               });
         _;
       };
     arguments = [];
    } ->
        let label =
          (* For dictionaries, the default iterator is keys. *)
          if
            CallResolution.resolve_ignoring_untracked ~resolution base
            |> Type.is_dictionary_or_mapping
          then
            AccessPath.dictionary_keys
          else
            Abstract.TreeDomain.Label.AnyIndex
        in

        let taint = BackwardState.Tree.prepend [label] taint in
        analyze_expression ~resolution ~taint ~state ~expression:base
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
    } ->
        analyze_assignment
          ~resolution
          ~target:
            (Expression.Name (Name.Attribute { base = self; attribute; special = true })
            |> Node.create ~location)
          ~value
          state
    (* `getattr(a, "field", default)` should evaluate to the join of `a.field` and `default`. *)
    | {
     callee = { Node.value = Name (Name.Identifier "getattr"); _ };
     arguments =
       [
         { Call.Argument.value = base; _ };
         {
           Call.Argument.value =
             {
               Node.value =
                 Expression.Constant (Constant.String { StringLiteral.value = attribute; _ });
               _;
             };
           _;
         };
         { Call.Argument.value = default; _ };
       ];
    } ->
        let attribute_expression =
          Expression.Name (Name.Attribute { base; attribute; special = false })
          |> Node.create ~location
        in
        let state = analyze_expression ~resolution ~state ~expression:attribute_expression ~taint in
        analyze_expression ~resolution ~state ~expression:default ~taint
    (* `zip(a, b, ...)` creates a taint object whose first index has a's taint, second index has b's
       taint, etc. *)
    | { callee = { Node.value = Name (Name.Identifier "zip"); _ }; arguments = lists } ->
        let taint = BackwardState.Tree.read [Abstract.TreeDomain.Label.AnyIndex] taint in
        let analyze_zipped_list index state { Call.Argument.value; _ } =
          let index_name = Abstract.TreeDomain.Label.Index (string_of_int index) in
          let taint =
            BackwardState.Tree.read [index_name] taint
            |> BackwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]
          in
          analyze_expression ~resolution ~state ~taint ~expression:value
        in
        List.foldi lists ~init:state ~f:analyze_zipped_list
    (* dictionary .keys(), .values() and .items() functions are special, as they require handling of
       DictionaryKeys taint. *)
    | { callee = { Node.value = Name (Name.Attribute { base; attribute = "values"; _ }); _ }; _ }
      when CallResolution.resolve_ignoring_untracked ~resolution base
           |> Type.is_dictionary_or_mapping ->
        let taint =
          taint
          |> BackwardState.Tree.read [Abstract.TreeDomain.Label.AnyIndex]
          |> BackwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]
        in
        analyze_expression ~resolution ~taint ~state ~expression:base
    | { callee = { Node.value = Name (Name.Attribute { base; attribute = "keys"; _ }); _ }; _ }
      when CallResolution.resolve_ignoring_untracked ~resolution base
           |> Type.is_dictionary_or_mapping ->
        let taint =
          taint
          |> BackwardState.Tree.read [AccessPath.dictionary_keys]
          |> BackwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]
        in
        analyze_expression ~resolution ~taint ~state ~expression:base
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
           _;
         };
       ];
    }
      when CallResolution.resolve_ignoring_untracked ~resolution base
           |> Type.is_dictionary_or_mapping
           && Option.is_some (Dictionary.string_literal_keys entries) ->
        let entries = Option.value_exn (Dictionary.string_literal_keys entries) in
        let access_path =
          Some { AccessPath.root = AccessPath.Root.Variable identifier; path = [] }
        in
        let dict_taint =
          let global_taint =
            GlobalModel.from_expression
              ~resolution
              ~call_graph:FunctionContext.call_graph_of_define
              ~get_callee_model:FunctionContext.get_callee_model
              ~qualifier:FunctionContext.qualifier
              ~expression:base
              ~interval:FunctionContext.caller_class_interval
            |> GlobalModel.get_sinks
            |> Issue.SinkTreeWithHandle.join
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
          let state = analyze_expression ~resolution ~taint:value_taint ~state ~expression:value in
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
                 base = { Node.value = Name (Name.Identifier identifier); _ } as base;
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
           _;
         };
       ];
    }
      when CallResolution.resolve_ignoring_untracked ~resolution base
           |> Type.is_dictionary_or_mapping ->
        let access_path =
          Some { AccessPath.root = AccessPath.Root.Variable identifier; path = [] }
        in
        let old_taint = get_taint access_path state in
        let new_taint =
          BackwardState.Tree.assign
            ~tree:old_taint
            [Abstract.TreeDomain.Label.Index value]
            ~subtree:taint
        in
        store_taint ~root:(AccessPath.Root.Variable identifier) ~path:[] new_taint state
    | { callee = { Node.value = Name (Name.Attribute { base; attribute = "items"; _ }); _ }; _ }
      when CallResolution.resolve_ignoring_untracked ~resolution base
           |> Type.is_dictionary_or_mapping ->
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
        analyze_expression ~resolution ~taint ~state ~expression:base
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
          ~taint
          ~state
          ~expression:
            {
              Node.location;
              value =
                Expression.Tuple
                  (List.map arguments ~f:(fun argument -> argument.Call.Argument.value));
            }
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
                        location;
                      };
                    attribute = "__mod__";
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
                        location;
                      };
                    attribute = "format";
                    _;
                  });
            _;
          };
        arguments;
      } ->
        let formatted_string = Substring.Literal (Node.create ~location value) in
        let arguments_formatted_string =
          List.map ~f:(fun call_argument -> Substring.Format call_argument.value) arguments
        in
        let substrings = formatted_string :: arguments_formatted_string in
        analyze_joined_string
          ~resolution
          ~taint
          ~state
          ~location:(Location.with_module ~module_reference:FunctionContext.qualifier location)
          ~breadcrumbs:(Features.BreadcrumbSet.singleton (Features.format_string ()))
          substrings
    (* Special case `"str" + s` and `s + "str"` for Literal String Sinks *)
    | {
     callee =
       { Node.value = Name (Name.Attribute { base = expression; attribute = "__add__"; _ }); _ };
     arguments =
       [
         {
           Call.Argument.value =
             { Node.value = Expression.Constant (Constant.String { StringLiteral.value; _ }); _ };
           _;
         };
       ];
    } ->
        let formatted_string = Substring.Literal (Node.create ~location value) in
        let substrings = [formatted_string; Substring.Format expression] in
        analyze_joined_string
          ~resolution
          ~taint
          ~state
          ~location:(Location.with_module ~module_reference:FunctionContext.qualifier location)
          ~breadcrumbs:(Features.BreadcrumbSet.singleton (Features.string_concat_left_hand_side ()))
          substrings
    | {
     callee =
       {
         Node.value =
           Name
             (Name.Attribute
               {
                 base = { Node.value = Constant (Constant.String { StringLiteral.value; _ }); _ };
                 attribute = "__add__";
                 _;
               });
         _;
       };
     arguments = [{ Call.Argument.value = expression; _ }];
    } ->
        let formatted_string = Substring.Literal (Node.create ~location value) in
        let substrings = [formatted_string; Substring.Format expression] in
        analyze_joined_string
          ~resolution
          ~taint
          ~state
          ~location:(Location.with_module ~module_reference:FunctionContext.qualifier location)
          ~breadcrumbs:
            (Features.BreadcrumbSet.singleton (Features.string_concat_right_hand_side ()))
          substrings
    | {
     Call.callee = { Node.value = Name (Name.Identifier "reveal_taint"); _ };
     arguments = [{ Call.Argument.value = expression; _ }];
    } ->
        begin
          match AccessPath.of_expression expression with
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
    | { Call.callee = { Node.value = Name (Name.Identifier "super"); _ }; arguments } -> (
        match arguments with
        | [_; Call.Argument.{ value = object_; _ }] ->
            analyze_expression ~resolution ~taint ~state ~expression:object_
        | _ -> (
            (* Use implicit self *)
            match first_parameter () with
            | Some root -> store_taint ~weak:true ~root ~path:[] taint state
            | None -> state))
    | _ ->
        let { Call.callee; arguments } =
          CallGraph.redirect_special_calls ~resolution { Call.callee; arguments }
        in
        let callees = get_call_callees ~location ~call:{ Call.callee; arguments } in
        apply_callees
          ~resolution
          ~is_property:false
          ~call_location:location
          ~state
          ~callee
          ~arguments
          ~call_taint:taint
          callees


  and analyze_joined_string ~resolution ~taint ~state ~location ~breadcrumbs substrings =
    let taint =
      let literal_string_sinks = TaintConfiguration.literal_string_sinks () in
      if List.is_empty literal_string_sinks then
        taint
      else
        let value =
          List.map substrings ~f:(function
              | Substring.Format _ -> ""
              | Substring.Literal { Node.value; _ } -> value)
          |> String.concat ~sep:""
        in
        List.fold
          literal_string_sinks
          ~f:(fun taint { TaintConfiguration.sink_kind; pattern } ->
            if Re2.matches pattern value then
              BackwardTaint.singleton ~location sink_kind Frame.initial
              |> BackwardState.Tree.create_leaf
              |> BackwardState.Tree.join taint
            else
              taint)
          ~init:taint
    in
    let taint = BackwardState.Tree.add_local_breadcrumbs breadcrumbs taint in
    let analyze_stringify_callee
        ~taint_to_join
        ~state_to_join
        ~call_target
        ~call_location
        ~base
        ~base_state
      =
      let { arguments_taint = _; self_taint; callee_taint; state = new_state } =
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
        apply_callees_and_return_arguments_taint
          ~resolution
          ~callee
          ~call_location
          ~arguments:[]
          ~state:base_state
          ~call_taint:taint
          callees
      in
      let new_taint =
        match callee_taint, self_taint with
        | None, Some self_taint -> BackwardState.Tree.join taint_to_join self_taint
        | None, None -> taint_to_join
        | Some _, None ->
            failwith "Probably a rare case: callee_taint is Some and self_taint is None"
        | Some _, Some _ -> failwith "callee_taint and self_taint should not co-exist"
      in
      new_taint, join state_to_join new_state
    in
    let analyze_nested_expression state = function
      | Substring.Format ({ Node.location = expression_location; _ } as expression) ->
          let new_taint, new_state =
            (match get_format_string_callees ~location:expression_location with
            | Some { CallGraph.FormatStringCallees.call_targets } ->
                List.fold
                  call_targets
                  ~init:(taint, state)
                  ~f:(fun (taint_to_join, state_to_join) call_target ->
                    analyze_stringify_callee
                      ~taint_to_join
                      ~state_to_join
                      ~call_target
                      ~call_location:expression_location
                      ~base:expression
                      ~base_state:state)
            | _ -> taint, state)
            |>> BackwardState.Tree.transform
                  Features.TitoPositionSet.Element
                  Add
                  ~f:expression_location
            |>> BackwardState.Tree.add_local_breadcrumb (Features.tito ())
          in
          analyze_expression ~resolution ~taint:new_taint ~state:new_state ~expression
      | Substring.Literal _ -> state
    in
    List.fold (List.rev substrings) ~f:analyze_nested_expression ~init:state


  and analyze_expression ~resolution ~taint ~state ~expression:({ Node.location; _ } as expression) =
    log
      "Backward analysis of expression: `%a` with backward taint: %a"
      Expression.pp
      expression
      BackwardState.Tree.pp
      taint;
    match expression.Node.value with
    | Await expression -> analyze_expression ~resolution ~taint ~state ~expression
    | BooleanOperator { left; operator = _; right } ->
        analyze_expression ~resolution ~taint ~state ~expression:right
        |> fun state -> analyze_expression ~resolution ~taint ~state ~expression:left
    | ComparisonOperator ({ left; operator = _; right } as comparison) -> (
        match ComparisonOperator.override ~location comparison with
        | Some override -> analyze_expression ~resolution ~taint ~state ~expression:override
        | None ->
            let taint =
              BackwardState.Tree.add_local_breadcrumbs (Features.type_bool_scalar_set ()) taint
            in
            analyze_expression ~resolution ~taint ~state ~expression:right
            |> fun state -> analyze_expression ~resolution ~taint ~state ~expression:left)
    | Call { callee; arguments } ->
        analyze_call ~resolution ~location ~taint ~state ~callee ~arguments
    | Constant _ -> state
    | Dictionary { Dictionary.entries; keywords } ->
        let state = List.fold ~f:(analyze_dictionary_entry ~resolution taint) entries ~init:state in
        let analyze_dictionary_keywords state keywords =
          analyze_expression ~resolution ~taint ~state ~expression:keywords
        in
        List.fold keywords ~f:analyze_dictionary_keywords ~init:state
    | DictionaryComprehension
        { Comprehension.element = { Dictionary.Entry.key; value }; generators; _ } ->
        let resolution = generator_resolution ~resolution generators in
        let state =
          analyze_expression
            ~resolution
            ~taint:(read_tree [AccessPath.dictionary_keys] taint)
            ~state
            ~expression:key
        in
        let state =
          analyze_expression
            ~resolution
            ~taint:(read_tree [Abstract.TreeDomain.Label.AnyIndex] taint)
            ~state
            ~expression:value
        in
        analyze_generators ~resolution ~state generators
    | Generator comprehension -> analyze_comprehension ~resolution taint comprehension state
    | Lambda { parameters = _; body } ->
        (* Ignore parameter bindings and pretend body is inlined *)
        analyze_expression ~resolution ~taint ~state ~expression:body
    | List list ->
        let total = List.length list in
        List.rev list
        |> List.foldi ~f:(analyze_reverse_list_element ~total ~resolution taint) ~init:state
    | ListComprehension comprehension -> analyze_comprehension ~resolution taint comprehension state
    | Name (Name.Identifier identifier) ->
        store_taint ~weak:true ~root:(AccessPath.Root.Variable identifier) ~path:[] taint state
    | Name (Name.Attribute { base; attribute = "__dict__"; _ }) ->
        analyze_expression ~resolution ~taint ~state ~expression:base
    | Name (Name.Attribute { base; attribute; special }) ->
        analyze_attribute_access
          ~resolution
          ~location
          ~resolve_properties:true
          ~base
          ~attribute
          ~special
          ~base_taint:BackwardState.Tree.bottom
          ~attribute_taint:taint
          ~state
    | Set set ->
        let element_taint = read_tree [Abstract.TreeDomain.Label.AnyIndex] taint in
        List.fold
          set
          ~f:(fun state expression ->
            analyze_expression ~resolution ~taint:element_taint ~state ~expression)
          ~init:state
    | SetComprehension comprehension -> analyze_comprehension ~resolution taint comprehension state
    | Starred (Starred.Once expression)
    | Starred (Starred.Twice expression) ->
        let taint = BackwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex] taint in
        analyze_expression ~resolution ~taint ~state ~expression
    | FormatString substrings ->
        analyze_joined_string
          ~resolution
          ~taint
          ~state
          ~location:(Location.with_module ~module_reference:FunctionContext.qualifier location)
          ~breadcrumbs:(Features.BreadcrumbSet.singleton (Features.format_string ()))
          substrings
    | Ternary { target; test; alternative } ->
        let state_then = analyze_expression ~resolution ~taint ~state ~expression:target in
        let state_else = analyze_expression ~resolution ~taint ~state ~expression:alternative in
        join state_then state_else
        |> fun state ->
        analyze_expression ~resolution ~taint:BackwardState.Tree.empty ~state ~expression:test
    | Tuple list ->
        let total = List.length list in
        List.rev list
        |> List.foldi ~f:(analyze_reverse_list_element ~total ~resolution taint) ~init:state
    | UnaryOperator { operator = _; operand } ->
        analyze_expression ~resolution ~taint ~state ~expression:operand
    | WalrusOperator { target; value } ->
        let state = analyze_assignment ~resolution ~target ~value state in
        analyze_expression ~resolution ~taint ~state ~expression:value
    | Yield None -> state
    | Yield (Some expression)
    | YieldFrom expression ->
        let access_path = { AccessPath.root = AccessPath.Root.LocalResult; path = [] } in
        let return_taint = get_taint (Some access_path) state in
        analyze_expression ~resolution ~taint:return_taint ~state ~expression


  (* Returns the taint, and whether to collapse one level (due to star expression) *)
  and compute_assignment_taint ~resolution target state =
    match target.Node.value with
    | Expression.Starred (Once target | Twice target) ->
        (* This is approximate. Unless we can get the tuple type on the right to tell how many total
           elements there will be, we just pick up the entire collection. *)
        let taint, _ = compute_assignment_taint ~resolution target state in
        taint, true
    | List targets
    | Tuple targets ->
        let compute_tuple_target_taint position taint_accumulator target =
          let taint, collapse = compute_assignment_taint ~resolution target state in
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
        } ->
        let taint =
          compute_assignment_taint ~resolution base state
          |> fst
          |> BackwardState.Tree.read [AccessPath.get_index index]
        in
        taint, false
    | _ ->
        let taint =
          let local_taint =
            let access_path = AccessPath.of_expression target in
            get_taint access_path state
          in
          let global_taint =
            GlobalModel.from_expression
              ~resolution
              ~call_graph:FunctionContext.call_graph_of_define
              ~get_callee_model:FunctionContext.get_callee_model
              ~qualifier:FunctionContext.qualifier
              ~expression:target
              ~interval:FunctionContext.caller_class_interval
            |> GlobalModel.get_sinks
            |> Issue.SinkTreeWithHandle.join
          in
          BackwardState.Tree.join local_taint global_taint
        in
        taint, false


  and analyze_assignment ?(weak = false) ~resolution ?(fields = []) ~target ~value state =
    let taint = compute_assignment_taint ~resolution target state |> fst |> read_tree fields in
    let state =
      let rec clear_taint state target =
        match Node.value target with
        | Expression.Tuple items -> List.fold items ~f:clear_taint ~init:state
        | _ -> (
            match AccessPath.of_expression target with
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
      in
      if weak then (* Weak updates do not remove the taint. *)
        state
      else
        clear_taint state target
    in
    analyze_expression ~resolution ~taint ~state ~expression:value


  let analyze_statement ~resolution state { Node.value = statement; _ } =
    match statement with
    | Statement.Statement.Assign
        { value = { Node.value = Expression.Constant Constant.Ellipsis; _ }; _ } ->
        state
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
          analyze_expression ~resolution ~taint:BackwardState.Tree.bottom ~state ~expression:value
        else
          match target_value with
          | Expression.Name (Name.Attribute { base; attribute; _ }) ->
              let attribute_access_callees = get_attribute_access_callees ~location ~attribute in

              let property_call_state =
                match attribute_access_callees with
                | Some { property_targets = _ :: _ as property_targets; _ } ->
                    (* Treat `a.property = x` as `a = a.property(x)` *)
                    let taint = compute_assignment_taint ~resolution base state |> fst in
                    apply_callees
                      ~resolution
                      ~is_property:true
                      ~callee:target
                      ~call_location:location
                      ~arguments:[{ name = None; value }]
                      ~state
                      ~call_taint:taint
                      (CallGraph.CallCallees.create ~call_targets:property_targets ())
                | _ -> bottom
              in

              let attribute_state =
                match attribute_access_callees with
                | Some { is_attribute = true; _ }
                | None ->
                    analyze_assignment ~resolution ~target ~value state
                | _ -> bottom
              in

              join property_call_state attribute_state
          | _ -> analyze_assignment ~resolution ~target ~value state)
    | Assert { test; _ } ->
        analyze_expression ~resolution ~taint:BackwardState.Tree.empty ~state ~expression:test
    | Break
    | Class _
    | Continue ->
        state
    | Define define -> analyze_definition ~define state
    | Delete expressions ->
        let process_expression state expression =
          match AccessPath.of_expression expression with
          | Some { AccessPath.root; path } ->
              { taint = BackwardState.assign ~root ~path BackwardState.Tree.bottom state.taint }
          | _ -> state
        in
        List.fold expressions ~init:state ~f:process_expression
    | Expression expression ->
        analyze_expression ~resolution ~taint:BackwardState.Tree.empty ~state ~expression
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
        analyze_expression ~resolution ~taint:BackwardState.Tree.empty ~state ~expression
    | Return { expression = Some expression; _ } ->
        let access_path = { AccessPath.root = AccessPath.Root.LocalResult; path = [] } in
        let return_taint = get_taint (Some access_path) state in
        analyze_expression ~resolution ~taint:return_taint ~state ~expression
    | Return { expression = None; _ }
    | Try _
    | With _
    | While _ ->
        state


  let backward ~statement_key state ~statement =
    TaintProfiler.track_statement_analysis
      ~profiler
      ~analysis:TaintProfiler.Backward
      ~statement
      ~f:(fun () ->
        log
          "Backward analysis of statement: `%a`@,With backward state: %a"
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
        analyze_statement ~resolution state statement)


  let forward ~statement_key:_ _ ~statement:_ = failwith "Don't call me"
end

(* Split the inferred entry state into externally visible taint_in_taint_out parts and sink_taint. *)
let extract_tito_and_sink_models define ~is_constructor ~resolution ~existing_backward entry_taint =
  let { Statement.Define.signature = { parameters; _ }; _ } = define in
  let {
    TaintConfiguration.analysis_model_constraints =
      {
        maximum_model_width;
        maximum_return_access_path_length;
        maximum_trace_length;
        maximum_tito_depth;
        _;
      };
    _;
  }
    =
    TaintConfiguration.get ()
  in
  let normalized_parameters = AccessPath.Root.normalize_parameters parameters in
  (* Simplify trees by keeping only essential structure and merging details back into that. *)
  let simplify annotation tree =
    let type_breadcrumbs =
      annotation
      >>| GlobalResolution.parse_annotation resolution
      >>| CallGraph.ReturnType.from_annotation ~resolution
      |> Option.value ~default:CallGraph.ReturnType.none
      |> Features.type_breadcrumbs
    in

    let essential =
      if is_constructor then
        BackwardState.Tree.essential_for_constructor tree
      else
        BackwardState.Tree.essential tree
    in
    BackwardState.Tree.shape
      ~transform:(BackwardTaint.add_local_breadcrumbs (Features.widen_broadening_set ()))
      tree
      ~mold:essential
    |> BackwardState.Tree.add_local_breadcrumbs type_breadcrumbs
    |> BackwardState.Tree.limit_to
         ~transform:(BackwardTaint.add_local_breadcrumbs (Features.widen_broadening_set ()))
         ~width:maximum_model_width
    |> BackwardState.Tree.approximate_return_access_paths ~maximum_return_access_path_length
  in

  let split_and_simplify model (parameter, name, original) =
    let annotation = original.Node.value.Parameter.annotation in
    let partition =
      BackwardState.read ~root:(AccessPath.Root.Variable name) ~path:[] entry_taint
      |> BackwardState.Tree.partition BackwardTaint.kind By ~f:Sinks.discard_transforms
    in
    let taint_in_taint_out =
      let breadcrumbs_to_attach, via_features_to_attach =
        BackwardState.extract_features_to_attach
          ~root:parameter
          ~attach_to_kind:Sinks.Attach
          existing_backward.Model.Backward.taint_in_taint_out
      in
      let candidate_tree =
        Map.Poly.find partition Sinks.LocalReturn
        |> Option.value ~default:BackwardState.Tree.empty
        |> simplify annotation
      in
      let candidate_tree =
        match maximum_tito_depth with
        | Some maximum_tito_depth ->
            BackwardState.Tree.prune_maximum_length maximum_tito_depth candidate_tree
        | _ -> candidate_tree
      in
      let candidate_tree =
        candidate_tree
        |> BackwardState.Tree.add_local_breadcrumbs breadcrumbs_to_attach
        |> BackwardState.Tree.add_via_features via_features_to_attach
      in
      let number_of_paths =
        BackwardState.Tree.fold
          BackwardState.Tree.Path
          ~init:0
          ~f:(fun _ count -> count + 1)
          candidate_tree
      in
      if number_of_paths > TaintConfiguration.maximum_tito_leaves then
        BackwardState.Tree.collapse_to
          ~transform:(BackwardTaint.add_local_breadcrumbs (Features.widen_broadening_set ()))
          ~depth:0
          candidate_tree
      else
        candidate_tree
    in
    let sink_taint =
      let simplify_sink_taint ~key:sink ~data:sink_tree accumulator =
        match sink with
        | Sinks.LocalReturn
        (* For now, we don't propagate partial sinks at all. *)
        | Sinks.PartialSink _
        | Sinks.Attach ->
            accumulator
        | _ ->
            let sink_tree =
              match maximum_trace_length with
              | Some maximum_trace_length ->
                  BackwardState.Tree.prune_maximum_length maximum_trace_length sink_tree
              | _ -> sink_tree
            in
            simplify annotation sink_tree |> BackwardState.Tree.join accumulator
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


let run
    ?(profiler = TaintProfiler.none)
    ~environment
    ~class_interval_graph
    ~qualifier
    ~callable
    ~define
    ~call_graph_of_define
    ~get_callee_model
    ~existing_model
    ~triggered_sinks
    ()
  =
  let timer = Timer.start () in
  let define =
    (* Apply decorators to make sure we match parameters up correctly. *)
    let resolution = TypeEnvironment.ReadOnly.global_resolution environment in
    Annotated.Define.create define
    |> Annotated.Define.decorate ~resolution
    |> Annotated.Define.define
  in
  let module FunctionContext = struct
    let qualifier = qualifier

    let definition = define

    let debug = Statement.Define.dump define.value

    let profiler = profiler

    let environment = environment

    let class_interval_graph = class_interval_graph

    let call_graph_of_define = call_graph_of_define

    let get_callee_model = get_callee_model

    let triggered_sinks = triggered_sinks

    let caller_class_interval =
      Interprocedural.ClassIntervalSetGraph.SharedMemory.of_definition
        class_interval_graph
        definition
  end
  in
  let module State = State (FunctionContext) in
  let module Fixpoint = Analysis.Fixpoint.Make (State) in
  let initial = State.{ taint = initial_taint } in
  let cfg = Cfg.create define.value in
  let () =
    State.log "Backward analysis of callable: `%a`" Interprocedural.Target.pp_pretty callable
  in
  let entry_state =
    Metrics.with_alarm callable (fun () -> Fixpoint.backward ~cfg ~initial |> Fixpoint.entry) ()
  in
  let () =
    match entry_state with
    | Some entry_state -> State.log "Entry state:@,%a" State.pp entry_state
    | None -> State.log "No entry state found"
  in
  let resolution = TypeEnvironment.ReadOnly.global_resolution environment in
  let extract_model State.{ taint; _ } =
    let model =
      extract_tito_and_sink_models
        ~is_constructor:(State.is_constructor ())
        define.value
        ~resolution
        ~existing_backward:existing_model.Model.backward
        taint
    in
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

  entry_state >>| extract_model |> Option.value ~default:Model.Backward.empty
