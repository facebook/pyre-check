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

type triggered_sinks = (AccessPath.Root.t * Sinks.t) list Location.Table.t

module CallGraph = Interprocedural.CallGraph

module type FUNCTION_CONTEXT = sig
  val qualifier : Reference.t

  val definition : Statement.Define.t Node.t

  val debug : bool

  val profiler : TaintProfiler.t

  val environment : TypeEnvironment.ReadOnly.t

  val call_graph_of_define : CallGraph.DefineCallGraph.t

  val existing_model : Model.t

  val triggered_sinks : triggered_sinks
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
          Log.error
            "Could not find callees for `%a` at `%a:%a` in the call graph."
            Expression.pp
            (Node.create_with_default_location (Expression.Call call))
            Reference.pp
            FunctionContext.qualifier
            Location.pp
            location;
          CallGraph.CallCallees.create_unresolved Type.Any
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


  let candidates = Location.WithModule.Table.create ()

  let add_flow_candidate candidate =
    let key = candidate.Flow.location in
    let candidate =
      match Hashtbl.find candidates key with
      | Some { Flow.flows; location } ->
          { Flow.flows = List.rev_append candidate.Flow.flows flows; location }
      | None -> candidate
    in
    Location.WithModule.Table.set candidates ~key ~data:candidate


  let check_flow ~location ~source_tree ~sink_tree =
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
    let flow_candidate = Flow.generate_source_sink_matches ~location ~source_tree ~sink_tree in
    add_flow_candidate flow_candidate


  let check_triggered_flows ~triggered_sinks ~location ~source_tree ~sink_tree =
    let triggered, candidates =
      Flow.compute_triggered_sinks ~triggered_sinks ~location ~source_tree ~sink_tree
    in
    List.iter triggered ~f:(fun sink -> Hash_set.add triggered_sinks (Sinks.show_partial_sink sink));
    List.iter candidates ~f:add_flow_candidate


  let generate_issues () =
    let accumulate ~key:_ ~data:candidate issues =
      let new_issues = Flow.generate_issues ~define:FunctionContext.definition candidate in
      List.rev_append new_issues issues
    in
    Location.WithModule.Table.fold candidates ~f:accumulate ~init:[]


  let return_sink ~return_location =
    let taint =
      BackwardState.read
        ~root:AccessPath.Root.LocalResult
        ~path:[]
        FunctionContext.existing_model.Model.backward.sink_taint
      |> BackwardState.Tree.apply_call return_location ~callees:[] ~port:AccessPath.Root.LocalResult
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


  let add_triggered_sinks ~location ~triggered_sinks:new_triggered_sinks =
    Hashtbl.set FunctionContext.triggered_sinks ~key:location ~data:new_triggered_sinks


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


    let remove map ~kind = Map.Poly.remove map kind

    let get map ~kind = Map.Poly.find map kind |> Option.value ~default:ForwardState.Tree.empty

    let fold map ~init ~f = Map.Poly.fold map ~init ~f:(fun ~key ~data -> f ~kind:key ~taint:data)
  end

  let apply_call_target
      ~resolution
      ~triggered_sinks
      ~call_location
      ~self
      ~self_taint
      ~callee
      ~callee_taint
      ~arguments
      ~arguments_taint
      ~return_type_breadcrumbs
      ~state:initial_state
      {
        CallGraph.CallTarget.target = call_target;
        implicit_self;
        implicit_dunder_call;
        collapse_tito;
      }
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
    let ({ Model.forward; backward; modes; _ } as taint_model) =
      TaintProfiler.track_model_fetch
        ~profiler
        ~analysis:TaintProfiler.Forward
        ~call_target
        ~f:(fun () -> CallModel.at_callsite ~resolution ~call_target ~arguments)
    in
    log
      "Forward analysis of call to `%a` with arguments (%a)@,Call site model:@,%a"
      Interprocedural.Target.pretty_print
      call_target
      Ast.Expression.pp_expression_argument_list
      arguments
      Model.pp
      taint_model;
    let convert_tito_path_to_taint
        ~argument
        ~argument_taint
        ~kind
        (path, tito_taint)
        accumulated_tito
      =
      let breadcrumbs =
        BackwardTaint.joined_breadcrumbs tito_taint |> Features.BreadcrumbSet.add (Features.tito ())
      in
      let taint_to_propagate =
        if collapse_tito then
          ForwardState.Tree.read path argument_taint
          |> ForwardState.Tree.collapse
               ~transform:(ForwardTaint.add_local_breadcrumbs (Features.tito_broadening_set ()))
          |> ForwardTaint.transform Features.TitoPositionSet.Element Add ~f:argument.Node.location
          |> ForwardTaint.add_local_breadcrumbs breadcrumbs
          |> ForwardState.Tree.create_leaf
        else
          ForwardState.Tree.read path argument_taint
          |> ForwardState.Tree.transform
               Features.TitoPositionSet.Element
               Add
               ~f:argument.Node.location
          |> ForwardState.Tree.add_local_breadcrumbs breadcrumbs
      in
      let taint_to_propagate =
        match kind with
        | Sinks.Transform { sanitize_local; sanitize_global; _ } ->
            (* Apply source- and sink- specific tito sanitizers. *)
            let transforms = SanitizeTransform.Set.union sanitize_local sanitize_global in
            let sanitized_tito_sources =
              Sources.extract_sanitized_sources_from_transforms transforms
            in
            let sanitized_tito_sinks = SanitizeTransform.Set.filter_sinks transforms in
            taint_to_propagate
            |> ForwardState.Tree.sanitize sanitized_tito_sources
            |> ForwardState.Tree.apply_sanitize_transforms sanitized_tito_sinks
            |> ForwardState.Tree.transform ForwardTaint.kind Filter ~f:Flow.source_can_match_rule
        | _ -> taint_to_propagate
      in
      let create_tito_return_paths tito return_path =
        ForwardState.Tree.prepend return_path taint_to_propagate |> ForwardState.Tree.join tito
      in
      CallModel.return_paths ~kind ~tito_taint
      |> List.fold ~f:create_tito_return_paths ~init:accumulated_tito
    in
    let convert_tito_tree_to_taint ~argument ~argument_taint ~kind ~tito_tree tito_effects =
      let tito_tree =
        BackwardState.Tree.fold
          BackwardState.Tree.Path
          tito_tree
          ~init:ForwardState.Tree.empty
          ~f:(convert_tito_path_to_taint ~argument ~argument_taint ~kind)
      in
      TaintInTaintOutEffects.add tito_effects ~kind:(Sinks.discard_transforms kind) ~taint:tito_tree
    in
    let compute_argument_tito_effect
        (tito_effects, state)
        ( argument_taint,
          { CallModel.ArgumentMatches.argument; sink_matches; tito_matches; sanitize_matches } )
      =
      let tito_effects =
        CallModel.taint_in_taint_out_mapping
          ~transform_non_leaves:(fun _ tito -> tito)
          ~model:taint_model
          ~tito_matches
        |> CallModel.TaintInTaintOutMap.fold
             ~init:tito_effects
             ~f:(convert_tito_tree_to_taint ~argument ~argument_taint)
      in
      let tito_effects =
        if Model.ModeSet.contains Obscure modes then
          (* Apply source- and sink- specific tito sanitizers for obscure models,
           * since the tito is not materialized in `backward.taint_in_taint_out`. *)
          let obscure_sanitize =
            CallModel.sanitize_of_argument ~model:taint_model ~sanitize_matches
          in
          match obscure_sanitize.tito with
          | Some AllTito -> TaintInTaintOutEffects.remove tito_effects ~kind:Sinks.LocalReturn
          | Some (SpecificTito { sanitized_tito_sources; sanitized_tito_sinks }) ->
              let apply_taint_transforms = function
                | None -> ForwardState.Tree.bottom
                | Some taint_tree ->
                    let sanitized_tito_sinks =
                      Sinks.Set.to_sanitize_transforms_exn sanitized_tito_sinks
                    in
                    taint_tree
                    |> ForwardState.Tree.sanitize sanitized_tito_sources
                    |> ForwardState.Tree.apply_sanitize_transforms sanitized_tito_sinks
                    |> ForwardState.Tree.transform
                         ForwardTaint.kind
                         Filter
                         ~f:Flow.source_can_match_rule
              in
              TaintInTaintOutEffects.update
                tito_effects
                ~kind:Sinks.LocalReturn
                ~f:apply_taint_transforms
          | None -> tito_effects
        else
          tito_effects
      in
      let location =
        Location.with_module ~qualifier:FunctionContext.qualifier argument.Node.location
      in
      let sink_tree =
        CallModel.sink_tree_of_argument
          ~transform_non_leaves:(fun _ tree -> tree)
          ~model:taint_model
          ~location
          ~call_target
          ~sink_matches
      in
      (* Compute triggered partial sinks, if any. *)
      let () =
        check_triggered_flows ~triggered_sinks ~location ~source_tree:argument_taint ~sink_tree
      in

      (* Add features to arguments. *)
      let state =
        match AccessPath.of_expression argument with
        | Some { AccessPath.root; path } ->
            let breadcrumbs_to_add =
              BackwardState.Tree.filter_by_kind ~kind:Sinks.AddFeatureToArgument sink_tree
              |> BackwardTaint.accumulated_breadcrumbs
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
      check_flow ~location ~source_tree:argument_taint ~sink_tree;
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
      ForwardState.read ~root:AccessPath.Root.LocalResult ~path:[] forward.source_taint
      |> ForwardState.Tree.apply_call
           (Location.with_module ~qualifier:FunctionContext.qualifier call_location)
           ~callees:[call_target]
           ~port:AccessPath.Root.LocalResult
    in
    let tito = TaintInTaintOutEffects.get tito_effects ~kind:Sinks.LocalReturn in
    (if not (Hash_set.is_empty triggered_sinks) then
       let add_sink (key, taint) roots_and_sinks =
         let add roots_and_sinks sink =
           match Sinks.extract_partial_sink sink with
           | Some sink ->
               if Hash_set.mem triggered_sinks (Sinks.show_partial_sink sink) then
                 (key, Sinks.TriggeredPartialSink sink) :: roots_and_sinks
               else
                 roots_and_sinks
           | None -> roots_and_sinks
         in
         BackwardTaint.kinds
           (BackwardState.Tree.collapse
              ~transform:(BackwardTaint.add_local_breadcrumbs (Features.issue_broadening_set ()))
              taint)
         |> List.fold ~f:add ~init:roots_and_sinks
       in
       let triggered_sinks =
         BackwardState.fold BackwardState.KeyValue backward.sink_taint ~init:[] ~f:add_sink
       in
       add_triggered_sinks ~location:call_location ~triggered_sinks);
    let apply_tito_side_effects tito_effects state =
      (* We also have to consider the cases when the updated parameter has a global model, in which
         case we need to capture the flow. *)
      let apply_argument_effect ~argument:{ Call.Argument.value = argument; _ } ~source_tree state =
        let sink_tree =
          GlobalModel.from_expression
            ~resolution
            ~call_graph:FunctionContext.call_graph_of_define
            ~qualifier:FunctionContext.qualifier
            ~expression:argument
          |> GlobalModel.get_sink
        in
        check_flow
          ~location:
            (Location.with_module ~qualifier:FunctionContext.qualifier argument.Node.location)
          ~source_tree
          ~sink_tree;
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
        | Attach -> state (* These synthetic nodes should be ignored for analysis.*)
        | _ -> Format.asprintf "unexpected sink `%a` in tito" Sinks.pp target |> failwith
      in
      TaintInTaintOutEffects.fold tito_effects ~f:for_each_target ~init:state
    in
    let returned_taint = ForwardState.Tree.join result_taint tito in
    let returned_taint =
      if Model.ModeSet.contains Obscure modes then
        ForwardState.Tree.add_local_breadcrumbs (Lazy.force return_type_breadcrumbs) returned_taint
      else
        returned_taint
    in
    let state = apply_tito_side_effects tito_effects state in
    returned_taint, state


  let apply_obscure_call ~callee ~callee_taint ~arguments ~arguments_taint ~state:initial_state =
    log
      "Forward analysis of obscure call to `%a` with arguments (%a)"
      Expression.pp
      callee
      Ast.Expression.pp_expression_argument_list
      arguments;
    let callee_taint =
      Option.value_exn callee_taint
      |> ForwardState.Tree.transform Features.TitoPositionSet.Element Add ~f:callee.Node.location
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
      |> ForwardState.Tree.transform Features.TitoPositionSet.Element Add ~f:argument.Node.location
      |> ForwardState.Tree.join taint_accumulator
    in
    let taint =
      List.zip_exn arguments arguments_taint
      |> List.fold ~f:analyze_argument ~init:callee_taint
      |> ForwardState.Tree.add_local_breadcrumb (Features.obscure_unknown_callee ())
    in
    taint, initial_state


  let apply_constructor_targets
      ~resolution
      ~triggered_sinks
      ~call_location
      ~callee
      ~callee_taint
      ~arguments
      ~arguments_taint
      ~new_targets
      ~init_targets
      ~return_type_breadcrumbs
      ~state:initial_state
    =
    (* Call `__new__`. Add the `cls` implicit argument. *)
    let new_return_taint, state =
      match new_targets with
      | [] -> ForwardState.Tree.bottom, initial_state
      | [`Method { Interprocedural.Target.class_name = "object"; method_name = "__new__" }] ->
          ForwardState.Tree.bottom, initial_state
      | new_targets ->
          List.map new_targets ~f:(fun target ->
              apply_call_target
                ~resolution
                ~triggered_sinks
                ~call_location
                ~self:(Some callee)
                ~self_taint:callee_taint
                ~callee
                ~callee_taint:None
                ~arguments
                ~arguments_taint
                ~return_type_breadcrumbs
                ~state:initial_state
                {
                  CallGraph.CallTarget.target;
                  implicit_self = true;
                  implicit_dunder_call = false;
                  collapse_tito = true;
                })
          |> List.fold
               ~init:(ForwardState.Tree.empty, bottom)
               ~f:(fun (taint, state) (new_taint, new_state) ->
                 ForwardState.Tree.join taint new_taint, join state new_state)
    in

    (* Call `__init__`. Add the `self` implicit argument. *)
    let taint, state =
      match init_targets with
      | [] -> new_return_taint, state
      | init_targets ->
          let call_expression =
            Expression.Call { Call.callee; arguments } |> Node.create ~location:call_location
          in
          List.map init_targets ~f:(fun target ->
              apply_call_target
                ~resolution
                ~triggered_sinks
                ~call_location
                ~self:(Some call_expression)
                ~self_taint:(Some new_return_taint)
                ~callee
                ~callee_taint:None
                ~arguments
                ~arguments_taint
                ~return_type_breadcrumbs
                ~state
                {
                  CallGraph.CallTarget.target;
                  implicit_self = true;
                  implicit_dunder_call = false;
                  collapse_tito = true;
                })
          |> List.fold
               ~init:(ForwardState.Tree.empty, bottom)
               ~f:(fun (taint, state) (new_taint, new_state) ->
                 ForwardState.Tree.join taint new_taint, join state new_state)
    in

    taint, state


  let apply_callees_with_arguments_taint
      ~resolution
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
        return_type;
        higher_order_parameter = _;
        unresolved;
      }
    =
    (* We keep a table of kind -> set of triggered labels across all targets,
     * and merge triggered sinks at the end. *)
    let triggered_sinks = String.Hash_set.create () in

    let call_targets =
      (* Special handling for the missing-flow analysis. *)
      if unresolved && TaintConfiguration.is_missing_flow_analysis Type then (
        let callable =
          MissingFlow.unknown_callee
            ~location:(Location.with_module ~qualifier:FunctionContext.qualifier call_location)
            ~call:(Expression.Call { Call.callee; arguments })
        in
        if not (Interprocedural.FixpointState.has_model callable) then
          MissingFlow.register_unknown_callee_model callable;
        let target =
          {
            CallGraph.CallTarget.target = callable;
            implicit_self = false;
            implicit_dunder_call = false;
            collapse_tito = true;
          }
        in
        target :: call_targets)
      else
        call_targets
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

    let return_type_breadcrumbs =
      lazy
        (let resolution = Resolution.global_resolution resolution in
         Features.type_breadcrumbs ~resolution (Some return_type))
    in

    (* Apply regular call targets. *)
    let taint, state =
      List.map
        call_targets
        ~f:
          (apply_call_target
             ~resolution
             ~triggered_sinks
             ~call_location
             ~self
             ~self_taint
             ~callee
             ~callee_taint
             ~arguments
             ~arguments_taint
             ~return_type_breadcrumbs
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
          apply_obscure_call ~callee ~callee_taint ~arguments ~arguments_taint ~state:initial_state
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
              ~triggered_sinks
              ~call_location
              ~callee
              ~callee_taint
              ~arguments
              ~arguments_taint
              ~new_targets
              ~init_targets
              ~return_type_breadcrumbs
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
            ~location:callee.Node.location
            ~resolve_properties
            ~base
            ~attribute
            ~special
        in
        { self_taint = Some base_taint; callee_taint = Some attribute_taint; state }
    | _ ->
        let taint, state = analyze_expression ~resolution ~state ~expression:callee in
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
              analyze_expression ~resolution ~state ~expression:base
          | _ -> ForwardState.Tree.bottom, state
        in
        { self_taint = Some taint; callee_taint = None; state }
    | _ ->
        (* We can ignore the callee entirely. *)
        { self_taint = None; callee_taint = None; state }


  and analyze_arguments ~resolution ~state ~arguments =
    let compute_argument_taint (arguments_taint, state) argument =
      let taint, state =
        analyze_unstarred_expression ~resolution argument.Call.Argument.value state
      in
      taint :: arguments_taint, state
    in
    (* Explicitly analyze arguments from left to right. *)
    let arguments_taint, state = List.fold ~init:([], state) ~f:compute_argument_taint arguments in
    List.rev arguments_taint, state


  and analyze_dictionary_entry ~resolution (taint, state) { Dictionary.Entry.key; value } =
    let field_name =
      match key.Node.value with
      | Constant (Constant.String literal) -> Abstract.TreeDomain.Label.Index literal.value
      | _ -> Abstract.TreeDomain.Label.AnyIndex
    in
    let key_taint, state =
      analyze_expression ~resolution ~state ~expression:key
      |>> ForwardState.Tree.prepend [AccessPath.dictionary_keys]
    in
    analyze_expression ~resolution ~state ~expression:value
    |>> ForwardState.Tree.prepend [field_name]
    |>> ForwardState.Tree.join taint
    |>> ForwardState.Tree.join key_taint


  and analyze_list_element ~resolution position (taint, state) expression =
    let index_name = Abstract.TreeDomain.Label.Index (string_of_int position) in
    analyze_expression ~resolution ~state ~expression
    |>> ForwardState.Tree.prepend [index_name]
    |>> ForwardState.Tree.join taint


  and analyze_set_element ~resolution (taint, state) expression =
    let value_taint, state =
      analyze_expression ~resolution ~state ~expression
      |>> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]
    in
    ForwardState.Tree.join taint value_taint, state


  and analyze_comprehension_generators ~resolution ~state generators =
    let add_binding (state, resolution) ({ Comprehension.Generator.conditions; _ } as generator) =
      let ({ Statement.Assign.target; value; _ } as assignment) =
        Statement.Statement.generator_assignment generator
      in
      let assign_value_taint, state = analyze_expression ~resolution ~state ~expression:value in
      let state =
        analyze_assignment ~resolution target assign_value_taint assign_value_taint state
      in
      (* Since generators create variables that Pyre sees as scoped within the generator, handle
         them by adding the generator's bindings to the resolution. *)
      let resolution = Resolution.resolve_assignment resolution assignment in
      (* Analyzing the conditions might have issues and side effects. *)
      let analyze_condition state condiiton =
        analyze_expression ~resolution ~state ~expression:condiiton |> snd
      in
      List.fold conditions ~init:state ~f:analyze_condition, resolution
    in
    List.fold ~f:add_binding generators ~init:(state, resolution)


  and analyze_comprehension ~resolution { Comprehension.element; generators; _ } state =
    let bound_state, resolution = analyze_comprehension_generators ~resolution ~state generators in
    analyze_expression ~resolution ~state:bound_state ~expression:element
    |>> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]


  and analyze_dictionary_comprehension
      ~resolution
      ~state
      { Comprehension.element = { Dictionary.Entry.key; value }; generators; _ }
    =
    let state, resolution = analyze_comprehension_generators ~resolution ~state generators in
    let value_taint, state =
      analyze_expression ~resolution ~state ~expression:value
      |>> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]
    in
    let key_taint, state =
      analyze_expression ~resolution ~state ~expression:key
      |>> ForwardState.Tree.prepend [AccessPath.dictionary_keys]
    in
    ForwardState.Tree.join key_taint value_taint, state


  (* Skip through * and **. Used at call sites where * and ** are handled explicitly *)
  and analyze_unstarred_expression ~resolution expression state =
    match expression.Node.value with
    | Starred (Starred.Once expression)
    | Starred (Starred.Twice expression) ->
        analyze_expression ~resolution ~state ~expression
    | _ -> analyze_expression ~resolution ~state ~expression


  and analyze_arguments_for_lambda_call
      ~resolution
      ~arguments
      ~state
      ~lambda_argument:
        { Call.Argument.value = { location = lambda_location; _ } as lambda_callee; _ }
      { CallGraph.HigherOrderParameter.index = lambda_index; call_targets; return_type }
    =
    (* If we have a lambda `fn` getting passed into `hof`, we use the following strategy:
     * hof(q, fn, x, y) gets translated into the following block: (analyzed forwards)
     * if rand():
     *   $all = {q, x, y}
     *   $result = fn( *all, **all)
     * else:
     *   $result = fn
     * hof(q, $result, x, y)
     *)
    let non_lambda_arguments =
      List.mapi ~f:(fun index value -> index, value) (List.take arguments lambda_index)
      @ List.mapi
          ~f:(fun relative_index value -> lambda_index + 1 + relative_index, value)
          (List.drop arguments (lambda_index + 1))
    in

    (* Analyze all arguments once. *)
    let { self_taint = lambda_self_taint; callee_taint = lambda_taint; state } =
      analyze_callee ~resolution ~is_property_call:false ~state ~callee:lambda_callee
    in
    let lambda_taint = Option.value_exn lambda_taint in
    let non_lambda_arguments_taint, state =
      let compute_argument_taint (arguments_taint, state) (index, argument) =
        let taint, state =
          analyze_unstarred_expression ~resolution argument.Call.Argument.value state
        in
        (index, taint) :: arguments_taint, state
      in
      List.fold ~init:([], state) ~f:compute_argument_taint non_lambda_arguments
    in

    (* Simulate if branch. *)
    let if_branch_result_taint, if_branch_state =
      (* Simulate `$all = {q, x, y}`. *)
      let all_argument =
        Expression.Name (Name.Identifier "$all") |> Node.create ~location:lambda_location
      in
      let all_argument_taint =
        List.fold
          non_lambda_arguments_taint
          ~f:(fun taint (_, argument_taint) -> ForwardState.Tree.join taint argument_taint)
          ~init:ForwardState.Tree.empty
        |> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]
      in

      (* Simulate `$result = fn( *all, **all)`. *)
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
      let arguments_taint = [all_argument_taint; all_argument_taint] in
      let taint, state =
        apply_callees_with_arguments_taint
          ~resolution
          ~callee:lambda_callee
          ~call_location:lambda_location
          ~arguments
          ~self_taint:lambda_self_taint
          ~callee_taint:(Some lambda_taint)
          ~arguments_taint
          ~state
          (CallGraph.CallCallees.create ~call_targets ~return_type ())
      in
      let taint = ForwardState.Tree.add_local_breadcrumb (Features.lambda ()) taint in
      taint, state
    in

    (* Simulate else branch: nothing to do. *)
    (* Join both branches. *)
    let result_taint = ForwardState.Tree.join if_branch_result_taint lambda_taint in
    let state = join state if_branch_state in

    (* Return the arguments taint. The caller will simulate `hof(q, $result, x, y)`. *)
    let index_map_to_ordered_list map =
      let compare_by_index (left_index, _) (right_index, _) = Int.compare left_index right_index in
      map |> List.sort ~compare:compare_by_index |> List.map ~f:snd
    in
    let arguments_taint =
      index_map_to_ordered_list ((lambda_index, result_taint) :: non_lambda_arguments_taint)
    in
    arguments_taint, state


  and apply_callees ~resolution ~is_property ~callee ~call_location ~arguments ~state callees =
    let { self_taint; callee_taint; state } =
      analyze_callee_for_callees ~resolution ~is_property_call:is_property ~state ~callee callees
    in

    let arguments_taint, state =
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
                ~state
                ~lambda_argument
                higher_order_parameter
          | _ -> analyze_arguments ~resolution ~state ~arguments)
      | _ -> analyze_arguments ~resolution ~state ~arguments
    in

    apply_callees_with_arguments_taint
      ~resolution
      ~callee
      ~call_location
      ~arguments
      ~self_taint
      ~callee_taint
      ~arguments_taint
      ~state
      callees


  and analyze_call ~resolution ~location ~state ~callee ~arguments =
    let assign_super_constructor_taint_to_self_if_necessary taint state =
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
          } ) ->
          if
            is_constructor ()
            && Interprocedural.CallResolution.is_super
                 ~resolution
                 ~define:FunctionContext.definition
                 base
          then
            let self = AccessPath.Root.Variable self_parameter in
            let self_taint =
              ForwardState.read ~root:self ~path:[] state.taint |> ForwardState.Tree.join taint
            in
            taint, { taint = ForwardState.assign ~root:self ~path:[] self_taint state.taint }
          else
            taint, state
      | _ -> taint, state
    in

    let taint, state =
      match { Call.callee; arguments } with
      | {
       callee = { Node.value = Name (Name.Attribute { base; attribute = "__getitem__"; _ }); _ };
       arguments = [{ Call.Argument.value = argument_value; _ }];
      } ->
          let index = AccessPath.get_index argument_value in
          analyze_expression ~resolution ~state ~expression:base
          |>> ForwardState.Tree.read [index]
          |>> ForwardState.Tree.add_local_first_index index
      (* We read the taint at the `__iter__` call to be able to properly reference key taint as
         appropriate. *)
      | {
       callee = { Node.value = Name (Name.Attribute { base; attribute = "__next__"; _ }); _ };
       arguments = [];
      } ->
          analyze_expression ~resolution ~state ~expression:base
      | {
       callee =
         { Node.value = Name (Name.Attribute { base; attribute = "__iter__"; special = true }); _ };
       arguments = [];
      } ->
          let taint, state = analyze_expression ~resolution ~state ~expression:base in
          let label =
            (* For dictionaries, the default iterator is keys. *)
            if
              Resolution.resolve_expression_to_type resolution base |> Type.is_dictionary_or_mapping
            then
              AccessPath.dictionary_keys
            else
              Abstract.TreeDomain.Label.AnyIndex
          in
          ForwardState.Tree.read [label] taint, state
      (* x[0] = value is converted to x.__setitem__(0, value). in parsing. *)
      | {
       callee =
         { Node.value = Name (Name.Attribute { base; attribute = "__setitem__"; _ }); _ } as callee;
       arguments = [{ Call.Argument.value = index; _ }; { Call.Argument.value; _ }] as arguments;
      } ->
          let taint, state = analyze_expression ~resolution ~state ~expression:value in
          let state =
            analyze_assignment
              ~resolution
              ~fields:[AccessPath.get_index index]
              base
              taint
              taint
              state
          in
          (* Also make sure we analyze the __setitem__ call in case the __setitem__ function body is
             tainted. *)
          let callees = get_call_callees ~location ~call:{ Call.callee; arguments } in
          if CallGraph.CallCallees.is_partially_resolved callees then
            apply_callees
              ~resolution
              ~is_property:false
              ~callee
              ~call_location:location
              ~arguments
              ~state
              callees
          else
            taint, state
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
          let taint, state = analyze_expression ~resolution ~state ~expression:assigned_value in
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
          let attribute_taint, state =
            analyze_expression ~resolution ~state ~expression:attribute_expression
          in
          let default_taint, state = analyze_expression ~resolution ~state ~expression:default in
          ForwardState.Tree.join attribute_taint default_taint, state
      (* `zip(a, b, ...)` creates a taint object which, when iterated on, has first index equal to
         a[*]'s taint, second index with b[*]'s taint, etc. *)
      | { callee = { Node.value = Name (Name.Identifier "zip"); _ }; arguments = lists } ->
          let add_list_to_taint index (taint, state) { Call.Argument.value; _ } =
            let index_name = Abstract.TreeDomain.Label.Index (string_of_int index) in
            analyze_expression ~resolution ~state ~expression:value
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
            ~expression:
              {
                Node.location = Node.location callee;
                value =
                  Expression.Tuple
                    (List.map arguments ~f:(fun argument -> argument.Call.Argument.value));
              }
      (* dictionary .keys(), .values() and .items() functions are special, as they require handling
         of dictionary_keys taint. *)
      | { callee = { Node.value = Name (Name.Attribute { base; attribute = "values"; _ }); _ }; _ }
        when Resolution.resolve_expression_to_type resolution base |> Type.is_dictionary_or_mapping
        ->
          analyze_expression ~resolution ~state ~expression:base
          |>> ForwardState.Tree.read [Abstract.TreeDomain.Label.AnyIndex]
          |>> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]
      | { callee = { Node.value = Name (Name.Attribute { base; attribute = "keys"; _ }); _ }; _ }
        when Resolution.resolve_expression_to_type resolution base |> Type.is_dictionary_or_mapping
        ->
          analyze_expression ~resolution ~state ~expression:base
          |>> ForwardState.Tree.read [AccessPath.dictionary_keys]
          |>> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]
      | { callee = { Node.value = Name (Name.Attribute { base; attribute = "items"; _ }); _ }; _ }
        when Resolution.resolve_expression_to_type resolution base |> Type.is_dictionary_or_mapping
        ->
          let taint, state = analyze_expression ~resolution ~state ~expression:base in
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
      | {
       callee = { Node.value = Expression.Name (Name.Identifier "reveal_taint"); _ };
       arguments = [{ Call.Argument.value = expression; _ }];
      } ->
          let taint, _ = analyze_expression ~resolution ~state ~expression in
          let location =
            Node.location callee |> Location.with_module ~qualifier:FunctionContext.qualifier
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
            Node.location callee |> Location.with_module ~qualifier:FunctionContext.qualifier
          in
          Log.dump
            "%a: Revealed type for %s: %s"
            Location.WithModule.pp
            location
            (Transform.sanitize_expression expression |> Expression.show)
            (Resolution.resolve_expression_to_type resolution expression |> Type.show);
          ForwardState.Tree.bottom, state
      | _ ->
          let call = { Call.callee; arguments } in
          let { Call.callee; arguments } = CallGraph.redirect_special_calls ~resolution call in
          let callees = get_call_callees ~location ~call:{ Call.callee; arguments } in
          let taint, state =
            apply_callees
              ~resolution
              ~is_property:false
              ~call_location:location
              ~callee
              ~arguments
              ~state
              callees
          in
          let taint =
            match Node.value callee with
            | Name
                (Name.Attribute
                  {
                    base = { Node.value = Expression.Constant (Constant.String _); _ };
                    attribute = "format";
                    _;
                  }) ->
                ForwardState.Tree.add_local_breadcrumb (Features.format_string ()) taint
            | _ -> taint
          in
          taint, state
    in
    let taint, state = assign_super_constructor_taint_to_self_if_necessary taint state in
    let configuration = TaintConfiguration.get () in
    if not configuration.lineage_analysis then
      taint, state
    else
      let analyze_expression_unwrap ~resolution ~state ~expression =
        let taint, state = analyze_expression ~resolution ~state:{ taint = state } ~expression in
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
      ~location
      ~resolve_properties
      ~base
      ~attribute
      ~special
    =
    let expression =
      Expression.Name (Name.Attribute { base; attribute; special }) |> Node.create ~location
    in
    let base_taint, state = analyze_expression ~resolution ~state ~expression:base in
    let attribute_access_callees =
      if resolve_properties then get_attribute_access_callees ~location ~attribute else None
    in

    let property_call_result =
      match attribute_access_callees with
      | Some { property_targets = _ :: _ as property_targets; return_type; _ } ->
          let call_targets =
            List.map
              ~f:(fun target ->
                {
                  CallGraph.CallTarget.target;
                  implicit_self = true;
                  implicit_dunder_call = false;
                  collapse_tito = true;
                })
              property_targets
          in
          let taint, state =
            apply_callees_with_arguments_taint
              ~resolution
              ~callee:expression
              ~call_location:location
              ~arguments:[]
              ~self_taint:(Some base_taint)
              ~callee_taint:None
              ~arguments_taint:[]
              ~state
              (CallGraph.CallCallees.create ~call_targets ~return_type ())
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
              ~qualifier:FunctionContext.qualifier
              ~expression
          in
          let attribute_taint = GlobalModel.get_source global_model in
          let add_tito_features taint =
            let attribute_breadcrumbs =
              global_model |> GlobalModel.get_tito |> BackwardState.Tree.accumulated_breadcrumbs
            in
            ForwardState.Tree.add_local_breadcrumbs attribute_breadcrumbs taint
          in
          let apply_attribute_sanitizers taint =
            let sanitizer = GlobalModel.get_sanitize global_model in
            let taint =
              match sanitizer.sources with
              | Some AllSources -> ForwardState.Tree.empty
              | Some (SpecificSources sanitized_sources) ->
                  ForwardState.Tree.sanitize sanitized_sources taint
              | None -> taint
            in
            let taint =
              match sanitizer.sinks with
              | Some (SpecificSinks sanitized_sinks) ->
                  let sanitized_sinks_transforms =
                    Sinks.Set.to_sanitize_transforms_exn sanitized_sinks
                  in
                  taint
                  |> ForwardState.Tree.apply_sanitize_transforms sanitized_sinks_transforms
                  |> ForwardState.Tree.transform
                       ForwardTaint.kind
                       Filter
                       ~f:Flow.source_can_match_rule
              | _ -> taint
            in
            taint
          in
          let attribute_taint =
            base_taint
            |> add_tito_features
            |> ForwardState.Tree.read [Abstract.TreeDomain.Label.Index attribute]
            |> ForwardState.Tree.add_local_first_field attribute
            (* This should be applied before the join with the attribute taint, so inferred taint
             * is sanitized, but user-specified taint on the attribute is still propagated. *)
            |> apply_attribute_sanitizers
            |> ForwardState.Tree.join attribute_taint
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


  and analyze_string_literal ~resolution ~state ~location ~nested_expressions value =
    let location = Location.with_module ~qualifier:FunctionContext.qualifier location in
    let value_taint =
      let literal_string_regular_expressions = TaintConfiguration.literal_string_sources () in
      if List.is_empty literal_string_regular_expressions then
        ForwardState.Tree.empty
      else
        let add_matching_source_kind tree { TaintConfiguration.pattern; source_kind = kind } =
          if Re2.matches pattern value then
            ForwardTaint.singleton ~location kind Frame.initial
            |> ForwardState.Tree.create_leaf
            |> ForwardState.Tree.join tree
          else
            tree
        in
        List.fold
          literal_string_regular_expressions
          ~init:ForwardState.Tree.empty
          ~f:add_matching_source_kind
    in
    match nested_expressions with
    | [] -> value_taint, state
    | _ ->
        let taint, state =
          List.fold
            nested_expressions
            ~f:(fun (taint, state) expression ->
              analyze_expression ~resolution ~state ~expression |>> ForwardState.Tree.join taint)
            ~init:(ForwardState.Tree.empty, state)
          |>> ForwardState.Tree.add_local_breadcrumb (Features.format_string ())
          |>> ForwardState.Tree.join value_taint
        in
        (* Compute flows of user-controlled data -> literal string sinks if applicable. *)
        let () =
          let literal_string_sinks = TaintConfiguration.literal_string_sinks () in
          (* We try to be a bit clever about bailing out early and not computing the matches. *)
          if (not (List.is_empty literal_string_sinks)) && not (ForwardState.Tree.is_bottom taint)
          then
            let backwards_taint =
              List.fold
                literal_string_sinks
                ~f:(fun taint { TaintConfiguration.sink_kind; pattern } ->
                  if Re2.matches pattern value then
                    BackwardTaint.singleton ~location sink_kind Frame.initial
                    |> BackwardState.Tree.create_leaf
                    |> BackwardState.Tree.join taint
                  else
                    taint)
                ~init:BackwardState.Tree.bottom
            in
            check_flow ~location ~source_tree:taint ~sink_tree:backwards_taint
        in
        taint, state


  and analyze_expression ~resolution ~state ~expression:({ Node.location; _ } as expression) =
    let taint, state =
      match expression.Node.value with
      | Await expression -> analyze_expression ~resolution ~state ~expression
      | BooleanOperator { left; operator = _; right } ->
          let left_taint, state = analyze_expression ~resolution ~state ~expression:left in
          let right_taint, state = analyze_expression ~resolution ~state ~expression:right in
          ForwardState.Tree.join left_taint right_taint, state
      | ComparisonOperator ({ left; operator = _; right } as comparison) -> (
          match ComparisonOperator.override ~location comparison with
          | Some override -> analyze_expression ~resolution ~state ~expression:override
          | None ->
              let left_taint, state = analyze_expression ~resolution ~state ~expression:left in
              let right_taint, state = analyze_expression ~resolution ~state ~expression:right in
              let taint =
                ForwardState.Tree.join left_taint right_taint
                |> ForwardState.Tree.add_local_breadcrumbs (Features.type_bool_scalar_set ())
              in
              taint, state)
      | Call { callee; arguments } -> analyze_call ~resolution ~location ~state ~callee ~arguments
      | Constant (Constant.String { StringLiteral.value; _ }) ->
          analyze_string_literal ~resolution ~state ~location ~nested_expressions:[] value
      | Constant _ -> ForwardState.Tree.empty, state
      | Dictionary { Dictionary.entries; keywords } ->
          let taint, state =
            List.fold
              entries
              ~f:(analyze_dictionary_entry ~resolution)
              ~init:(ForwardState.Tree.empty, state)
          in
          let analyze_dictionary_keywords (taint, state) keywords =
            let new_taint, state = analyze_expression ~resolution ~state ~expression:keywords in
            ForwardState.Tree.join new_taint taint, state
          in
          List.fold keywords ~f:analyze_dictionary_keywords ~init:(taint, state)
      | DictionaryComprehension comprehension ->
          analyze_dictionary_comprehension ~resolution ~state comprehension
      | Generator comprehension -> analyze_comprehension ~resolution comprehension state
      | Lambda { parameters = _; body } ->
          (* Ignore parameter bindings and pretend body is inlined *)
          analyze_expression ~resolution ~state ~expression:body
      | List list ->
          List.foldi
            list
            ~f:(analyze_list_element ~resolution)
            ~init:(ForwardState.Tree.empty, state)
      | ListComprehension comprehension -> analyze_comprehension ~resolution comprehension state
      | Name (Name.Identifier identifier) ->
          ForwardState.read ~root:(AccessPath.Root.Variable identifier) ~path:[] state.taint, state
      (* __dict__ reveals an object's underlying data structure, so we should analyze the base under
         the existing taint instead of adding the index to the taint. *)
      | Name (Name.Attribute { base; attribute = "__dict__"; _ }) ->
          analyze_expression ~resolution ~state ~expression:base
      | Name (Name.Attribute { base; attribute; special }) ->
          let { attribute_taint; state; _ } =
            analyze_attribute_access
              ~resolution
              ~state
              ~resolve_properties:true
              ~location
              ~base
              ~attribute
              ~special
          in
          attribute_taint, state
      | Set set ->
          List.fold ~f:(analyze_set_element ~resolution) set ~init:(ForwardState.Tree.empty, state)
      | SetComprehension comprehension -> analyze_comprehension ~resolution comprehension state
      | Starred (Starred.Once expression)
      | Starred (Starred.Twice expression) ->
          analyze_expression ~resolution ~state ~expression
          |>> ForwardState.Tree.read [Abstract.TreeDomain.Label.AnyIndex]
      | FormatString substrings ->
          let value =
            List.map substrings ~f:(function
                | Substring.Format _ -> "{}"
                | Substring.Literal { Node.value; _ } -> value)
            |> String.concat ~sep:""
          in
          let nested_expressions =
            List.filter_map substrings ~f:(function
                | Substring.Format expression -> Some expression
                | _ -> None)
          in
          analyze_string_literal ~resolution ~state ~location ~nested_expressions value
      | Ternary { target; test; alternative } ->
          let state = analyze_condition ~resolution test state in
          let taint_then, state_then = analyze_expression ~resolution ~state ~expression:target in
          let taint_else, state_else =
            analyze_expression ~resolution ~state ~expression:alternative
          in
          ForwardState.Tree.join taint_then taint_else, join state_then state_else
      | Tuple expressions ->
          List.foldi
            ~f:(analyze_list_element ~resolution)
            expressions
            ~init:(ForwardState.Tree.empty, state)
      | UnaryOperator { operator = _; operand } ->
          analyze_expression ~resolution ~state ~expression:operand
      | WalrusOperator { target; value } ->
          let target_taint, state = analyze_expression ~resolution ~state ~expression:target in
          let value_taint, state = analyze_expression ~resolution ~state ~expression:value in
          ForwardState.Tree.join target_taint value_taint, state
      | Yield None -> ForwardState.Tree.empty, state
      | Yield (Some expression)
      | YieldFrom expression ->
          let taint, state = analyze_expression ~resolution ~state ~expression in
          taint, store_taint ~root:AccessPath.Root.LocalResult ~path:[] taint state
    in
    log "Forward taint of expression `%a`: %a" Expression.pp expression ForwardState.Tree.pp taint;
    taint, state


  and analyze_assignment
      ~resolution
      ?(fields = [])
      ({ Node.location; value } as target)
      taint
      surrounding_taint
      state
    =
    match value with
    | Starred (Once target | Twice target) ->
        (* This is approximate. Unless we can get the tuple type on the right to tell how many total
           elements there will be, we just pick up the entire collection. *)
        analyze_assignment ~resolution target surrounding_taint surrounding_taint state
    | List targets
    | Tuple targets ->
        let analyze_target_element i state target =
          let index = Abstract.TreeDomain.Label.Index (string_of_int i) in
          let indexed_taint = ForwardState.Tree.read [index] taint in
          analyze_assignment ~resolution target indexed_taint taint state
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
          ~fields:(index :: fields)
          base
          taint
          surrounding_taint
          state
    | _ ->
        (* Check flows to tainted globals/attributes. *)
        let source_tree = taint in
        let sink_tree =
          GlobalModel.from_expression
            ~resolution
            ~call_graph:FunctionContext.call_graph_of_define
            ~qualifier:FunctionContext.qualifier
            ~expression:target
          |> GlobalModel.get_sink
        in
        check_flow
          ~location:(Location.with_module ~qualifier:FunctionContext.qualifier location)
          ~source_tree
          ~sink_tree;

        (* Propagate taint. *)
        let access_path = AccessPath.of_expression target >>| AccessPath.extend ~path:fields in
        store_taint_option access_path taint state


  and analyze_condition ~resolution expression state =
    let { Node.location; _ } = expression in
    let location = Location.with_module ~qualifier:FunctionContext.qualifier location in
    let taint, state = analyze_expression ~resolution ~state ~expression in
    (* There maybe configured sinks for conditionals, so test them here. *)
    let sink_taint =
      TaintConfiguration.conditional_test_sinks () |> BackwardTaint.of_list ~location
    in
    let sink_tree = BackwardState.Tree.create_leaf sink_taint in
    check_flow ~location ~source_tree:taint ~sink_tree;
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
            ~qualifier:FunctionContext.qualifier
            ~expression:target
        in
        if GlobalModel.is_sanitized target_global_model then
          analyze_expression ~resolution ~state ~expression:value |> snd
        else
          match target_value with
          | Expression.Name (Name.Attribute { base; attribute; _ }) ->
              let attribute_access_callees = get_attribute_access_callees ~location ~attribute in

              let property_call_state =
                match attribute_access_callees with
                | Some { property_targets = _ :: _ as property_targets; return_type; _ } ->
                    (* Treat `a.property = x` as `a = a.property(x)` *)
                    let call_targets =
                      List.map
                        ~f:(fun target ->
                          {
                            CallGraph.CallTarget.target;
                            implicit_self = true;
                            implicit_dunder_call = false;
                            collapse_tito = true;
                          })
                        property_targets
                    in
                    let taint, state =
                      apply_callees
                        ~resolution
                        ~is_property:true
                        ~callee:target
                        ~call_location:location
                        ~arguments:[{ Call.Argument.name = None; value }]
                        ~state
                        (CallGraph.CallCallees.create ~call_targets ~return_type ())
                    in
                    store_taint_option (AccessPath.of_expression base) taint state
                | _ -> bottom
              in

              let regular_attribute_state =
                match attribute_access_callees with
                | Some { is_attribute = true; _ }
                | None ->
                    let taint, state = analyze_expression ~resolution ~state ~expression:value in
                    analyze_assignment ~resolution target taint taint state
                | _ -> bottom
              in

              join property_call_state regular_attribute_state
          | _ ->
              let taint, state = analyze_expression ~resolution ~state ~expression:value in
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
        let _, state = analyze_expression ~resolution ~state ~expression in
        state
    | For _
    | Global _
    | If _
    | Import _
    | Match _
    | Nonlocal _
    | Pass
    | Raise _ ->
        state
    | Return { expression = Some expression; _ } ->
        let taint, state = analyze_expression ~resolution ~state ~expression in
        let location = Location.with_module ~qualifier:FunctionContext.qualifier location in
        check_flow ~location ~source_tree:taint ~sink_tree:(return_sink ~return_location:location);
        store_taint ~root:AccessPath.Root.LocalResult ~path:[] taint state
    | Return { expression = None; _ }
    | Try _
    | With _
    | While _ ->
        state


  let create ~existing_model parameters =
    (* Use primed sources to populate initial state of parameters *)
    let forward_primed_taint = existing_model.Model.forward.source_taint in
    let prime_parameter
        state
        (parameter_root, name, { Node.location; value = { Parameter.value; _ } })
      =
      let prime =
        let location = Location.with_module ~qualifier:FunctionContext.qualifier location in
        ForwardState.read ~root:parameter_root ~path:[] forward_primed_taint
        |> ForwardState.Tree.apply_call
             location
             ~callees:[Interprocedural.Target.create FunctionContext.definition]
             ~port:parameter_root
      in
      let default_value_taint, state =
        match value with
        | None -> ForwardState.Tree.bottom, state
        | Some expression ->
            let resolution =
              TypeCheck.resolution
                global_resolution
                (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
                (module TypeCheck.DummyContext)
            in

            analyze_expression ~resolution ~state ~expression
      in
      let root = AccessPath.Root.Variable name in
      let taint =
        ForwardState.assign
          ~root
          ~path:[]
          (ForwardState.Tree.join prime default_value_taint)
          state.taint
      in
      { state with taint }
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
    ~breadcrumbs_to_attach
    ~via_features_to_attach
    exit_taint
  =
  let { Statement.Define.signature = { return_annotation; name; parameters; _ }; _ } = define in
  let return_annotation =
    Option.map ~f:(GlobalResolution.parse_annotation resolution) return_annotation
  in
  let return_type_breadcrumbs = Features.type_breadcrumbs ~resolution return_annotation in
  let {
    TaintConfiguration.analysis_model_constraints =
      { maximum_model_width; maximum_return_access_path_length; maximum_trace_length; _ };
    _;
  }
    =
    TaintConfiguration.get ()
  in

  let simplify tree =
    let tree =
      match maximum_trace_length with
      | Some maximum_trace_length ->
          ForwardState.Tree.prune_maximum_length maximum_trace_length tree
      | _ -> tree
    in
    let essential = ForwardState.Tree.essential tree in
    ForwardState.Tree.shape
      ~transform:(ForwardTaint.add_local_breadcrumbs (Features.widen_broadening_set ()))
      tree
      ~mold:essential
    |> ForwardState.Tree.add_local_breadcrumbs return_type_breadcrumbs
    |> ForwardState.Tree.limit_to
         ~transform:(ForwardTaint.add_local_breadcrumbs (Features.widen_broadening_set ()))
         ~width:maximum_model_width
    |> ForwardState.Tree.approximate_return_access_paths ~maximum_return_access_path_length
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
    ~environment
    ~qualifier
    ~define
    ~call_graph_of_define
    ~existing_model
  =
  let { Node.value = { Statement.Define.signature = { name; parameters; _ }; _ }; _ } = define in
  let module FunctionContext = struct
    let qualifier = qualifier

    let definition = define

    let debug = Statement.Define.dump define.value

    let profiler = profiler

    let environment = environment

    let call_graph_of_define = call_graph_of_define

    let existing_model = existing_model

    let triggered_sinks = Location.Table.create ()
  end
  in
  let module State = State (FunctionContext) in
  let module Fixpoint = Fixpoint.Make (State) in
  if FunctionContext.debug || Statement.Define.dump_call_graph define.value then
    Log.dump
      "Call graph of `%a`:@,%a"
      Reference.pp
      (Statement.Define.name define.Node.value)
      CallGraph.DefineCallGraph.pp
      call_graph_of_define;
  State.log "Forward analysis of callable: `%a`" Reference.pp name;
  let timer = Timer.start () in
  let cfg = Cfg.create define.value in
  let initial =
    let normalized_parameters = AccessPath.Root.normalize_parameters parameters in
    State.create ~existing_model normalized_parameters
  in
  let () = State.log "Processing CFG:@.%a" Cfg.pp cfg in
  let exit_state =
    Metrics.with_alarm name (fun () -> Fixpoint.forward ~cfg ~initial |> Fixpoint.exit) ()
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
      extract_source_model
        ~define:define.value
        ~resolution
        ~breadcrumbs_to_attach
        ~via_features_to_attach
        taint
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
    ~normals:["callable", Reference.show name]
    ~timer
    ();
  model, issues, FunctionContext.triggered_sinks
