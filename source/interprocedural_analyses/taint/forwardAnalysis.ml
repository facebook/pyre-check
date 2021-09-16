(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
open AccessPath

module type FixpointState = sig
  type t = { taint: ForwardState.t }

  include Fixpoint.State with type t := t

  val create
    :  existing_model:TaintResult.call_model ->
    (Root.t * Identifier.t * Parameter.t) list ->
    t
end

type triggered_sinks = (AccessPath.Root.t * Sinks.t) list Location.Table.t

module type FUNCTION_CONTEXT = sig
  val qualifier : Reference.t

  val definition : Statement.Define.t Node.t

  val get_callees
    :  location:Location.t ->
    call:Call.t ->
    Interprocedural.CallGraph.raw_callees option

  val get_property_callees
    :  location:Location.t ->
    attribute:string ->
    Interprocedural.CallGraph.raw_callees option

  val is_constructor : unit -> bool

  val global_resolution : GlobalResolution.t

  val local_annotations : LocalAnnotationMap.ReadOnly.t option

  val check_flow
    :  location:Location.WithModule.t ->
    source_tree:ForwardState.Tree.t ->
    sink_tree:BackwardState.Tree.t ->
    unit

  val check_triggered_flows
    :  triggered_sinks:Flow.triggered_sinks ->
    location:Location.WithModule.t ->
    source_tree:ForwardState.Tree.t ->
    sink_tree:BackwardState.Tree.t ->
    unit

  val return_sink : return_location:Location.WithModule.t -> BackwardState.Tree.t

  val log : ('a, Format.formatter, unit, unit, unit, unit) Core.format6 -> 'a

  val add_triggered_sinks : location:Location.t -> triggered_sinks:(Root.t * Sinks.t) list -> unit
end

let ( |>> ) (taint, state) f = f taint, state

module AnalysisInstance (FunctionContext : FUNCTION_CONTEXT) = struct
  let log = FunctionContext.log

  module rec FixpointState : FixpointState = struct
    type t = { taint: ForwardState.t }

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


    let store_taint ?(weak = false) ~root ~path taint { taint = state_taint } =
      { taint = ForwardState.assign ~weak ~root ~path taint state_taint }


    let store_taint_option ?(weak = false) access_path taint state =
      match access_path with
      | Some { AccessPath.root; path } -> store_taint ~weak ~root ~path taint state
      | None -> state


    let add_first_index index indices =
      if Features.FirstIndexSet.is_bottom indices then
        Features.to_first_name index
        >>| Features.FirstIndexSet.singleton
        |> Option.value ~default:Features.FirstIndexSet.bottom
      else
        indices


    let add_first_field field fields =
      if Features.FirstFieldSet.is_bottom fields then
        Features.FirstFieldSet.singleton field
      else
        fields


    let rec apply_call_targets
        ~resolution
        ~callee
        ?(collapse_tito = true)
        call_location
        arguments
        initial_state
        call_targets
      =
      (* We keep a table of kind -> set of triggered labels across all targets, and merge triggered
         sinks at the end. *)
      let triggered_sinks = String.Hash_set.create () in
      let apply_call_target state arguments_taint call_target =
        let ({ Model.model = { TaintResult.forward; backward; sanitizers; modes }; _ } as
            taint_model)
          =
          Model.get_callsite_model ~resolution ~call_target ~arguments
        in
        log
          "Forward analysis of call: %a@,Call site model:@,%a"
          Expression.pp
          (Expression.Call { Call.callee; arguments } |> Node.create_with_default_location)
          Model.pp
          taint_model;
        let sink_argument_matches =
          BackwardState.roots backward.sink_taint |> AccessPath.match_actuals_to_formals arguments
        in
        let tito_argument_matches =
          BackwardState.roots backward.taint_in_taint_out
          |> AccessPath.match_actuals_to_formals arguments
        in
        let sanitize_argument_matches =
          SanitizeRootMap.roots sanitizers.roots |> AccessPath.match_actuals_to_formals arguments
        in
        let combined_matches =
          List.zip_exn tito_argument_matches sanitize_argument_matches
          |> List.zip_exn sink_argument_matches
          |> List.zip_exn arguments_taint
        in
        let combine_sink_taint location taint_tree { root; actual_path; formal_path } =
          BackwardState.read ~root ~path:[] backward.sink_taint
          |> BackwardState.Tree.apply_call location ~callees:[call_target] ~port:root
          |> BackwardState.Tree.read formal_path
          |> BackwardState.Tree.prepend actual_path
          |> BackwardState.Tree.join taint_tree
        in
        let merge_tito_effect join ~key:_ = function
          | `Left left -> Some left
          | `Right right -> Some right
          | `Both (left, right) -> Some (join left right)
        in
        let combine_tito tito_map { AccessPath.root; actual_path; formal_path } =
          let new_tito_map =
            BackwardState.read ~root ~path:formal_path backward.taint_in_taint_out
            |> BackwardState.Tree.prepend actual_path
            |> BackwardState.Tree.partition Domains.BackwardTaint.kind By ~f:Fn.id
          in
          Map.Poly.merge tito_map new_tito_map ~f:(merge_tito_effect BackwardState.Tree.join)
        in
        let apply_tito_sanitizers sanitize_matches taint_to_propagate =
          let sanitize =
            List.map
              ~f:(fun { AccessPath.root; _ } -> SanitizeRootMap.get root sanitizers.roots)
              sanitize_matches
            |> List.fold ~f:Sanitize.join ~init:Sanitize.empty
            |> Sanitize.join sanitizers.global
            |> Sanitize.join sanitizers.parameters
          in
          match sanitize.Sanitize.tito with
          | Some AllTito -> ForwardState.Tree.bottom
          | Some (SpecificTito { sanitized_tito_sources; _ }) ->
              ForwardState.Tree.transform
                ForwardTaint.kind
                Filter
                ~f:(fun source -> not (Sources.Set.mem source sanitized_tito_sources))
                taint_to_propagate
          | None -> taint_to_propagate
        in
        let compute_argument_tito_effect
            (tito_effects, state)
            (argument_taint, ((argument, sink_matches), ((_, tito_matches), (_, sanitize_matches))))
          =
          let taint_to_propagate = apply_tito_sanitizers sanitize_matches argument_taint in
          let tito =
            let convert_tito_path kind (path, return_taint) accumulated_tito =
              let breadcrumbs =
                BackwardTaint.breadcrumbs return_taint |> Features.BreadcrumbSet.add Features.tito
              in
              let add_features_and_position leaf_taint =
                leaf_taint
                |> FlowDetails.add_tito_position argument.Node.location
                |> FlowDetails.add_breadcrumbs breadcrumbs
              in
              let taint_to_propagate =
                if collapse_tito then
                  ForwardState.Tree.read path taint_to_propagate
                  |> ForwardState.Tree.collapse
                       ~transform:(ForwardTaint.add_breadcrumbs Features.tito_broadening)
                  |> ForwardTaint.transform FlowDetails.Self Map ~f:add_features_and_position
                  |> ForwardState.Tree.create_leaf
                else
                  ForwardState.Tree.read path taint_to_propagate
                  |> ForwardState.Tree.transform FlowDetails.Self Map ~f:add_features_and_position
              in
              let return_paths =
                match kind with
                | Sinks.LocalReturn ->
                    BackwardTaint.fold
                      Features.ReturnAccessPathSet.Element
                      return_taint
                      ~f:List.cons
                      ~init:[]
                | _ ->
                    (* No special handling of paths for side effects *)
                    [[]]
              in
              let create_tito_return_paths tito return_path =
                ForwardState.Tree.prepend return_path taint_to_propagate
                |> ForwardState.Tree.join tito
              in
              List.fold return_paths ~f:create_tito_return_paths ~init:accumulated_tito
            in
            let convert_tito ~key:kind ~data:tito_tree =
              BackwardState.Tree.fold
                BackwardState.Tree.Path
                tito_tree
                ~init:ForwardState.Tree.empty
                ~f:(convert_tito_path kind)
            in
            List.fold tito_matches ~f:combine_tito ~init:Map.Poly.empty
            |> Map.Poly.mapi ~f:convert_tito
            |> Map.Poly.merge tito_effects ~f:(merge_tito_effect ForwardState.Tree.join)
          in
          let tito =
            if TaintResult.ModeSet.contains Obscure modes then
              let obscure_tito =
                ForwardState.Tree.collapse
                  ~transform:(ForwardTaint.add_breadcrumbs Features.tito_broadening)
                  taint_to_propagate
                |> ForwardTaint.transform
                     Features.TitoPositionSet.Element
                     Add
                     ~f:argument.Node.location
                |> ForwardTaint.add_breadcrumb Features.obscure
                |> ForwardState.Tree.create_leaf
              in
              let returned_tito =
                match Map.Poly.find tito_effects Sinks.LocalReturn with
                | Some regular_tito -> ForwardState.Tree.join regular_tito obscure_tito
                | None -> obscure_tito
              in
              Map.Poly.set tito ~key:Sinks.LocalReturn ~data:returned_tito
            else
              tito
          in
          let location =
            Location.with_module ~qualifier:FunctionContext.qualifier argument.Node.location
          in
          let sink_tree =
            List.fold sink_matches ~f:(combine_sink_taint location) ~init:BackwardState.Tree.empty
          in
          (* Compute triggered partial sinks, if any. *)
          let () =
            FunctionContext.check_triggered_flows
              ~triggered_sinks
              ~location
              ~source_tree:argument_taint
              ~sink_tree
          in

          (* Add features to arguments. *)
          let state =
            match AccessPath.of_expression ~resolution argument with
            | Some { AccessPath.root; path } ->
                let breadcrumbs_to_add =
                  BackwardState.Tree.filter_by_kind ~kind:Sinks.AddFeatureToArgument sink_tree
                  |> BackwardTaint.breadcrumbs
                in
                if Features.BreadcrumbSet.is_bottom breadcrumbs_to_add then
                  state
                else
                  let taint =
                    ForwardState.read state.taint ~root ~path
                    |> ForwardState.Tree.add_breadcrumbs breadcrumbs_to_add
                  in
                  store_taint ~root ~path taint state
            | None -> state
          in
          FunctionContext.check_flow ~location ~source_tree:argument_taint ~sink_tree;
          tito, state
        in
        let tito_effects, state =
          List.fold ~f:compute_argument_tito_effect combined_matches ~init:(Map.Poly.empty, state)
        in
        let result_taint =
          ForwardState.read ~root:AccessPath.Root.LocalResult ~path:[] forward.source_taint
          |> ForwardState.Tree.apply_call
               call_location
               ~callees:[call_target]
               ~port:AccessPath.Root.LocalResult
        in
        let tito =
          Map.Poly.find tito_effects Sinks.LocalReturn
          |> Option.value ~default:ForwardState.Tree.empty
        in
        (if not (Hash_set.is_empty triggered_sinks) then
           let add_sink (key, taint) roots_and_sinks =
             let add roots_and_sinks = function
               | Sinks.PartialSink sink ->
                   if Hash_set.mem triggered_sinks (Sinks.show_partial_sink sink) then
                     (key, Sinks.TriggeredPartialSink sink) :: roots_and_sinks
                   else
                     roots_and_sinks
               | _ -> roots_and_sinks
             in
             BackwardTaint.kinds
               (BackwardState.Tree.collapse
                  ~transform:(BackwardTaint.add_breadcrumbs Features.issue_broadening)
                  taint)
             |> List.fold ~f:add ~init:roots_and_sinks
           in
           let triggered_sinks =
             BackwardState.fold BackwardState.KeyValue backward.sink_taint ~init:[] ~f:add_sink
           in
           let { Location.WithModule.start; stop; _ } = call_location in
           FunctionContext.add_triggered_sinks ~location:{ Location.start; stop } ~triggered_sinks);
        let apply_tito_side_effects tito_effects state =
          (* We also have to consider the cases when the updated parameter has a global model, in
             which case we need to capture the flow. *)
          let apply_argument_effect
              ~argument:{ Call.Argument.value = argument; _ }
              ~source_tree
              state
            =
            let location =
              Location.with_module ~qualifier:FunctionContext.qualifier argument.Node.location
            in
            let sink_tree =
              Model.get_global_model ~resolution ~location ~expression:argument
              |> Model.GlobalModel.get_sink
            in
            FunctionContext.check_flow ~location ~source_tree ~sink_tree;
            let access_path = AccessPath.of_expression ~resolution argument in
            log
              "Propagating taint to argument `%a`: %a"
              Expression.pp
              argument
              ForwardState.Tree.pp
              source_tree;
            store_taint_option ~weak:true access_path source_tree state
          in
          let for_each_target ~key:target ~data:taint state =
            match target with
            | Sinks.LocalReturn -> state (* This is regular tito which was computed above *)
            | ParameterUpdate n -> (
                (* Side effect on argument n *)
                match List.nth arguments n with
                | None -> state
                | Some argument -> apply_argument_effect ~argument ~source_tree:taint state)
            | Attach -> state (* These synthetic nodes should be ignored for analysis.*)
            | _ -> failwith "unexpected sink in tito"
          in
          Map.Poly.fold tito_effects ~f:for_each_target ~init:state
        in
        let returned_taint =
          let joined = ForwardState.Tree.join result_taint tito in
          if TaintResult.ModeSet.contains Obscure modes then
            let annotation =
              Resolution.resolve_expression_to_type
                resolution
                (Node.create_with_default_location (Expression.Call { Call.callee; arguments }))
            in
            let type_breadcrumbs =
              Features.type_breadcrumbs
                ~resolution:(Resolution.global_resolution resolution)
                (Some annotation)
            in
            ForwardState.Tree.add_breadcrumbs type_breadcrumbs joined
          else
            joined
        in
        returned_taint, apply_tito_side_effects tito_effects state
      in
      let call_targets =
        match call_targets with
        | [] when TaintConfiguration.is_missing_flow_analysis Type ->
            let callable =
              Model.unknown_callee
                ~location:call_location
                ~call:(Expression.Call { callee; arguments })
            in
            if not (Interprocedural.FixpointState.has_model callable) then
              Model.register_unknown_callee_model callable;
            [callable]
        | _ -> call_targets
      in
      match call_targets with
      | [] ->
          (* If we don't have a call target: propagate argument taint. *)
          let analyze_argument
              ~resolution
              (taint_accumulator, state)
              { Call.Argument.value = argument; _ }
            =
            analyze_expression ~resolution ~state ~expression:argument
            |>> ForwardState.Tree.transform
                  Features.TitoPositionSet.Element
                  Add
                  ~f:argument.Node.location
            |>> ForwardState.Tree.join taint_accumulator
          in
          let callee_taint, state =
            analyze_expression ~resolution ~state:initial_state ~expression:callee
            |>> ForwardState.Tree.transform
                  Features.TitoPositionSet.Element
                  Add
                  ~f:callee.Node.location
          in
          List.fold_left arguments ~init:(callee_taint, state) ~f:(analyze_argument ~resolution)
          |>> ForwardState.Tree.add_breadcrumb Features.obscure
      | call_targets ->
          let arguments_taint, state =
            let compute_argument_taint (arguments_taint, state) argument =
              let taint, state =
                analyze_unstarred_expression ~resolution argument.Call.Argument.value state
              in
              taint :: arguments_taint, state
            in
            List.rev arguments |> List.fold ~init:([], initial_state) ~f:compute_argument_taint
          in
          List.map call_targets ~f:(apply_call_target state arguments_taint)
          |> List.fold
               ~init:(ForwardState.Tree.empty, { taint = ForwardState.empty })
               ~f:(fun (taint, state) (new_taint, new_state) ->
                 ForwardState.Tree.join taint new_taint, join state new_state)


    and analyze_dictionary_entry ~resolution (taint, state) { Dictionary.Entry.key; value } =
      let field_name =
        match key.Node.value with
        | String literal -> Abstract.TreeDomain.Label.Index literal.value
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
      let bound_state, resolution =
        analyze_comprehension_generators ~resolution ~state generators
      in
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


    and analyze_constructor_call
        ~resolution
        ~callee
        ~arguments
        ~new_targets
        ~init_targets
        ~location
        ~state
      =
      (* Since we're searching for ClassType.__new/init__(), Pyre will correctly not insert an
         implicit type there. However, since the actual call was ClassType(), insert the implicit
         receiver here ourselves and ignore the value from get_indirect_targets. *)
      let apply_call_targets state targets =
        let arguments = { Call.Argument.name = None; value = callee } :: arguments in
        apply_call_targets ~resolution ~callee location arguments state targets
      in
      let state =
        match new_targets with
        | [] -> state
        | targets -> apply_call_targets state targets |> snd
      in
      apply_call_targets state init_targets


    and analyze_call ~resolution ~location ~state ~callee ~arguments =
      let analyze_regular_targets
          ~state
          ~callee
          ~arguments
          { Interprocedural.CallGraph.implicit_self; targets; collapse_tito }
        =
        let arguments =
          if implicit_self then
            let receiver =
              match Node.value callee with
              | Expression.Name (Name.Attribute { base; _ }) -> base
              | _ ->
                  (* Default to a benign self if we don't understand/retain information of what self
                     is. *)
                  Node.create_with_default_location (Expression.Name (Name.Identifier "None"))
            in
            { Call.Argument.name = None; value = receiver } :: arguments
          else
            arguments
        in
        let add_index_breadcrumb_if_necessary taint =
          let is_get_method =
            match Node.value callee with
            | Expression.Name (Name.Attribute { attribute = "get"; _ }) -> true
            | _ -> false
          in
          if not is_get_method then
            taint
          else
            match arguments with
            | _receiver :: index :: _ ->
                let label = get_index index.value in
                ForwardState.Tree.transform
                  Features.FirstIndexSet.Self
                  Map
                  ~f:(add_first_index label)
                  taint
            | _ -> taint
        in
        apply_call_targets ~resolution ~callee ~collapse_tito location arguments state targets
        |>> add_index_breadcrumb_if_necessary
      in

      let analyze_lambda_call
          ~callee
          ~lambda_argument
          ~non_lambda_arguments
          ~higher_order_function
          ~callable_argument
        =
        (* If we have a lambda `fn` getting passed into `hof`, we use the following strategy:
         * hof(q, fn, x, y) gets translated into the following block: (analyzed backwards)
         * if rand():
         *   $all = {q, x, y}
         *   $result = fn( *all, **all)
         * else:
         *   $result = fn
         * hof(q, fn, x, y)
         *)
        let ( lambda_index,
              {
                Call.Argument.value = { location = lambda_location; _ } as lambda_callee;
                name = lambda_name;
              } )
          =
          lambda_argument
        in
        let location = lambda_callee.Node.location in
        let result =
          Node.create ~location:lambda_location (Expression.Name (Name.Identifier "$result"))
        in

        (* Simulate if branch. *)
        let if_branch_state =
          (* Simulate `$all = {q, x, y}`. *)
          let all_argument =
            Node.create ~location:lambda_location (Expression.Name (Name.Identifier "$all"))
          in
          let state =
            let all_assignee =
              Node.create
                ~location
                (Expression.Set
                   (List.map non_lambda_arguments ~f:(fun (_, argument) ->
                        argument.Call.Argument.value)))
            in
            let taint, state = analyze_expression ~resolution ~state ~expression:all_assignee in
            analyze_assignment ~resolution all_argument taint taint state
          in

          (* Simulate `$result = fn( *all, **all)`. *)
          let arguments =
            List.map non_lambda_arguments ~f:(fun (_, argument) ->
                { argument with Call.Argument.value = all_argument })
          in
          let { Call.callee; arguments } =
            Interprocedural.CallGraph.redirect_special_calls
              ~resolution
              { Call.callee = lambda_callee; arguments }
          in
          let taint, state = analyze_regular_targets ~state ~callee ~arguments callable_argument in
          let taint = ForwardState.Tree.add_breadcrumb Features.lambda taint in
          analyze_assignment ~resolution result taint taint state
        in

        (* Simulate else branch. *)
        let else_branch_state =
          let taint, state = analyze_expression ~resolution ~state ~expression:lambda_callee in
          analyze_assignment ~resolution result taint taint state
        in
        let state = join if_branch_state else_branch_state in

        (* Simulate `hof(q, $result, x, y)`. *)
        let higher_order_function_arguments =
          let lambda_argument_with_index =
            lambda_index, { Call.Argument.value = result; name = lambda_name }
          in
          let compare_by_index (left_index, _) (right_index, _) =
            Int.compare left_index right_index
          in
          lambda_argument_with_index :: non_lambda_arguments
          |> List.sort ~compare:compare_by_index
          |> List.map ~f:snd
        in
        analyze_regular_targets
          ~state
          ~callee
          ~arguments:higher_order_function_arguments
          higher_order_function
      in

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
              FunctionContext.is_constructor ()
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
            |>> ForwardState.Tree.transform
                  Features.FirstIndexSet.Self
                  Map
                  ~f:(add_first_index index)
        (* We read the taint at the `__iter__` call to be able to properly reference key taint as
           appropriate. *)
        | {
         callee = { Node.value = Name (Name.Attribute { base; attribute = "__next__"; _ }); _ };
         arguments = [];
        } ->
            analyze_expression ~resolution ~state ~expression:base
        | {
         callee =
           {
             Node.value = Name (Name.Attribute { base; attribute = "__iter__"; special = true });
             _;
           };
         arguments = [];
        } ->
            let taint, state = analyze_expression ~resolution ~state ~expression:base in
            let label =
              (* For dictionaries, the default iterator is keys. *)
              if
                Resolution.resolve_expression_to_type resolution base
                |> Type.is_dictionary_or_mapping
              then
                AccessPath.dictionary_keys
              else
                Abstract.TreeDomain.Label.AnyIndex
            in
            ForwardState.Tree.read [label] taint, state
        (* x[0] = value is converted to x.__setitem__(0, value). in parsing. *)
        | {
         callee =
           { Node.value = Name (Name.Attribute { base; attribute = "__setitem__"; _ }); _ } as
           callee;
         arguments = [{ Call.Argument.value = index; _ }; { Call.Argument.value; _ }] as arguments;
        } -> (
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
            (* Also make sure we analyze the __setitem__ call in case the __setitem__ function body
               is tainted. *)
            match
              FunctionContext.get_callees
                ~location:(Location.strip_module location)
                ~call:{ Call.callee; arguments }
            with
            | Some (RegularTargets targets) ->
                analyze_regular_targets ~state ~callee ~arguments targets
            | _ -> taint, state)
        (* We special object.__setattr__, which is sometimes used in order to work around
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
                 { Node.value = Expression.String { StringLiteral.value = attribute; _ }; _ };
               name = None;
             };
             { Call.Argument.value = assigned_value; name = None };
           ];
        } ->
            let taint, state = analyze_expression ~resolution ~state ~expression:assigned_value in
            let state =
              analyze_assignment
                ~resolution
                {
                  Node.value =
                    Expression.Name (Name.Attribute { base = self; attribute; special = false });
                  location = { Location.start = location.start; stop = location.stop };
                }
                taint
                taint
                state
            in
            taint, state
        (* `getattr(a, "field", default)` should evaluate to the join of `a.field` and `default`. *)
        | {
         callee = { Node.value = Name (Name.Identifier "getattr"); location };
         arguments =
           [
             { Call.Argument.value = base; _ };
             {
               Call.Argument.value =
                 { Node.value = Expression.String { StringLiteral.value = attribute; _ }; _ };
               _;
             };
             { Call.Argument.value = default; _ };
           ];
        } ->
            let attribute_expression =
              {
                Node.location;
                value = Expression.Name (Name.Attribute { base; attribute; special = false });
              }
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
        (* dictionary .keys(), .values() and .items() functions are special, as they require
           handling of dictionary_keys taint. *)
        | {
         callee = { Node.value = Name (Name.Attribute { base; attribute = "values"; _ }); _ };
         _;
        }
          when Resolution.resolve_expression_to_type resolution base
               |> Type.is_dictionary_or_mapping ->
            analyze_expression ~resolution ~state ~expression:base
            |>> ForwardState.Tree.read [Abstract.TreeDomain.Label.AnyIndex]
            |>> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]
        | { callee = { Node.value = Name (Name.Attribute { base; attribute = "keys"; _ }); _ }; _ }
          when Resolution.resolve_expression_to_type resolution base
               |> Type.is_dictionary_or_mapping ->
            analyze_expression ~resolution ~state ~expression:base
            |>> ForwardState.Tree.read [AccessPath.dictionary_keys]
            |>> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]
        | { callee = { Node.value = Name (Name.Attribute { base; attribute = "items"; _ }); _ }; _ }
          when Resolution.resolve_expression_to_type resolution base
               |> Type.is_dictionary_or_mapping ->
            let taint, state = analyze_expression ~resolution ~state ~expression:base in
            let taint =
              let key_taint = ForwardState.Tree.read [AccessPath.dictionary_keys] taint in
              let value_taint = ForwardState.Tree.read [Abstract.TreeDomain.Label.AnyIndex] taint in
              ForwardState.Tree.join
                (ForwardState.Tree.prepend [Abstract.TreeDomain.Label.create_int_index 0] key_taint)
                (ForwardState.Tree.prepend
                   [Abstract.TreeDomain.Label.create_int_index 1]
                   value_taint)
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
                ForwardState.read ~root ~path:[] state.taint
                |> ForwardState.Tree.prepend path_of_root
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
        | _ ->
            let call = { Call.callee; arguments } in
            let { Call.callee; arguments } =
              Interprocedural.CallGraph.redirect_special_calls ~resolution call
            in
            let taint, state =
              match
                FunctionContext.get_callees
                  ~location:(Location.strip_module location)
                  ~call:{ Call.callee; arguments }
              with
              | Some (RegularTargets targets) ->
                  analyze_regular_targets ~state ~callee ~arguments targets
              | Some
                  (HigherOrderTargets
                    { higher_order_function; callable_argument = index, callable_argument }) -> (
                  let lambda_argument = index, List.nth arguments index in
                  match lambda_argument with
                  | index, Some lambda_argument ->
                      let non_lambda_arguments =
                        List.mapi ~f:(fun index value -> index, value) (List.take arguments index)
                        @ List.mapi
                            ~f:(fun relative_index value -> index + 1 + relative_index, value)
                            (List.drop arguments (index + 1))
                      in
                      analyze_lambda_call
                        ~callee
                        ~lambda_argument:(index, lambda_argument)
                        ~non_lambda_arguments
                        ~higher_order_function
                        ~callable_argument
                  | _ -> analyze_regular_targets ~state ~callee ~arguments higher_order_function)
              | Some (ConstructorTargets { new_targets; init_targets }) ->
                  analyze_constructor_call
                    ~resolution
                    ~callee
                    ~arguments
                    ~new_targets
                    ~init_targets
                    ~location
                    ~state
              | None ->
                  (* No target, treat call as obscure *)
                  (* reveal_taint(). *)
                  begin
                    match Node.value callee, arguments with
                    | ( Expression.Name (Name.Identifier "reveal_taint"),
                        [{ Call.Argument.value = expression; _ }] ) ->
                        let taint, _ = analyze_expression ~resolution ~state ~expression in
                        let location =
                          Node.location callee
                          |> Location.with_module ~qualifier:FunctionContext.qualifier
                        in
                        Log.dump
                          "%a: Revealed forward taint for `%s`: %s"
                          Location.WithModule.pp
                          location
                          (Transform.sanitize_expression expression |> Expression.show)
                          (ForwardState.Tree.show taint)
                    | ( Expression.Name (Name.Identifier "reveal_type"),
                        [{ Call.Argument.value = expression; _ }] ) ->
                        let location =
                          Node.location callee
                          |> Location.with_module ~qualifier:FunctionContext.qualifier
                        in
                        Log.dump
                          "%a: Revealed type for %s: %s"
                          Location.WithModule.pp
                          location
                          (Transform.sanitize_expression expression |> Expression.show)
                          (Resolution.resolve_expression_to_type resolution expression |> Type.show)
                    | _ -> ()
                  end;
                  apply_call_targets
                    ~resolution
                    ~callee
                    ~collapse_tito:false
                    location
                    arguments
                    state
                    []
            in
            let taint =
              match Node.value callee with
              | Name
                  (Name.Attribute
                    { base = { Node.value = Expression.String _; _ }; attribute = "format"; _ }) ->
                  ForwardState.Tree.add_breadcrumb Features.format_string taint
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


    and analyze_attribute_access ~resolution ~state ~location base attribute =
      let expression =
        Node.create_with_default_location
          (Expression.Name (Name.Attribute { Name.Attribute.base; attribute; special = false }))
      in
      let global_model = Model.get_global_model ~resolution ~location ~expression in
      let attribute_taint = Model.GlobalModel.get_source global_model in
      let add_tito_features taint =
        let attribute_breadcrumbs =
          global_model |> Model.GlobalModel.get_tito |> BackwardState.Tree.breadcrumbs
        in
        ForwardState.Tree.add_breadcrumbs attribute_breadcrumbs taint
      in
      let apply_attribute_sanitizers taint =
        match Model.GlobalModel.get_sanitize global_model with
        | { Sanitize.sources = Some AllSources; _ } -> ForwardState.Tree.empty
        | { Sanitize.sources = Some (SpecificSources sanitized_sources); _ } ->
            ForwardState.Tree.transform
              ForwardTaint.kind
              Filter
              ~f:(fun source -> not (Sources.Set.mem source sanitized_sources))
              taint
        | _ -> taint
      in

      let field = Abstract.TreeDomain.Label.Index attribute in
      analyze_expression ~resolution ~state ~expression:base
      |>> add_tito_features
      |>> ForwardState.Tree.read [field]
      |>> ForwardState.Tree.transform Features.FirstFieldSet.Self Map ~f:(add_first_field attribute)
      (* This should be applied before the join with the attribute taint, so inferred taint
       * is sanitized, but user-specified taint on the attribute is still propagated. *)
      |>> apply_attribute_sanitizers
      |>> ForwardState.Tree.join attribute_taint


    and analyze_string_literal ~resolution ~state ~location { StringLiteral.value; kind } =
      let value_taint =
        let literal_string_regular_expressions = TaintConfiguration.literal_string_sources () in
        if List.is_empty literal_string_regular_expressions then
          ForwardState.Tree.empty
        else
          let add_matching_source_kind tree { TaintConfiguration.pattern; source_kind = kind } =
            if Re2.matches pattern value then
              ForwardState.Tree.join
                tree
                (ForwardState.Tree.create_leaf (ForwardTaint.singleton ~location kind))
            else
              tree
          in
          List.fold
            literal_string_regular_expressions
            ~init:ForwardState.Tree.empty
            ~f:add_matching_source_kind
      in
      match kind with
      | StringLiteral.Format expressions ->
          let taint, state =
            List.fold
              expressions
              ~f:(fun (taint, state) expression ->
                analyze_expression ~resolution ~state ~expression |>> ForwardState.Tree.join taint)
              ~init:(ForwardState.Tree.empty, state)
            |>> ForwardState.Tree.add_breadcrumb Features.format_string
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
                      BackwardState.Tree.join
                        taint
                        (BackwardState.Tree.create_leaf
                           (BackwardTaint.singleton ~location sink_kind))
                    else
                      taint)
                  ~init:BackwardState.Tree.bottom
              in
              FunctionContext.check_flow ~location ~source_tree:taint ~sink_tree:backwards_taint
          in
          taint, state
      | _ -> value_taint, state


    and analyze_expression ~resolution ~state ~expression:({ Node.location; _ } as expression) =
      let location = Location.with_module ~qualifier:FunctionContext.qualifier location in
      let taint, state =
        match expression.Node.value with
        | Await expression -> analyze_expression ~resolution ~state ~expression
        | BooleanOperator { left; operator = _; right } ->
            let left_taint, state = analyze_expression ~resolution ~state ~expression:left in
            let right_taint, state = analyze_expression ~resolution ~state ~expression:right in
            ForwardState.Tree.join left_taint right_taint, state
        | ComparisonOperator ({ left; operator = _; right } as comparison) -> (
            match
              ComparisonOperator.override ~location:(Location.strip_module location) comparison
            with
            | Some override -> analyze_expression ~resolution ~state ~expression:override
            | None ->
                let left_taint, state = analyze_expression ~resolution ~state ~expression:left in
                let right_taint, state = analyze_expression ~resolution ~state ~expression:right in
                let taint =
                  ForwardState.Tree.join left_taint right_taint
                  |> ForwardState.Tree.add_breadcrumbs Features.type_bool
                in
                taint, state)
        | Call { callee; arguments } -> analyze_call ~resolution ~location ~state ~callee ~arguments
        | Complex _ -> ForwardState.Tree.empty, state
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
        | Ellipsis
        | False
        | Float _ ->
            ForwardState.Tree.empty, state
        | Generator comprehension -> analyze_comprehension ~resolution comprehension state
        | Integer _ -> ForwardState.Tree.empty, state
        | Lambda { parameters = _; body } ->
            (* Ignore parameter bindings and pretend body is inlined *)
            analyze_expression ~resolution ~state ~expression:body
        | List list ->
            List.foldi
              list
              ~f:(analyze_list_element ~resolution)
              ~init:(ForwardState.Tree.empty, state)
        | ListComprehension comprehension -> analyze_comprehension ~resolution comprehension state
        | Name _ when AccessPath.is_global ~resolution expression ->
            let taint =
              Model.get_global_model ~resolution ~location ~expression
              |> Model.GlobalModel.get_source
            in
            taint, state
        | Name (Name.Identifier identifier) ->
            ( ForwardState.read ~root:(AccessPath.Root.Variable identifier) ~path:[] state.taint,
              state )
        (* __dict__ reveals an object's underlying data structure, so we should analyze the base
           under the existing taint instead of adding the index to the taint. *)
        | Name (Name.Attribute { base; attribute = "__dict__"; _ }) ->
            analyze_expression ~resolution ~state ~expression:base
        | Name (Name.Attribute { base; attribute; _ }) -> (
            match
              FunctionContext.get_property_callees
                ~location:(Location.strip_module location)
                ~attribute
            with
            | Some (RegularTargets { targets; _ }) ->
                let arguments = [{ Call.Argument.name = None; value = base }] in
                apply_call_targets ~resolution ~callee:expression location arguments state targets
            | _ -> analyze_attribute_access ~resolution ~state ~location base attribute)
        | Set set ->
            List.fold ~f:(analyze_set_element ~resolution) set ~init:(ForwardState.Tree.empty, state)
        | SetComprehension comprehension -> analyze_comprehension ~resolution comprehension state
        | Starred (Starred.Once expression)
        | Starred (Starred.Twice expression) ->
            analyze_expression ~resolution ~state ~expression
            |>> ForwardState.Tree.read [Abstract.TreeDomain.Label.AnyIndex]
        | String string_literal ->
            analyze_string_literal ~resolution ~state ~location string_literal
        | Ternary { target; test; alternative } ->
            let state = analyze_condition ~resolution test state in
            let taint_then, state_then = analyze_expression ~resolution ~state ~expression:target in
            let taint_else, state_else =
              analyze_expression ~resolution ~state ~expression:alternative
            in
            ForwardState.Tree.join taint_then taint_else, join state_then state_else
        | True -> ForwardState.Tree.empty, state
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
          (* This is approximate. Unless we can get the tuple type on the right to tell how many
             total elements there will be, we just pick up the entire collection. *)
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
            callee =
              { Node.value = Name (Name.Attribute { base; attribute = "__getitem__"; _ }); _ };
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
          let location = Location.with_module ~qualifier:FunctionContext.qualifier location in
          let source_tree = taint in
          let sink_tree =
            Model.get_global_model ~resolution ~location ~expression:target
            |> Model.GlobalModel.get_sink
          in
          FunctionContext.check_flow ~location ~source_tree ~sink_tree;

          (* Propagate taint. *)
          let access_path =
            AccessPath.of_expression ~resolution target >>| AccessPath.extend ~path:fields
          in
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
      FunctionContext.check_flow ~location ~source_tree:taint ~sink_tree;
      state


    let analyze_definition ~define:_ state = state

    let analyze_statement ~resolution { Node.value = statement; location } state =
      match statement with
      | Statement.Statement.Assign { value = { Node.value = Expression.Ellipsis; _ }; _ } -> state
      | Assign { value = { Node.value = Expression.Name (Name.Identifier "None"); _ }; target; _ }
        -> (
          match AccessPath.of_expression ~resolution target with
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
          let location_with_module =
            Location.with_module ~qualifier:FunctionContext.qualifier location
          in
          let target_is_sanitized =
            (* Optimization: We only view names as being sanitizable to avoid unnecessary type
               checking. *)
            match Node.value target with
            | Name (Name.Attribute _) ->
                Model.get_global_model ~resolution ~location:location_with_module ~expression:target
                |> Model.GlobalModel.is_sanitized
            | _ -> false
          in
          if target_is_sanitized then
            analyze_expression ~resolution ~state ~expression:value |> snd
          else
            match target_value with
            | Expression.Name (Name.Attribute { base; attribute; _ }) -> (
                match FunctionContext.get_property_callees ~location ~attribute with
                | Some (RegularTargets { targets; _ }) ->
                    (* Treat `a.property = x` as `a = a.property(x)` *)
                    let arguments =
                      [{ Call.Argument.name = None; value = base }; { name = None; value }]
                    in
                    let taint, state =
                      apply_call_targets
                        ~resolution
                        ~callee:target
                        location_with_module
                        arguments
                        state
                        targets
                    in
                    store_taint_option (AccessPath.of_expression ~resolution base) taint state
                | _ ->
                    let taint, state = analyze_expression ~resolution ~state ~expression:value in
                    analyze_assignment ~resolution target taint taint state)
            | _ ->
                let taint, state = analyze_expression ~resolution ~state ~expression:value in
                analyze_assignment ~resolution target taint taint state)
      | Assert { test; _ } -> analyze_condition ~resolution test state
      | Break
      | Class _
      | Continue ->
          state
      | Define define -> analyze_definition ~define state
      | Delete _ -> state
      | Expression expression ->
          let _, state = analyze_expression ~resolution ~state ~expression in
          state
      | For _
      | Global _
      | If _
      | Import _
      | Nonlocal _
      | Pass
      | Raise _ ->
          state
      | Return { expression = Some expression; _ } ->
          let taint, state = analyze_expression ~resolution ~state ~expression in
          let location = Location.with_module ~qualifier:FunctionContext.qualifier location in
          FunctionContext.check_flow
            ~location
            ~source_tree:taint
            ~sink_tree:(FunctionContext.return_sink ~return_location:location);
          store_taint ~root:AccessPath.Root.LocalResult ~path:[] taint state
      | Return { expression = None; _ }
      | Try _
      | With _
      | While _ ->
          state


    let create ~existing_model parameters =
      (* Use primed sources to populate initial state of parameters *)
      let forward_primed_taint = existing_model.TaintResult.forward.source_taint in
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
                  FunctionContext.global_resolution
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
      List.fold parameters ~init:{ taint = ForwardState.empty } ~f:prime_parameter


    let forward ~key state ~statement =
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
          ~global_resolution:FunctionContext.global_resolution
          ~local_annotations:FunctionContext.local_annotations
          ~parent
          ~key (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
          (module TypeCheck.DummyContext)
      in

      analyze_statement ~resolution statement state


    let backward ~key:_ _ ~statement:_ = failwith "Don't call me"
  end

  and Analyzer : (Fixpoint.Fixpoint with type state = FixpointState.t) =
    Fixpoint.Make (FixpointState)
end

let extract_source_model
    ~define
    ~resolution
    ~breadcrumbs_to_attach
    ~via_features_to_attach
    exit_taint
  =
  let {
    Statement.Define.signature =
      { return_annotation; name = { Node.value = name; _ }; parameters; _ };
    _;
  }
    =
    define
  in
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
      ~transform:(ForwardTaint.add_breadcrumbs Features.widen_broadening)
      tree
      ~mold:essential
    |> ForwardState.Tree.add_breadcrumbs return_type_breadcrumbs
    |> ForwardState.Tree.limit_to
         ~transform:(ForwardTaint.add_breadcrumbs Features.widen_broadening)
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

  ForwardState.assign ~root:AccessPath.Root.LocalResult ~path:[] return_taint ForwardState.empty
  |> ForwardState.add_breadcrumbs breadcrumbs_to_attach
  |> ForwardState.add_via_features via_features_to_attach


let run ~environment ~qualifier ~define ~call_graph_of_define ~existing_model =
  let {
    Node.value =
      { Statement.Define.signature = { name = { Node.value = name; _ }; parameters; _ }; _ };
    _;
  }
    =
    define
  in
  let module Context = struct
    let qualifier = qualifier

    let definition = define

    let debug = Statement.Define.dump define.value

    let log format =
      if debug then
        Log.dump format
      else
        Log.log ~section:`Taint format


    let get_callees ~location ~call =
      let callees =
        match Map.find call_graph_of_define location with
        | Some (Interprocedural.CallGraph.Callees callees) -> Some callees
        | Some (Interprocedural.CallGraph.SyntheticCallees name_to_callees) ->
            String.Map.Tree.find name_to_callees (Interprocedural.CallGraph.call_name call)
        | None -> None
      in
      log
        "Resolved callees for call `%a`:@,%a"
        Expression.pp
        (Node.create_with_default_location (Expression.Call call))
        Interprocedural.CallGraph.pp_raw_callees_option
        callees;
      callees


    let get_property_callees ~location ~attribute =
      match Map.find call_graph_of_define location with
      | Some (Interprocedural.CallGraph.Callees callees) -> Some callees
      | Some (Interprocedural.CallGraph.SyntheticCallees name_to_callees) ->
          String.Map.Tree.find name_to_callees attribute
      | None -> None


    let is_constructor () =
      match Reference.last name with
      | "__init__" -> true
      | _ -> false


    let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment

    let local_annotations =
      TypeEnvironment.ReadOnly.get_local_annotations
        environment
        (Node.value define |> Statement.Define.name |> Node.value)


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
      List.iter triggered ~f:(fun sink ->
          Hash_set.add triggered_sinks (Sinks.show_partial_sink sink));
      List.iter candidates ~f:add_flow_candidate


    let generate_issues () =
      let accumulate ~key:_ ~data:candidate issues =
        let new_issues = Flow.generate_issues ~define candidate in
        List.rev_append new_issues issues
      in
      Location.WithModule.Table.fold candidates ~f:accumulate ~init:[]


    let return_sink ~return_location =
      let taint =
        BackwardState.read
          ~root:AccessPath.Root.LocalResult
          ~path:[]
          existing_model.TaintResult.backward.sink_taint
        |> BackwardState.Tree.apply_call
             return_location
             ~callees:[]
             ~port:AccessPath.Root.LocalResult
      in
      let breadcrumbs_to_attach, via_features_to_attach =
        BackwardState.extract_features_to_attach
          ~root:AccessPath.Root.LocalResult
          ~attach_to_kind:Sinks.Attach
          existing_model.TaintResult.backward.sink_taint
      in
      taint
      |> BackwardState.Tree.add_breadcrumbs breadcrumbs_to_attach
      |> BackwardState.Tree.add_via_features via_features_to_attach


    let triggered_sinks = Location.Table.create ()

    let add_triggered_sinks ~location ~triggered_sinks:new_triggered_sinks =
      Hashtbl.set triggered_sinks ~key:location ~data:new_triggered_sinks
  end
  in
  if Statement.Define.dump_call_graph (Node.value define) then
    Map.to_alist call_graph_of_define
    |> List.map ~f:(fun (key, callees) ->
           Format.sprintf
             "%s: %s"
             (Location.show key)
             (Interprocedural.CallGraph.show_callees callees))
    |> String.concat ~sep:"\n"
    |> Log.dump
         "Call graph of `%s`:\n %s"
         (Statement.Define.name (Node.value define) |> Node.value |> Reference.show);
  let module AnalysisInstance = AnalysisInstance (Context) in
  let open AnalysisInstance in
  log "Forward analysis of callable: `%a`" Reference.pp name;
  let timer = Timer.start () in
  let cfg = Cfg.create define.value in
  let initial =
    let normalized_parameters = AccessPath.Root.normalize_parameters parameters in
    FixpointState.create ~existing_model normalized_parameters
  in
  let () = log "Processing CFG:@.%a" Cfg.pp cfg in
  let exit_state =
    Metrics.with_alarm name (fun () -> Analyzer.forward ~cfg ~initial |> Analyzer.exit) ()
  in
  let () =
    match exit_state with
    | Some exit_state -> log "Exit state:@,%a" FixpointState.pp exit_state
    | None -> log "No exit state found"
  in
  let resolution = TypeEnvironment.ReadOnly.global_resolution environment in
  let extract_model { FixpointState.taint; _ } =
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
    let model = TaintResult.Forward.{ source_taint } in
    let () = log "Forward Model:@,%a" TaintResult.Forward.pp_model model in
    model
  in
  let issues = Context.generate_issues () in
  let model = exit_state >>| extract_model |> Option.value ~default:TaintResult.Forward.empty in
  Statistics.performance
    ~randomly_log_every:1000
    ~always_log_time_threshold:1.0 (* Seconds *)
    ~name:"Forward analysis"
    ~section:`Taint
    ~normals:["callable", Reference.show name]
    ~timer
    ();
  model, issues, Context.triggered_sinks
