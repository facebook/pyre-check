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
  type t = { taint: BackwardState.t }

  include Fixpoint.State with type t := t

  val join : t -> t -> t

  val create : unit -> t
end

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

  val first_parameter : unit -> Root.t option (* For implicit self reference in super() *)

  val is_constructor : unit -> bool

  val global_resolution : GlobalResolution.t

  val local_annotations : LocalAnnotationMap.ReadOnly.t option

  val log : ('a, Format.formatter, unit, unit, unit, unit) Core.format6 -> 'a

  val triggered_sinks : ForwardAnalysis.triggered_sinks
end

module AnalysisInstance (FunctionContext : FUNCTION_CONTEXT) = struct
  let log = FunctionContext.log

  let add_first_index index indices =
    if Features.FirstIndexSet.is_bottom indices then
      Features.to_first_name index
      >>| Features.FirstIndexSet.singleton
      |> Option.value ~default:Features.FirstIndexSet.bottom
    else
      indices


  (* This is where we can observe access paths reaching into LocalReturn and record the extraneous
     paths for more precise tito. *)
  let initial_taint =
    (* We handle constructors and property setters specially and track effects. *)
    if
      FunctionContext.is_constructor ()
      || Statement.Define.is_property_setter (Node.value FunctionContext.definition)
    then
      match FunctionContext.first_parameter () with
      | Some root ->
          BackwardState.assign
            ~root
            ~path:[]
            (BackwardState.Tree.create_leaf Domains.local_return_taint)
            BackwardState.empty
      | _ -> BackwardState.empty
    else
      BackwardState.assign
        ~root:Root.LocalResult
        ~path:[]
        (BackwardState.Tree.create_leaf Domains.local_return_taint)
        BackwardState.empty


  let transform_non_leaves path taint =
    let f prefix = prefix @ path in
    match path with
    | Abstract.TreeDomain.Label.AnyIndex :: _ -> taint
    | _ -> BackwardTaint.transform Features.ReturnAccessPathSet.Element Map ~f taint


  let read_tree = BackwardState.Tree.read ~transform_non_leaves

  module rec FixpointState : FixpointState = struct
    type t = { taint: BackwardState.t }

    let pp formatter { taint } = BackwardState.pp formatter taint

    let show = Format.asprintf "%a" pp

    let create () = { taint = BackwardState.empty }

    let less_or_equal ~left:{ taint = left; _ } ~right:{ taint = right; _ } =
      BackwardState.less_or_equal ~left ~right


    let join { taint = left } { taint = right; _ } =
      let taint = BackwardState.join left right in
      { taint }


    let widen ~previous:{ taint = prev; _ } ~next:{ taint = next; _ } ~iteration =
      let taint = BackwardState.widen ~iteration ~prev ~next in
      { taint }


    let get_taint access_path { taint; _ } =
      match access_path with
      | None -> BackwardState.Tree.empty
      | Some { root; path } -> BackwardState.read ~transform_non_leaves ~root ~path taint


    let store_weak_taint ~root ~path taint { taint = state_taint } =
      { taint = BackwardState.assign ~weak:true ~root ~path taint state_taint }


    let analyze_definition ~define:_ state = state

    let rec apply_call_targets
        ~resolution
        ~call_expression
        location
        arguments
        initial_state
        call_taint
        call_targets
      =
      let analyze_call_target call_target =
        let triggered_taint =
          match Hashtbl.find FunctionContext.triggered_sinks location with
          | Some items ->
              List.fold
                items
                ~f:(fun state (root, sink) ->
                  let new_taint = BackwardState.Tree.create_leaf (BackwardTaint.singleton sink) in
                  BackwardState.assign ~root ~path:[] new_taint state)
                ~init:BackwardState.bottom
          | None -> BackwardState.bottom
        in
        let taint_model = Model.get_callsite_model ~resolution ~call_target ~arguments in
        log
          "Backward analysis of call: %a@,Call site model:@,%a"
          Expression.pp
          (Node.create_with_default_location call_expression)
          Model.pp
          taint_model;
        let { TaintResult.backward; sanitizers; modes; _ } = taint_model.model in
        let sink_taint = BackwardState.join backward.sink_taint triggered_taint in
        let sink_argument_matches =
          BackwardState.roots sink_taint |> AccessPath.match_actuals_to_formals arguments
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
        in
        let combine_sink_taint location taint_tree { root; actual_path; formal_path } =
          BackwardState.read ~transform_non_leaves ~root ~path:[] sink_taint
          |> BackwardState.Tree.apply_call location ~callees:[call_target] ~port:root
          |> read_tree formal_path
          |> BackwardState.Tree.prepend actual_path
          |> BackwardState.Tree.join taint_tree
        in
        let get_argument_taint ~resolution ~argument:{ Call.Argument.value = argument; _ } =
          let global_sink =
            Model.get_global_model
              ~resolution
              ~location:
                (Location.with_module ~qualifier:FunctionContext.qualifier (Node.location argument))
              ~expression:argument
            |> Model.GlobalModel.get_sink
          in
          let access_path = of_expression ~resolution argument in
          get_taint access_path initial_state |> BackwardState.Tree.join global_sink
        in
        let combine_tito location taint_tree { AccessPath.root; actual_path; formal_path } =
          let translate_tito (tito_path, element) argument_taint =
            let compute_parameter_tito ~key:kind ~data:element argument_taint =
              let extra_paths =
                match kind with
                | Sinks.LocalReturn ->
                    BackwardTaint.fold
                      Features.ReturnAccessPathSet.Element
                      element
                      ~f:List.cons
                      ~init:[]
                | _ ->
                    (* No special path handling for side effect taint *)
                    [[]]
              in
              let breadcrumbs = BackwardTaint.breadcrumbs element in
              let tito_depth =
                BackwardTaint.fold
                  TraceLength.Self
                  element
                  ~f:TraceLength.join
                  ~init:TraceLength.bottom
              in
              let taint_to_propagate =
                match kind with
                | Sinks.LocalReturn -> call_taint
                (* Attach nodes shouldn't affect analysis. *)
                | Sinks.Attach -> BackwardState.Tree.empty
                | Sinks.ParameterUpdate n -> (
                    match List.nth arguments n with
                    | None -> BackwardState.Tree.empty
                    | Some argument -> get_argument_taint ~resolution ~argument)
                | _ -> failwith "unexpected tito sink"
              in
              let compute_tito_depth kind depth =
                match kind with
                | Sinks.LocalReturn -> max depth (1 + tito_depth)
                | _ -> depth
              in
              List.fold
                extra_paths
                ~f:(fun taint extra_path ->
                  read_tree extra_path taint_to_propagate
                  |> BackwardState.Tree.collapse
                       ~transform:(BackwardTaint.add_breadcrumbs Features.tito_broadening)
                  |> BackwardTaint.add_breadcrumbs breadcrumbs
                  |> BackwardTaint.transform
                       TraceLength.Self
                       (Context (BackwardTaint.kind, Map))
                       ~f:compute_tito_depth
                  |> BackwardState.Tree.create_leaf
                  |> BackwardState.Tree.prepend tito_path
                  |> BackwardState.Tree.join taint)
                ~init:argument_taint
            in
            BackwardTaint.partition BackwardTaint.kind By ~f:Fn.id element
            |> Map.Poly.fold ~f:compute_parameter_tito ~init:argument_taint
          in
          let add_tito_feature_and_position leaf_taint =
            leaf_taint
            |> FlowDetails.add_tito_position location
            |> FlowDetails.add_breadcrumb Features.tito
          in
          BackwardState.read
            ~transform_non_leaves
            ~root
            ~path:formal_path
            backward.taint_in_taint_out
          |> BackwardState.Tree.fold
               BackwardState.Tree.Path
               ~f:translate_tito
               ~init:BackwardState.Tree.bottom
          |> BackwardState.Tree.transform FlowDetails.Self Map ~f:add_tito_feature_and_position
          |> BackwardState.Tree.prepend actual_path
          |> BackwardState.Tree.join taint_tree
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
          | Some AllTito -> BackwardState.Tree.bottom
          | Some (SpecificTito { sanitized_tito_sinks; _ }) ->
              BackwardState.Tree.transform
                BackwardTaint.kind
                Filter
                ~f:(fun sink -> not (Sinks.Set.mem sink sanitized_tito_sinks))
                taint_to_propagate
          | None -> taint_to_propagate
        in
        let analyze_argument
            ~obscure_taint
            (arguments_taint, state)
            ((argument, sink_matches), ((_, tito_matches), (_, sanitize_matches)))
          =
          let location =
            Location.with_module ~qualifier:FunctionContext.qualifier argument.Node.location
          in
          let sink_taint =
            List.fold sink_matches ~f:(combine_sink_taint location) ~init:BackwardState.Tree.empty
          in
          let taint_in_taint_out =
            List.fold
              tito_matches
              ~f:(combine_tito argument.Node.location)
              ~init:BackwardState.Tree.empty
          in
          let taint_in_taint_out =
            BackwardState.Tree.transform
              Features.TitoPositionSet.Element
              Add
              ~f:argument.Node.location
              obscure_taint
            |> BackwardState.Tree.join taint_in_taint_out
          in
          let taint_in_taint_out = apply_tito_sanitizers sanitize_matches taint_in_taint_out in
          let taint = BackwardState.Tree.join sink_taint taint_in_taint_out in
          let state =
            match AccessPath.of_expression ~resolution argument with
            | Some { AccessPath.root; path } ->
                let breadcrumbs_to_add =
                  BackwardState.Tree.filter_by_kind ~kind:Sinks.AddFeatureToArgument sink_taint
                  |> BackwardTaint.breadcrumbs
                in
                if Features.BreadcrumbSet.is_bottom breadcrumbs_to_add then
                  state
                else
                  let taint =
                    BackwardState.read state.taint ~root ~path
                    |> BackwardState.Tree.add_breadcrumbs breadcrumbs_to_add
                  in
                  { taint = BackwardState.assign ~root ~path taint state.taint }
            | None -> state
          in
          taint :: arguments_taint, state
        in
        let obscure_taint =
          if TaintResult.ModeSet.contains Obscure modes then
            let annotation =
              Resolution.resolve_expression_to_type
                resolution
                { Node.value = call_expression; location }
            in
            let breadcrumbs =
              Features.type_breadcrumbs
                ~resolution:(Resolution.global_resolution resolution)
                (Some annotation)
              |> Features.BreadcrumbSet.add Features.obscure
            in
            BackwardState.Tree.collapse
              ~transform:(BackwardTaint.add_breadcrumbs Features.tito_broadening)
              call_taint
            |> BackwardTaint.add_breadcrumbs breadcrumbs
            |> BackwardState.Tree.create_leaf
          else
            BackwardState.Tree.bottom
        in
        List.rev combined_matches
        |> List.fold ~f:(analyze_argument ~obscure_taint) ~init:([], initial_state)
      in
      let call_targets =
        match call_targets with
        | [] when TaintConfiguration.is_missing_flow_analysis Type ->
            (* Create a symbolic callable, using the location as the name *)
            let callable =
              Model.unknown_callee
                ~location:(Location.with_module ~qualifier:FunctionContext.qualifier location)
                ~call:call_expression
            in
            if not (Interprocedural.FixpointState.has_model callable) then
              Model.register_unknown_callee_model callable;
            [callable]
        | _ -> call_targets
      in
      match call_targets with
      | [] -> (
          (* If we don't have a call target: propagate argument taint. *)
          let analyze_argument ~resolution taint { Call.Argument.value = argument; _ } state =
            let taint =
              BackwardState.Tree.transform
                Features.TitoPositionSet.Element
                Add
                ~f:argument.Node.location
                taint
            in
            analyze_expression ~resolution ~taint ~state ~expression:argument
          in
          let obscure_taint =
            BackwardState.Tree.collapse
              ~transform:(BackwardTaint.add_breadcrumbs Features.tito_broadening)
              call_taint
            |> BackwardTaint.add_breadcrumb Features.obscure
            |> BackwardState.Tree.create_leaf
          in
          let state =
            List.fold_right
              ~f:(analyze_argument ~resolution obscure_taint)
              arguments
              ~init:initial_state
          in
          match call_expression with
          | Expression.Call { callee; _ } ->
              analyze_expression ~resolution ~taint:obscure_taint ~state ~expression:callee
          | _ -> state)
      | call_targets ->
          let arguments_taint, state =
            List.map call_targets ~f:analyze_call_target
            |> List.fold
                 ~f:(fun (arguments_taint, state) (new_arguments_taint, new_state) ->
                   ( List.map2_exn arguments_taint new_arguments_taint ~f:BackwardState.Tree.join,
                     FixpointState.join state new_state ))
                 ~init:
                   ( List.map arguments ~f:(fun _ -> BackwardState.Tree.bottom),
                     FixpointState.create () )
          in
          List.zip_exn arguments arguments_taint
          |> List.fold
               ~init:state
               ~f:(fun state ({ Call.Argument.value = argument; _ }, argument_taint) ->
                 analyze_unstarred_expression ~resolution argument_taint argument state)


    and analyze_constructor_call
        ~resolution
        ~location
        ~callee
        ~arguments
        ~state
        ~taint
        ~new_targets
        ~init_targets
      =
      (* Since we're searching for ClassType.__new/init__(), Pyre will correctly not insert an
         implicit type there. However, since the actual call was ClassType(), insert the implicit
         receiver here ourselves and ignore the value from get_indirect_targets. *)
      let apply_call_targets state targets =
        apply_call_targets
          ~resolution
          ~call_expression:(Expression.Call { Call.callee; arguments })
          location
          ({ Call.Argument.name = None; value = callee } :: arguments)
          state
          taint
          targets
      in
      let state = apply_call_targets state init_targets in
      match new_targets with
      | [] -> state
      | [`Method { Interprocedural.Target.class_name = "object"; method_name = "__new__" }] -> state
      | targets -> apply_call_targets state targets


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
        Resolution.resolve_assignment
          resolution
          (Statement.Statement.generator_assignment generator)
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


    and analyze_call ~resolution location ~taint ~state ~callee ~arguments =
      let analyze_regular_targets ~state ~callee ~arguments ~taint targets =
        let { Call.callee; arguments } =
          Interprocedural.CallGraph.redirect_special_calls ~resolution { Call.callee; arguments }
        in
        match targets with
        | None ->
            (* Obscure. *)
            apply_call_targets
              ~call_expression:(Expression.Call { Call.callee; arguments })
              ~resolution
              location
              arguments
              state
              taint
              []
        | Some { Interprocedural.CallGraph.implicit_self; targets; _ } ->
            let arguments =
              if implicit_self then
                let receiver =
                  match Node.value callee with
                  | Expression.Name (Name.Attribute { base; _ }) -> base
                  | _ ->
                      (* Default to a benign self if we don't understand/retain information of what
                         self is. *)
                      Node.create_with_default_location (Expression.Name (Name.Identifier "None"))
                in
                { Call.Argument.name = None; value = receiver } :: arguments
              else
                arguments
            in
            let targets, taint =
              match Node.value callee with
              | Name (Name.Attribute { base; attribute; _ }) ->
                  (* Add index breadcrumb if appropriate. *)
                  let taint =
                    if not (String.equal attribute "get") then
                      taint
                    else
                      match arguments with
                      | _ :: index :: _ ->
                          let label = get_index index.value in
                          BackwardState.Tree.transform
                            Features.FirstIndexSet.Self
                            Map
                            ~f:(add_first_index label)
                            taint
                      | _ -> taint
                  in
                  (* Specially handle super.__init__ calls and explicit calls to superclass'
                     `__init__` in constructors for tito. *)
                  if
                    FunctionContext.is_constructor ()
                    && String.equal attribute "__init__"
                    && Interprocedural.CallResolution.is_super
                         ~resolution
                         ~define:FunctionContext.definition
                         base
                  then
                    (* If the super call is `object.__init__`, this is likely due to a lack of type
                       information for that constructor - we treat that case as obscure to not lose
                       argument taint for these calls. *)
                    let targets =
                      match targets with
                      | [`Method { class_name = "object"; method_name = "__init__" }] -> []
                      | _ -> targets
                    in
                    ( targets,
                      BackwardState.Tree.create_leaf Domains.local_return_taint
                      |> BackwardState.Tree.join taint )
                  else
                    targets, taint
              | _ -> targets, taint
            in
            apply_call_targets
              ~call_expression:(Expression.Call { Call.callee; arguments })
              ~resolution
              location
              arguments
              state
              taint
              targets
      in
      let analyze_lambda_call
          ~lambda_argument
          ~non_lambda_arguments
          ~higher_order_function
          ~callable_argument
        =
        let ( lambda_index,
              {
                Call.Argument.value = { location = lambda_location; _ } as lambda_callee;
                name = lambda_name;
              } )
          =
          lambda_argument
        in
        (* If we have a lambda `fn` getting passed into `hof`, we use the following strategy:
         * hof(q, fn, x, y) gets translated into (analyzed backwards)
         * if rand():
         *   $all = {q, x, y}
         *   $result = fn( *all, **all)
         * else:
         *   $result = fn
         * hof(q, $result, x, y)
         *)
        (* Simulate hof(q, $result, x, y). *)
        let result = "$result" in
        let higher_order_function_arguments =
          let lambda_argument_with_index =
            ( lambda_index,
              {
                Call.Argument.value =
                  Node.create ~location:lambda_location (Expression.Name (Name.Identifier result));
                name = lambda_name;
              } )
          in
          let compare_by_index (left_index, _) (right_index, _) =
            Int.compare left_index right_index
          in
          lambda_argument_with_index :: non_lambda_arguments
          |> List.sort ~compare:compare_by_index
          |> List.map ~f:snd
        in
        let state =
          analyze_regular_targets
            ~state
            ~taint
            ~callee
            ~arguments:higher_order_function_arguments
            (Some higher_order_function)
        in
        let result_taint =
          BackwardState.Tree.join
            taint
            (get_taint (Some (AccessPath.create (Root.Variable result) [])) state)
        in

        (* Simulate else branch. *)
        let else_branch_state =
          (* Simulate $result = fn. *)
          analyze_expression ~resolution ~taint:result_taint ~state ~expression:lambda_callee
        in

        (* Simulate if branch. *)
        let if_branch_state =
          (* Simulate $result = fn( all, all). *)
          let all_argument =
            Node.create ~location:lambda_location (Expression.Name (Name.Identifier "$all"))
          in
          let arguments_with_all_value =
            List.map non_lambda_arguments ~f:snd
            |> List.map ~f:(fun argument -> { argument with Call.Argument.value = all_argument })
          in
          let state =
            analyze_regular_targets
              ~state
              ~taint:result_taint
              ~callee:lambda_callee
              ~arguments:arguments_with_all_value
              (Some callable_argument)
          in

          (* Simulate `$all = {q, x, y}`. *)
          let all_taint =
            BackwardState.Tree.join
              taint
              (get_taint (Some (AccessPath.create (Root.Variable "$all") [])) state)
            |> BackwardState.Tree.add_breadcrumb Features.lambda
          in
          let all_assignee =
            Node.create
              ~location
              (Expression.Set
                 (List.map non_lambda_arguments ~f:(fun (_, argument) ->
                      argument.Call.Argument.value)))
          in
          analyze_expression ~resolution ~taint:all_taint ~state ~expression:all_assignee
        in
        join if_branch_state else_branch_state
      in
      match { Call.callee; arguments } with
      | {
       callee =
         { Node.value = Name (Name.Attribute { base; attribute = "__setitem__"; _ }); _ } as callee;
       arguments = [{ Call.Argument.value = index; _ }; { Call.Argument.value; _ }] as arguments;
      } ->
          (* Ensure we simulate the body of __setitem__ in case the function contains taint. *)
          let state =
            match FunctionContext.get_callees ~location ~call:{ Call.callee; arguments } with
            | Some (RegularTargets targets) ->
                analyze_regular_targets ~state ~callee ~arguments ~taint (Some targets)
            | _ -> state
          in
          (* Handle base[index] = value. *)
          analyze_assignment
            ~resolution
            ~fields:[AccessPath.get_index index]
            ~target:base
            ~value
            state
      | {
       callee = { Node.value = Name (Name.Attribute { base; attribute = "__getitem__"; _ }); _ };
       arguments = [{ Call.Argument.value = argument_value; _ }];
      } ->
          let index = AccessPath.get_index argument_value in
          let taint =
            BackwardState.Tree.prepend [index] taint
            |> BackwardState.Tree.transform
                 Features.FirstIndexSet.Self
                 Map
                 ~f:(add_first_index index)
          in

          analyze_expression ~resolution ~taint ~state ~expression:base
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
              Resolution.resolve_expression_to_type resolution base |> Type.is_dictionary_or_mapping
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
           location;
         };
       arguments =
         [
           { Call.Argument.value = self; name = None };
           {
             Call.Argument.value =
               { Node.value = Expression.String { value = attribute; kind = String }; _ };
             name = None;
           };
           { Call.Argument.value; name = None };
         ];
      } ->
          analyze_assignment
            ~resolution
            ~target:
              {
                Node.value = Name (Name.Attribute { base = self; attribute; special = true });
                location;
              }
            ~value
            state
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
          let state =
            analyze_expression ~resolution ~state ~expression:attribute_expression ~taint
          in
          analyze_expression ~resolution ~state ~expression:default ~taint
      (* `zip(a, b, ...)` creates a taint object whose first index has a's taint, second index has
         b's taint, etc. *)
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
      (* dictionary .keys(), .values() and .items() functions are special, as they require handling
         of DictionaryKeys taint. *)
      | { callee = { Node.value = Name (Name.Attribute { base; attribute = "values"; _ }); _ }; _ }
        when Resolution.resolve_expression_to_type resolution base |> Type.is_dictionary_or_mapping
        ->
          let taint =
            taint
            |> BackwardState.Tree.read [Abstract.TreeDomain.Label.AnyIndex]
            |> BackwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]
          in
          analyze_expression ~resolution ~taint ~state ~expression:base
      | { callee = { Node.value = Name (Name.Attribute { base; attribute = "keys"; _ }); _ }; _ }
        when Resolution.resolve_expression_to_type resolution base |> Type.is_dictionary_or_mapping
        ->
          let taint =
            taint
            |> BackwardState.Tree.read [AccessPath.dictionary_keys]
            |> BackwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex]
          in
          analyze_expression ~resolution ~taint ~state ~expression:base
      | { callee = { Node.value = Name (Name.Attribute { base; attribute = "items"; _ }); _ }; _ }
        when Resolution.resolve_expression_to_type resolution base |> Type.is_dictionary_or_mapping
        ->
          (* When we're faced with an assign of the form `k, v = d.items().__iter__().__next__()`,
             the taint we analyze d.items() under will be {* -> {0 -> k, 1 -> v} }. We want to
             analyze d itself under the taint of `{* -> v, $keys -> k}`. *)
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
      | {
       Call.callee = { Node.value = Name (Name.Identifier "reveal_taint"); _ };
       arguments = [{ Call.Argument.value = expression; _ }];
      } ->
          begin
            match of_expression ~resolution expression with
            | None ->
                Log.dump
                  "%a: Revealed backward taint for `%s`: expression is too complex"
                  Location.WithModule.pp
                  (Location.with_module location ~qualifier:FunctionContext.qualifier)
                  (Transform.sanitize_expression expression |> Expression.show)
            | access_path ->
                let taint = get_taint access_path state in
                Log.dump
                  "%a: Revealed backward taint for `%s`: %s"
                  Location.WithModule.pp
                  (Location.with_module location ~qualifier:FunctionContext.qualifier)
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
              match FunctionContext.first_parameter () with
              | Some root -> store_weak_taint ~root ~path:[] taint state
              | None -> state))
      | _ -> (
          let taint =
            match Node.value callee with
            | Name
                (Name.Attribute
                  { base = { Node.value = Expression.String _; _ }; attribute = "format"; _ }) ->
                BackwardState.Tree.add_breadcrumb Features.format_string taint
            | _ -> taint
          in
          match FunctionContext.get_callees ~location ~call:{ Call.callee; arguments } with
          | Some (RegularTargets targets) ->
              analyze_regular_targets ~state ~callee ~arguments ~taint (Some targets)
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
                    ~lambda_argument:(index, lambda_argument)
                    ~non_lambda_arguments
                    ~higher_order_function
                    ~callable_argument
              | _ ->
                  analyze_regular_targets
                    ~state
                    ~callee
                    ~arguments
                    ~taint
                    (Some higher_order_function))
          | Some (ConstructorTargets { new_targets; init_targets }) ->
              analyze_constructor_call
                ~resolution
                ~new_targets
                ~init_targets
                ~location
                ~state
                ~taint
                ~callee
                ~arguments
          | None -> analyze_regular_targets ~state ~callee ~arguments ~taint None)


    and analyze_string_literal ~resolution ~taint ~state ~location { StringLiteral.value; kind } =
      match kind with
      | StringLiteral.Format expressions ->
          let taint =
            let literal_string_sinks = TaintConfiguration.literal_string_sinks () in
            if List.is_empty literal_string_sinks then
              taint
            else
              List.fold
                literal_string_sinks
                ~f:(fun taint { TaintConfiguration.sink_kind; pattern } ->
                  if Re2.matches pattern value then
                    BackwardState.Tree.join
                      taint
                      (BackwardState.Tree.create_leaf (BackwardTaint.singleton ~location sink_kind))
                  else
                    taint)
                ~init:taint
          in
          let taint = BackwardState.Tree.add_breadcrumb Features.format_string taint in
          List.fold
            expressions
            ~f:(fun state expression -> analyze_expression ~resolution ~taint ~state ~expression)
            ~init:state
      | _ -> state


    and analyze_expression ~resolution ~taint ~state ~expression =
      log
        "Backward analysis of expression: `%a` with backward taint: %a"
        Expression.pp
        expression
        BackwardState.Tree.pp
        taint;
      let { Node.location; value = expression } = expression in
      match expression with
      | Await expression -> analyze_expression ~resolution ~taint ~state ~expression
      | BooleanOperator { left; operator = _; right } ->
          analyze_expression ~resolution ~taint ~state ~expression:right
          |> fun state -> analyze_expression ~resolution ~taint ~state ~expression:left
      | ComparisonOperator ({ left; operator = _; right } as comparison) -> (
          match ComparisonOperator.override ~location comparison with
          | Some override -> analyze_expression ~resolution ~taint ~state ~expression:override
          | None ->
              let taint = BackwardState.Tree.add_breadcrumbs Features.type_bool taint in
              analyze_expression ~resolution ~taint ~state ~expression:right
              |> fun state -> analyze_expression ~resolution ~taint ~state ~expression:left)
      | Call { callee; arguments } ->
          analyze_call ~resolution location ~taint ~state ~callee ~arguments
      | Complex _ -> state
      | Dictionary { Dictionary.entries; keywords } ->
          let state =
            List.fold ~f:(analyze_dictionary_entry ~resolution taint) entries ~init:state
          in
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
      | Ellipsis
      | False
      | Float _ ->
          state
      | Generator comprehension -> analyze_comprehension ~resolution taint comprehension state
      | Integer _ -> state
      | Lambda { parameters = _; body } ->
          (* Ignore parameter bindings and pretend body is inlined *)
          analyze_expression ~resolution ~taint ~state ~expression:body
      | List list ->
          let total = List.length list in
          List.rev list
          |> List.foldi ~f:(analyze_reverse_list_element ~total ~resolution taint) ~init:state
      | ListComprehension comprehension ->
          analyze_comprehension ~resolution taint comprehension state
      | Name _ when AccessPath.is_global ~resolution { Node.location; value = expression } -> state
      | Name (Name.Identifier identifier) ->
          store_weak_taint ~root:(Root.Variable identifier) ~path:[] taint state
      | Name (Name.Attribute { base; attribute = "__dict__"; _ }) ->
          analyze_expression ~resolution ~taint ~state ~expression:base
      | Name (Name.Attribute { base; attribute; _ }) -> (
          match FunctionContext.get_property_callees ~location ~attribute with
          | Some (RegularTargets { targets; _ }) ->
              let arguments = [{ Call.Argument.name = None; value = base }] in
              apply_call_targets
                ~resolution
                ~call_expression:expression
                location
                arguments
                state
                taint
                targets
          | _ ->
              let field = Abstract.TreeDomain.Label.Index attribute in
              let expression =
                Node.create_with_default_location
                  (Expression.Name
                     (Name.Attribute { Name.Attribute.base; attribute; special = false }))
              in
              let location = Location.with_module ~qualifier:FunctionContext.qualifier location in
              let global_model = Model.get_global_model ~resolution ~expression ~location in
              let add_tito_features taint =
                let attribute_breadcrumbs =
                  global_model |> Model.GlobalModel.get_tito |> BackwardState.Tree.breadcrumbs
                in
                BackwardState.Tree.add_breadcrumbs attribute_breadcrumbs taint
              in

              let apply_attribute_sanitizers taint =
                match Model.GlobalModel.get_sanitize global_model with
                | { Sanitize.sinks = Some AllSinks; _ } -> BackwardState.Tree.empty
                | { Sanitize.sinks = Some (SpecificSinks sanitized_sinks); _ } ->
                    BackwardState.Tree.transform
                      BackwardTaint.kind
                      Filter
                      ~f:(fun sink -> not (Sinks.Set.mem sink sanitized_sinks))
                      taint
                | _ -> taint
              in
              let taint =
                BackwardState.Tree.prepend [field] (add_tito_features taint)
                |> apply_attribute_sanitizers
              in
              analyze_expression ~resolution ~taint ~state ~expression:base)
      | Set set ->
          let element_taint = read_tree [Abstract.TreeDomain.Label.AnyIndex] taint in
          List.fold
            set
            ~f:(fun state expression ->
              analyze_expression ~resolution ~taint:element_taint ~state ~expression)
            ~init:state
      | SetComprehension comprehension ->
          analyze_comprehension ~resolution taint comprehension state
      | Starred (Starred.Once expression)
      | Starred (Starred.Twice expression) ->
          let taint = BackwardState.Tree.prepend [Abstract.TreeDomain.Label.AnyIndex] taint in
          analyze_expression ~resolution ~taint ~state ~expression
      | String string_literal ->
          analyze_string_literal
            ~resolution
            ~taint
            ~state
            ~location:(Location.with_module ~qualifier:FunctionContext.qualifier location)
            string_literal
      | Ternary { target; test; alternative } ->
          let state_then = analyze_expression ~resolution ~taint ~state ~expression:target in
          let state_else = analyze_expression ~resolution ~taint ~state ~expression:alternative in
          join state_then state_else
          |> fun state ->
          analyze_expression ~resolution ~taint:BackwardState.Tree.empty ~state ~expression:test
      | True -> state
      | Tuple list ->
          let total = List.length list in
          List.rev list
          |> List.foldi ~f:(analyze_reverse_list_element ~total ~resolution taint) ~init:state
      | UnaryOperator { operator = _; operand } ->
          analyze_expression ~resolution ~taint ~state ~expression:operand
      | WalrusOperator { target; value } ->
          analyze_expression ~resolution ~taint ~state ~expression:value
          |> fun state -> analyze_expression ~resolution ~taint ~state ~expression:target
      | Yield None -> state
      | Yield (Some expression)
      | YieldFrom expression ->
          let access_path = { root = Root.LocalResult; path = [] } in
          let return_taint = get_taint (Some access_path) state in
          analyze_expression ~resolution ~taint:return_taint ~state ~expression


    (* Returns the taint, and whether to collapse one level (due to star expression) *)
    and compute_assignment_taint ~resolution target state =
      match target.Node.value with
      | Expression.Starred (Once target | Twice target) ->
          (* This is approximate. Unless we can get the tuple type on the right to tell how many
             total elements there will be, we just pick up the entire collection. *)
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
            callee =
              { Node.value = Name (Name.Attribute { base; attribute = "__getitem__"; _ }); _ };
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
              let access_path = of_expression ~resolution target in
              get_taint access_path state
            in
            let global_taint =
              let location =
                Location.with_module ~qualifier:FunctionContext.qualifier target.Node.location
              in
              Model.get_global_model ~resolution ~location ~expression:target
              |> Model.GlobalModel.get_sink
            in
            BackwardState.Tree.join local_taint global_taint
          in
          taint, false


    and analyze_assignment ~resolution ?(fields = []) ~target ~value state =
      let taint = compute_assignment_taint ~resolution target state |> fst |> read_tree fields in
      let state =
        let rec clear_taint state target =
          match Node.value target with
          | Expression.Tuple items -> List.fold items ~f:clear_taint ~init:state
          | _ -> (
              match of_expression ~resolution target with
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
        clear_taint state target
      in
      analyze_expression ~resolution ~taint ~state ~expression:value


    let analyze_statement ~resolution state { Node.value = statement; _ } =
      match statement with
      | Statement.Statement.Assign { value = { Node.value = Expression.Ellipsis; _ }; _ } -> state
      | Assign { target = { Node.location; value = target_value } as target; value; _ } -> (
          let target_is_sanitized =
            match target_value with
            | Name (Name.Attribute _) ->
                let location =
                  Location.with_module ~qualifier:FunctionContext.qualifier target.Node.location
                in
                Model.get_global_model ~resolution ~location ~expression:target
                |> Model.GlobalModel.is_sanitized
            | _ -> false
          in
          if target_is_sanitized then
            analyze_expression ~resolution ~taint:BackwardState.Tree.bottom ~state ~expression:value
          else
            match target_value with
            | Expression.Name (Name.Attribute { base; attribute; _ }) -> (
                match FunctionContext.get_property_callees ~location ~attribute with
                | Some (RegularTargets { targets; _ }) ->
                    (* Treat `a.property = x` as `a = a.property(x)` *)
                    let taint = compute_assignment_taint ~resolution base state |> fst in
                    let arguments =
                      [{ Call.Argument.name = None; value = base }; { name = None; value }]
                    in
                    apply_call_targets
                      ~resolution
                      ~call_expression:(Expression.Call { Call.callee = target; arguments })
                      location
                      arguments
                      state
                      taint
                      targets
                | _ -> analyze_assignment ~resolution ~target ~value state)
            | _ -> analyze_assignment ~resolution ~target ~value state)
      | Assert _
      | Break
      | Class _
      | Continue ->
          state
      | Define define -> analyze_definition ~define state
      | Delete _ -> state
      | Expression expression ->
          analyze_expression ~resolution ~taint:BackwardState.Tree.empty ~state ~expression
      | For _
      | Global _
      | If _
      | Import _
      | Nonlocal _
      | Pass
      | Raise _ ->
          state
      | Return { expression = Some expression; _ } ->
          let access_path = { root = Root.LocalResult; path = [] } in
          let return_taint = get_taint (Some access_path) state in
          analyze_expression ~resolution ~taint:return_taint ~state ~expression
      | Return { expression = None; _ }
      | Try _
      | With _
      | While _ ->
          state


    let backward ~key state ~statement =
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
          ~global_resolution:FunctionContext.global_resolution
          ~local_annotations:FunctionContext.local_annotations
          ~parent
          ~key
          (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
          (module TypeCheck.DummyContext)
      in

      analyze_statement ~resolution state statement


    let forward ~key:_ _ ~statement:_ = failwith "Don't call me"
  end

  and Analyzer : (Fixpoint.Fixpoint with type state = FixpointState.t) =
    Fixpoint.Make (FixpointState)
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
    let annotation = Option.map ~f:(GlobalResolution.parse_annotation resolution) annotation in
    let type_breadcrumbs = Features.type_breadcrumbs ~resolution annotation in

    let essential =
      if is_constructor then
        BackwardState.Tree.essential_for_constructor tree
      else
        BackwardState.Tree.essential tree
    in
    BackwardState.Tree.shape
      ~transform:(BackwardTaint.add_breadcrumbs Features.widen_broadening)
      tree
      ~mold:essential
    |> BackwardState.Tree.add_breadcrumbs type_breadcrumbs
    |> BackwardState.Tree.limit_to
         ~transform:(BackwardTaint.add_breadcrumbs Features.widen_broadening)
         ~width:maximum_model_width
    |> BackwardState.Tree.approximate_return_access_paths ~maximum_return_access_path_length
  in

  let split_and_simplify model (parameter, name, original) =
    let annotation = original.Node.value.Parameter.annotation in
    let partition =
      BackwardState.read ~root:(Root.Variable name) ~path:[] entry_taint
      |> BackwardState.Tree.partition BackwardTaint.kind By ~f:Fn.id
    in
    let taint_in_taint_out =
      let breadcrumbs_to_attach, via_features_to_attach =
        BackwardState.extract_features_to_attach
          ~root:parameter
          ~attach_to_kind:Sinks.Attach
          existing_backward.TaintResult.Backward.taint_in_taint_out
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
        |> BackwardState.Tree.add_breadcrumbs breadcrumbs_to_attach
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
          ~transform:(BackwardTaint.add_breadcrumbs Features.widen_broadening)
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
          existing_backward.TaintResult.Backward.sink_taint
      in
      sink_taint
      |> BackwardState.Tree.add_breadcrumbs breadcrumbs_to_attach
      |> BackwardState.Tree.add_via_features via_features_to_attach
    in
    TaintResult.Backward.
      {
        taint_in_taint_out =
          BackwardState.assign ~root:parameter ~path:[] taint_in_taint_out model.taint_in_taint_out;
        sink_taint = BackwardState.assign ~root:parameter ~path:[] sink_taint model.sink_taint;
      }
  in
  List.fold normalized_parameters ~f:split_and_simplify ~init:TaintResult.Backward.empty


let run ~environment ~qualifier ~define ~call_graph_of_define ~existing_model ~triggered_sinks =
  let timer = Timer.start () in
  let ({
         Node.value = { Statement.Define.signature = { name = { Node.value = name; _ }; _ }; _ };
         _;
       } as define)
    =
    (* Apply decorators to make sure we match parameters up correctly. *)
    let resolution = TypeEnvironment.ReadOnly.global_resolution environment in
    Annotated.Define.create define
    |> Annotated.Define.decorate ~resolution
    |> Annotated.Define.define
  in
  let is_constructor () =
    match Reference.last name with
    | "__init__" -> true
    | _ -> false
  in
  let module AnalysisInstance = AnalysisInstance (struct
    let qualifier = qualifier

    let definition = define

    let debug = Statement.Define.dump define.value

    let log format =
      if debug then
        Log.dump format
      else
        Log.log ~section:`Taint format


    let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment

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


    let local_annotations =
      TypeEnvironment.ReadOnly.get_local_annotations
        environment
        (Node.value define |> Statement.Define.name |> Node.value)


    let is_constructor = is_constructor

    let first_parameter () =
      match define.value.Statement.Define.signature.parameters with
      | { Node.value = { Parameter.name; _ }; _ } :: _ -> Some (Root.Variable name)
      | _ -> None


    let triggered_sinks = triggered_sinks
  end)
  in
  let open AnalysisInstance in
  let initial = FixpointState.{ taint = initial_taint } in
  let cfg = Cfg.create define.value in
  let () = log "Backward analysis of callable: `%a`" Reference.pp name in
  let entry_state =
    Metrics.with_alarm name (fun () -> Analyzer.backward ~cfg ~initial |> Analyzer.entry) ()
  in
  let () =
    match entry_state with
    | Some entry_state -> log "Entry state:@,%a" FixpointState.pp entry_state
    | None -> log "No entry state found"
  in
  let resolution = TypeEnvironment.ReadOnly.global_resolution environment in
  let extract_model FixpointState.{ taint; _ } =
    let model =
      extract_tito_and_sink_models
        ~is_constructor:(is_constructor ())
        define.value
        ~resolution
        ~existing_backward:existing_model.TaintResult.backward
        taint
    in
    let () = log "Backward Model:@,%a" TaintResult.Backward.pp_model model in
    model
  in
  Statistics.performance
    ~randomly_log_every:1000
    ~always_log_time_threshold:1.0 (* Seconds *)
    ~name:"Backward analysis"
    ~normals:["callable", Reference.show name]
    ~section:`Taint
    ~timer
    ();

  entry_state >>| extract_model |> Option.value ~default:TaintResult.Backward.empty
