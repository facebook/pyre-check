(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Analysis
open Ast
open Expression
open Pyre
open Statement
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

  val definition : Define.t Node.t

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

  val return_sink : BackwardState.Tree.t

  val debug : bool

  val add_triggered_sinks : location:Location.t -> triggered_sinks:(Root.t * Sinks.t) list -> unit
end

let number_regexp = Str.regexp "[0-9]+"

let is_numeric name = Str.string_match number_regexp name 0

let ( |>> ) (taint, state) f = f taint, state

module AnalysisInstance (FunctionContext : FUNCTION_CONTEXT) = struct
  let log format =
    if FunctionContext.debug then
      Log.dump format
    else
      Log.log ~section:`Taint format


  module rec FixpointState : FixpointState = struct
    type t = { taint: ForwardState.t } [@@deriving show { with_path = false }]

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


    let add_first kind name set =
      let open Features in
      let already_has_first feature =
        match feature.Abstract.OverUnderSetDomain.element with
        | Simple.Breadcrumb (Breadcrumb.First { kind = has_kind; _ }) ->
            [%compare.equal: Breadcrumb.first_kind] has_kind kind
        | _ -> false
      in
      if List.exists set ~f:already_has_first then
        set
      else
        SimpleSet.inject (Simple.Breadcrumb (Breadcrumb.HasFirst kind))
        :: SimpleSet.inject (Simple.Breadcrumb (Breadcrumb.First { kind; name }))
        :: set


    let add_first_index index =
      let feature =
        match index with
        | Abstract.TreeDomain.Label.Field name when is_numeric name -> "<numeric>"
        | Field name -> name
        | DictionaryKeys -> "<keys>"
        | Any -> "<unknown>"
      in
      add_first Features.Breadcrumb.FirstIndex feature


    let global_model ~location reference =
      (* Fields are handled like methods *)
      let target_candidates =
        [
          Interprocedural.Callable.create_method reference;
          Interprocedural.Callable.create_object reference;
        ]
      in
      let merge_models result candidate =
        let model =
          Interprocedural.Fixpoint.get_model candidate
          >>= Interprocedural.Result.get_model TaintResult.kind
        in
        match model with
        | None -> result
        | Some { forward = { source_taint }; _ } ->
            ForwardState.read ~root:AccessPath.Root.LocalResult ~path:[] source_taint
            |> ForwardState.Tree.apply_call
                 location
                 ~callees:[candidate]
                 ~port:AccessPath.Root.LocalResult
      in
      List.fold target_candidates ~f:merge_models ~init:ForwardState.Tree.empty


    let rec analyze_argument
        ~resolution
        (taint_accumulator, state)
        { Call.Argument.value = argument; _ }
      =
      analyze_expression ~resolution ~state ~expression:argument
      |>> ForwardState.Tree.join taint_accumulator


    and apply_call_targets
        ~resolution
        ~callee
        ?(collapse_tito = true)
        call_location
        arguments
        state
        call_targets
      =
      (* We keep a table of kind -> set of triggered labels across all targets, and merge triggered
         sinks at the end. *)
      let triggered_sinks = String.Hash_set.create () in
      let apply_call_target state argument_taint (call_target, _implicit) =
        let taint_model = Model.get_callsite_model ~call_target ~arguments in
        let { TaintResult.forward; backward; _ } = taint_model.model in
        let sink_argument_matches =
          BackwardState.roots backward.sink_taint |> AccessPath.match_actuals_to_formals arguments
        in
        let tito_argument_matches =
          BackwardState.roots backward.taint_in_taint_out
          |> AccessPath.match_actuals_to_formals arguments
        in
        let combined_matches =
          List.zip_exn sink_argument_matches tito_argument_matches |> List.zip_exn argument_taint
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
            |> BackwardState.Tree.partition Domains.BackwardTaint.leaf ~f:Option.some
          in
          Map.Poly.merge tito_map new_tito_map ~f:(merge_tito_effect BackwardState.Tree.join)
        in
        let compute_argument_tito_effect
            (tito_effects, state)
            (argument_taint, ((argument, sink_matches), (_dup, tito_matches)))
          =
          let location =
            Location.with_module ~qualifier:FunctionContext.qualifier argument.Node.location
          in
          let tito =
            let convert_tito_path
                kind
                { BackwardState.Tree.path; tip = return_taint; _ }
                accumulated_tito
              =
              let breadcrumbs =
                BackwardTaint.fold
                  BackwardTaint.simple_feature_element
                  return_taint
                  ~f:Features.gather_breadcrumbs
                  ~init:
                    Features.
                      [
                        SimpleSet.inject (Simple.Breadcrumb Breadcrumb.Tito);
                        SimpleSet.inject (Simple.TitoPosition location);
                      ]
              in
              let add_features features = List.rev_append breadcrumbs features in
              let taint_to_propagate =
                if collapse_tito then
                  ForwardState.Tree.read path argument_taint
                  |> ForwardState.Tree.collapse
                  |> ForwardTaint.transform
                       ForwardTaint.simple_feature_set
                       Abstract.Domain.(Map add_features)
                  |> ForwardState.Tree.create_leaf
                else
                  ForwardState.Tree.read path argument_taint
                  |> ForwardState.Tree.transform
                       ForwardTaint.simple_feature_set
                       Abstract.Domain.(Map add_features)
              in
              let return_paths =
                match kind with
                | Sinks.LocalReturn ->
                    let gather_paths (Features.Complex.ReturnAccessPath extra_path) paths =
                      extra_path :: paths
                    in
                    BackwardTaint.fold
                      BackwardTaint.complex_feature
                      return_taint
                      ~f:gather_paths
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
                BackwardState.Tree.RawPath
                tito_tree
                ~init:ForwardState.Tree.empty
                ~f:(convert_tito_path kind)
            in
            List.fold tito_matches ~f:combine_tito ~init:Map.Poly.empty
            |> Map.Poly.mapi ~f:convert_tito
            |> Map.Poly.merge tito_effects ~f:(merge_tito_effect ForwardState.Tree.join)
          in
          let tito =
            if taint_model.is_obscure then
              let obscure_tito =
                ForwardState.Tree.collapse argument_taint
                |> ForwardTaint.transform
                     ForwardTaint.simple_feature
                     Abstract.Domain.(Add Features.obscure)
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
            | Some { AccessPath.root; path } -> (
                let features_to_add =
                  BackwardState.Tree.filter_by_leaf ~leaf:Sinks.AddFeatureToArgument sink_tree
                  |> BackwardTaint.fold BackwardTaint.simple_feature_set ~f:List.rev_append ~init:[]
                  |> List.filter ~f:Features.is_breadcrumb
                in
                match features_to_add with
                | _ :: _ as features ->
                    let taint =
                      ForwardState.read state.taint ~root ~path
                      |> ForwardState.Tree.transform
                           ForwardTaint.simple_feature_set
                           Abstract.Domain.(Map (List.rev_append features))
                    in
                    store_taint ~root ~path taint state
                | [] -> state )
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
        ( if not (Hash_set.is_empty triggered_sinks) then
            let add_sink (key, taint) roots_and_sinks =
              let add roots_and_sinks = function
                | Sinks.PartialSink sink ->
                    if Hash_set.mem triggered_sinks (Sinks.show_partial_sink sink) then
                      (key, Sinks.TriggeredPartialSink sink) :: roots_and_sinks
                    else
                      roots_and_sinks
                | _ -> roots_and_sinks
              in
              BackwardTaint.leaves (BackwardState.Tree.collapse taint)
              |> List.fold ~f:add ~init:roots_and_sinks
            in
            let triggered_sinks =
              BackwardState.fold BackwardState.KeyValue backward.sink_taint ~init:[] ~f:add_sink
            in
            let { Location.WithModule.start; stop; _ } = call_location in
            FunctionContext.add_triggered_sinks ~location:{ Location.start; stop } ~triggered_sinks
        );
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
            begin
              match Model.get_global_sink_model ~resolution ~location ~expression:argument with
              | None -> ()
              | Some sink_tree -> FunctionContext.check_flow ~location ~source_tree ~sink_tree
            end;
            let access_path = AccessPath.of_expression ~resolution argument in
            store_taint_option ~weak:true access_path source_tree state
          in
          let for_each_target ~key:target ~data:taint state =
            match target with
            | Sinks.LocalReturn -> state (* This is regular tito which was computed above *)
            | ParameterUpdate n -> (
                (* Side effect on argument n *)
                match List.nth arguments n with
                | None -> state
                | Some argument -> apply_argument_effect ~argument ~source_tree:taint state )
            | Attach -> state (* These synthetic nodes should be ignored for analysis.*)
            | _ -> failwith "unexpected sink in tito"
          in
          Map.Poly.fold tito_effects ~f:for_each_target ~init:state
        in
        let returned_taint =
          let joined = ForwardState.Tree.join result_taint tito in
          if taint_model.is_obscure then
            let annotation =
              Resolution.resolve_expression_to_type
                resolution
                (Node.create_with_default_location (Expression.Call { Call.callee; arguments }))
            in
            ForwardState.Tree.transform
              ForwardTaint.simple_feature_set
              Abstract.Domain.(
                Map
                  (Features.add_type_breadcrumb
                     ~resolution:(Resolution.global_resolution resolution)
                     (Some annotation)))
              joined
          else
            joined
        in
        returned_taint, apply_tito_side_effects tito_effects state
      in
      match call_targets with
      | [] ->
          (* If we don't have a call target: propagate argument taint. *)
          List.fold
            arguments
            ~init:(ForwardState.Tree.empty, state)
            ~f:(analyze_argument ~resolution)
          |>> ForwardState.Tree.transform
                ForwardTaint.simple_feature
                Abstract.Domain.(Add Features.obscure)
      | call_targets ->
          let argument_taint, state =
            let compute_argument_taint (argument_taint, state) argument =
              let taint, state =
                analyze_unstarred_expression ~resolution argument.Call.Argument.value state
              in
              taint :: argument_taint, state
            in
            List.rev arguments |> List.fold ~init:([], state) ~f:compute_argument_taint
          in
          List.map call_targets ~f:(apply_call_target state argument_taint)
          |> List.fold
               ~init:(ForwardState.Tree.empty, { taint = ForwardState.empty })
               ~f:(fun (taint, state) (new_taint, new_state) ->
                 ForwardState.Tree.join taint new_taint, join state new_state)


    and analyze_dictionary_entry ~resolution (taint, state) { Dictionary.Entry.key; value } =
      let field_name =
        match key.Node.value with
        | String literal -> Abstract.TreeDomain.Label.Field literal.value
        | _ -> Abstract.TreeDomain.Label.Any
      in
      let key_taint, state =
        analyze_expression ~resolution ~state ~expression:key
        |>> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.DictionaryKeys]
      in
      analyze_expression ~resolution ~state ~expression:value
      |>> ForwardState.Tree.prepend [field_name]
      |>> ForwardState.Tree.join taint
      |>> ForwardState.Tree.join key_taint


    and analyze_list_element ~resolution position (taint, state) expression =
      let index_name = Abstract.TreeDomain.Label.Field (string_of_int position) in
      analyze_expression ~resolution ~state ~expression
      |>> ForwardState.Tree.prepend [index_name]
      |>> ForwardState.Tree.join taint


    and analyze_set_element ~resolution (taint, state) expression =
      let value_taint, state =
        analyze_expression ~resolution ~state ~expression
        |>> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.Any]
      in
      ForwardState.Tree.join taint value_taint, state


    and analyze_comprehension_generators ~resolution ~state generators =
      let add_binding
          (state, resolution)
          ({ Comprehension.Generator.target; iterator; conditions; _ } as generator)
        =
        let taint, state =
          let iterator_is_dictionary =
            match Resolution.resolve_expression_to_type resolution iterator with
            | Type.Parametric { name; _ } ->
                GlobalResolution.is_transitive_successor
                  (Resolution.global_resolution resolution)
                  ~predecessor:name
                  ~successor:Type.mapping_primitive
            | _ -> false
          in
          let label =
            if iterator_is_dictionary then
              Abstract.TreeDomain.Label.DictionaryKeys
            else
              Abstract.TreeDomain.Label.Any
          in
          analyze_expression ~resolution ~state ~expression:iterator
          |>> ForwardState.Tree.read [label]
        in
        (* Since generators create variables that Pyre sees as scoped within the generator, handle
           them by adding the generator's bindings to the resolution. *)
        let resolution =
          Resolution.resolve_assignment
            resolution
            (Ast.Statement.Statement.generator_assignment generator)
        in
        let access_path = AccessPath.of_expression ~resolution target in
        let state = store_taint_option access_path taint state in
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
      |>> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.Any]


    and analyze_dictionary_comprehension
        ~resolution
        ~state
        { Comprehension.element = { Dictionary.Entry.key; value }; generators; _ }
      =
      let state, resolution = analyze_comprehension_generators ~resolution ~state generators in
      let value_taint, state =
        analyze_expression ~resolution ~state ~expression:value
        |>> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.Any]
      in
      let key_taint, state =
        analyze_expression ~resolution ~state ~expression:key
        |>> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.DictionaryKeys]
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
        ~constructor_targets
        ~arguments
        ~location
        ~state
      =
      (* Since we're searching for ClassType.__new/init__(), Pyre will correctly not insert an
         implicit type there. However, since the actual call was ClassType(), insert the implicit
         receiver here ourselves and ignore the value from get_indirect_targets. *)
      let { Interprocedural.CallResolution.new_targets; init_targets } = constructor_targets in
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
      let analyze_regular_call ~state ~callee ~arguments =
        (* reveal_taint(). *)
        begin
          match Node.value callee, arguments with
          | ( Expression.Name (Name.Identifier "reveal_taint"),
              [{ Call.Argument.value = expression; _ }] ) ->
              let taint, _ = analyze_expression ~resolution ~state ~expression in
              let location =
                Node.location callee |> Location.with_module ~qualifier:FunctionContext.qualifier
              in
              Log.dump
                "%a: Revealed forward taint for `%s`: %s"
                Location.WithModule.pp
                location
                (Ast.Transform.sanitize_expression expression |> Expression.show)
                (ForwardState.Tree.show taint)
          | ( Expression.Name (Name.Identifier "reveal_type"),
              [{ Call.Argument.value = expression; _ }] ) ->
              let location =
                Node.location callee |> Location.with_module ~qualifier:FunctionContext.qualifier
              in
              Log.dump
                "%a: Revealed type for %s: %s"
                Location.WithModule.pp
                location
                (Ast.Transform.sanitize_expression expression |> Expression.show)
                (Resolution.resolve_expression_to_type resolution expression |> Type.show)
          | _ -> ()
        end;
        match AccessPath.get_global ~resolution callee, Node.value callee with
        | Some global, _ -> (
            let targets = Interprocedural.CallResolution.get_global_targets ~resolution global in
            match targets with
            | Interprocedural.CallResolution.GlobalTargets targets ->
                apply_call_targets ~resolution ~callee location arguments state targets
            | Interprocedural.CallResolution.ConstructorTargets { constructor_targets; callee } ->
                analyze_constructor_call
                  ~resolution
                  ~location
                  ~arguments
                  ~callee
                  ~state
                  ~constructor_targets )
        | None, Name (Name.Attribute { base = receiver; attribute = method_name; _ }) ->
            let indirect_targets, receiver =
              Interprocedural.CallResolution.get_indirect_targets ~resolution ~receiver ~method_name
            in
            let collapse_tito =
              (* For most cases, it is simply incorrect to not collapse tito, as it will lead to
               * incorrect mapping from input to output taint. However, the collapsing of tito
               * adversely affects our analysis in the case of the builder pattern, i.e.
               *
               * class C:
               *   def set_field(self, field) -> "C":
               *   self.field = field
               *   return self
               *
               * In this case, collapsing tito leads to field tainting the entire `self` for chained
               * call. To prevent this problem, we special case builders to preserve the tito
               * structure. *)
              match Resolution.resolve_expression resolution callee with
              | ( _,
                  Type.Parametric
                    { name = "BoundMethod"; parameters = [_; Type.Parameter.Single implicit] } ) ->
                  let return_annotation =
                    (* To properly substitute type variables, we simulate `callee.__call__` for the
                       bound method. *)
                    let to_simulate =
                      Node.create_with_default_location
                        (Expression.Name
                           (Name.Attribute { base = callee; attribute = "__call__"; special = true }))
                    in
                    Resolution.resolve_expression resolution to_simulate
                    |> snd
                    |> function
                    | Type.Callable { Type.Callable.implementation; _ } ->
                        Type.Callable.Overload.return_annotation implementation
                    | _ -> Type.Top
                  in
                  not (Type.equal implicit return_annotation)
              | _ -> true
            in
            let arguments = Option.to_list receiver @ arguments in
            let add_index_breadcrumb_if_necessary taint =
              if not (String.equal method_name "get") then
                taint
              else
                match arguments with
                | _receiver :: index :: _ ->
                    let label = get_index index.value in
                    ForwardState.Tree.transform
                      ForwardTaint.simple_feature_set
                      Abstract.Domain.(Map (add_first_index label))
                      taint
                | _ -> taint
            in
            apply_call_targets
              ~resolution
              ~callee
              ~collapse_tito
              location
              arguments
              state
              indirect_targets
            |>> add_index_breadcrumb_if_necessary
        | None, Name (Name.Identifier _name) -> (
            match Interprocedural.CallResolution.resolve_target ~resolution callee with
            | (_, implicit) :: _ as targets ->
                (* If a method was assigned to a local, we no longer have taint on self. If that's
                   the case, we need to add a dummy receiver with no taint (since we're not storing
                   this information in the earlier assignment. *)
                let arguments =
                  match implicit with
                  | None -> arguments
                  | Some _ ->
                      {
                        Call.Argument.name = None;
                        value =
                          Node.create_with_default_location
                            (Expression.Name (Name.Identifier "None"));
                      }
                      :: arguments
                in
                apply_call_targets ~resolution ~callee location arguments state targets
            | [] ->
                let constructor_targets =
                  Interprocedural.CallResolution.get_constructor_targets
                    ~resolution
                    ~receiver:callee
                in
                analyze_constructor_call
                  ~resolution
                  ~location
                  ~arguments
                  ~state
                  ~constructor_targets
                  ~callee )
        | _ ->
            (* No target, treat call as obscure *)
            let callee_taint, state = analyze_expression ~resolution ~state ~expression:callee in
            List.fold_left arguments ~f:(analyze_argument ~resolution) ~init:(callee_taint, state)
            |>> ForwardState.Tree.transform
                  ForwardTaint.simple_feature
                  Abstract.Domain.(Add Features.obscure)
      in
      let analyze_lambda_call ~callee ~lambda_argument ~non_lambda_arguments =
        (* If we have a lambda `fn` getting passed into `hof`, we use the following strategy:
         * hof(q, fn, x, y) gets translated into the following block: (analyzed backwards)
         * $all = {q, x, y}
         * $result = fn( *all, **all)
         * hof(q, $result, x, y)
         *)
        let lambda_index, { Call.Argument.value = lambda_callee; name = lambda_name } =
          lambda_argument
        in
        let location = lambda_callee.Node.location in
        let all_argument = Node.create ~location (Expression.Name (Name.Identifier "$all")) in
        (* Simulate `$all = {q, x, y}`. *)
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
        let result = Node.create ~location (Expression.Name (Name.Identifier "$result")) in
        let state =
          let arguments =
            List.map non_lambda_arguments ~f:(fun (_, argument) ->
                { argument with Call.Argument.value = all_argument })
          in
          let { Call.callee; arguments } =
            Interprocedural.CallResolution.redirect_special_calls
              ~resolution
              { Call.callee = lambda_callee; arguments }
          in
          let taint, state = analyze_regular_call ~state ~callee ~arguments in
          let taint =
            ForwardState.Tree.transform
              ForwardTaint.simple_feature
              Abstract.Domain.(Add Features.lambda)
              taint
          in
          analyze_assignment ~resolution result taint taint state
        in
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
        analyze_regular_call ~state ~callee ~arguments:higher_order_function_arguments
      in
      match { Call.callee; arguments } with
      | {
       callee = { Node.value = Name (Name.Attribute { base; attribute = "__getitem__"; _ }); _ };
       arguments = [{ Call.Argument.value = argument_value; _ }];
      } ->
          let index = AccessPath.get_index argument_value in
          analyze_expression ~resolution ~state ~expression:base
          |>> ForwardState.Tree.read [index]
          |>> ForwardState.Tree.transform
                ForwardTaint.simple_feature_set
                Abstract.Domain.(Map (add_first_index index))
      (* Special case x.__next__() as being a random index access (this pattern is the desugaring of
         `for element in x`). *)
      | {
       callee = { Node.value = Name (Name.Attribute { base; attribute = "__next__"; _ }); _ };
       arguments = [];
      } ->
          analyze_expression ~resolution ~state ~expression:base
          |>> ForwardState.Tree.read [Abstract.TreeDomain.Label.Any]
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
          analyze_regular_call ~state ~callee ~arguments
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
      (* dictionary .keys() and .values() functions are special, as they require handling of
         DictionaryKeys taint. *)
      | { callee = { Node.value = Name (Name.Attribute { base; attribute = "values"; _ }); _ }; _ }
        when Resolution.resolve_expression_to_type resolution base |> Type.is_dictionary_or_mapping
        ->
          analyze_expression ~resolution ~state ~expression:base
          |>> ForwardState.Tree.read [Abstract.TreeDomain.Label.Any]
          |>> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.Any]
      | { callee = { Node.value = Name (Name.Attribute { base; attribute = "keys"; _ }); _ }; _ }
        when Resolution.resolve_expression_to_type resolution base |> Type.is_dictionary_or_mapping
        ->
          analyze_expression ~resolution ~state ~expression:base
          |>> ForwardState.Tree.read [Abstract.TreeDomain.Label.DictionaryKeys]
          |>> ForwardState.Tree.prepend [Abstract.TreeDomain.Label.Any]
      (* `locals()` is a dictionary from all local names -> values. *)
      | { callee = { Node.value = Name (Name.Identifier "locals"); _ }; arguments = [] } ->
          let add_root_taint locals_taint root =
            let path_of_root =
              match root with
              | AccessPath.Root.Variable variable ->
                  [Abstract.TreeDomain.Label.Field (Identifier.sanitized variable)]
              | NamedParameter { name }
              | PositionalParameter { name; _ } ->
                  [Abstract.TreeDomain.Label.Field (Identifier.sanitized name)]
              | _ -> [Abstract.TreeDomain.Label.Any]
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
      | _ -> (
          let call = { Call.callee; arguments } in
          let { Call.callee; arguments } =
            Interprocedural.CallResolution.redirect_special_calls ~resolution call
          in
          let lambda_arguments, reversed_non_lambda_arguments =
            let is_callable argument =
              match Resolution.resolve_expression resolution argument.Call.Argument.value with
              | _, Type.Callable _
              | _, Type.Parametric { name = "BoundMethod"; _ } ->
                  true
              | _ -> false
            in
            let classify_argument index (lambdas, non_lambdas) argument =
              if is_callable argument then
                (index, argument) :: lambdas, non_lambdas
              else
                lambdas, (index, argument) :: non_lambdas
            in

            List.foldi arguments ~f:classify_argument ~init:([], [])
          in

          match lambda_arguments with
          | [lambda_argument] ->
              analyze_lambda_call
                ~callee
                ~lambda_argument
                ~non_lambda_arguments:(List.rev reversed_non_lambda_arguments)
          | _ -> analyze_regular_call ~state ~callee ~arguments )


    and analyze_attribute_access ~resolution ~state ~location base attribute =
      let annotation = Interprocedural.CallResolution.resolve_ignoring_optional ~resolution base in
      let attribute_taint =
        let annotations =
          let successors =
            GlobalResolution.class_metadata (Resolution.global_resolution resolution) annotation
            >>| (fun { ClassMetadataEnvironment.successors; _ } -> successors)
            |> Option.value ~default:[]
            |> List.map ~f:(fun name -> Type.Primitive name)
          in
          let base_annotation =
            (* Our model definitions are ambiguous. Models could either refer to a class variable or
               an instance variable. We explore both. *)
            if Type.is_meta annotation then
              [Type.single_parameter annotation]
            else
              []
          in
          (annotation :: successors) @ base_annotation
        in
        let attribute_taint sofar annotation =
          Reference.create ~prefix:(Type.class_name annotation) attribute
          |> global_model ~location
          |> ForwardState.Tree.join sofar
        in
        List.fold annotations ~init:ForwardState.Tree.empty ~f:attribute_taint
      in
      let add_tito_features taint =
        let attribute_breadcrumbs =
          Model.get_global_tito_model
            ~resolution
            ~expression:
              (Node.create_with_default_location
                 (Expression.Name
                    (Name.Attribute { Name.Attribute.base; attribute; special = false })))
          >>| BackwardState.Tree.get_all_breadcrumbs
        in
        match attribute_breadcrumbs with
        | Some (_ :: _ as breadcrumbs) ->
            ForwardState.Tree.transform
              ForwardTaint.simple_feature_set
              Abstract.Domain.(Map (List.rev_append breadcrumbs))
              taint
        | _ -> taint
      in

      let field = Abstract.TreeDomain.Label.Field attribute in
      analyze_expression ~resolution ~state ~expression:base
      |>> add_tito_features
      |>> ForwardState.Tree.read [field]
      |>> ForwardState.Tree.transform
            ForwardTaint.simple_feature_set
            Abstract.Domain.(Map (add_first Features.Breadcrumb.FirstField attribute))
      |>> ForwardState.Tree.join attribute_taint


    and analyze_expression ~resolution ~state ~expression:({ Node.location; _ } as expression) =
      let location = Location.with_module ~qualifier:FunctionContext.qualifier location in
      match expression.Node.value with
      | Await expression -> analyze_expression ~resolution ~state ~expression
      | BooleanOperator { left; operator = _; right } ->
          let left_taint, state = analyze_expression ~resolution ~state ~expression:left in
          let right_taint, state = analyze_expression ~resolution ~state ~expression:right in
          ForwardState.Tree.join left_taint right_taint, state
      | ComparisonOperator ({ left; operator = _; right } as comparison) -> (
          match ComparisonOperator.override comparison with
          | Some override -> analyze_expression ~resolution ~state ~expression:override
          | None ->
              let left_taint, state = analyze_expression ~resolution ~state ~expression:left in
              let right_taint, state = analyze_expression ~resolution ~state ~expression:right in
              ForwardState.Tree.join left_taint right_taint, state )
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
          let global = Option.value_exn (AccessPath.get_global ~resolution expression) in
          global_model ~location global, state
      | Name (Name.Identifier identifier) ->
          ForwardState.read ~root:(AccessPath.Root.Variable identifier) ~path:[] state.taint, state
      | Name (Name.Attribute { base; attribute; _ }) -> (
          match
            Interprocedural.CallResolution.resolve_property_targets
              ~resolution
              ~base
              ~attribute
              ~setter:false
          with
          | None -> analyze_attribute_access ~resolution ~state ~location base attribute
          | Some targets ->
              let arguments = [{ Call.Argument.name = None; value = base }] in
              apply_call_targets ~resolution ~callee:expression location arguments state targets )
      | Set set ->
          List.fold ~f:(analyze_set_element ~resolution) set ~init:(ForwardState.Tree.empty, state)
      | SetComprehension comprehension -> analyze_comprehension ~resolution comprehension state
      | Starred (Starred.Once expression)
      | Starred (Starred.Twice expression) ->
          analyze_expression ~resolution ~state ~expression
          |>> ForwardState.Tree.read [Abstract.TreeDomain.Label.Any]
      | String { StringLiteral.kind = StringLiteral.Format expressions; _ } ->
          List.fold
            expressions
            ~f:(fun (taint, state) expression ->
              analyze_expression ~resolution ~state ~expression |>> ForwardState.Tree.join taint)
            ~init:(ForwardState.Tree.empty, state)
          |>> ForwardState.Tree.transform
                ForwardTaint.simple_feature
                Abstract.Domain.(Add Domains.format_string_feature)
      | String _ -> ForwardState.Tree.empty, state
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
      | Yield (Some expression) -> analyze_expression ~resolution ~state ~expression
      | Yield None -> ForwardState.Tree.empty, state


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
            let index = Abstract.TreeDomain.Label.Field (string_of_int i) in
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
            Model.get_global_sink_model ~resolution ~location ~expression:target
            |> Option.value ~default:BackwardState.Tree.empty
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
      let sink_taint = Configuration.conditional_test_sinks () |> BackwardTaint.of_list ~location in
      let sink_tree = BackwardState.Tree.create_leaf sink_taint in
      FunctionContext.check_flow ~location ~source_tree:taint ~sink_tree;
      state


    let analyze_definition ~define:_ state = state

    let analyze_statement ~resolution { Node.value = statement; location } state =
      match statement with
      | Statement.Assign { value = { Node.value = Expression.Ellipsis; _ }; _ } -> state
      | Statement.Assign
          { value = { Node.value = Expression.Name (Name.Identifier "None"); _ }; target; _ } -> (
          match AccessPath.of_expression ~resolution target with
          | Some { AccessPath.root; path } ->
              (* We need to take some care to ensure we clear existing taint, without adding new
                 taint. *)
              let taint = ForwardState.read ~root ~path state.taint in
              if not (ForwardState.Tree.is_bottom taint) then
                { taint = ForwardState.assign ~root ~path ForwardState.Tree.bottom state.taint }
              else
                state
          | _ -> state )
      | Statement.Assign { target = { Node.location; value = target_value } as target; value; _ }
        -> (
          let target_is_sanitized =
            (* Optimization: We only view names as being sanitizable to avoid unnecessary type
               checking. *)
            match Node.value target with
            | Name (Name.Attribute _) -> Model.global_is_sanitized ~resolution ~expression:target
            | _ -> false
          in
          if target_is_sanitized then
            analyze_expression ~resolution ~state ~expression:value |> snd
          else
            match target_value with
            | Name (Name.Attribute { base; attribute; _ }) -> (
                let property_targets =
                  Interprocedural.CallResolution.resolve_property_targets
                    ~resolution
                    ~base
                    ~attribute
                    ~setter:true
                in
                match property_targets with
                | Some targets ->
                    let taint, state =
                      apply_call_targets
                        ~resolution
                        ~callee:target
                        (Location.with_module ~qualifier:FunctionContext.qualifier location)
                        [{ Call.Argument.value; name = None }]
                        state
                        targets
                    in
                    store_taint_option (AccessPath.of_expression ~resolution base) taint state
                | None ->
                    let taint, state = analyze_expression ~resolution ~state ~expression:value in
                    analyze_assignment ~resolution target taint taint state )
            | _ ->
                let taint, state = analyze_expression ~resolution ~state ~expression:value in
                analyze_assignment ~resolution target taint taint state )
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
            ~sink_tree:FunctionContext.return_sink;
          store_taint ~root:AccessPath.Root.LocalResult ~path:[] taint state
      | Return { expression = None; _ }
      | Try _
      | With _
      | While _ ->
          state
      | Yield expression
      | YieldFrom expression ->
          let taint, state = analyze_expression ~resolution ~state ~expression in
          store_taint ~root:AccessPath.Root.LocalResult ~path:[] taint state


    let create ~existing_model parameters =
      (* Use primed sources to populate initial state of parameters *)
      let forward_primed_taint = existing_model.TaintResult.forward.source_taint in
      let prime_parameter
          state
          (parameter_root, name, { Ast.Node.location; value = { Parameter.value; _ } })
        =
        let prime =
          let location = Location.with_module ~qualifier:FunctionContext.qualifier location in
          ForwardState.read ~root:parameter_root ~path:[] forward_primed_taint
          |> ForwardState.Tree.apply_call
               location
               ~callees:[Interprocedural.Callable.create FunctionContext.definition]
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
      log "State: %a\nAnalyzing statement: %a" pp state Statement.pp statement;
      let resolution =
        let { Node.value = { Define.signature = { Define.Signature.parent; _ }; _ }; _ } =
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

let extract_features_to_attach existing_taint =
  ForwardState.read ~root:AccessPath.Root.LocalResult ~path:[] existing_taint
  |> ForwardState.Tree.collapse
  |> ForwardTaint.partition ForwardTaint.leaf ~f:(fun source ->
         if Sources.equal Sources.Attach source then Some true else None)
  |> (fun map -> Map.Poly.find map true)
  |> function
  | Some taint ->
      let gather_features feature features =
        let open Features in
        match feature.Abstract.OverUnderSetDomain.element with
        | Simple.Breadcrumb _ -> feature :: features
        (* The ViaValueOf models will be converted to breadcrumbs at the call site via
           `get_callsite_model`. *)
        | Simple.ViaValueOf _ -> feature :: features
        | _ -> features
      in
      ForwardTaint.fold ForwardTaint.simple_feature_element ~f:gather_features ~init:[] taint
      |> Features.SimpleSet.of_approximation
  | None -> Features.SimpleSet.bottom


let extract_source_model ~define ~resolution ~features_to_attach exit_taint =
  let {
    Define.signature = { return_annotation; name = { Node.value = name; _ }; parameters; _ };
    _;
  }
    =
    define
  in
  let return_annotation =
    Option.map ~f:(GlobalResolution.parse_annotation resolution) return_annotation
  in
  let simplify tree =
    let essential = ForwardState.Tree.essential tree in
    ForwardState.Tree.shape tree ~mold:essential
    |> ForwardState.Tree.transform
         ForwardTaint.simple_feature_set
         Abstract.Domain.(Map (Features.add_type_breadcrumb ~resolution return_annotation))
    |> ForwardState.Tree.limit_to
         ~width:Configuration.analysis_model_constraints.maximum_model_width
    |> ForwardState.Tree.approximate_complex_access_paths
  in
  let attach_features taint =
    if not (Features.SimpleSet.is_bottom features_to_attach) then
      ForwardState.transform Features.SimpleSet.Self Abstract.Domain.(Add features_to_attach) taint
    else
      taint
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
      if String.equal (Reference.last name) "__init__" || Define.is_property_setter define then
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
  |> attach_features


let run ~environment ~qualifier ~define ~existing_model =
  let {
    Node.value =
      {
        Define.signature = { name = { Node.value = name; _ }; parameters; return_annotation; _ };
        _;
      };
    _;
  }
    =
    define
  in
  let module Context = struct
    let qualifier = qualifier

    let definition = define

    let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment

    let local_annotations =
      TypeEnvironment.ReadOnly.get_local_annotations
        environment
        (Node.value define |> Define.name |> Node.value)


    let debug = Define.dump define.value

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


    let return_sink =
      let return_location =
        match return_annotation with
        | Some node -> node.Node.location
        | None -> define.Node.location
      in
      BackwardState.read
        ~root:AccessPath.Root.LocalResult
        ~path:[]
        existing_model.TaintResult.backward.sink_taint
      |> BackwardState.Tree.apply_call
           (Location.with_module ~qualifier return_location)
           ~callees:[]
           ~port:AccessPath.Root.LocalResult


    let triggered_sinks = Location.Table.create ()

    let add_triggered_sinks ~location ~triggered_sinks:new_triggered_sinks =
      Hashtbl.set triggered_sinks ~key:location ~data:new_triggered_sinks
  end
  in
  let module AnalysisInstance = AnalysisInstance (Context) in
  let open AnalysisInstance in
  log "Starting analysis of %a" Interprocedural.Callable.pp (Interprocedural.Callable.create define);
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
  let resolution = TypeEnvironment.ReadOnly.global_resolution environment in
  let extract_model ({ FixpointState.taint; _ } as result) =
    (* Explicitly declared taint is not propagated to the result and needs to be picked up from the
       existing model. *)
    let features_to_attach = extract_features_to_attach existing_model.forward.source_taint in
    let source_taint =
      extract_source_model ~define:define.value ~resolution ~features_to_attach taint
    in
    let () = log "Model: %a" FixpointState.pp result in
    TaintResult.Forward.{ source_taint }
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
