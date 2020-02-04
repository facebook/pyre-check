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
  type t = { taint: BackwardState.t }

  include Fixpoint.State with type t := t

  val create : unit -> t
end

module type FUNCTION_CONTEXT = sig
  val qualifier : Reference.t

  val definition : Define.t Node.t

  val first_parameter : unit -> Root.t option (* For implicit self reference in super() *)

  val is_constructor : unit -> bool

  val global_resolution : GlobalResolution.t

  val local_annotations : LocalAnnotationMap.t option

  val debug : bool
end

module AnalysisInstance (FunctionContext : FUNCTION_CONTEXT) = struct
  let log format =
    if FunctionContext.debug then
      Log.dump format
    else
      Log.log ~section:`Taint format


  (* This is where we can observe access paths reaching into LocalReturn and record the extraneous
     paths for more precise tito. *)
  let initial_taint =
    if FunctionContext.is_constructor () then (* Constructor. Make self the return value *)
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
    let f feature =
      match feature with
      | Features.Complex.ReturnAccessPath prefix -> Features.Complex.ReturnAccessPath (prefix @ path)
    in
    match path with
    | AbstractTreeDomain.Label.Any :: _ -> taint
    | _ -> BackwardTaint.transform BackwardTaint.complex_feature ~f taint


  let read_tree = BackwardState.Tree.read ~transform_non_leaves

  let is_super expression =
    match expression.Node.value with
    | Expression.Call { callee = { Node.value = Name (Name.Identifier "super"); _ }; _ } -> true
    | _ -> false


  module rec FixpointState : FixpointState = struct
    type t = { taint: BackwardState.t } [@@deriving show]

    let create () = { taint = BackwardState.empty }

    let less_or_equal ~left:{ taint = left; _ } ~right:{ taint = right; _ } =
      BackwardState.less_or_equal ~left ~right


    let join { taint = left } { taint = right; _ } =
      let taint = BackwardState.join left right in
      { taint }


    let widen ~previous:{ taint = previous; _ } ~next:{ taint = next; _ } ~iteration =
      let taint = BackwardState.widen ~iteration ~previous ~next in
      { taint }


    let get_taint access_path { taint; _ } =
      match access_path with
      | None -> BackwardState.Tree.empty
      | Some { root; path } -> BackwardState.read ~transform_non_leaves ~root ~path taint


    let store_weak_taint ~root ~path taint { taint = state_taint } =
      { taint = BackwardState.assign ~weak:true ~root ~path taint state_taint }


    let analyze_definition ~define:_ state = state

    let rec analyze_argument ~resolution taint { Call.Argument.value = argument; _ } state =
      analyze_expression ~resolution ~taint ~state ~expression:argument


    and apply_call_targets
        ~resolution
        ~call_expression
        location
        arguments
        state
        call_taint
        call_targets
      =
      let analyze_call_target (call_target, _implicit) =
        let taint_model = Model.get_callsite_model ~call_target ~arguments in
        let { TaintResult.backward; _ } = taint_model.model in
        let sink_argument_matches =
          BackwardState.roots backward.sink_taint |> AccessPath.match_actuals_to_formals arguments
        in
        let tito_argument_matches =
          BackwardState.roots backward.taint_in_taint_out
          |> AccessPath.match_actuals_to_formals arguments
        in
        let combined_matches = List.zip_exn sink_argument_matches tito_argument_matches in
        let combine_sink_taint location taint_tree { root; actual_path; formal_path } =
          BackwardState.read ~transform_non_leaves ~root ~path:[] backward.sink_taint
          |> BackwardState.Tree.apply_call location ~callees:[call_target] ~port:root
          |> read_tree formal_path
          |> BackwardState.Tree.prepend actual_path
          |> BackwardState.Tree.join taint_tree
        in
        let get_argument_taint ~resolution ~argument:{ Call.Argument.value = argument; _ } state =
          match
            Model.get_global_sink_model
              ~resolution
              ~location:
                (Location.with_module ~qualifier:FunctionContext.qualifier (Node.location argument))
              ~expression:argument
          with
          | Some global_taint -> global_taint
          | None ->
              let access_path = of_expression ~resolution argument in
              get_taint access_path state
        in
        let combine_tito location taint_tree { AccessPath.root; actual_path; formal_path } =
          let add_tito_location features =
            Features.SimpleSet.element (Features.Simple.Breadcrumb Features.Breadcrumb.Tito)
            :: Features.SimpleSet.element (Features.Simple.TitoPosition location)
            :: features
          in
          let translate_tito
              argument_taint
              { BackwardState.Tree.path = tito_path; tip = element; _ }
            =
            let compute_parameter_tito ~key:kind ~data:element argument_taint =
              let extra_paths =
                match kind with
                | Sinks.LocalReturn ->
                    let gather_paths paths (Features.Complex.ReturnAccessPath extra_path) =
                      extra_path :: paths
                    in
                    BackwardTaint.fold
                      BackwardTaint.complex_feature
                      element
                      ~f:gather_paths
                      ~init:[]
                | _ ->
                    (* No special path handling for side effect taint *)
                    [[]]
              in
              let breadcrumbs =
                BackwardTaint.fold
                  BackwardTaint.simple_feature_element
                  element
                  ~f:Features.gather_breadcrumbs
                  ~init:[]
              in
              let add_features features = List.rev_append breadcrumbs features in
              let taint_to_propagate =
                match kind with
                | Sinks.LocalReturn -> call_taint
                (* Attach nodes shouldn't affect analysis. *)
                | Sinks.Attach -> BackwardState.Tree.empty
                | Sinks.ParameterUpdate n -> (
                    match List.nth arguments n with
                    | None -> BackwardState.Tree.empty
                    | Some argument -> get_argument_taint ~resolution ~argument state )
                | _ -> failwith "unexpected tito sink"
              in
              List.fold
                extra_paths
                ~f:(fun taint extra_path ->
                  read_tree extra_path taint_to_propagate
                  |> BackwardState.Tree.collapse
                  |> BackwardTaint.transform BackwardTaint.simple_feature_set ~f:add_features
                  |> BackwardState.Tree.create_leaf
                  |> BackwardState.Tree.prepend tito_path
                  |> BackwardState.Tree.join taint)
                ~init:argument_taint
            in
            BackwardTaint.partition BackwardTaint.leaf ~f:Fn.id element
            |> Map.Poly.fold ~f:compute_parameter_tito ~init:argument_taint
          in
          BackwardState.read
            ~transform_non_leaves
            ~root
            ~path:formal_path
            backward.taint_in_taint_out
          |> BackwardState.Tree.fold
               BackwardState.Tree.RawPath
               ~f:translate_tito
               ~init:BackwardState.Tree.bottom
          |> BackwardState.Tree.transform BackwardTaint.simple_feature_set ~f:add_tito_location
          |> BackwardState.Tree.prepend actual_path
          |> BackwardState.Tree.join taint_tree
        in
        let analyze_argument ~obscure_taint state ((argument, sink_matches), (_dup, tito_matches)) =
          let location =
            Location.with_module ~qualifier:FunctionContext.qualifier argument.Node.location
          in
          let sink_taint =
            List.fold sink_matches ~f:(combine_sink_taint location) ~init:BackwardState.Tree.empty
          in
          let taint_in_taint_out =
            List.fold tito_matches ~f:(combine_tito location) ~init:BackwardState.Tree.empty
          in
          let argument_taint =
            BackwardState.Tree.join sink_taint taint_in_taint_out
            |> BackwardState.Tree.join obscure_taint
          in

          let state =
            match AccessPath.of_expression ~resolution argument with
            | Some { AccessPath.root; path } -> (
                let features_to_add =
                  BackwardState.Tree.filter_by_leaf ~leaf:Sinks.AddFeatureToArgument sink_taint
                  |> BackwardTaint.fold BackwardTaint.simple_feature_set ~f:List.rev_append ~init:[]
                  |> List.filter ~f:Features.is_breadcrumb
                in
                match features_to_add with
                | _ :: _ as features ->
                    let taint =
                      BackwardState.read state.taint ~root ~path
                      |> BackwardState.Tree.transform
                           BackwardTaint.simple_feature_set
                           ~f:(List.rev_append features)
                    in
                    { taint = BackwardState.assign ~root ~path taint state.taint }
                | [] -> state )
            | None -> state
          in
          analyze_unstarred_expression ~resolution argument_taint argument state
        in
        let obscure_taint =
          if taint_model.is_obscure then
            let annotation =
              Resolution.resolve resolution { Node.value = call_expression; location }
            in
            BackwardState.Tree.collapse call_taint
            |> BackwardTaint.transform BackwardTaint.simple_feature_set ~f:Features.add_obscure
            |> BackwardTaint.transform
                 BackwardTaint.simple_feature_set
                 ~f:
                   (Features.add_type_breadcrumb
                      ~resolution:(Resolution.global_resolution resolution)
                      (Some annotation))
            |> BackwardState.Tree.create_leaf
          else
            BackwardState.Tree.bottom
        in
        List.fold ~f:(analyze_argument ~obscure_taint) combined_matches ~init:state
      in
      match call_targets with
      | [] ->
          (* If we don't have a call target: propagate argument taint. *)
          let obscure_taint =
            BackwardState.Tree.collapse call_taint
            |> BackwardTaint.transform BackwardTaint.simple_feature_set ~f:Features.add_obscure
            |> BackwardState.Tree.create_leaf
          in
          List.fold_right ~f:(analyze_argument ~resolution obscure_taint) arguments ~init:state
      | call_targets ->
          List.map call_targets ~f:analyze_call_target
          |> List.fold ~init:(FixpointState.create ()) ~f:FixpointState.join


    and analyze_constructor_call
        ~resolution
        ~call_expression
        ~location
        ~callee
        ~arguments
        ~state
        ~taint
        ~constructor_targets
      =
      (* Since we're searching for ClassType.__new/init__(), Pyre will correctly not insert an
         implicit type there. However, since the actual call was ClassType(), insert the implicit
         receiver here ourselves and ignore the value from get_indirect_targets. *)
      let { Interprocedural.CallResolution.new_targets; init_targets } = constructor_targets in
      let apply_call_targets state targets =
        apply_call_targets
          ~resolution
          ~call_expression:(Expression.Call call_expression)
          location
          ({ Call.Argument.name = None; value = callee } :: arguments)
          state
          taint
          targets
      in

      let state =
        match new_targets with
        | [] -> state
        | targets -> apply_call_targets state targets
      in
      apply_call_targets state init_targets


    and analyze_dictionary_entry ~resolution taint state { Dictionary.Entry.key; value } =
      let state = analyze_expression ~resolution ~taint ~state ~expression:key in
      let field_name = AccessPath.get_index key in
      let value_taint = read_tree [field_name] taint in
      analyze_expression ~resolution ~taint:value_taint ~state ~expression:value


    and analyze_reverse_list_element ~total ~resolution taint reverse_position state expression =
      let position = total - reverse_position - 1 in
      let index_name = AbstractTreeDomain.Label.Field (string_of_int position) in
      let value_taint = read_tree [index_name] taint in
      analyze_expression ~resolution ~taint:value_taint ~state ~expression


    and generator_resolution ~resolution generators =
      let resolve_generator resolution generator =
        Resolution.resolve_assignment
          resolution
          (Ast.Statement.Statement.generator_assignment generator)
      in
      List.fold generators ~init:resolution ~f:resolve_generator


    and analyze_generators ~resolution ~state generators =
      let handle_generator state { Comprehension.Generator.target; iterator; _ } =
        let access_path = of_expression ~resolution target in
        let bound_variable_taint = get_taint access_path state in
        let iterator_taint =
          (* If the type is a dict, we special case `DictionaryKeys` as the taint being iterated on. *)
          let global_resolution = Resolution.global_resolution resolution in
          let label =
            let iterator_is_dictionary =
              match Resolution.resolve resolution iterator with
              | Type.Parametric { name; _ } ->
                  GlobalResolution.is_transitive_successor
                    global_resolution
                    ~predecessor:name
                    ~successor:Type.mapping_primitive
              | _ -> false
            in
            if iterator_is_dictionary then
              AbstractTreeDomain.Label.DictionaryKeys
            else
              AbstractTreeDomain.Label.Any
          in
          BackwardState.Tree.prepend [label] bound_variable_taint
        in
        analyze_expression ~resolution ~taint:iterator_taint ~state ~expression:iterator
      in
      List.fold ~f:handle_generator generators ~init:state


    and analyze_comprehension ~resolution taint { Comprehension.element; generators; _ } state =
      let resolution = generator_resolution ~resolution generators in
      let element_taint = read_tree [AbstractTreeDomain.Label.Any] taint in
      let state = analyze_expression ~resolution ~taint:element_taint ~state ~expression:element in
      analyze_generators ~resolution ~state generators


    (* Skip through * and **. Used at call sites where * and ** are handled explicitly *)
    and analyze_unstarred_expression ~resolution taint expression state =
      match expression.Node.value with
      | Starred (Starred.Once expression)
      | Starred (Starred.Twice expression) ->
          analyze_expression ~resolution ~taint ~state ~expression
      | _ -> analyze_expression ~resolution ~taint ~state ~expression


    and analyze_call ~resolution location ~taint ~state callee arguments =
      let ({ Call.callee; arguments } as call_expression) =
        Interprocedural.CallResolution.redirect_special_calls ~resolution { Call.callee; arguments }
      in
      match AccessPath.get_global ~resolution callee, Node.value callee with
      | _, Name (Name.Identifier "super") -> (
          match arguments with
          | [_; Call.Argument.{ value = object_; _ }] ->
              analyze_expression ~resolution ~taint ~state ~expression:object_
          | _ -> (
              (* Use implicit self *)
              match FunctionContext.first_parameter () with
              | Some root -> store_weak_taint ~root ~path:[] taint state
              | None -> state ) )
      | Some global, _ -> (
          match Interprocedural.CallResolution.get_global_targets ~resolution global with
          | Interprocedural.CallResolution.GlobalTargets targets ->
              apply_call_targets
                ~resolution
                ~call_expression:(Expression.Call call_expression)
                location
                arguments
                state
                taint
                targets
          | Interprocedural.CallResolution.ConstructorTargets { constructor_targets; callee } ->
              analyze_constructor_call
                ~resolution
                ~call_expression
                ~location
                ~arguments
                ~callee
                ~state
                ~taint
                ~constructor_targets )
      | None, Name (Name.Attribute { base = receiver; attribute; _ }) ->
          let taint =
            (* Specially handle super.__init__ calls in constructors for tito *)
            if FunctionContext.is_constructor () && attribute = "__init__" && is_super receiver then
              BackwardState.Tree.create_leaf Domains.local_return_taint
              |> BackwardState.Tree.join taint
            else
              taint
          in
          let indirect_targets, receiver =
            Interprocedural.CallResolution.get_indirect_targets
              ~resolution
              ~receiver
              ~method_name:attribute
          in
          apply_call_targets
            ~resolution
            ~call_expression:(Expression.Call call_expression)
            location
            (Option.to_list receiver @ arguments)
            state
            taint
            indirect_targets
      | None, Name (Name.Identifier _name) ->
          let constructor_targets =
            Interprocedural.CallResolution.get_constructor_targets ~resolution ~receiver:callee
          in
          analyze_constructor_call
            ~resolution
            ~call_expression
            ~location
            ~callee
            ~arguments
            ~state
            ~taint
            ~constructor_targets
      | _ ->
          (* No targets, treat call as obscure *)
          let obscure_taint =
            BackwardState.Tree.collapse taint
            |> BackwardTaint.transform BackwardTaint.simple_feature_set ~f:Features.add_obscure
            |> BackwardState.Tree.create_leaf
          in
          let state =
            List.fold_right arguments ~f:(analyze_argument ~resolution obscure_taint) ~init:state
          in
          analyze_expression ~resolution ~taint:obscure_taint ~state ~expression:callee


    and analyze_expression
        ~resolution
        ~taint
        ~state
        ~expression:{ Node.location; value = expression }
      =
      log "analyze_expression: %a" Expression.pp_expression expression;
      match expression with
      | Await expression -> analyze_expression ~resolution ~taint ~state ~expression
      | BooleanOperator { left; operator = _; right } ->
          analyze_expression ~resolution ~taint ~state ~expression:right
          |> fun state -> analyze_expression ~resolution ~taint ~state ~expression:left
      | ComparisonOperator ({ left; operator = _; right } as comparison) -> (
          match ComparisonOperator.override comparison with
          | Some override -> analyze_expression ~resolution ~taint ~state ~expression:override
          | None ->
              analyze_expression ~resolution ~taint ~state ~expression:right
              |> fun state -> analyze_expression ~resolution ~taint ~state ~expression:left )
      | Call
          {
            callee =
              { Node.value = Name (Name.Attribute { base; attribute = "__setitem__"; _ }); _ } as
              callee;
            arguments =
              [{ Call.Argument.value = index; _ }; { Call.Argument.value; _ }] as arguments;
          } ->
          (* Ensure we simulate the body of __setitem__ in case the function contains taint. *)
          let state = analyze_call ~resolution location ~taint ~state callee arguments in
          (* Handle base[index] = value. *)
          analyze_assignment
            ~resolution
            ~fields:[AccessPath.get_index index]
            ~target:base
            ~value
            state
      | Call
          {
            callee =
              { Node.value = Name (Name.Attribute { base; attribute = "__getitem__"; _ }); _ };
            arguments = [{ Call.Argument.value = argument_value; _ }];
          } ->
          let index = AccessPath.get_index argument_value in
          let taint = BackwardState.Tree.prepend [index] taint in
          analyze_expression ~resolution ~taint ~state ~expression:base
      (* Special case x.__next__() as being a random index access (this pattern is the desugaring of
         `for element in x`). *)
      | Call
          {
            callee = { Node.value = Name (Name.Attribute { base; attribute = "__next__"; _ }); _ };
            arguments = [];
          } ->
          let taint = BackwardState.Tree.prepend [AbstractTreeDomain.Label.Any] taint in
          analyze_expression ~resolution ~taint ~state ~expression:base
      (* We special object.__setattr__, which is sometimes used in order to work around dataclasses
         being frozen post-initialization. *)
      | Call
          {
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
                  Call.Argument.value = { Node.value = Expression.String _; _ } as attribute;
                  name = None;
                };
                { Call.Argument.value; name = None };
              ];
          } ->
          analyze_assignment
            ~resolution
            ~target:self
            ~fields:[AccessPath.get_index attribute]
            ~value
            state
      | Call { callee; arguments } ->
          analyze_call ~resolution location ~taint ~state callee arguments
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
              ~taint:(read_tree [AbstractTreeDomain.Label.DictionaryKeys] taint)
              ~state
              ~expression:key
          in
          let state =
            analyze_expression
              ~resolution
              ~taint:(read_tree [AbstractTreeDomain.Label.Any] taint)
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
          List.foldi list ~f:(analyze_reverse_list_element ~total ~resolution taint) ~init:state
      | ListComprehension comprehension ->
          analyze_comprehension ~resolution taint comprehension state
      | Name _ when AccessPath.is_global ~resolution { Node.location; value = expression } -> state
      | Name (Name.Identifier identifier) ->
          store_weak_taint ~root:(Root.Variable identifier) ~path:[] taint state
      | Name (Name.Attribute { base; attribute; _ }) -> (
          match
            Interprocedural.CallResolution.resolve_property_targets ~resolution ~base ~attribute
          with
          | None ->
              let field = AbstractTreeDomain.Label.Field attribute in
              let taint =
                BackwardState.Tree.assign [field] ~tree:BackwardState.Tree.empty ~subtree:taint
              in
              analyze_expression ~resolution ~taint ~state ~expression:base
          | Some targets ->
              let arguments = [{ Call.Argument.name = None; value = base }] in
              apply_call_targets
                ~resolution
                ~call_expression:expression
                location
                arguments
                state
                taint
                targets )
      | Set set ->
          let element_taint = read_tree [AbstractTreeDomain.Label.Any] taint in
          List.fold
            set
            ~f:(fun state expression ->
              analyze_expression ~resolution ~taint:element_taint ~state ~expression)
            ~init:state
      | SetComprehension comprehension ->
          analyze_comprehension ~resolution taint comprehension state
      | Starred (Starred.Once expression)
      | Starred (Starred.Twice expression) ->
          let taint = BackwardState.Tree.prepend [AbstractTreeDomain.Label.Any] taint in
          analyze_expression ~resolution ~taint ~state ~expression
      | String { StringLiteral.kind = StringLiteral.Format expressions; _ } ->
          let taint =
            BackwardState.Tree.transform
              BackwardTaint.simple_feature_set
              ~f:Domains.add_format_string_feature
              taint
          in
          List.fold
            expressions
            ~f:(fun state expression -> analyze_expression ~resolution ~taint ~state ~expression)
            ~init:state
      | String _ -> state
      | Ternary { target; test; alternative } ->
          let state_then = analyze_expression ~resolution ~taint ~state ~expression:target in
          let state_else = analyze_expression ~resolution ~taint ~state ~expression:alternative in
          join state_then state_else
          |> fun state ->
          analyze_expression ~resolution ~taint:BackwardState.Tree.empty ~state ~expression:test
      | True -> state
      | Tuple list ->
          let total = List.length list in
          List.foldi list ~f:(analyze_reverse_list_element ~total ~resolution taint) ~init:state
      | UnaryOperator { operator = _; operand } ->
          analyze_expression ~resolution ~taint ~state ~expression:operand
      | WalrusOperator { target; value } ->
          analyze_expression ~resolution ~taint ~state ~expression:value
          |> fun state -> analyze_expression ~resolution ~taint ~state ~expression:target
      | Yield (Some expression) -> analyze_expression ~resolution ~taint ~state ~expression
      | Yield None -> state


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
                let index_name = AbstractTreeDomain.Label.Field (string_of_int position) in
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
              Model.get_global_sink_model ~resolution ~location ~expression:target
              |> Option.value ~default:BackwardState.Tree.empty
            in
            BackwardState.Tree.join local_taint global_taint
          in
          taint, false


    and analyze_assignment ~resolution ?(fields = []) ~target ~value state =
      let taint =
        compute_assignment_taint ~resolution target state |> fst |> BackwardState.Tree.read fields
      in
      let state =
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
        | None -> state
      in
      analyze_expression ~resolution ~taint ~state ~expression:value


    let analyze_statement ~resolution state statement =
      log "State: %a\nStmt: %a" pp state pp_statement statement;
      match statement with
      | Statement.Assign { target; value; _ } -> analyze_assignment ~resolution ~target ~value state
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
      | Yield expression
      | YieldFrom expression ->
          let access_path = { root = Root.LocalResult; path = [] } in
          let return_taint = get_taint (Some access_path) state in
          analyze_expression ~resolution ~taint:return_taint ~state ~expression


    let backward ?key state ~statement:{ Node.value = statement; _ } =
      let resolution =
        let { Node.value = { Define.signature = { Define.Signature.parent; _ }; _ }; _ } =
          FunctionContext.definition
        in
        TypeCheck.resolution_with_key
          ~global_resolution:FunctionContext.global_resolution
          ~local_annotations:FunctionContext.local_annotations
          ~parent
          ~key
      in
      analyze_statement ~resolution state statement


    let forward ?key:_ _ ~statement:_ = failwith "Don't call me"
  end

  and Analyzer : (Fixpoint.Fixpoint with type state = FixpointState.t) =
    Fixpoint.Make (FixpointState)
end

(* Split the inferred entry state into externally visible taint_in_taint_out parts and sink_taint. *)
let extract_tito_and_sink_models define ~resolution ~existing_backward entry_taint =
  let { Define.signature = { parameters; _ }; _ } = define in
  let normalized_parameters = AccessPath.Root.normalize_parameters parameters in
  (* Simplify trees by keeping only essential structure and merging details back into that. *)
  let simplify annotation tree =
    let annotation = Option.map ~f:(GlobalResolution.parse_annotation resolution) annotation in
    let essential = BackwardState.Tree.essential tree in
    BackwardState.Tree.shape tree ~mold:essential
    |> BackwardState.Tree.transform
         BackwardTaint.simple_feature_set
         ~f:(Features.add_type_breadcrumb ~resolution annotation)
  in
  let split_and_simplify model (parameter, name, original) =
    let annotation = original.Node.value.Parameter.annotation in
    let partition =
      BackwardState.read ~root:(Root.Variable name) ~path:[] entry_taint
      |> BackwardState.Tree.partition BackwardTaint.leaf ~f:Fn.id
    in
    let compute_features_to_attach taint =
      BackwardState.read ~root:parameter ~path:[] taint
      |> BackwardState.Tree.collapse
      |> BackwardTaint.partition BackwardTaint.leaf ~f:(Sinks.equal Sinks.Attach)
      |> (fun map -> Map.Poly.find map true)
      >>| BackwardTaint.fold BackwardTaint.simple_feature_set ~f:List.rev_append ~init:[]
      |> Option.value ~default:[]
    in
    let taint_in_taint_out =
      let features_to_attach =
        compute_features_to_attach existing_backward.TaintResult.Backward.taint_in_taint_out
      in
      let candidate_tree =
        Map.Poly.find partition Sinks.LocalReturn
        |> Option.value ~default:BackwardState.Tree.empty
        |> simplify annotation
      in
      let candidate_tree =
        if List.is_empty features_to_attach then
          candidate_tree
        else
          BackwardState.Tree.transform
            BackwardTaint.simple_feature_set
            ~f:(List.rev_append features_to_attach)
            candidate_tree
      in
      let number_of_paths =
        BackwardState.Tree.fold
          BackwardState.Tree.RawPath
          ~init:0
          ~f:(fun count _ -> count + 1)
          candidate_tree
      in
      if number_of_paths > 5 then
        BackwardState.Tree.collapse_to ~depth:0 candidate_tree
      else
        candidate_tree
    in
    let sink_taint =
      let simplify_sink_taint ~key:sink ~data:sink_tree accumulator =
        match sink with
        | Sinks.LocalReturn
        | Sinks.Attach ->
            accumulator
        | _ -> simplify annotation sink_tree |> BackwardState.Tree.join accumulator
      in
      Map.Poly.fold ~init:BackwardState.Tree.empty ~f:simplify_sink_taint partition
    in
    let sink_taint =
      let features_to_attach =
        compute_features_to_attach existing_backward.TaintResult.Backward.sink_taint
      in
      if not (List.is_empty features_to_attach) then
        BackwardState.Tree.transform
          BackwardTaint.simple_feature_set
          ~f:(List.rev_append features_to_attach)
          sink_taint
      else
        sink_taint
    in
    TaintResult.Backward.
      {
        taint_in_taint_out =
          BackwardState.assign ~root:parameter ~path:[] taint_in_taint_out model.taint_in_taint_out;
        sink_taint = BackwardState.assign ~root:parameter ~path:[] sink_taint model.sink_taint;
      }
  in
  List.fold normalized_parameters ~f:split_and_simplify ~init:TaintResult.Backward.empty


let run ~environment ~qualifier ~define ~existing_model =
  let ( { Node.value = { Define.signature = { name = { Node.value = name; _ }; _ }; _ }; _ } as
      define )
    =
    (* Apply decorators to make sure we match parameters up correctly. *)
    let resolution = TypeEnvironment.ReadOnly.global_resolution environment in
    Annotated.Define.create define
    |> Annotated.Define.decorate ~resolution
    |> Annotated.Define.define
  in
  let module AnalysisInstance = AnalysisInstance (struct
    let qualifier = qualifier

    let definition = define

    let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment

    let local_annotations =
      TypeEnvironment.ReadOnly.get_local_annotations
        environment
        (Node.value define |> Define.name |> Node.value)


    let is_constructor () =
      match Reference.last name with
      | "__init__" -> true
      | _ -> false


    let first_parameter () =
      match define.value.Define.signature.parameters with
      | { Node.value = { Parameter.name; _ }; _ } :: _ -> Some (Root.Variable name)
      | _ -> None


    let debug = Define.dump define.value
  end)
  in
  let open AnalysisInstance in
  let initial = FixpointState.{ taint = initial_taint } in
  let cfg = Cfg.create define.value in
  let () = log "Backward analysis of callable: %a" Reference.pp name in
  let entry_state = Analyzer.backward ~cfg ~initial |> Analyzer.entry in
  let () =
    match entry_state with
    | Some entry_state -> log "Final state: %a" FixpointState.pp entry_state
    | None -> log "No final state found"
  in
  let resolution = TypeEnvironment.ReadOnly.global_resolution environment in
  let extract_model FixpointState.{ taint; _ } =
    let model =
      extract_tito_and_sink_models
        define.value
        ~resolution
        ~existing_backward:existing_model.TaintResult.backward
        taint
    in
    let () = log "Callable: %a Models: %a" Reference.pp name TaintResult.Backward.pp_model model in
    model
  in
  entry_state >>| extract_model |> Option.value ~default:TaintResult.Backward.empty
