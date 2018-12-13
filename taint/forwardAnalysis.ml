(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

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
  [@@deriving show]

  include Fixpoint.State with type t := t

  val create: unit -> t
end


module type FUNCTION_CONTEXT = sig
  val definition: Define.t Node.t
  val environment: (module Environment.Handler)

  val add_flow_candidate: Flow.candidate -> unit
  val generate_issues: unit -> Flow.issue list
end


module AnalysisInstance(FunctionContext: FUNCTION_CONTEXT) = struct
  module rec FixpointState : FixpointState = struct
    type t = { taint: ForwardState.t }
    [@@deriving show]


    let initial_taint = ForwardState.empty


    let create () =
      { taint = ForwardState.empty }


    let less_or_equal ~left:{ taint = left } ~right:{ taint = right } =
      ForwardState.less_or_equal ~left ~right


    let join { taint = left; } { taint = right; _ } =
      let taint = ForwardState.join left right in
      { taint }


    let widen ~previous:{ taint = previous; _ } ~next:{ taint = next } ~iteration =
      let taint = ForwardState.widen ~iteration ~previous ~next in
      { taint }


    let get_taint_option access_path state =
      match access_path with
      | None ->
          ForwardState.Tree.empty
      | Some (root, path) ->
          ForwardState.read ~root ~path state.taint


    let store_taint ~root ~path taint { taint = state_taint } =
      { taint = ForwardState.assign ~root ~path taint state_taint }


    let store_taint_option access_path taint state =
      match access_path with
      | Some { AccessPath.root; path } -> store_taint ~root ~path taint state
      | None -> state


    let rec analyze_argument ~resolution state taint_accumulator { Argument.value = argument; _ } =
      analyze_expression ~resolution argument state
      |> ForwardState.Tree.join taint_accumulator


    and apply_call_targets ~resolution call_location arguments state call_targets =
      let apply_call_target (call_target, _implicit) =
        let taint_model = Model.get_callsite_model ~resolution ~call_target ~arguments in
        if not taint_model.is_obscure then
          let { TaintResult.forward; backward; _ } = taint_model.model in
          let sink_roots = BackwardState.roots backward.sink_taint in
          let sink_argument_matches = AccessPath.match_actuals_to_formals arguments sink_roots in
          let tito_roots = BackwardState.roots backward.taint_in_taint_out in
          let tito_argument_matches = AccessPath.match_actuals_to_formals arguments tito_roots in
          let combined_matches = List.zip_exn sink_argument_matches tito_argument_matches in
          let combine_sink_taint location taint_tree { root; actual_path; formal_path; } =
            BackwardState.read
              ~root
              ~path:[]
              backward.sink_taint
            |> BackwardState.Tree.apply_call location ~callees:[ call_target ] ~port:root
            |> BackwardState.Tree.read formal_path
            |> BackwardState.Tree.prepend actual_path
            |> BackwardState.Tree.join taint_tree
          in
          let combine_tito taint_tree { AccessPath.root; actual_path; formal_path; } =
            let new_tito =
              BackwardState.read
                ~root
                ~path:formal_path
                backward.taint_in_taint_out
              |> BackwardState.Tree.prepend actual_path
            in
            BackwardState.Tree.join taint_tree new_tito
          in
          let analyze_argument tito ((argument, sink_matches), (_dup, tito_matches)) =
            let { Node.location; _ } = argument in
            let argument_taint = analyze_unstarred_expression ~resolution argument state in
            let tito =
              let add_tito_location features =
                (SimpleFeatures.TitoPosition location) :: features
              in
              let convert_tito tito {BackwardState.Tree.path; tip=return_taint; _} =
                let taint_to_propagate =
                  ForwardState.Tree.read path argument_taint
                  |> ForwardState.Tree.collapse
                  |> ForwardTaint.transform ForwardTaint.simple_feature_set ~f:add_tito_location
                  |> ForwardState.Tree.create_leaf
                in
                let return_paths =
                  let gather_paths paths (ComplexFeatures.ReturnAccessPath extra_path) =
                    extra_path :: paths
                  in
                  BackwardTaint.fold
                    BackwardTaint.complex_feature
                    return_taint
                    ~f:gather_paths
                    ~init:[]
                in
                let create_tito_return_paths tito return_path =
                  ForwardState.Tree.prepend return_path taint_to_propagate
                  |> ForwardState.Tree.join tito
                in
                List.fold return_paths ~f:create_tito_return_paths ~init:tito
              in
              let taint_in_taint_out =
                List.fold tito_matches ~f:combine_tito ~init:BackwardState.Tree.empty
              in
              BackwardState.Tree.fold
                BackwardState.Tree.RawPath
                taint_in_taint_out
                ~init:tito
                ~f:convert_tito
            in
            let flow_candidate =
              let sink_tree =
                List.fold
                  sink_matches
                  ~f:(combine_sink_taint location)
                  ~init:BackwardState.Tree.empty
              in
              Flow.generate_source_sink_matches
                ~location
                ~source_tree:argument_taint
                ~sink_tree
            in
            FunctionContext.add_flow_candidate flow_candidate;
            tito
          in
          let tito =
            List.fold ~f:analyze_argument combined_matches ~init:ForwardState.Tree.empty
          in
          let result_taint =
            ForwardState.read ~root:AccessPath.Root.LocalResult ~path:[] forward.source_taint
            |> ForwardState.Tree.apply_call
              call_location
              ~callees:[call_target]
              ~port:AccessPath.Root.LocalResult
          in
          ForwardState.Tree.join result_taint tito
        else
          (* Obscure/no model. *)
          List.fold
            arguments
            ~init:ForwardState.Tree.empty
            ~f:(analyze_argument ~resolution state)
      in
      match call_targets with
      | [] ->
          (* If we don't have a call target: propagate argument taint. *)
          List.fold arguments ~init:ForwardState.Tree.empty ~f:(analyze_argument ~resolution state)
      | call_targets ->
          List.map call_targets ~f:apply_call_target
          |> List.fold ~init:ForwardState.Tree.empty ~f:ForwardState.Tree.join


    and analyze_call ~resolution location ~callee arguments state =
      match callee with
      | AccessPath.Global access ->
          let targets = Interprocedural.CallResolution.get_targets ~resolution ~global:access in
          let _, extra_arguments =
            Interprocedural.CallResolution.normalize_global ~resolution access
          in
          let arguments = extra_arguments @ arguments in
          apply_call_targets ~resolution location arguments state targets
      | AccessPath.Access { expression; member = method_name } ->
          let receiver = AccessPath.as_access expression in
          let arguments =
            let receiver = {
              Argument.name = None;
              value = Access.expression ~location receiver;
            } in
            receiver :: arguments
          in
          Interprocedural.CallResolution.get_indirect_targets ~resolution ~receiver ~method_name
          |> apply_call_targets ~resolution location arguments state
      | callee ->
          (* TODO(T31435135): figure out the BW and TITO model for whatever is called here. *)
          let callee_taint = analyze_normalized_expression ~resolution location state callee in
          (* For now just join all argument and receiver taint and propagate to result. *)
          let taint =
            List.fold_left
              arguments
              ~f:(analyze_argument ~resolution state)
              ~init:callee_taint
          in
          taint


    and analyze_normalized_expression ~resolution location state expression =
      let global_model access =
        (* Fields are handled like methods *)
        let target_candidates = [
          Interprocedural.Callable.create_method access;
          Interprocedural.Callable.create_object access;
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
      in
      match expression with
      | Access { expression; member } ->
          let attribute_taint =
            let annotations =
              let annotation =
                AccessPath.as_access expression
                |> Access.expression
                |> Resolution.resolve resolution
              in
              let successors =
                Resolution.class_representation resolution annotation
                >>| (fun { Resolution.successors; _ } -> successors)
                |> Option.value ~default:[]
              in
              annotation :: successors
            in
            let attribute_taint sofar annotation =
              (Type.class_name annotation) @ [Access.Identifier member]
              |> global_model
              |> ForwardState.Tree.join sofar
            in
            List.fold annotations ~init:ForwardState.Tree.empty ~f:attribute_taint
          in
          let inferred_taint =
            let taint = analyze_normalized_expression ~resolution location state expression in
            let field = AbstractTreeDomain.Label.Field (Identifier.show member) in
            ForwardState.Tree.read
              [field]
              taint
          in
          ForwardState.Tree.join inferred_taint attribute_taint
      | Index { expression; index; _ } ->
          let taint = analyze_normalized_expression ~resolution location state expression in
          ForwardState.Tree.read [index] taint
      | Call { callee; arguments; } ->
          analyze_call ~resolution arguments.location ~callee arguments.value state
      | Expression expression ->
          analyze_expression ~resolution expression state
      | Global access ->
          global_model access
      | Local identifier ->
          ForwardState.read
            ~root:(AccessPath.Root.Variable identifier)
            ~path:[]
            state.taint

    and analyze_dictionary_entry ~resolution state taint { Dictionary.key; value; } =
      let field_name =
        match key.Node.value with
        | String literal -> AbstractTreeDomain.Label.Field literal.value
        | _ -> AbstractTreeDomain.Label.Any
      in
      let value_taint = analyze_expression ~resolution value state in
      ForwardState.Tree.prepend [field_name] value_taint
      |> ForwardState.Tree.join taint

    and analyze_list_element ~resolution state position taint expression =
      let index_name = AbstractTreeDomain.Label.Field (string_of_int position) in
      let value_taint = analyze_expression ~resolution expression state in
      ForwardState.Tree.prepend [index_name] value_taint
      |> ForwardState.Tree.join taint

    and analyze_set_element ~resolution state taint expression =
      let value_taint =
        analyze_expression ~resolution expression state
        |> ForwardState.Tree.prepend [AbstractTreeDomain.Label.Any]
      in
      ForwardState.Tree.join taint value_taint

    and analyze_comprehension ~resolution { Comprehension.element; generators; _ } state =
      let add_binding state { Comprehension.target; iterator; _ } =
        let taint =
          analyze_expression ~resolution iterator state
          |> ForwardState.Tree.read [AbstractTreeDomain.Label.Any]
        in
        let access_path = AccessPath.of_expression target in
        store_taint_option access_path taint state
      in
      let bound_state = List.fold ~f:add_binding generators ~init:state in
      let collection_taint = analyze_expression ~resolution element bound_state in
      ForwardState.Tree.prepend [AbstractTreeDomain.Label.Any] collection_taint

    (* Skip through * and **. Used at call sites where * and ** are handled explicitly *)
    and analyze_unstarred_expression ~resolution expression state =
      match expression.Node.value with
      | Starred (Starred.Once expression)
      | Starred (Starred.Twice expression) ->
          analyze_expression ~resolution expression state
      | _ ->
          analyze_expression ~resolution expression state

    and analyze_expression ~resolution expression state =
      match expression.Node.value with
      | Access access ->
          AccessPath.normalize_access access ~resolution
          |> analyze_normalized_expression ~resolution expression.Node.location state
      | Await expression ->
          analyze_expression ~resolution expression state
      | BooleanOperator { left; operator = _; right }
      | ComparisonOperator { left; operator = _; right } ->
          let left_taint = analyze_expression ~resolution left state in
          let right_taint = analyze_expression ~resolution right state in
          ForwardState.Tree.join left_taint right_taint
      | Complex _ ->
          ForwardState.Tree.empty
      | Dictionary dictionary ->
          List.fold
            dictionary.entries
            ~f:(analyze_dictionary_entry ~resolution state)
            ~init:ForwardState.Tree.empty
      | DictionaryComprehension _
      | Ellipses
      | False
      | Float _ ->
          ForwardState.Tree.empty
      | Generator comprehension ->
          analyze_comprehension ~resolution comprehension state
      | Integer _ ->
          ForwardState.Tree.empty
      | Lambda { parameters = _; body } ->
          (* Ignore parameter bindings and pretend body is inlined *)
          analyze_expression ~resolution body state
      | List list ->
          List.foldi ~f:(analyze_list_element ~resolution state) list ~init:ForwardState.Tree.empty
      | ListComprehension comprehension ->
          analyze_comprehension ~resolution comprehension state
      | Set set ->
          List.fold ~f:(analyze_set_element ~resolution state) set ~init:ForwardState.Tree.empty
      | SetComprehension comprehension ->
          analyze_comprehension ~resolution comprehension state
      | Starred (Starred.Once expression)
      | Starred (Starred.Twice expression) ->
          analyze_expression ~resolution expression state
          |> ForwardState.Tree.read [AbstractTreeDomain.Label.Any]
      | String { StringLiteral.kind = StringLiteral.Format expressions; _ } ->
          expressions
          |> List.map ~f:(fun expression -> analyze_expression ~resolution expression state)
          |> List.fold ~f:(ForwardState.Tree.join) ~init:ForwardState.Tree.empty
      | String _ ->
          ForwardState.Tree.empty
      | Ternary { target; test; alternative } ->
          let taint_then = analyze_expression ~resolution target state in
          let taint_else = analyze_expression ~resolution alternative state in
          let _ = analyze_expression ~resolution test state in
          ForwardState.Tree.join taint_then taint_else
      | True ->
          ForwardState.Tree.empty
      | Tuple expressions ->
          List.foldi
            ~f:(analyze_list_element ~resolution state)
            expressions
            ~init:ForwardState.Tree.empty
      | UnaryOperator { operator = _; operand } ->
          analyze_expression ~resolution operand state
      | Yield (Some expression) ->
          analyze_expression ~resolution expression state
      | Yield None ->
          ForwardState.Tree.empty


    let analyze_definition ~define:_ state =
      state


    let rec analyze_assignment target taint surrounding_taint state =
      match target.Node.value with
      | Starred (Once target | Twice target) ->
          (* This is approximate. Unless we can get the tuple type on the right
             to tell how many total elements there will be, we just pick up the
             entire collection. *)
          analyze_assignment target surrounding_taint surrounding_taint state
      | List targets
      | Tuple targets ->
          let analyze_target_element i state target =
            let index = AbstractTreeDomain.Label.Field (string_of_int i) in
            let indexed_taint = ForwardState.Tree.read [index] taint in
            analyze_assignment target indexed_taint taint state
          in
          List.foldi targets ~f:analyze_target_element ~init:state
      | _ ->
          let access_path = AccessPath.of_expression target in
          store_taint_option access_path taint state


    let rec analyze_statement ~resolution statement state =
      match statement with
      | Assign { target; value; _ } ->
          let taint = analyze_expression ~resolution value state in
          analyze_assignment target taint taint state
      | Assert _
      | Break
      | Class _
      | Continue ->
          state
      | Define define ->
          analyze_definition ~define state
      | Delete _ ->
          state
      | Expression expression ->
          let _ = analyze_expression ~resolution expression state in
          state
      | For _
      | Global _
      | If _
      | Import _
      | Nonlocal _
      | Pass
      | Raise _ -> state
      | Return { expression = Some expression; _ } ->
          let taint = analyze_expression ~resolution expression state in
          store_taint ~root:AccessPath.Root.LocalResult ~path:[] taint state
      | Return { expression = None; _ }
      | Try _
      | With _
      | While _ ->
          state
      | Yield expression
      | YieldFrom expression ->
          let taint = analyze_expression ~resolution expression state in
          store_taint ~root:AccessPath.Root.LocalResult ~path:[] taint state


    let forward ?key state ~statement:({ Node.value = statement; _ }) =
      Log.log
        ~section:`Taint
        "State: %a\nAnalyzing statement: %a"
        pp state
        Statement.pp_statement statement;
      let resolution =
        TypeCheck.resolution_with_key
          ~environment:FunctionContext.environment
          ~parent:FunctionContext.definition.value.parent
          ~access:FunctionContext.definition.value.name
          ~key
      in
      analyze_statement ~resolution statement state


    let backward ?key:_ _ ~statement:_ =
      failwith "Don't call me"
  end


  and Analyzer : Fixpoint.Fixpoint with type state = FixpointState.t = Fixpoint.Make(FixpointState)
end


let extract_source_model _parameters exit_taint =
  let simplify tree =
    let essential = ForwardState.Tree.essential tree in
    ForwardState.Tree.shape tree ~mold:essential
  in
  let return_taint =
    ForwardState.read ~root:AccessPath.Root.LocalResult ~path:[] exit_taint
    |> simplify
  in
  ForwardState.assign ~root:AccessPath.Root.LocalResult ~path:[] return_taint ForwardState.empty


let run ~environment ~define:({ Node.value = { Define.parameters; _ }; _ } as define) =
  let module Context = struct
    let definition = define
    let environment = environment

    let candidates = Location.Reference.Table.create ()

    let add_flow_candidate candidate =
      Location.Reference.Table.set
        candidates
        ~key:candidate.Flow.location
        ~data:candidate

    let generate_issues () =
      let accumulate ~key:_ ~data:candidate issues =
        let new_issues = Flow.generate_issues ~define candidate in
        List.rev_append new_issues issues
      in
      Location.Reference.Table.fold candidates ~f:accumulate ~init:[]
  end
  in
  let module AnalysisInstance = AnalysisInstance(Context) in
  let open AnalysisInstance in
  Log.log
    ~section:`Taint
    "Starting analysis of %a"
    Interprocedural.Callable.pp (Interprocedural.Callable.create define);
  let cfg = Cfg.create define.value in
  let initial = FixpointState.create () in
  let () = Log.log ~section:`Taint "Processing CFG:@.%a" Cfg.pp cfg in
  let exit_state =
    Analyzer.forward ~cfg ~initial
    |> Analyzer.exit
  in
  let extract_model ({ FixpointState.taint; _ } as result) =
    let source_taint = extract_source_model parameters taint in
    let () = Log.log ~section:`Taint "Model: %a" FixpointState.pp result in
    TaintResult.Forward.{ source_taint; }
  in
  let issues = Context.generate_issues () in
  let () =
    Log.log
      ~section:`Taint
      "Issues %a"
      Sexp.pp [%message (issues: Flow.issue list)]
  in
  let model =
    exit_state
    >>| extract_model
    |> Option.value ~default:TaintResult.Forward.empty
  in
  model, issues
