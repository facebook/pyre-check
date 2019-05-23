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

  val create:
    existing_model:TaintResult.call_model
    -> (Root.t * Identifier.t * 'a Parameter.t) list
    -> t

end


module type FUNCTION_CONTEXT = sig
  val definition: Define.t Node.t
  val environment: (module Environment.Handler)

  val check_flow
    :  location: Location.t
    -> source_tree: ForwardState.Tree.t
    -> sink_tree: BackwardState.Tree.t
    -> unit

  val return_sink: BackwardState.Tree.t

  val debug: bool
end


let number_regexp = Str.regexp "[0-9]+"
let is_numeric name =
  Str.string_match number_regexp name 0

let (|>>) (taint, state) f = (f taint, state)

module AnalysisInstance(FunctionContext: FUNCTION_CONTEXT) = struct
  let log format =
    if FunctionContext.debug then
      Log.dump format
    else
      Log.log ~section:`Taint format

  module rec FixpointState : FixpointState = struct
    type t = { taint: ForwardState.t }
    [@@deriving show { with_path = false }]


    let initial_taint = ForwardState.empty

    let empty_state = { taint = initial_taint }

    let create ~existing_model parameters =
      (* Use primed sources to populate initial state of parameters *)
      let forward_primed_taint = existing_model.TaintResult.forward.source_taint in
      let prime_parameter state (parameter_root, name, original) =
        let location = original.Node.location in
        let prime =
          ForwardState.read ~root:parameter_root ~path:[] forward_primed_taint
          |> ForwardState.Tree.apply_call location ~callees:[] ~port:parameter_root
        in
        let root = AccessPath.Root.Variable name in
        let taint = ForwardState.assign ~root ~path:[] prime state.taint in
        { state with taint }
      in
      List.fold
        parameters
        ~init:{ taint = ForwardState.empty }
        ~f:prime_parameter


    let less_or_equal ~left:{ taint = left } ~right:{ taint = right } =
      ForwardState.less_or_equal ~left ~right


    let join { taint = left; } { taint = right; _ } =
      let taint = ForwardState.join left right in
      { taint }


    let widen ~previous:{ taint = previous; _ } ~next:{ taint = next } ~iteration =
      let taint = ForwardState.widen ~iteration ~previous ~next in
      { taint }


    let store_taint ?(weak = false) ~root ~path taint { taint = state_taint } =
      { taint = ForwardState.assign ~weak ~root ~path taint state_taint }


    let store_taint_option ?(weak = false) access_path taint state =
      match access_path with
      | Some { AccessPath.root; path } -> store_taint ~weak ~root ~path taint state
      | None -> state


    let add_first kind name set =
      let already_has_first = function
        | Features.Simple.Breadcrumb (Features.Breadcrumb.First { kind = has_kind; _ }) ->
            has_kind = kind
        | _ ->
            false
      in
      if List.exists set ~f:already_has_first then
        set
      else
        (Features.Simple.Breadcrumb (Features.Breadcrumb.HasFirst kind)) ::
        (Features.Simple.Breadcrumb (Features.Breadcrumb.First { kind; name })) :: set


    let add_first_index index =
      match index with
      | AbstractTreeDomain.Label.Field name when is_numeric name ->
          add_first Features.Breadcrumb.FirstIndex "<numeric>"
      | AbstractTreeDomain.Label.Field name ->
          add_first Features.Breadcrumb.FirstIndex name
      | AbstractTreeDomain.Label.Any ->
          add_first Features.Breadcrumb.FirstIndex "<unknown>"


    let rec analyze_argument
        ~resolution
        (taint_accumulator, state)
        { Argument.value = argument; _ } =
      analyze_expression ~resolution ~state ~expression:argument
      |>> ForwardState.Tree.join taint_accumulator


    and apply_call_targets ~resolution call_location arguments state call_targets =
      let apply_call_target (call_target, _implicit) =
        let taint_model = Model.get_callsite_model ~call_target in
        if not taint_model.is_obscure then
          let { TaintResult.forward; backward; _ } = taint_model.model in
          let sink_argument_matches =
            BackwardState.roots backward.sink_taint
            |> AccessPath.match_actuals_to_formals arguments
          in
          let tito_argument_matches =
            BackwardState.roots backward.taint_in_taint_out
            |> AccessPath.match_actuals_to_formals arguments
          in
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
          let merge_tito_effect join ~key:_ = function
            | `Left left -> Some left
            | `Right right -> Some right
            | `Both (left, right) -> Some (join left right)
          in
          let combine_tito tito_map { AccessPath.root; actual_path; formal_path; } =
            let new_tito_map =
              BackwardState.read
                ~root
                ~path:formal_path
                backward.taint_in_taint_out
              |> BackwardState.Tree.prepend actual_path
              |> BackwardState.Tree.partition Domains.BackwardTaint.leaf ~f:Fn.id
            in
            Map.Poly.merge tito_map new_tito_map ~f:(merge_tito_effect BackwardState.Tree.join)
          in
          let analyze_argument_and_compute_tito_effect
              (tito_effects, state)
              ((argument, sink_matches), (_dup, tito_matches)) =
            let { Node.location; _ } = argument in
            let argument_taint, state = analyze_unstarred_expression ~resolution argument state in
            let tito =
              let convert_tito_path
                  kind
                  accumulated_tito
                  {BackwardState.Tree.path; tip=return_taint; _}
                =
                let breadcrumbs =
                  let gather_breadcrumbs breadcrumbs feature =
                    match feature with
                    | (Features.Simple.Breadcrumb _) as breadcrumb ->
                        breadcrumb :: breadcrumbs
                    | _ ->
                        breadcrumbs
                  in
                  BackwardTaint.fold
                    BackwardTaint.simple_feature
                    return_taint
                    ~f:gather_breadcrumbs
                    ~init:[
                      Features.Simple.Breadcrumb Features.Breadcrumb.Tito;
                      Features.Simple.TitoPosition location;
                    ]
                in
                let add_features features =
                  List.rev_append breadcrumbs features
                in
                let taint_to_propagate =
                  ForwardState.Tree.read path argument_taint
                  |> ForwardState.Tree.collapse
                  |> ForwardTaint.transform ForwardTaint.simple_feature_set ~f:add_features
                  |> ForwardState.Tree.create_leaf
                in
                let return_paths =
                  match kind with
                  | Sinks.LocalReturn ->
                      let gather_paths paths (Features.Complex.ReturnAccessPath extra_path) =
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
            let sink_tree =
              List.fold
                sink_matches
                ~f:(combine_sink_taint location)
                ~init:BackwardState.Tree.empty
            in
            FunctionContext.check_flow ~location ~source_tree:argument_taint ~sink_tree;
            tito, state
          in
          let tito_effects, state =
            List.fold
              ~f:analyze_argument_and_compute_tito_effect combined_matches
              ~init:(Map.Poly.empty, state)
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
          let apply_tito_side_effects tito_effects state =
            let for_each_target ~key:target ~data:taint state =
              match target with
              | Sinks.LocalReturn -> state  (* This is regular tito which was computed above *)
              | ParameterUpdate n ->  (* Side effect on argument n *)
                  begin
                    match List.nth arguments n with
                    | None -> state
                    | Some { Argument.value = exp; _ } ->
                        let access_path = AccessPath.of_expression exp in
                        store_taint_option ~weak:true access_path taint state
                  end
              | _ ->
                  failwith "unexpected sink in tito"
            in
            Map.Poly.fold tito_effects ~f:for_each_target ~init:state
          in
          ForwardState.Tree.join result_taint tito, apply_tito_side_effects tito_effects state
        else
          (* Obscure/no model. *)
          List.fold
            arguments
            ~init:(ForwardState.Tree.empty, state)
            ~f:(analyze_argument ~resolution)
          |>> ForwardState.Tree.transform ForwardTaint.simple_feature_set ~f:Features.add_obscure
      in
      match call_targets with
      | [] ->
          (* If we don't have a call target: propagate argument taint. *)
          List.fold arguments
            ~init:(ForwardState.Tree.empty, state)
            ~f:(analyze_argument ~resolution)
          |>> ForwardState.Tree.transform ForwardTaint.simple_feature_set ~f:Features.add_obscure
      | call_targets ->
          List.map call_targets ~f:apply_call_target
          |> List.fold
            ~init:(ForwardState.Tree.empty, empty_state)
            ~f:(
              fun (taint, state) (new_taint, new_state) ->
                ForwardState.Tree.join taint new_taint, join state new_state
            )

    and analyze_call ~resolution location ~callee arguments state =
      match callee with
      | AccessPath.Global access ->
          let targets =
            Interprocedural.CallResolution.get_global_targets ~resolution ~global:access
          in
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
              value = Node.create (Expression.Access receiver) ~location;
            } in
            receiver :: arguments
          in
          begin
            let add_index_breadcrumb_if_necessary taint =
              if not (String.equal method_name "get") then
                taint
              else
                match arguments with
                | _receiver :: index:: _ ->
                    let label = get_index index.value in
                    ForwardState.Tree.transform
                      ForwardTaint.simple_feature_set
                      ~f:(add_first_index label)
                      taint
                | _ ->
                    taint
            in
            Interprocedural.CallResolution.get_indirect_targets
              ~resolution
              ~receiver
              ~method_name
            |> apply_call_targets ~resolution location arguments state
            |>> add_index_breadcrumb_if_necessary
          end
      | callee ->
          (* TODO(T31435135): figure out the BW and TITO model for whatever is called here. *)
          let callee_taint, state =
            analyze_normalized_expression
              ~resolution
              ~state
              ~expression:(Node.create ~location callee)
          in
          (* For now just join all argument and receiver taint and propagate to result. *)
          List.fold_left
            arguments
            ~f:(analyze_argument ~resolution)
            ~init:(callee_taint, state)


    and analyze_normalized_expression
        ~resolution
        ~state
        ~expression =
      let result, state = analyze_normalized_expression_internal
          ~resolution
          ~state
          ~expression
      in
      let () =
        log
          "Expression taint: %a:\n  result taint: %a"
          AccessPath.pp_normalized_expression expression.value
          ForwardState.Tree.pp result
      in
      result, state


    and analyze_normalized_expression_internal
        ~resolution
        ~state
        ~expression:({ Node.location; value = expression } as expression_node) =
      let global_model reference =
        (* Fields are handled like methods *)
        let target_candidates = [
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
      in
      match expression with
      | Access { expression; _ }
        when AccessPath.is_property_access ~resolution ~expression:expression_node ->
          let property_call =
            Call { callee = expression; arguments = { Node.location; value = [] } }
            |> Node.create ~location
          in
          analyze_normalized_expression ~resolution ~state ~expression:property_call

      | Access { expression; member } ->
          let access = AccessPath.as_access expression in
          let annotation =
            Node.create (Expression.Access access) ~location
            |> Resolution.resolve resolution
          in
          let attribute_taint =
            let annotations =
              let successors =
                Resolution.class_metadata resolution annotation
                >>| (fun { Resolution.successors; _ } -> successors)
                |> Option.value ~default:[]
                |> List.map ~f:(fun name -> Type.Primitive name)
              in
              let base_annotation =
                (* Our model definitions are ambiguous. Models could either refer to a class
                   variable or an instance variable. We explore both. *)
                if Type.is_meta annotation then
                  [Type.single_parameter annotation]
                else
                  []
              in
              annotation :: successors @ base_annotation
            in
            let attribute_taint sofar annotation =
              Reference.create ~prefix:(Type.class_name annotation) member
              |> global_model
              |> ForwardState.Tree.join sofar
            in
            List.fold annotations ~init:ForwardState.Tree.empty ~f:attribute_taint
          in
          let field = AbstractTreeDomain.Label.Field member in
          analyze_normalized_expression
            ~resolution
            ~state
            ~expression:(Node.create ~location expression)
          |>> ForwardState.Tree.read [field]
          |>> ForwardState.Tree.transform
            ForwardTaint.simple_feature_set
            ~f:(add_first Features.Breadcrumb.FirstField member)
          |>> ForwardState.Tree.join attribute_taint

      | Index { expression; index; _ } ->
          analyze_normalized_expression
            ~resolution
            ~state
            ~expression:(Node.create ~location expression)
          |>> ForwardState.Tree.read [index]
          |>> ForwardState.Tree.transform
            ForwardTaint.simple_feature_set
            ~f:(add_first_index index)

      | Call { callee; arguments; } ->
          analyze_call ~resolution arguments.location ~callee arguments.value state
      | Expression expression ->
          analyze_expression ~resolution ~state ~expression
      | Global access ->
          global_model (Reference.from_access access), state
      | Local identifier ->
          ForwardState.read
            ~root:(AccessPath.Root.Variable identifier)
            ~path:[]
            state.taint,
          state

    and analyze_dictionary_entry ~resolution (taint, state) { Dictionary.key; value; } =
      let field_name =
        match key.Node.value with
        | String literal -> AbstractTreeDomain.Label.Field literal.value
        | _ -> AbstractTreeDomain.Label.Any
      in
      analyze_expression ~resolution ~state ~expression:value
      |>> ForwardState.Tree.prepend [field_name]
      |>> ForwardState.Tree.join taint

    and analyze_list_element ~resolution position (taint, state) expression =
      let index_name = AbstractTreeDomain.Label.Field (string_of_int position) in
      analyze_expression ~resolution ~state ~expression
      |>> ForwardState.Tree.prepend [index_name]
      |>> ForwardState.Tree.join taint

    and analyze_set_element ~resolution (taint, state) expression =
      let value_taint, state =
        analyze_expression ~resolution ~state ~expression
        |>> ForwardState.Tree.prepend [AbstractTreeDomain.Label.Any]
      in
      ForwardState.Tree.join taint value_taint, state

    and analyze_comprehension ~resolution { Comprehension.element; generators; _ } state =
      let add_binding state { Comprehension.target; iterator; _ } =
        let taint, state =
          analyze_expression ~resolution ~state ~expression:iterator
          |>> ForwardState.Tree.read [AbstractTreeDomain.Label.Any]
        in
        let access_path = AccessPath.of_expression target in
        store_taint_option access_path taint state
      in
      let bound_state = List.fold ~f:add_binding generators ~init:state in
      analyze_expression
        ~resolution
        ~state:bound_state
        ~expression:element
      |>> ForwardState.Tree.prepend [AbstractTreeDomain.Label.Any]

    (* Skip through * and **. Used at call sites where * and ** are handled explicitly *)
    and analyze_unstarred_expression ~resolution expression state =
      match expression.Node.value with
      | Starred (Starred.Once expression)
      | Starred (Starred.Twice expression) ->
          analyze_expression ~resolution ~state ~expression
      | _ ->
          analyze_expression ~resolution ~state ~expression

    and analyze_expression ~resolution ~state ~expression:({ Node.location; _ } as expression) =
      match expression.Node.value with
      | Access access ->
          let expression =
            AccessPath.normalize_access access ~resolution
            |> Node.create ~location
          in
          analyze_normalized_expression ~resolution ~state ~expression
      | Await expression ->
          analyze_expression ~resolution ~state ~expression
      | BooleanOperator { left; operator = _; right }
      | ComparisonOperator { left; operator = _; right } ->
          let left_taint, state = analyze_expression ~resolution ~state ~expression:left in
          let right_taint, state = analyze_expression ~resolution ~state ~expression:right in
          ForwardState.Tree.join left_taint right_taint, state
      | Call _ ->
          (* TODO: T37313693 *)
          ForwardState.Tree.empty, state
      | Complex _ ->
          ForwardState.Tree.empty, state
      | Dictionary dictionary ->
          List.fold
            dictionary.entries
            ~f:(analyze_dictionary_entry ~resolution)
            ~init:(ForwardState.Tree.empty, state)
      | DictionaryComprehension _
      | Ellipsis
      | False
      | Float _ ->
          ForwardState.Tree.empty, state
      | Generator comprehension ->
          analyze_comprehension ~resolution comprehension state
      | Integer _ ->
          ForwardState.Tree.empty, state
      | Lambda { parameters = _; body } ->
          (* Ignore parameter bindings and pretend body is inlined *)
          analyze_expression ~resolution ~state ~expression:body
      | List list ->
          List.foldi
            list
            ~f:(analyze_list_element ~resolution)
            ~init:(ForwardState.Tree.empty, state)
      | ListComprehension comprehension ->
          analyze_comprehension ~resolution comprehension state
      | Name _ ->
          (* TODO: T37313693 *)
          ForwardState.Tree.empty, state
      | Set set ->
          List.fold ~f:(analyze_set_element ~resolution) set ~init:(ForwardState.Tree.empty, state)
      | SetComprehension comprehension ->
          analyze_comprehension ~resolution comprehension state
      | Starred (Starred.Once expression)
      | Starred (Starred.Twice expression) ->
          analyze_expression ~resolution ~state ~expression
          |>> ForwardState.Tree.read [AbstractTreeDomain.Label.Any]
      | String { StringLiteral.kind = StringLiteral.Format expressions; _ } ->
          List.fold expressions
            ~f:(fun (taint, state) expression ->
                analyze_expression ~resolution ~state ~expression
                |>> ForwardState.Tree.join taint)
            ~init:(ForwardState.Tree.empty, state)
      | String _ ->
          ForwardState.Tree.empty, state
      | Ternary { target; test; alternative } ->
          let _, state = analyze_expression ~resolution ~state ~expression:test in
          let taint_then, state_then =
            analyze_expression ~resolution ~state ~expression:target
          in
          let taint_else, state_else =
            analyze_expression ~resolution ~state ~expression:alternative
          in
          ForwardState.Tree.join taint_then taint_else, join state_then state_else
      | True ->
          ForwardState.Tree.empty, state
      | Tuple expressions ->
          List.foldi
            ~f:(analyze_list_element ~resolution)
            expressions
            ~init:(ForwardState.Tree.empty, state)
      | UnaryOperator { operator = _; operand } ->
          analyze_expression ~resolution ~state ~expression:operand
      | Yield (Some expression) ->
          analyze_expression ~resolution ~state ~expression
      | Yield None ->
          ForwardState.Tree.empty, state


    let analyze_definition ~define:_ state =
      state


    let rec analyze_assignment
        ~resolution
        ({ Node.location; value } as target)
        taint
        surrounding_taint
        state =
      match value with
      | Starred (Once target | Twice target) ->
          (* This is approximate. Unless we can get the tuple type on the right
             to tell how many total elements there will be, we just pick up the
             entire collection. *)
          analyze_assignment ~resolution target surrounding_taint surrounding_taint state
      | List targets
      | Tuple targets ->
          let analyze_target_element i state target =
            let index = AbstractTreeDomain.Label.Field (string_of_int i) in
            let indexed_taint = ForwardState.Tree.read [index] taint in
            analyze_assignment ~resolution target indexed_taint taint state
          in
          List.foldi targets ~f:analyze_target_element ~init:state
      | _ ->
          (* Check flows to tainted globals/attributes. *)
          let source_tree = taint in
          let sink_tree =
            Model.get_global_model ~resolution ~expression:target
            >>| (fun {
                Model.model = {
                  TaintResult.backward = { TaintResult.Backward.sink_taint; _ };
                  _;
                };
                _;
              } ->
                BackwardState.read
                  ~root:(Root.PositionalParameter { position = 0; name = "$global" })
                  ~path:[]
                  sink_taint
              )
            |> Option.value ~default:BackwardState.Tree.empty
          in
          FunctionContext.check_flow ~location ~source_tree ~sink_tree;

          (* Propagate taint. *)
          let access_path = AccessPath.of_expression target in
          store_taint_option access_path taint state

    let rec analyze_statement ~resolution { Node.value = statement; location } state =
      match statement with
      | Assign { target; value; _ } ->
          let taint, state = analyze_expression ~resolution ~state ~expression:value in
          analyze_assignment ~resolution target taint taint state
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
          let _, state = analyze_expression ~resolution ~state ~expression in
          state
      | For _
      | Global _
      | If _
      | Import _
      | Nonlocal _
      | Pass
      | Raise _ -> state
      | Return { expression = Some expression; _ } ->
          let taint, state = analyze_expression ~resolution ~state ~expression in
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


    let forward ?key state ~statement =
      log
        "State: %a\nAnalyzing statement: %a"
        pp state
        Statement.pp statement;
      let resolution =
        TypeCheck.resolution_with_key
          ~environment:FunctionContext.environment
          ~parent:FunctionContext.definition.value.signature.parent
          ~name:FunctionContext.definition.value.signature.name
          ~key
      in
      analyze_statement ~resolution statement state


    let backward ?key:_ _ ~statement:_ =
      failwith "Don't call me"
  end


  and Analyzer : Fixpoint.Fixpoint with type state = FixpointState.t = Fixpoint.Make(FixpointState)
end


let extract_source_model ~define ~resolution exit_taint =
  let { Define.signature = { return_annotation; _ }; _ } = define in
  let simplify tree =
    let essential = ForwardState.Tree.essential tree in
    ForwardState.Tree.shape tree ~mold:essential
    |> ForwardState.Tree.transform
      ForwardTaint.simple_feature_set
      ~f:(Features.add_type_breadcrumb ~resolution return_annotation)
  in
  let return_taint =
    ForwardState.read ~root:AccessPath.Root.LocalResult ~path:[] exit_taint
    |> simplify
  in
  ForwardState.assign ~root:AccessPath.Root.LocalResult ~path:[] return_taint ForwardState.empty


let run
    ~environment
    ~define
    ~existing_model =
  let { Node.value = { Define.signature = { parameters; return_annotation; _ }; _ }; _ } = define in
  let module Context = struct
    let definition = define
    let environment = environment
    let debug = Define.dump define.value

    let candidates = Location.Reference.Table.create ()

    let add_flow_candidate candidate =
      Location.Reference.Table.set
        candidates
        ~key:candidate.Flow.location
        ~data:candidate

    let check_flow ~location ~source_tree ~sink_tree =
      let flow_candidate =
        Flow.generate_source_sink_matches
          ~location
          ~source_tree
          ~sink_tree
      in
      add_flow_candidate flow_candidate

    let generate_issues () =
      let accumulate ~key:_ ~data:candidate issues =
        let new_issues = Flow.generate_issues ~define candidate in
        List.rev_append new_issues issues
      in
      Location.Reference.Table.fold candidates ~f:accumulate ~init:[]

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
        return_location
        ~callees:[]
        ~port:AccessPath.Root.LocalResult
  end
  in
  let module AnalysisInstance = AnalysisInstance(Context) in
  let open AnalysisInstance in
  log
    "Starting analysis of %a"
    Interprocedural.Callable.pp (Interprocedural.Callable.create define);
  let cfg = Cfg.create ~convert:true define.value in
  let initial =
    let normalized_parameters = AccessPath.Root.normalize_parameters parameters in
    FixpointState.create ~existing_model normalized_parameters
  in
  let () = log "Processing CFG:@.%a" Cfg.pp cfg in
  let exit_state =
    Analyzer.forward ~cfg ~initial
    |> Analyzer.exit
  in
  let resolution = TypeCheck.resolution environment () in
  let extract_model ({ FixpointState.taint; _ } as result) =
    let source_taint = extract_source_model ~define:define.value ~resolution taint in
    let () = log "Model: %a" FixpointState.pp result in
    TaintResult.Forward.{ source_taint; }
  in
  let issues = Context.generate_issues () in
  let () =
    log
      "Issues %a"
      Sexp.pp [%message (issues: Flow.issue list)]
  in
  let model =
    exit_state
    >>| extract_model
    |> Option.value ~default:TaintResult.Forward.empty
  in
  model, issues
