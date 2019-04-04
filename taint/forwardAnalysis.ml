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

  val create:
    existing_model:TaintResult.call_model
    -> (Root.t * Identifier.t * 'a option) list
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

  val generate_issues: unit -> Flow.issue list
  val return_sink: BackwardState.Tree.t
end


let number_regexp = Str.regexp "[0-9]+"
let is_numeric name =
  Str.string_match number_regexp name 0


module AnalysisInstance(FunctionContext: FUNCTION_CONTEXT) = struct
  module rec FixpointState : FixpointState = struct
    type t = { taint: ForwardState.t }
    [@@deriving show]


    let initial_taint = ForwardState.empty


    let create ~existing_model parameters =
      (* Use primed sources to populate initial state of parameters *)
      let forward_primed_taint = existing_model.TaintResult.forward.source_taint in
      let prime_parameter state (parameter_root, name, _) =
        let prime = ForwardState.read ~root:parameter_root ~path:[] forward_primed_taint in
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


    let add_first kind name set =
      let already_has_first = function
        | SimpleFeatures.Breadcrumb (Breadcrumb.First { kind = has_kind; _ }) ->
            has_kind = kind
        | _ ->
            false
      in
      if List.exists set ~f:already_has_first then
        set
      else
        (SimpleFeatures.Breadcrumb (Breadcrumb.HasFirst kind)) ::
        (SimpleFeatures.Breadcrumb (Breadcrumb.First { kind; name })) :: set


    let add_first_index index =
      match index with
      | AbstractTreeDomain.Label.Field name when is_numeric name ->
          add_first Breadcrumb.FirstIndex "<numeric>"
      | AbstractTreeDomain.Label.Field name ->
          add_first Breadcrumb.FirstIndex name
      | AbstractTreeDomain.Label.Any ->
          add_first Breadcrumb.FirstIndex "<unknown>"


    let rec analyze_argument ~resolution state taint_accumulator { Argument.value = argument; _ } =
      analyze_expression ~resolution ~state ~expression:argument
      |> ForwardState.Tree.join taint_accumulator


    and apply_call_targets ~resolution call_location arguments state call_targets =
      let add_obscure set =
        SimpleFeatures.Breadcrumb Breadcrumb.Obscure :: set
      in
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
              let convert_tito tito {BackwardState.Tree.path; tip=return_taint; _} =
                let breadcrumbs =
                  let gather_breadcrumbs breadcrumbs feature =
                    match feature with
                    | (SimpleFeatures.Breadcrumb _) as breadcrumb ->
                        breadcrumb :: breadcrumbs
                    | _ ->
                        breadcrumbs
                  in
                  BackwardTaint.fold
                    BackwardTaint.simple_feature
                    return_taint
                    ~f:gather_breadcrumbs
                    ~init:[
                      SimpleFeatures.Breadcrumb Breadcrumb.Tito;
                      SimpleFeatures.TitoPosition location;
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
            let sink_tree =
              List.fold
                sink_matches
                ~f:(combine_sink_taint location)
                ~init:BackwardState.Tree.empty
            in
            FunctionContext.check_flow ~location ~source_tree:argument_taint ~sink_tree;
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
          |> ForwardState.Tree.transform ForwardTaint.simple_feature_set ~f:add_obscure
      in
      match call_targets with
      | [] ->
          (* If we don't have a call target: propagate argument taint. *)
          List.fold arguments ~init:ForwardState.Tree.empty ~f:(analyze_argument ~resolution state)
          |> ForwardState.Tree.transform ForwardTaint.simple_feature_set ~f:add_obscure
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
          let receiver =
            AccessPath.as_access expression
            |> fun access -> Node.create (Expression.Access access) ~location
          in
          let arguments =
            let receiver = {
              Argument.name = None;
              value = receiver;
            } in
            receiver :: arguments
          in
          begin
            let add_index_breadcrumb_if_necessary taint =
              if method_name <> "get" then
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
            match Node.value receiver with
            | Access (SimpleAccess receiver) ->
                Interprocedural.CallResolution.get_indirect_targets
                  ~resolution
                  ~receiver
                  ~method_name
                |> apply_call_targets ~resolution location arguments state
                |> add_index_breadcrumb_if_necessary
            | _ ->
                analyze_expression ~resolution ~state ~expression:receiver
          end
      | callee ->
          (* TODO(T31435135): figure out the BW and TITO model for whatever is called here. *)
          let callee_taint =
            analyze_normalized_expression
              ~resolution
              ~state
              ~expression:(Node.create ~location callee)
          in
          (* For now just join all argument and receiver taint and propagate to result. *)
          let taint =
            List.fold_left
              arguments
              ~f:(analyze_argument ~resolution state)
              ~init:callee_taint
          in
          taint


    and analyze_normalized_expression
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
          let inferred_taint =
            let field = AbstractTreeDomain.Label.Field member in
            analyze_normalized_expression
              ~resolution
              ~state
              ~expression:(Node.create ~location expression)
            |> ForwardState.Tree.read [field]
            |> ForwardState.Tree.transform
              ForwardTaint.simple_feature_set
              ~f:(add_first Breadcrumb.FirstField member)
          in
          ForwardState.Tree.join inferred_taint attribute_taint
      | Index { expression; index; _ } ->
          let taint =
            analyze_normalized_expression
              ~resolution
              ~state
              ~expression:(Node.create ~location expression)
            |> ForwardState.Tree.read [index]
          in
          ForwardState.Tree.transform
            ForwardTaint.simple_feature_set
            ~f:(add_first_index index)
            taint

      | Call { callee; arguments; } ->
          analyze_call ~resolution arguments.location ~callee arguments.value state
      | Expression expression ->
          analyze_expression ~resolution ~state ~expression
      | Global access ->
          global_model (Reference.from_access access)
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
      let value_taint = analyze_expression ~resolution ~state ~expression:value in
      ForwardState.Tree.prepend [field_name] value_taint
      |> ForwardState.Tree.join taint

    and analyze_list_element ~resolution state position taint expression =
      let index_name = AbstractTreeDomain.Label.Field (string_of_int position) in
      let value_taint = analyze_expression ~resolution ~state ~expression in
      ForwardState.Tree.prepend [index_name] value_taint
      |> ForwardState.Tree.join taint

    and analyze_set_element ~resolution state taint expression =
      let value_taint =
        analyze_expression ~resolution ~state ~expression
        |> ForwardState.Tree.prepend [AbstractTreeDomain.Label.Any]
      in
      ForwardState.Tree.join taint value_taint

    and analyze_comprehension ~resolution { Comprehension.element; generators; _ } state =
      let add_binding state { Comprehension.target; iterator; _ } =
        let taint =
          analyze_expression ~resolution ~state ~expression:iterator
          |> ForwardState.Tree.read [AbstractTreeDomain.Label.Any]
        in
        let access_path = AccessPath.of_expression target in
        store_taint_option access_path taint state
      in
      let bound_state = List.fold ~f:add_binding generators ~init:state in
      let collection_taint =
        analyze_expression
          ~resolution
          ~state:bound_state
          ~expression:element
      in
      ForwardState.Tree.prepend [AbstractTreeDomain.Label.Any] collection_taint

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
      | Access (SimpleAccess access) ->
          let expression =
            AccessPath.normalize_access access ~resolution
            |> Node.create ~location
          in
          analyze_normalized_expression ~resolution ~state ~expression
      | Await expression ->
          analyze_expression ~resolution ~state ~expression
      | BooleanOperator { left; operator = _; right }
      | ComparisonOperator { left; operator = _; right } ->
          let left_taint = analyze_expression ~resolution ~state ~expression:left in
          let right_taint = analyze_expression ~resolution ~state ~expression:right in
          ForwardState.Tree.join left_taint right_taint
      | Call _ ->
          (* TODO: T37313693 *)
          ForwardState.Tree.empty
      | Complex _ ->
          ForwardState.Tree.empty
      | Dictionary dictionary ->
          List.fold
            dictionary.entries
            ~f:(analyze_dictionary_entry ~resolution state)
            ~init:ForwardState.Tree.empty
      | DictionaryComprehension _
      | Ellipsis
      | False
      | Float _ ->
          ForwardState.Tree.empty
      | Access (ExpressionAccess { expression; _ }) ->
          analyze_expression ~resolution ~state ~expression
      | Generator comprehension ->
          analyze_comprehension ~resolution comprehension state
      | Integer _ ->
          ForwardState.Tree.empty
      | Lambda { parameters = _; body } ->
          (* Ignore parameter bindings and pretend body is inlined *)
          analyze_expression ~resolution ~state ~expression:body
      | List list ->
          List.foldi ~f:(analyze_list_element ~resolution state) list ~init:ForwardState.Tree.empty
      | ListComprehension comprehension ->
          analyze_comprehension ~resolution comprehension state
      | Name _ ->
          (* TODO: T37313693 *)
          ForwardState.Tree.empty
      | Set set ->
          List.fold ~f:(analyze_set_element ~resolution state) set ~init:ForwardState.Tree.empty
      | SetComprehension comprehension ->
          analyze_comprehension ~resolution comprehension state
      | Starred (Starred.Once expression)
      | Starred (Starred.Twice expression) ->
          analyze_expression ~resolution ~state ~expression
          |> ForwardState.Tree.read [AbstractTreeDomain.Label.Any]
      | String { StringLiteral.kind = StringLiteral.Format expressions; _ } ->
          expressions
          |> List.map ~f:(fun expression -> analyze_expression ~resolution ~state ~expression)
          |> List.fold ~f:(ForwardState.Tree.join) ~init:ForwardState.Tree.empty
      | String _ ->
          ForwardState.Tree.empty
      | Ternary { target; test; alternative } ->
          let taint_then = analyze_expression ~resolution ~state ~expression:target in
          let taint_else = analyze_expression ~resolution ~state ~expression:alternative in
          let _ = analyze_expression ~resolution ~state ~expression:test in
          ForwardState.Tree.join taint_then taint_else
      | True ->
          ForwardState.Tree.empty
      | Tuple expressions ->
          List.foldi
            ~f:(analyze_list_element ~resolution state)
            expressions
            ~init:ForwardState.Tree.empty
      | UnaryOperator { operator = _; operand } ->
          analyze_expression ~resolution ~state ~expression:operand
      | Yield (Some expression) ->
          analyze_expression ~resolution ~state ~expression
      | Yield None ->
          ForwardState.Tree.empty


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
          let taint = analyze_expression ~resolution ~state ~expression:value in
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
          let _ = analyze_expression ~resolution ~state ~expression in
          state
      | For _
      | Global _
      | If _
      | Import _
      | Nonlocal _
      | Pass
      | Raise _ -> state
      | Return { expression = Some expression; _ } ->
          let taint = analyze_expression ~resolution ~state ~expression in
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
          let taint = analyze_expression ~resolution ~state ~expression in
          store_taint ~root:AccessPath.Root.LocalResult ~path:[] taint state


    let forward ?key state ~statement =
      Log.log
        ~section:`Taint
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


let run
    ~environment
    ~define:({ Node.value = { Define.signature = { parameters; _ }; _ }; _ } as define)
    ~existing_model =
  let module Context = struct
    let definition = define
    let environment = environment

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
      BackwardState.read
        ~root:AccessPath.Root.LocalResult
        ~path:[]
        existing_model.TaintResult.backward.sink_taint
  end
  in
  let module AnalysisInstance = AnalysisInstance(Context) in
  let open AnalysisInstance in
  Log.log
    ~section:`Taint
    "Starting analysis of %a"
    Interprocedural.Callable.pp (Interprocedural.Callable.create define);
  let cfg = Cfg.create define.value in
  let initial =
    let normalized_parameters = AccessPath.Root.normalize_parameters parameters in
    FixpointState.create ~existing_model normalized_parameters
  in
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
