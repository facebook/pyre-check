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
          ForwardState.empty_tree
      | Some (root, path) ->
          ForwardState.read_access_path ~root ~path state.taint


    let store_taint ~root ~path taint { taint = state_taint } =
      { taint = ForwardState.assign ~root ~path taint state_taint }


    let store_taint_option access_path taint state =
      match access_path with
      | Some { root; path } -> store_taint ~root ~path taint state
      | None -> state


    let rec analyze_argument ~resolution state taint_accumulator { Argument.value = argument; _ } =
      analyze_expression ~resolution argument state
      |> ForwardState.join_trees taint_accumulator


    and apply_call_targets ~resolution location arguments state call_targets =
      let apply_call_target call_target =
        let is_obscure, taint_model =
          match Interprocedural.Fixpoint.get_model call_target with
          | None -> true, None
          | Some model ->
              model.is_obscure, Interprocedural.Result.get_model TaintResult.kind model
        in
        match taint_model with
        | Some { TaintResult.forward; backward; _ } when not is_obscure ->
            let analyze_argument_position position tito { Argument.value = argument; _ } =
              let { Node.location; _ } = argument in
              let argument_taint = analyze_expression ~resolution argument state in
              let read_argument_taint ~path ~path_element:_ ~element:_ tito =
                ForwardState.read_tree path argument_taint
                |> ForwardState.collapse
                |> ForwardTaint.join tito
              in
              let argument_port = AccessPath.Root.Parameter { position } in
              let tito =
                BackwardState.read argument_port backward.taint_in_taint_out
                |> BackwardState.fold_tree_paths ~init:tito ~f:read_argument_taint
              in
              let flow_candidate =
                let sink_tree =
                  BackwardState.read argument_port backward.sink_taint
                  |> BackwardState.apply_call
                    location
                    ~callees:[ call_target ]
                    ~port:argument_port
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
              List.foldi ~f:analyze_argument_position arguments ~init:ForwardTaint.bottom
            in
            let result_taint =
              ForwardState.read AccessPath.Root.LocalResult forward.source_taint
              |> ForwardState.apply_call location ~callees:[ call_target ] ~port:Root.LocalResult
            in
            ForwardState.join_root_element result_taint tito
        | _ ->
            (* Obscure/no model. *)
            List.fold
              arguments
              ~init:ForwardState.empty_tree
              ~f:(analyze_argument ~resolution state)
      in
      match call_targets with
      | [] ->
          (* If we don't have a call target: propagate argument taint. *)
          List.fold arguments ~init:ForwardState.empty_tree ~f:(analyze_argument ~resolution state)
      | call_targets ->
          List.map call_targets ~f:apply_call_target
          |> List.fold ~init:ForwardState.empty_tree ~f:ForwardState.join_trees


    and analyze_call ~resolution location ~callee arguments state =
      match callee with
      | Global access ->
          let access = Access.create_from_identifiers access in
          let call_target = Interprocedural.Callable.create_real access in
          apply_call_targets ~resolution location arguments state [call_target]
      | Access { expression; member = method_name } ->
          let access = as_access expression in
          let arguments =
            let receiver = {
              Argument.name = None;
              value = Access.expression ~location access;
            } in
            receiver :: arguments
          in
          let call_targets =
            let receiver_type =
              let annotation =
                Access.expression access
                |> Resolution.resolve resolution
              in
              if Type.equal annotation Type.Top then
                None
              else
                Some annotation
            in
            match receiver_type with
            | Some (Type.Primitive receiver) ->
                let access = Access.create_from_identifiers [receiver; method_name] in
                let call_target = Interprocedural.Callable.create_real access in
                [call_target]
            | Some (Type.Union annotations) ->
                let filter_receivers = function
                  | Type.Primitive receiver ->
                      Access.create_from_identifiers [receiver; method_name]
                      |> Interprocedural.Callable.create_real
                      |> Option.some
                  | _ ->
                      None
                in
                List.filter_map annotations ~f:filter_receivers
            | _ ->
                (* TODO(T32332602): handle additional call expressions here *)
                []
          in
          apply_call_targets ~resolution location arguments state call_targets
      | callee ->
          (* TODO(T31435135): figure out the BW and TITO model for whatever is called here. *)
          let callee_taint = analyze_normalized_expression ~resolution state callee in
          (* For now just join all argument and receiver taint and propagate to result. *)
          let taint =
            List.fold_left
              arguments
              ~f:(analyze_argument ~resolution state)
              ~init:callee_taint
          in
          taint


    and analyze_normalized_expression ~resolution state expression =
      match expression with
      | Access { expression; member } ->
          let attribute_taint =
            let annotations =
              let annotation =
                as_access expression
                |> Access.expression
                |> Resolution.resolve resolution
              in
              Resolution.less_or_equal
                resolution
                ~left:receiver_type
                ~right:(Type.primitive "django.http.Request")
            in
            let attributes = String.Set.of_list ["GET"; "POST"; "FILES"; "META"] in
            if String.Set.mem attributes (Identifier.show member) &&
               receiver_is_http_request then
              ForwardTaint.singleton Sources.UserControlled
              |> ForwardState.create_leaf
            else
              ForwardState.empty_tree
          in
          let inferred_taint =
            let taint = analyze_normalized_expression ~resolution state expression in
            let field = AccessPathTree.Label.Field member in
            ForwardState.assign_tree_path
              [field]
              ~tree:ForwardState.empty_tree
              ~subtree:taint
          in
          ForwardState.join_trees inferred_taint attribute_taint
      | Index { expression; index; _ } ->
          let taint = analyze_normalized_expression ~resolution state expression in
          ForwardState.read_tree [index] taint
      | Call { callee; arguments; } ->
          analyze_call ~resolution arguments.location ~callee arguments.value state
      | Expression expression ->
          analyze_expression ~resolution expression state
      | Global _ ->
          ForwardState.empty_tree
      | Local identifier ->
          Log.log
            ~section:`Taint
            "Analyzing identifier: %a"
            Identifier.pp identifier;
          ForwardState.read_access_path ~root:(Root.Variable identifier) ~path:[] state.taint

    and analyze_dictionary_entry ~resolution state taint { Dictionary.key; value; } =
      let field_name =
        match key.Node.value with
        | String literal -> AccessPathTree.Label.Field (Identifier.create literal.value)
        | _ -> AccessPathTree.Label.Any
      in
      let value_taint = analyze_expression ~resolution value state in
      ForwardState.join_trees taint (ForwardState.create_tree [field_name] value_taint)

    and analyze_list_element ~resolution state position taint expression =
      let index_name = AccessPathTree.Label.Field (Identifier.create (string_of_int position)) in
      let value_taint = analyze_expression ~resolution expression state in
      ForwardState.join_trees taint (ForwardState.create_tree [index_name] value_taint)

    and analyze_list_comprehension ~resolution { Comprehension.element; generators; _ } state =
      let add_binding state { Comprehension.target; iterator; _ } =
        let taint =
          analyze_expression ~resolution iterator state
          |> ForwardState.read_tree [AccessPathTree.Label.Any]
        in
        let access_path = of_expression target in
        store_taint_option access_path taint state
      in
      let bound_state = List.fold ~f:add_binding generators ~init:state in
      let collection_taint = analyze_expression ~resolution element bound_state in
      ForwardState.create_tree [AccessPathTree.Label.Any] collection_taint

    and analyze_expression ~resolution expression state =
      match expression.Node.value with
      | Access access ->
          normalize_access access
          |> analyze_normalized_expression ~resolution state
      | Await expression ->
          analyze_expression ~resolution expression state
      | BooleanOperator { left; operator = _; right }
      | ComparisonOperator { left; operator = _; right } ->
          let left_taint = analyze_expression ~resolution left state in
          let right_taint = analyze_expression ~resolution right state in
          ForwardState.join_trees left_taint right_taint
      | Complex _ ->
          ForwardState.empty_tree
      | Dictionary dictionary ->
          List.fold
            dictionary.entries
            ~f:(analyze_dictionary_entry ~resolution state)
            ~init:ForwardState.empty_tree
      | DictionaryComprehension _
      | Ellipses
      | False
      | Float _
      | Generator _
      | Integer _
      | Lambda _ ->
          ForwardState.empty_tree
      | List list ->
          List.foldi ~f:(analyze_list_element ~resolution state) list ~init:ForwardState.empty_tree
      | ListComprehension list_comprehension ->
          analyze_list_comprehension ~resolution list_comprehension state
      | Set _
      | SetComprehension _
      | Starred _
      | String _
      | Ternary _
      | True
      | Tuple _
      | UnaryOperator _
      | Yield _ ->
          ForwardState.empty_tree


    let analyze_definition ~define:_ state =
      state


    let forward ?key state ~statement:({ Node.value = statement; _ }) =
      Log.log
        ~section:`Taint
        "State: %a\nAnalyzing statement: %a"
        pp state
        Statement.pp_statement statement;
      let resolution =
        let annotations =
          match key, TypeResolutionSharedMemory.get FunctionContext.definition.value.name with
          | Some key, Some define_mapping ->
              define_mapping
              |> Int.Map.of_tree
              |> (fun mapping -> Int.Map.find mapping key)
              >>| (fun { TypeResolutionSharedMemory.precondition; _ } -> precondition)
              >>| Access.Map.of_tree
              |> Option.value ~default:Access.Map.empty
          | _ ->
              Access.Map.empty
        in
        Environment.resolution FunctionContext.environment ~annotations ()
      in
      match statement with
      | Assign { target; value; _ } ->
          let taint = analyze_expression ~resolution value state in
          let access_path = of_expression target in
          store_taint_option access_path taint state
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
          store_taint ~root:Root.LocalResult ~path:[] taint state
      | Return { expression = None; _ }
      | Try _
      | With _
      | While _
      | Yield _
      | YieldFrom _ ->
          state


    let backward ?key:_ _ ~statement:_ =
      failwith "Don't call me"
  end


  and Analyzer : Fixpoint.Fixpoint with type state = FixpointState.t = Fixpoint.Make(FixpointState)
end


let extract_source_model _parameters exit_taint =
  let return_taint = ForwardState.read Root.LocalResult exit_taint in
  ForwardState.assign ~root:Root.LocalResult ~path:[] return_taint ForwardState.empty


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
      "Issues %s"
      (Sexp.to_string [%message (issues: Flow.issue list)])
  in
  let model =
    exit_state
    >>| extract_model
    |> Option.value ~default:TaintResult.Forward.empty
  in
  model, issues
