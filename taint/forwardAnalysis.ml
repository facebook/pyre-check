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

  val add_flow_candidate: Flow.candidate -> unit
  val generate_errors: unit -> Interprocedural.Error.t list
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


    let rec analyze_argument state taint_accumulator { Argument.value = argument; _ } =
      analyze_expression argument state
      |> ForwardState.join_trees taint_accumulator


    and apply_call_targets location arguments state call_targets =
      let apply_call_target call_target =
        let existing_model =
          Interprocedural.Fixpoint.get_model call_target
          >>= Interprocedural.Result.get_model TaintResult.kind
        in
        match existing_model with
        | Some ({ forward; backward; _ } as model) ->
            Log.log
              ~section:`Taint
              "Model for %a:\n%a\n"
              Interprocedural.Callable.pp call_target
              TaintResult.pp_call_model model;
            let analyze_argument_position position tito { Argument.value = argument; _ } =
              let { Node.location; _ } = argument in
              let argument_taint = analyze_expression argument state in
              let read_argument_taint ~path ~path_element:_ ~element:_ tito =
                ForwardState.read_tree path argument_taint
                |> ForwardState.collapse
                |> ForwardTaint.join tito
              in
              let tito =
                BackwardState.read
                  (AccessPath.Root.Parameter { position })
                  backward.taint_in_taint_out
                |> BackwardState.fold_tree_paths ~init:tito ~f:read_argument_taint
              in
              let flow_candidate =
                let sink_tree =
                  BackwardState.read
                    (AccessPath.Root.Parameter { position })
                    backward.sink_taint
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
              |> ForwardState.apply_call location [ call_target ]
            in
            ForwardState.join_root_element result_taint tito
        | None ->
            Log.log
              ~section:`Taint
              "No model for %a"
              Interprocedural.Callable.pp call_target;
            (* If we don't have a model: assume function propagates argument
               taint (join all argument taint) *)
            List.fold arguments ~init:ForwardState.empty_tree ~f:(analyze_argument state)
      in
      match call_targets with
      | [] ->
          (* If we don't have a call target: propagate argument taint. *)
          List.fold arguments ~init:ForwardState.empty_tree ~f:(analyze_argument state)
      | call_targets ->
          List.map call_targets ~f:apply_call_target
          |> List.fold ~init:ForwardState.empty_tree ~f:ForwardState.join_trees


    and analyze_call ?key location ~callee arguments state =
      match callee with
      | Global access ->
          let access = Access.create_from_identifiers access in
          let call_target = Interprocedural.Callable.create_real access in
          apply_call_targets location arguments state [call_target]
      | Access { expression; member = method_name } ->
          let access = as_access expression in
          let receiver_type =
            key
            >>= fun key -> TypeResolutionSharedMemory.get FunctionContext.definition.value.name
            >>| Int.Map.Tree.fold ~init:Int.Map.empty ~f:(fun ~key ~data -> Int.Map.set ~key ~data)
            >>= Fn.flip Int.Map.find key
            >>| Access.Map.of_tree
            >>= Fn.flip Access.Map.find access
          in
          let call_targets =
            match receiver_type with
            | Some { annotation = Primitive receiver ; _ } ->
                let access = Access.create_from_identifiers [receiver; method_name] in
                let call_target = Interprocedural.Callable.create_real access in
                [call_target]
            | Some { annotation = Union annotations; _ } ->
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
          let taint = apply_call_targets location arguments state call_targets in
          taint
      | callee ->
          (* TODO(T31435135): figure out the BW and TITO model for whatever is called here. *)
          let callee_taint = analyze_normalized_expression state callee in
          (* For now just join all argument and receiver taint and propagate to result. *)
          let taint = List.fold_left ~f:(analyze_argument state) arguments ~init:callee_taint in
          taint


    and analyze_normalized_expression ?key state expression =
      match expression with
      | Access { expression; member; } ->
          let taint = analyze_normalized_expression state expression in
          let field = AccessPathTree.Label.Field member in
          let taint =
            ForwardState.assign_tree_path
              [field]
              ~tree:ForwardState.empty_tree
              ~subtree:taint
          in
          taint
      | Call { callee; arguments; } ->
          analyze_call ?key arguments.location ~callee arguments.value state
      | Expression expression ->
          analyze_expression expression state
      | Global _ ->
          ForwardState.empty_tree
      | Local identifier ->
          Log.log
            ~section:`Taint
            "Analyzing identifier: %a"
            Identifier.pp identifier;
          ForwardState.read_access_path ~root:(Root.Variable identifier) ~path:[] state.taint


    and analyze_expression ?key expression state =
      match expression.Node.value with
      | Access access ->
          normalize_access access
          |> analyze_normalized_expression ?key state
      | Await _
      | BooleanOperator _
      | ComparisonOperator _
      | Complex _
      | Dictionary _
      | DictionaryComprehension _
      | Ellipses
      | False
      | Float _
      | Generator _
      | Integer _
      | Lambda _
      | List _
      | ListComprehension _
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
      match statement with
      | Assign { target; value; _ } ->
          let taint = analyze_expression ?key value state in
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
          let _ = analyze_expression ?key expression state in
          state
      | For _
      | Global _
      | If _
      | Import _
      | Nonlocal _
      | Pass
      | Raise _ -> state
      | Return { expression = Some expression; _ } ->
          let taint = analyze_expression expression state in
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


let run ({ Node.value = { Define.parameters; _ }; _ } as define) =
  let module Context = struct
    let definition = define

    let candidates = Location.Reference.Table.create ()

    let add_flow_candidate candidate =
      Location.Reference.Table.set
        candidates
        ~key:candidate.Flow.location
        ~data:candidate

    let generate_errors () =
      let accumulate ~key:_ ~data:candidate errors =
        let new_errors = Flow.generate_errors ~define:definition candidate in
        List.rev_append new_errors errors
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
  let errors = Context.generate_errors () in
  let () =
    Log.log
      ~section:`Taint
      "Errors %s"
      (Sexp.to_string [%message (errors: Interprocedural.Error.t list)])
  in
  let model =
    exit_state
    >>| extract_model
    |> Option.value ~default:TaintResult.Forward.empty
  in
  model, errors
