(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis
open Ast
open Expression
open Pyre
open Statement
open TaintDomains
open TaintAccessPath


module type FixpointState = sig
  type t = { taint: ForwardState.t }
  [@@deriving show]

  include Fixpoint.State with type t := t

  val create: unit -> t
end


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

  and analyze_call ~callee arguments state =
    match callee with
    | Identifier identifier ->
        let existing_model =
          let call_target = `RealTarget (Access.create_from_identifiers [identifier]) in
          Interprocedural.Fixpoint.get_model call_target
          >>= Interprocedural.Result.get_model TaintResult.kind
        in
        let taint =
          match existing_model with
          | Some { forward; _ } ->
              (* TODO(T31440488): analyze callee arguments *)
              ForwardState.read TaintAccessPath.Root.LocalResult forward.source_taint
          | None ->
              (* TODO(T31435739): if we don't have a model: assume function propagates argument
                 taint (join all argument taint) *)
              List.fold arguments ~init:ForwardState.empty_tree ~f:(analyze_argument state)
        in
        taint
    | Access { expression = receiver; member = method_name } ->
        (* TODO(T31435135): figure out the FW and TITO model for whatever is called here. *)
        (* Member access. Don't propagate the taint to the member, skip to the receiver. *)
        let receiver_taint = analyze_normalized_expression state receiver in
        (* For now just join all argument and receiver taint and propagate to result. *)
        let taint = List.fold_left ~f:(analyze_argument state) arguments ~init:receiver_taint in
        taint
    | callee ->
        (* TODO(T31435135): figure out the BW and TITO model for whatever is called here. *)
        let callee_taint = analyze_normalized_expression state callee in
        (* For now just join all argument and receiver taint and propagate to result. *)
        let taint = List.fold_left ~f:(analyze_argument state) arguments ~init:callee_taint in
        taint

  and analyze_normalized_expression state expression =
    match expression with
    | Access { expression; member; } ->
        let taint = analyze_normalized_expression state expression in
        let field = TaintAccessPathTree.Label.Field member in
        let taint =
          ForwardState.assign_tree_path
            [field]
            ~tree:ForwardState.empty_tree
            ~subtree:taint
        in
        taint
    | Call { callee; arguments; } ->
        analyze_call ~callee arguments state
    | Expression expression ->
        analyze_expression expression state
    | Identifier identifier ->
        ForwardState.read_access_path ~root:(Root.Variable identifier) ~path:[] state.taint

  and analyze_expression expression state =
    match expression.Node.value with
    | Access access ->
        normalize_access access
        |> analyze_normalized_expression state
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


  let analyze_expression_option expression state =
    match expression with
    | None -> ForwardState.empty_tree
    | Some expression -> analyze_expression expression state


  let analyze_definition ~define:_ _ =
    failwith "We don't handle nested defines right now"


  let forward ?key:_ state ~statement:({ Node.value = statement; _ }) =
    Log.log ~section:`Taint "Forward state: %s" (Log.Color.cyan (show state));
    match statement with
    | Assign { target; annotation; value; parent } ->
        let taint = analyze_expression_option value state in
        let access_path = of_expression target in
        store_taint_option access_path taint state
    | Assert _
    | Break
    | Class _
    | Continue ->
        state
    | Define define ->
        analyze_definition ~define state
    | Delete _
    | Expression _
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


  let backward state ~statement =
    failwith "Don't call me"
end


and Analyzer : Fixpoint.Fixpoint with type state = FixpointState.t = Fixpoint.Make(FixpointState)


let extract_source_model _parameters exit_taint =
  let return_taint = ForwardState.read Root.LocalResult exit_taint in
  ForwardState.assign ~root:Root.LocalResult ~path:[] return_taint ForwardState.empty


let run ({ Define.name; parameters; _ } as define) =
  let cfg = Cfg.create define in
  let initial = FixpointState.create () in
  let () = Log.log ~section:`Taint "Processing CFG:@.%s" (Log.Color.cyan (Cfg.show cfg)) in
  let exit_state =
    Analyzer.forward ~cfg ~initial
    |> Analyzer.exit
  in
  let extract_model ({ FixpointState.taint; _ } as result) =
    let source_taint = extract_source_model parameters taint in
    let () = Log.log ~section:`Taint "Model: %s" (Log.Color.cyan (FixpointState.show result)) in
    TaintResult.Forward.{ source_taint; }
  in
  exit_state
  >>| extract_model
  |> Option.value ~default:TaintResult.Forward.empty
