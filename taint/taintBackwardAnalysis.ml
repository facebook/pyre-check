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


type backward_model = {
  taint_in_taint_out: BackwardState.t;
  backward_taint: BackwardState.t;
}
[@@deriving show]


module type FixpointState = sig
  type t = { taint: BackwardState.t }
  [@@deriving show]

  include Fixpoint.State with type t := t

  val create: unit -> t
end


let initial_taint =
  let result_taint = BackwardTaint.add BackwardTaint.empty TaintSinks.LocalReturn in
  BackwardState.assign
    ~root:Root.LocalResult
    ~path:[]
    (BackwardState.make_leaf result_taint)
    BackwardState.empty


module rec FixpointState : FixpointState = struct
  type t = { taint: BackwardState.t }
  [@@deriving show]


  let create () =
    { taint = BackwardState.empty }


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
    | None ->
        BackwardState.empty_tree
    | Some { root; path } ->
        BackwardState.read_access_path ~root ~path taint


  let store_weak_taint ~root ~path taint { taint = state_taint } =
    { taint = BackwardState.assign_weak ~root ~path taint state_taint }


  let store_weak_taint_option access_path taint state =
    match access_path with
    | None -> state
    | Some (root, path) -> store_weak_taint ~root ~path taint state


  let rec analyze_argument taint { Argument.value = argument } state =
    analyze_expression taint argument state

  and analyze_normalized_expression state taint expression =
    match expression with
    | Access { expression; member } ->
        let field = TaintAccessPathTree.Label.Field member in
        let taint =
          BackwardState.assign_tree_path [field] ~tree:BackwardState.empty_tree ~subtree:taint
        in
        analyze_normalized_expression state taint expression
    | Call { callee = Access { expression = receiver; member = method_name}; arguments; } ->
        (* TODO: figure out the BW and TAINT_IN_TAINT_OUT model for whatever is called here. *)
        (* Member access. Don't propagate the taint to the member, skip to the receiver. *)
        let state = List.fold_right ~f:(analyze_argument taint) arguments ~init:state in
        analyze_normalized_expression state taint receiver
    | Call { callee; arguments } ->
        (* TODO: figure out the BW and TAINT_IN_TAINT_OUT model for whatever is called here. *)
        (* For now, we propagate taint to all args implicitly, and to the function. *)
        let state = List.fold_right ~f:(analyze_argument taint) arguments ~init:state in
        analyze_normalized_expression state taint callee
    | Expression expression ->
        analyze_expression taint expression state
    | Identifier name ->
        store_weak_taint ~root:(Root.Variable name) ~path:[] taint state

  and analyze_expression taint { Node.value = expression; _ } state =
    match expression with
    | Access access ->
        normalize_access access
        |> analyze_normalized_expression state taint
    | Await _
    | BooleanOperator _
    | Bytes _
    | ComparisonOperator _
    | Complex _
    | Dictionary _
    | DictionaryComprehension _
    | Ellipses
    | False
    | Float _
    | FormatString _
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
        state


  let analyze_expression_option taint expression state =
    match expression with
    | None -> state
    | Some expression -> analyze_expression taint expression state


  let analyze_definition ~define:_ _ =
    failwith "We don't handle nested defines right now"


  let analyze_statement state statement =
    Log.log ~section:`Taint "Backward state: %s" (Log.Color.yellow (show state));
    match statement with
    | Assign { target; value; _ } ->
        let access_path = of_expression target in
        analyze_expression_option (get_taint access_path state) value state
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
        let access_path = { root = Root.LocalResult; path = [] } in
        let return_taint = get_taint (Some access_path) state in
        analyze_expression return_taint expression state
    | Return { expression = None; _ }
    | Try _
    | With _
    | While _
    | Yield _
    | YieldFrom _ ->
        state


  let backward state ~statement:({ Node.value = statement; _ }) =
    analyze_statement state statement


  let forward ?key:_ state ~statement =
    failwith "Don't call me"
end


and Analyzer : Fixpoint.Fixpoint with type state = FixpointState.t = Fixpoint.Make(FixpointState)


let extract_taint_in_taint_out_model parameters entry_taint =
  let filter_to_local_return taint =
    BackwardTaint.filter ~f:((=) TaintSinks.LocalReturn) taint
  in
  let extract_taint_in_taint_out position model { Node.value = { Parameter.name; _ } } =
    let taint_in_taint_out_taint =
      BackwardState.read (Root.Variable name) entry_taint
      |> BackwardState.filter_map_tree ~f:filter_to_local_return
    in
    if not (BackwardState.is_empty_tree taint_in_taint_out_taint) then
      let parameter = Root.(Parameter { name; position }) in
      BackwardState.assign ~root:parameter ~path:[] taint_in_taint_out_taint model
    else
      model
  in
  List.foldi parameters ~f:extract_taint_in_taint_out ~init:BackwardState.empty


let run ({ Define.name; parameters } as define) =
  (* TODO(T31697954): initial_taint is hardcoded *)
  let initial = { FixpointState.taint = initial_taint } in
  let cfg = Cfg.create define in
  Log.log ~section:`Taint "Processing CFG:@.%s" (Log.Color.yellow (Cfg.show cfg));
  Analyzer.backward ~cfg ~initial
  |> Analyzer.entry
  >>| fun ({ taint; _ } as result) ->
  let taint_in_taint_out = extract_taint_in_taint_out_model parameters taint in
  Log.log ~section:`Taint "Models: %s" (Log.Color.yellow (FixpointState.show result));
  { taint_in_taint_out; backward_taint = BackwardState.empty }
