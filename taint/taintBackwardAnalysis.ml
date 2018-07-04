(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis
open Ast
open Statement
open TaintDomains
open TaintAccessPath

type model = {
  define_name: Access.t;
  taint_in_taint_out: BackwardState.t;
}
[@@deriving show]


module type FixpointState = sig
  type t = {
    taint: BackwardState.t;
    models: model list;
  }
  [@@deriving show]

  include Fixpoint.State with type t := t

  val create: unit -> t

  val show_models: t option -> string
end


module rec FixpointState : FixpointState = struct
  type t = {
    taint: BackwardState.t;
    models: model list;
  }
  [@@deriving show]

  let initial_taint =
    let result_taint = BackwardTaint.add BackwardTaint.empty TaintSinks.LocalReturn in
    BackwardState.assign
      ~root:Root.LocalResult
      ~path:[]
      (BackwardState.make_leaf result_taint)
      BackwardState.empty

  let create () = {
    taint = BackwardState.empty;
    models = [];
  }

  let less_or_equal ~left:{ taint = left; _ } ~right:{ taint = right; _ } =
    BackwardState.less_or_equal ~left ~right

  let join { taint = left; models } { taint = right; _ } =
    let taint = BackwardState.join left right in
    {
      taint;
      models;  (* There should be no joining at class/file level. *)
    }

  let widen ~previous:{ taint = previous; _ } ~next:{ taint = next; models; } ~iteration =
    let taint = BackwardState.widen ~iteration ~previous ~next in
    {
      taint;
      models;  (* There should be no joining at class/file level. *)
    }

  let extract_taint_in_taint_out_model parameters entry_taint =
    let filter_to_local_return taint =
      BackwardTaint.filter ~f:((=) TaintSinks.LocalReturn) taint
    in
    let extract_taint_in_taint_out position model { Node.value = parameter } =
      let taint_in_taint_out_taint =
        BackwardState.read (Root.Variable parameter.Parameter.name) entry_taint
        |> BackwardState.filter_map_tree ~f:filter_to_local_return
      in
      if not (BackwardState.is_empty_tree taint_in_taint_out_taint) then
        let parameter = Root.(Parameter { name = parameter.Parameter.name; position; }) in
        BackwardState.assign ~root:parameter ~path:[] taint_in_taint_out_taint model
      else
        model
    in
    List.foldi parameters ~f:extract_taint_in_taint_out ~init:BackwardState.empty

  let get_taint ap state =
    match ap with
    | None -> BackwardState.empty_tree
    | Some { root; path; } ->
        BackwardState.read_ap ~root ~path state.taint

  let store_weak_taint ~root ~path taint state =
    { state with taint = BackwardState.assign_weak ~root ~path taint state.taint; }

  let store_weak_taint_opt ap taint state =
    match ap with
    | None -> state
    | Some (root, path) -> store_weak_taint ~root ~path taint state

  let rec analyze_argument taint arg state =
    analyze_expression taint arg.Argument.value state

  and analyze_normalized_expression state taint e =
    match e with
    | Access { expression; member } ->
        let field = TaintAccessPathTree.Label.Field member in
        let taint = BackwardState.assign_tree_path [field] ~t:BackwardState.empty_tree ~st:taint in
        analyze_normalized_expression state taint e
    | Call { callee = Access { expression = receiver; member = method_name}; arguments; } ->
        (* TODO: figure out the BW and TAINT_IN_TAINT_OUT model for whatever is called here. *)
        (* Member access. Don't propagate the taint to the member, skip to the receiver. *)
        let state = List.fold_right ~f:(analyze_argument taint) arguments ~init:state in
        analyze_normalized_expression state taint receiver
    | Call { callee; arguments; } ->
        (* TODO: figure out the BW and TAINT_IN_TAINT_OUT model for whatever is called here. *)
        (* For now, we propagate taint to all args implicitly, and to the function. *)
        let state = List.fold_right ~f:(analyze_argument taint) arguments ~init:state in
        analyze_normalized_expression state taint callee
    | Expression expression ->
        analyze_expression taint expression state
    | Identifier name ->
        store_weak_taint ~root:(Root.Variable name) ~path:[] taint state

  and analyze_expression taint (expr : Expression.t) state =
    match expr.Node.value with
    | Expression.Access a ->
        normalize_access a
        |> analyze_normalized_expression state taint
    | Expression.Await _
    | Expression.BooleanOperator _
    | Expression.Bytes _
    | Expression.ComparisonOperator _
    | Expression.Complex _
    | Expression.Dictionary _
    | Expression.DictionaryComprehension _
    | Expression.False
    | Expression.Float _
    | Expression.FormatString _
    | Expression.Generator _
    | Expression.Integer _
    | Expression.Lambda _
    | Expression.List _
    | Expression.ListComprehension _
    | Expression.Set _
    | Expression.SetComprehension _
    | Expression.Starred _
    | Expression.String _
    | Expression.Ternary _
    | Expression.True
    | Expression.Tuple _
    | Expression.UnaryOperator _
    | Expression.Yield _ -> state

  let analyze_expression_opt taint e state =
    match e with
    | None -> state
    | Some e -> analyze_expression taint e state

  let analyze_definition define state =
    let cfg = Cfg.create define in
    let initial = { taint = initial_taint; models = []; } in
    let result = Analyzer.backward ~cfg ~initial |> Analyzer.entry in
    match result with
    | None ->
        (* Log.dump "definition %s did not produce result for entry node." (Cfg.show cfg); *)
        state
    | Some result ->
        let taint_in_taint_out_model =
          extract_taint_in_taint_out_model define.Record.Define.parameters result.taint in
        let model = { define_name=define.name; taint_in_taint_out=taint_in_taint_out_model; } in
        { state with models = model :: List.rev_append result.models state.models }

  let analyze_statement state statement =
    match statement with
    | Assign { target; annotation; value; parent } ->
        let access_path = of_expression target in
        analyze_expression_opt (get_taint access_path state) value state
    | Assert _
    | Break
    | Class _
    | Continue ->
        state
    | Define define ->
        analyze_definition define state
    | Delete _
    | Expression _
    | For _
    | Global _
    | If _
    | Import _
    | Nonlocal _
    | Pass
    | Raise _ -> state
    | Return { expression=Some expression; _ } ->
        let ap = { root = Root.LocalResult; path = []; } in
        let return_taint = get_taint (Some ap) state in
        analyze_expression return_taint expression state
    | Return { expression=None; _ }
    | Stub _
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

  let show_models = function
    | None ->
        "no result."
    | Some result ->
        String.concat ~sep:"\n" (List.map ~f:show_model result.FixpointState.models)

end


and Analyzer : Fixpoint.Fixpoint with type state = FixpointState.t = Fixpoint.Make(FixpointState)


let run cfg =
  let initial = FixpointState.create () in
  (* Log.dump "CFG: %s" (Cfg.show cfg); *)
  let result = Analyzer.backward ~cfg ~initial |> Analyzer.entry in
  (* let () = Log.dump "Result: %s" (FixpointState.show_models result) in *)
  result
