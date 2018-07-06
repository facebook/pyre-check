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
  source_taint: ForwardState.t;
}
[@@deriving show]


module type FixpointState = sig
  type t = {
    taint: ForwardState.t;
    models: model list;
  }
  [@@deriving show]

  include Fixpoint.State with type t := t

  val create: unit -> t

  val show_models: t option -> string
end


module rec FixpointState : FixpointState = struct
  type t = {
    taint: ForwardState.t;
    models: model list;
  }
  [@@deriving show]

  let initial_taint = ForwardState.empty

  let create () = {
    taint = ForwardState.empty;
    models = [];
  }

  let less_or_equal ~left:{ taint = left } ~right:{ taint = right } =
    ForwardState.less_or_equal ~left ~right

  let join { taint = left; models; } { taint = right; _ } =
    let taint = ForwardState.join left right in
    {
      taint;
      models;  (* There should be no joining at class/file level. *)
    }

  let widen ~previous:{ taint = previous; _ } ~next:{ taint = next; models; } ~iteration =
    let taint = ForwardState.widen ~iteration ~previous ~next in
    {
      taint;
      models;  (* There should be no joining at class/file level. *)
    }

  let extract_source_model _parameters exit_taint =
    let return_taint = ForwardState.read Root.LocalResult exit_taint in
    ForwardState.assign ~root:Root.LocalResult ~path:[] return_taint ForwardState.empty

  let get_taint_opt ap state =
    match ap with
    | None -> ForwardState.empty_tree
    | Some (root, path) ->
        ForwardState.read_ap ~root ~path state.taint

  let store_taint ~root ~path taint state =
    { state with taint = ForwardState.assign ~root ~path taint state.taint; }

  let store_taint_opt ap taint state =
    match ap with
    | Some { root; path} -> store_taint ~root ~path taint state
    | None -> state

  let rec analyze_argument state taint_accumulator arg =
    analyze_expression arg.Argument.value state
    |> ForwardState.join_trees taint_accumulator

  and analyze_normalized_expression state expression =
    match expression with
    | Access { expression; member; } ->
        let taint = analyze_normalized_expression state expression in
        let field = TaintAccessPathTree.Label.Field member in
        let taint = ForwardState.assign_tree_path [field] ~t:ForwardState.empty_tree ~st:taint in
        taint
    | Call { callee = Access { expression = receiver; member = method_name; }; arguments; } ->
        (* TODO: figure out the FW and TITO model for whatever is called here. *)
        (* Member access. Don't propagate the taint to the member, skip to the receiver. *)
        let receiver_taint = analyze_normalized_expression state receiver in
        (* For now just join all argument and receiver taint and propagate to result. *)
        let taint = List.fold_left ~f:(analyze_argument state) arguments ~init:receiver_taint in
        taint
    | Call { callee = Identifier name; arguments; } when Identifier.show name = "__testSource"->
        (* Builtin source for testing. *)
        ForwardTaint.test_source |> ForwardState.make_leaf
    | Call { callee; arguments } ->
        (* TODO: figure out the BW and TITO model for whatever is called here. *)
        let callee_taint = analyze_normalized_expression state callee in
        (* For now just join all argument and receiver taint and propagate to result. *)
        let taint = List.fold_left ~f:(analyze_argument state) arguments ~init:callee_taint in
        taint
    | Expression expression ->
        analyze_expression expression state
    | Identifier identifier ->
        ForwardState.read_ap ~root:(Root.Variable identifier) ~path:[] state.taint

  and analyze_expression expression state =
    match expression.Node.value with
    | Expression.Access a ->
        normalize_access a
        |> analyze_normalized_expression state
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
    | Expression.Yield _ ->
        ForwardState.empty_tree


  let analyze_expression_opt expression state =
    match expression with
    | None -> ForwardState.empty_tree
    | Some expression -> analyze_expression expression state


  let analyze_definition ({ Define.name; _  } as define) state =
    let cfg = Cfg.create define in
    let initial = { taint = initial_taint; models = []; } in
    let result = Analyzer.forward ~cfg ~initial |> Analyzer.exit in
    match result with
    | None ->
        Log.log
          ~section:`Taint
          "Definition %s did not produce result for entry node."
          (Log.Color.cyan (Access.show name));
        state
    | Some result ->
        let source_model = extract_source_model define.Record.Define.parameters result.taint in
        let model = { define_name = define.name; source_taint = source_model; } in
        { state with models = model :: List.rev_append result.models state.models }


  let forward ?key:_ state ~statement:({ Node.value = statement; _ }) =
    Log.log ~section:`Taint "Forward state: %s" (Log.Color.cyan (show state));
    match statement with
    | Assign { target; annotation; value; parent } ->
        let taint = analyze_expression_opt value state in
        let access_path = of_expression target in
        store_taint_opt access_path taint state
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
    | Return { expression = Some expression; _ } ->
        let taint = analyze_expression expression state in
        store_taint ~root:Root.LocalResult ~path:[] taint state
    | Return { expression = None; _ }
    | Stub _
    | Try _
    | With _
    | While _
    | Yield _
    | YieldFrom _ ->
        state

  let backward state ~statement =
    failwith "Don't call me"

  let show_models = function
    | None -> "no result."
    | Some result ->
        String.concat ~sep:"\n" (List.map ~f:show_model result.FixpointState.models)

end


and Analyzer : Fixpoint.Fixpoint with type state = FixpointState.t = Fixpoint.Make(FixpointState)


let run cfg =
  let initial = FixpointState.create () in
  Log.log ~section:`Taint "Processing CFG:@.%s" (Log.Color.cyan (Cfg.show cfg));
  let result = Analyzer.forward ~cfg ~initial |> Analyzer.exit in
  Log.log ~section:`Taint "Models: %s" (Log.Color.cyan (FixpointState.show_models result));
  result
