(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis
open Ast
open Expression
open Statement
open TaintDomains
open TaintAccessPath

module Model = struct
  type t = {
    define_name: Access.t;
    source_taint: ForwardState.t;
  }
  [@@deriving show]

  let taint_annotation = "TaintSource"

  let create defines resolution =

    let introduce_taint taint_source_kind =
      ForwardState.assign
        ~root:TaintAccessPath.Root.LocalResult
        ~path:[]
        (ForwardTaint.singleton taint_source_kind
         |> ForwardState.make_leaf)
        ForwardState.empty
    in

    Annotated.Callable.create defines ~resolution
    |> (fun callable -> Type.Callable callable)
    |> function
    | Type.Callable { kind = Named define_name ; overloads; implicit } ->
        begin match overloads with
          | {
            annotation = Type.Parametric { name; parameters = (Primitive primitive) :: _ }
          } :: _ when (Identifier.show name = taint_annotation) ->
              let taint_source_kind = TaintSources.create (Identifier.show primitive) in
              { define_name; source_taint = introduce_taint taint_source_kind }
          | _ ->
              failwith "Cannot create taint model: no annotation"
        end
    | _ ->
        failwith "Cannot create taint model: not a callable"
end


module type FixpointState = sig
  type t = {
    taint: ForwardState.t;
    models: Model.t list;
  }
  [@@deriving show]

  include Fixpoint.State with type t := t

  val create: ?models: Model.t list -> unit -> t

  val show_models: t option -> string
end


module rec FixpointState : FixpointState = struct
  type t = {
    taint: ForwardState.t;
    models: Model.t list;
  }
  [@@deriving show]


  let initial_taint = ForwardState.empty


  let create ?(models = []) () = {
    taint = ForwardState.empty;
    models;
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


  let get_taint_option access_path state =
    match access_path with
    | None ->
        ForwardState.empty_tree
    | Some (root, path) ->
        ForwardState.read_access_path ~root ~path state.taint


  let store_taint ~root ~path taint ({ taint = state_taint } as state) =
    { state with taint = ForwardState.assign ~root ~path taint state_taint }


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
          (* TODO(T31207999): look up models in shared memory *)
          let lookup { Model.define_name; _ } =
            Access.create_from_identifiers [identifier] = define_name
          in
          List.find state.models ~f:lookup
        in
        let taint =
          match existing_model with
          | Some { source_taint; _ } ->
              (* TODO(T31440488): analyze callee arguments *)
              ForwardState.read TaintAccessPath.Root.LocalResult source_taint
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
    | Bytes _
    | ComparisonOperator _
    | Complex _
    | Dictionary _
    | DictionaryComprehension _
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
        ForwardState.empty_tree


  let analyze_expression_option expression state =
    match expression with
    | None -> ForwardState.empty_tree
    | Some expression -> analyze_expression expression state


  let analyze_definition ~define:({ Define.name; parameters  } as define) state =
    let cfg = Cfg.create define in
    let result =
      Analyzer.forward ~cfg ~initial:state
      |> Analyzer.exit
    in
    match result with
    | None ->
        Log.log
          ~section:`Taint
          "Definition %s did not produce result for entry node."
          (Log.Color.cyan (Access.show name));
        state
    | Some { taint; models } ->
        let source_model = extract_source_model parameters taint in
        let model = { Model.define_name = name; source_taint = source_model } in
        { state with models = model :: List.rev_append models state.models }


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
        String.concat ~sep:"\n" (List.map ~f:Model.show result.FixpointState.models)
end


and Analyzer : Fixpoint.Fixpoint with type state = FixpointState.t = Fixpoint.Make(FixpointState)


let run ?models cfg =
  let initial = FixpointState.create ?models () in
  Log.log ~section:`Taint "Processing CFG:@.%s" (Log.Color.cyan (Cfg.show cfg));
  let result = Analyzer.forward ~cfg ~initial |> Analyzer.exit in
  Log.log ~section:`Taint "Models: %s" (Log.Color.cyan (FixpointState.show_models result));
  result
