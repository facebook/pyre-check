(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Statement
open Pyre

module Error = AnalysisError


module type Context = sig
  val define: Define.t Node.t
  val environment: (module Environment.Handler)
end


module State (Context: Context) = struct
  type state =
    | Unawaited of Location.t
    | Awaited
  [@@deriving show]
  let _ = show_state  (* unused *)

  type t = {
    unawaited: state Reference.Map.t;
  }


  let show { unawaited; _ } =
    Map.to_alist unawaited
    |> List.map
      ~f:(fun (reference, state) ->
          Format.asprintf "%a -> %a" Reference.pp reference pp_state state)
    |> String.concat ~sep:", "


  let pp format state =
    Format.fprintf format "%s" (show state)


  let initial =
    { unawaited = Reference.Map.empty }


  let errors { unawaited; _ } =
    let error (reference, state) =
      match state with
      | Unawaited location ->
          [
            Error.create
              ~location
              ~kind:(Error.UnawaitedAwaitable (Reference.delocalize reference))
              ~define:Context.define;
          ]
      | _ ->
          []
    in
    Map.to_alist unawaited
    |> List.concat_map ~f:error


  let less_or_equal ~left:{ unawaited = left; _ } ~right:{ unawaited = right; _ } =
    let less_or_equal (access, state) =
      match state, Map.find right access with
      | Unawaited _, Some _ -> true
      | Awaited, Some Awaited -> true
      | _ -> false
    in
    Map.to_alist left
    |> List.for_all ~f:less_or_equal


  let join left right =
    let merge ~key:_ = function
      | `Left state
      | `Right state -> Some state
      | `Both (_, Awaited) -> Some Awaited
      | `Both (Awaited, _) -> Some Awaited
      | `Both ((Unawaited _) as unawaited, Unawaited _) ->
          (* It does not matter which one we pick as long as we are consistent. *)
          Some unawaited
    in
    { left with unawaited = Map.merge left.unawaited right.unawaited ~f:merge }


  let widen ~previous ~next ~iteration:_ =
    join previous next


  let rec forward_generator
      { unawaited }
      { Expression.Comprehension.target = _; iterator; conditions; async = _ } =
    let { unawaited } =
      List.fold
        conditions
        ~f:(fun state expression -> { unawaited = forward_expression ~state ~expression })
        ~init:{ unawaited }
    in
    { unawaited = forward_expression ~state:{ unawaited } ~expression:iterator }


  and forward_expression
      ~state:{ unawaited }
      ~expression:{ Node.value; _ } =
    let open Expression in
    match value with
    | Await { Node.value = Name name; _ } when Expression.is_simple_name name ->
        Map.set unawaited ~key:(Reference.from_name_exn name) ~data:Awaited
    | Await _ ->
        unawaited
    | BooleanOperator { BooleanOperator.left; right; _ } ->
        let unawaited = forward_expression ~state:{ unawaited } ~expression:left in
        forward_expression ~state:{ unawaited } ~expression:right
    | Call { Call.callee; arguments } ->
        let unawaited = forward_expression ~state: { unawaited } ~expression:callee in
        let forward_argument unawaited { Call.Argument.value; _ } =
          forward_expression ~state:{ unawaited } ~expression:value
        in
        List.fold arguments ~init:unawaited ~f:forward_argument
    | ComparisonOperator { ComparisonOperator.left; right; _ } ->
        let unawaited = forward_expression ~state:{ unawaited } ~expression:left in
        forward_expression ~state:{ unawaited } ~expression:right
    | Dictionary { Dictionary.entries; keywords } ->
        let forward_entry unawaited { Dictionary.key; value } =
          let unawaited = forward_expression ~state:{ unawaited } ~expression:key in
          forward_expression ~state:{ unawaited } ~expression:value
        in
        let unawaited = List.fold entries ~init:unawaited ~f:forward_entry in
        List.fold
          keywords
          ~init:unawaited
          ~f:(fun unawaited expression -> forward_expression ~state:{ unawaited } ~expression)
    | Lambda { Lambda.body; _ } ->
        forward_expression ~state:{ unawaited } ~expression:body
    | Starred (Starred.Once expression)
    | Starred (Starred.Twice expression) ->
        forward_expression ~state:{ unawaited } ~expression
    | Ternary { Ternary.target; test; alternative } ->
        let unawaited = forward_expression ~state:{ unawaited } ~expression:target in
        let unawaited = forward_expression ~state:{ unawaited } ~expression:test in
        forward_expression ~state:{ unawaited } ~expression:alternative
    | List items
    | Set items
    | Tuple items ->
        List.fold
          items
          ~init:unawaited
          ~f:(fun unawaited expression -> forward_expression ~state:{ unawaited } ~expression)
    | UnaryOperator { UnaryOperator.operand; _ } ->
        forward_expression ~state:{ unawaited } ~expression:operand
    | Yield (Some expression) ->
        forward_expression ~state:{ unawaited } ~expression
    | Yield None ->
        unawaited
    | Generator { Expression.Comprehension.element; generators }
    | ListComprehension { Expression.Comprehension.element; generators }
    | SetComprehension { Expression.Comprehension.element; generators } ->
        let state = List.fold generators ~init:{ unawaited } ~f:forward_generator in
        forward_expression ~state ~expression:element
    | DictionaryComprehension {
        Expression.Comprehension.element = {
          Expression.Dictionary.key;
          value;
        };
        generators;
      } ->
        let state = List.fold generators ~init:{ unawaited } ~f:forward_generator in
        let unawaited = forward_expression ~state ~expression:key in
        forward_expression ~state:{ unawaited } ~expression:value
    (* Base cases. *)
    | Access _
    | Complex _
    | False
    | Float _
    | Integer _
    | String _
    | True
    | Name _
    | Ellipsis ->
        unawaited


  let forward
      ?key
      ({ unawaited } as state)
      ~statement:{ Node.value; _ } =
    let { Node.value = { Define.signature = { name; parent; _ }; _ }; _ } = Context.define in
    let resolution =
      TypeCheck.resolution_with_key
        ~environment:Context.environment
        ~parent
        ~name
        ~key
    in
    let unawaited =
      let is_awaitable value =
        try
          let annotation = Resolution.resolve resolution value in
          Resolution.less_or_equal resolution ~left:annotation ~right:(Type.awaitable Type.Top)
        with TypeOrder.Untracked _ ->
          false
      in
      let forward_return ~unawaited ~expression =
        match Node.value expression with
        | Expression.Name name when Expression.is_simple_name name ->
            Map.set unawaited ~key:(Reference.from_name_exn name) ~data:Awaited
        | _ ->
            unawaited
      in
      match value with
      | Assert { Assert.test; _ } ->
          forward_expression ~state ~expression:test
      | Assign { target = { Node.value = Name name; location }; value; _ }
        when Expression.is_simple_name name && is_awaitable value ->
          Map.set unawaited ~key:(Reference.from_name_exn name) ~data:(Unawaited location)

      | Assign {
          value = { Node.value = Await { Node.value = Name name; _ }; _ };
          _;
        } when Expression.is_simple_name name ->
          Map.set unawaited ~key:(Reference.from_name_exn name) ~data:Awaited
      | Delete expression
      | Expression expression ->
          forward_expression ~state ~expression
      | Raise None ->
          unawaited
      | Raise (Some expression) ->
          forward_expression ~state ~expression
      | Return { expression = Some expression; _ } ->
          let unawaited = forward_expression ~state ~expression in
          forward_return ~unawaited ~expression
      | Return { expression = None; _ } ->
          unawaited
      | Yield { Node.value = Expression.Yield (Some expression); _ } ->
          let unawaited = forward_expression ~state ~expression in
          forward_return ~unawaited ~expression
      | Yield _ ->
          unawaited
      | YieldFrom { Node.value = Expression.Yield (Some expression); _ } ->
          forward_expression ~state ~expression
      | YieldFrom _ ->
          unawaited
      (* Control flow and nested functions/classes doesn't need to be analyzed explicitly. *)
      | If _
      | Class _
      | Define _
      | For _
      | While _
      | With _
      | Try _ ->
          unawaited
      (* Trivial cases. *)
      | Break
      | Continue
      | Global _
      | Import _
      | Nonlocal _
      | Pass ->
          unawaited
      (* Need to implement. *)
      | Assign _ ->
          unawaited
    in
    { state with unawaited }


  let backward ?key:_ _ ~statement:_ =
    failwith "Not implemented"
end


let name =
  "Awaitable"


let run ~configuration:_ ~environment ~source =
  let check define =
    let module Context =
    struct
      let define = define
      let environment = environment
    end
    in
    let module State = State(Context) in
    let module Fixpoint = Fixpoint.Make(State) in
    Fixpoint.forward
      ~cfg:(Cfg.create (Node.value define))
      ~initial:State.initial
    |> Fixpoint.exit
    >>| State.errors
    |> Option.value ~default:[]
  in
  source
  |> Preprocessing.defines ~include_toplevels:true
  |> List.map ~f:check
  |> List.concat
