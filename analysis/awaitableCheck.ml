(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Statement
open Pyre
module Error = AnalysisError

module type Context = sig
  val define : Define.t Node.t

  val environment : (module Environment.Handler)
end

module State (Context : Context) = struct
  type state =
    | Unawaited of Expression.t
    | Awaited
  [@@deriving show]

  let _ = show_state (* unused *)

  type t = {
    unawaited: state Location.Reference.Map.t;
    locals: Location.Reference.Set.t Reference.Map.t
  }

  let show { unawaited; locals } =
    let unawaited =
      Map.to_alist unawaited
      |> List.map ~f:(fun (location, state) ->
             Format.asprintf "%a -> %a" Location.pp location pp_state state)
      |> String.concat ~sep:", "
    in
    let locals =
      let show_locations locations =
        Set.to_list locations |> List.map ~f:Location.show |> String.concat ~sep:", "
      in
      Map.to_alist locals
      |> List.map ~f:(fun (reference, locations) ->
             Format.asprintf "%a -> {%s}" Reference.pp reference (show_locations locations))
      |> String.concat ~sep:", "
    in
    Format.sprintf "Unawaited expressions: %s\nLocals: %s\n" unawaited locals


  let pp format state = Format.fprintf format "%s" (show state)

  let initial = { unawaited = Location.Reference.Map.empty; locals = Reference.Map.empty }

  let errors { unawaited; locals } =
    let errors =
      let keep_unawaited = function
        | Unawaited expression -> Some { Error.references = []; expression }
        | Awaited -> None
      in
      Map.filter_map unawaited ~f:keep_unawaited
    in
    let add_reference ~key:name ~data:locations errors =
      let add_reference errors location =
        match Map.find errors location with
        | Some { Error.references; expression } ->
            Map.set errors ~key:location ~data:{ references = name :: references; expression }
        | None -> errors
      in
      Location.Reference.Set.fold locations ~init:errors ~f:add_reference
    in
    let error (location, unawaited_awaitable) =
      Error.create
        ~location
        ~kind:(Error.UnawaitedAwaitable unawaited_awaitable)
        ~define:Context.define
    in
    Map.fold locals ~init:errors ~f:add_reference |> Map.to_alist |> List.map ~f:error


  let less_or_equal ~left ~right =
    let less_or_equal_unawaited (reference, state) =
      match state, Map.find right.unawaited reference with
      | Unawaited _, Some _ -> true
      | Awaited, Some Awaited -> true
      | _ -> false
    in
    let less_or_equal_locals (reference, locations) =
      match Map.find right.locals reference with
      | Some other_locations -> Set.is_subset locations ~of_:other_locations
      | None -> false
    in
    Map.to_alist left.unawaited |> List.for_all ~f:less_or_equal_unawaited
    && Map.to_alist left.locals |> List.for_all ~f:less_or_equal_locals


  let join left right =
    let merge_unawaited ~key:_ left right =
      match left, right with
      | Awaited, _
      | _, Awaited ->
          Awaited
      | unawaited, _ -> unawaited
    in
    let merge_locals ~key:_ left right = Set.union left right in
    { unawaited = Map.merge_skewed left.unawaited right.unawaited ~combine:merge_unawaited;
      locals = Map.merge_skewed left.locals right.locals ~combine:merge_locals
    }


  let widen ~previous ~next ~iteration:_ = join previous next

  let mark_name_as_awaited { unawaited; locals } ~name =
    if Expression.is_simple_name name then
      let unawaited =
        let await_location unawaited location = Map.set unawaited ~key:location ~data:Awaited in
        Map.find locals (Reference.from_name_exn name)
        >>| (fun locations -> Set.fold locations ~init:unawaited ~f:await_location)
        |> Option.value ~default:unawaited
      in
      { unawaited; locals }
    else (* Non-simple names cannot store awaitables. *)
      { unawaited; locals }


  let rec forward_generator
      state
      { Expression.Comprehension.target = _; iterator; conditions; async = _ }
    =
    let state =
      List.fold
        conditions
        ~f:(fun state expression -> forward_expression ~state ~expression)
        ~init:state
    in
    forward_expression ~state ~expression:iterator


  and forward_expression ~state
                         ~expression:{ Node.value; _ } =
    let open Expression in
    match value with
    | Await { Node.value = Name name; _ } when Expression.is_simple_name name ->
        mark_name_as_awaited state ~name
    | Await expression -> forward_expression ~state ~expression
    | BooleanOperator { BooleanOperator.left; right; _ } ->
        let state = forward_expression ~state ~expression:left in
        forward_expression ~state ~expression:right
    | Call { Call.callee; arguments } ->
        let state = forward_expression ~state ~expression:callee in
        let forward_argument state { Call.Argument.value; _ } =
          match value with
          | { Node.value = Name name; _ } -> mark_name_as_awaited state ~name
          | _ -> forward_expression ~state ~expression:value
        in
        List.fold arguments ~init:state ~f:forward_argument
    | ComparisonOperator { ComparisonOperator.left; right; _ } ->
        let state = forward_expression ~state ~expression:left in
        forward_expression ~state ~expression:right
    | Dictionary { Dictionary.entries; keywords } ->
        let forward_entry state { Dictionary.key; value } =
          let state = forward_expression ~state ~expression:key in
          forward_expression ~state ~expression:value
        in
        let state = List.fold entries ~init:state ~f:forward_entry in
        List.fold keywords ~init:state ~f:(fun state expression ->
            forward_expression ~state ~expression)
    | Lambda { Lambda.body; _ } -> forward_expression ~state ~expression:body
    | Starred (Starred.Once expression)
    | Starred (Starred.Twice expression) ->
        forward_expression ~state ~expression
    | Ternary { Ternary.target; test; alternative } ->
        let state = forward_expression ~state ~expression:target in
        let state = forward_expression ~state ~expression:test in
        forward_expression ~state ~expression:alternative
    | List items
    | Set items
    | Tuple items ->
        List.fold items ~init:state ~f:(fun state expression ->
            forward_expression ~state ~expression)
    | UnaryOperator { UnaryOperator.operand; _ } -> forward_expression ~state ~expression:operand
    | Yield (Some expression) -> forward_expression ~state ~expression
    | Yield None -> state
    | Generator { Expression.Comprehension.element; generators }
    | ListComprehension { Expression.Comprehension.element; generators }
    | SetComprehension { Expression.Comprehension.element; generators } ->
        let state = List.fold generators ~init:state ~f:forward_generator in
        forward_expression ~state ~expression:element
    | DictionaryComprehension
        { Expression.Comprehension.element = { Expression.Dictionary.key; value }; generators } ->
        let state = List.fold generators ~init:state ~f:forward_generator in
        let state = forward_expression ~state ~expression:key in
        forward_expression ~state ~expression:value
    (* Base cases. *)
    | Complex _
    | False
    | Float _
    | Integer _
    | String _
    | True
    | Name _
    | Ellipsis ->
        state


  let forward_assign
      ~resolution ~state:({ unawaited; locals } as state) ~annotation ~expression ~target
    =
    match target with
    | { Node.value = Expression.Name target; _ } when Expression.is_simple_name target -> (
      match expression with
      | { Node.value = Expression.Name value; _ } when Expression.is_simple_name value ->
          (* Aliasing. *)
          let locals =
            Map.find locals (Reference.from_name_exn value)
            >>| (fun locations ->
                  Map.set locals ~key:(Reference.from_name_exn target) ~data:locations)
            |> Option.value ~default:locals
          in
          { unawaited; locals }
      | _ ->
          if Resolution.less_or_equal resolution ~left:annotation ~right:(Type.awaitable Type.Top)
          then
            { unawaited =
                Map.set unawaited ~key:(Node.location expression) ~data:(Unawaited expression);
              locals =
                Map.set
                  locals
                  ~key:(Reference.from_name_exn target)
                  ~data:(Location.Reference.Set.singleton (Node.location expression))
            }
          else
            state )
    | _ -> state


  let forward ?key
              ({ unawaited; locals } as state)
              ~statement:{ Node.value; _ } =
    let { Node.value = { Define.signature = { name; parent; _ }; _ }; _ } = Context.define in
    let resolution =
      TypeCheck.resolution_with_key ~environment:Context.environment ~parent ~name ~key
    in
    let is_awaitable expression =
      try
        let annotation = Resolution.resolve resolution expression in
        Resolution.less_or_equal resolution ~left:annotation ~right:(Type.awaitable Type.Top)
      with
      | TypeOrder.Untracked _ -> false
    in
    let forward_return ~state:{ unawaited; locals } ~expression =
      match Node.value expression with
      | Expression.Name name when Expression.is_simple_name name ->
          mark_name_as_awaited state ~name
      | _ -> { unawaited; locals }
    in
    match value with
    | Assert { Assert.test; _ } -> forward_expression ~state ~expression:test
    | Assign { value; target; _ } ->
        (* TODO(T45559452): Need to support awaitables getting introduced in tuple assignments. *)
        let state = forward_expression ~state ~expression:value in
        let annotation = Resolution.resolve resolution value in
        forward_assign ~state ~resolution ~annotation ~expression:value ~target
    | Delete expression
    | Expression expression ->
        let state =
          if is_awaitable expression then
            { unawaited =
                Map.set unawaited ~key:(Node.location expression) ~data:(Unawaited expression);
              locals
            }
          else
            state
        in
        forward_expression ~state ~expression
    | Raise None -> state
    | Raise (Some expression) -> forward_expression ~state ~expression
    | Return { expression = Some expression; _ } ->
        let state = forward_expression ~state ~expression in
        forward_return ~state ~expression
    | Return { expression = None; _ } -> state
    | Yield { Node.value = Expression.Yield (Some expression); _ } ->
        let state = forward_expression ~state ~expression in
        forward_return ~state ~expression
    | Yield _ -> state
    | YieldFrom { Node.value = Expression.Yield (Some expression); _ } ->
        forward_expression ~state ~expression
    | YieldFrom _ -> state
    (* Control flow and nested functions/classes doesn't need to be analyzed explicitly. *)
    | If _
    | Class _
    | Define _
    | For _
    | While _
    | With _
    | Try _ ->
        state
    (* Trivial cases. *)
    | Break
    | Continue
    | Global _
    | Import _
    | Nonlocal _
    | Pass ->
        state


  let backward ?key:_ _ ~statement:_ = failwith "Not implemented"
end

let name = "Awaitable"

let run ~configuration:_ ~environment ~source =
  let check define =
    let module Context = struct
      let define = define

      let environment = environment
    end
    in
    let module State = State (Context) in
    let module Fixpoint = Fixpoint.Make (State) in
    Fixpoint.forward ~cfg:(Cfg.create (Node.value define)) ~initial:State.initial
    |> Fixpoint.exit
    >>| State.errors
    |> Option.value ~default:[]
  in
  source |> Preprocessing.defines ~include_toplevels:true |> List.map ~f:check |> List.concat
