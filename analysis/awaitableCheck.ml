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


  type t = {
    unawaited: state Access.Map.t;
  }


  let show { unawaited; _ } =
    Map.to_alist unawaited
    |> List.map
      ~f:(fun (access, state) -> Format.asprintf "%a -> %a" Access.pp access pp_state state)
    |> String.concat ~sep:", "


  let pp format state =
    Format.fprintf format "%s" (show state)


  let initial =
    { unawaited = Access.Map.empty }


  let errors { unawaited; _ } =
    let error (access, state) =
      match state with
      | Unawaited location ->
          [
            Error.create
              ~location
              ~kind:(Error.UnawaitedAwaitable (Access.delocalize access))
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


  let forward
      ?key
      ({ unawaited } as state)
      ~statement:{ Node.value; _ } =
    let { Node.value = { Define.name; parent; _ }; _ } = Context.define in
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
      match value with
      | Assign { target = { Node.value = Access (SimpleAccess access); location }; value; _ }
        when is_awaitable value ->
          Map.set unawaited ~key:access ~data:(Unawaited location)

      | Assign {
          value = { Node.value = Await { Node.value = Access (SimpleAccess access); _ }; _ };
          _
        }
      | Expression { Node.value = Await { Node.value = Access (SimpleAccess access); _ }; _ } ->
          Map.set unawaited ~key:access ~data:Awaited

      | _ ->
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
  Preprocessing.defines ~extract_into_toplevel:true source
  |> List.map ~f:check
  |> List.concat
