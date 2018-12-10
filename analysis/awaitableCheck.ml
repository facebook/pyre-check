(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Statement
open Pyre

module Error = AnalysisError


module State = struct
  type state =
    | Unawaited
    | Awaited
  [@@deriving show]


  type t = {
    define: Define.t Node.t;
    environment: (module Environment.Handler);
    unawaited: state Access.Map.t;
  }


  let show { unawaited; _ } =
    Map.to_alist unawaited
    |> List.map ~f:(fun (access, state) -> Format.asprintf "%a -> %a" Access.pp access pp_state state)
    |> String.concat ~sep:", "


  let pp format state =
    Format.fprintf format "%s" (show state)


  let initial ~environment ~define =
    {
      define;
      environment;
      unawaited = Access.Map.empty;
    }


  let errors { define; unawaited; _ } =
    let error (access, state) =
      match state with
      | Unawaited ->
          [
            Error.create
              ~location:(Node.location define)
              ~kind:(Error.UnawaitedAwaitable (Access.delocalize access))
              ~define:define;
          ]
      | _ ->
          []
    in
    Map.to_alist unawaited
    |> List.concat_map ~f:error


  let less_or_equal ~left:{ unawaited = left; _ } ~right:{ unawaited = right; _ } =
    let less_or_equal (access, state) =
      match state, Map.find right access with
      | Unawaited, Some _ -> true
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
      | `Both (Unawaited, Unawaited) -> Some Unawaited
    in
    { left with unawaited = Map.merge left.unawaited right.unawaited ~f:merge }


  let widen ~previous ~next ~iteration:_ =
    join previous next


  let forward
      ?key
      ({ define = { Node.value = { Define.name; parent; _ }; _ }; environment; unawaited } as state)
      ~statement:{ Node.value; _ } =
    let resolution = TypeCheck.resolution_with_key ~environment ~parent ~access:name ~key in

    let unawaited =
      let is_awaitable value =
        let annotation = Resolution.resolve resolution value in
        Resolution.less_or_equal resolution ~left:annotation ~right:(Type.awaitable Type.Object)
      in
      match value with
      | Assign { target = { Node.value = Access access; _ }; value; _ } when is_awaitable value ->
          Map.set unawaited ~key:access ~data:Unawaited

      | Assign {
          value = { Node.value = Await { Node.value = Access access; _ }; _ };
          _
        }
      | Expression { Node.value = Await { Node.value = Access access; _ }; _ } ->
          Map.set unawaited ~key:access ~data:Awaited

      | _ ->
          unawaited
    in

    { state with unawaited }


  let backward ?key:_ _ ~statement:_ =
    failwith "Not implemented"
end


module Fixpoint = Fixpoint.Make(State)


let name =
  "Awaitable"


let run ~configuration:_ ~environment ~source =
  let check define =
    Fixpoint.forward
      ~cfg:(Cfg.create (Node.value define))
      ~initial:(State.initial ~environment ~define)
    |> Fixpoint.exit
    >>| State.errors
    |> Option.value ~default:[]
  in
  Preprocessing.defines ~extract_into_toplevel:true source
  |> List.map ~f:check
  |> List.concat
