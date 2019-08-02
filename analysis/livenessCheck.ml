(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Statement
open Pyre
module Error = AnalysisError

module type Context = sig end

module State (Context : Context) = struct
  type t = {
    unused: Location.Reference.Set.t Identifier.Map.t;
    define: Define.t;
  }

  let show { unused; _ } = Map.keys unused |> String.concat ~sep:", "

  let pp format state = Format.fprintf format "%s" (show state)

  let initial ~define = { unused = Identifier.Map.empty; define }

  let less_or_equal ~left:{ unused = left; _ } ~right:{ unused = right; _ } =
    let less_or_equal (reference, location) =
      match location, Map.find right reference with
      | left, Some right -> Set.is_subset left ~of_:right
      | _ -> false
    in
    Map.to_alist left |> List.for_all ~f:less_or_equal


  let join left right =
    let merge ~key:_ = function
      | `Both (left, right) -> Some (Set.union left right)
      | `Left left -> Some left
      | `Right right -> Some right
    in
    { left with unused = Map.merge left.unused right.unused ~f:merge }


  let widen ~previous ~next ~iteration:_ = join previous next

  let errors _ = []

  let forward ?key:_ state ~statement:{ Node.value; _ } =
    match value with
    | Assert _
    | Assign _
    | Delete _
    | Expression _
    | Raise _
    | Return _
    | Yield _
    | YieldFrom _
    | If _
    | Class _
    | Define _
    | For _
    | While _
    | With _
    | Try _
    | Break
    | Continue
    | Global _
    | Import _
    | Nonlocal _
    | Pass ->
        state


  let backward ?key:_ _ ~statement:_ = failwith "Not implemented"
end

let name = "Liveness"

let run ~configuration:_ ~global_resolution:_ ~source =
  let check define =
    let module Context = struct end in
    let module State = State (Context) in
    let module Fixpoint = Fixpoint.Make (State) in
    Fixpoint.forward
      ~cfg:(Cfg.create (Node.value define))
      ~initial:(State.initial ~define:define.Node.value)
    |> Fixpoint.exit
    >>| State.errors
    |> Option.value ~default:[]
  in
  source |> Preprocessing.defines ~include_toplevels:true |> List.map ~f:check |> List.concat
