(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre
module Error = AnalysisError

let name = "UninitializedLocal"

module State = struct
  type t = unit

  let show _ = "empty"

  let pp format state = Format.fprintf format "%s" (show state)

  let initial = ()

  let errors _ = []

  let less_or_equal ~left:_ ~right:_ = true

  let join left _ = left

  let widen ~previous ~next ~iteration:_ = join previous next

  let forward ~key:_ state ~statement:_ =
    (* TODO (T69630394): Implement. *)
    state


  let backward ~key:_ _ ~statement:_ = failwith "Not implemented"
end

let run ~configuration:_ ~environment:_ ~source =
  let check define =
    let module State = State in
    let module Fixpoint = Fixpoint.Make (State) in
    Fixpoint.forward ~cfg:(Cfg.create (Node.value define)) ~initial:State.initial
    |> Fixpoint.exit
    >>| State.errors
    |> Option.value ~default:[]
  in
  source |> Preprocessing.defines ~include_toplevels:true |> List.map ~f:check |> List.concat
