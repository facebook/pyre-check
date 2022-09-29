(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Expression
module Error = AnalysisError

module Resolution = struct
  include
    Abstract.MapDomain.Make
      (struct
        include Reference

        let name = "Reference"

        let absence_implicitly_maps_to_bottom = false
      end)
      (Abstract.SimpleDomain.Make (ReadOnlyness))
end

module Resolved = struct
  type t = {
    resolution: Resolution.t;
    resolved: ReadOnlyness.t;
    errors: Error.t list;
  }
  [@@deriving show]
end

module State = struct
  include Resolution

  let widen ~previous ~next ~iteration = widen ~prev:previous ~next ~iteration

  let forward_expression ~resolution { Node.value; _ } =
    let open ReadOnlyness in
    match value with
    | Expression.Constant _ ->
        { Resolved.resolution; errors = []; resolved = ReadOnlyness.ReadOnly }
    | Expression.Name (Name.Identifier identifier) ->
        {
          Resolved.resolution;
          errors = [];
          resolved =
            Resolution.get_opt (Reference.create identifier) resolution
            |> Option.value ~default:Mutable;
        }
    | _ -> failwith "TODO(T130377746)"


  let forward_statement ~state ~statement:_ = state, []

  let initial = Resolution.of_list []

  let forward ~statement_key:_ state ~statement =
    let new_state, _ = forward_statement ~state ~statement in
    new_state


  let backward ~statement_key:_ _state ~statement:_ =
    failwith "Not implementing this for readonly analysis"
end

let populate_error_map define =
  let module Fixpoint = Fixpoint.Make (State) in
  let cfg = Node.value define |> Cfg.create in
  let _state = Fixpoint.forward ~cfg ~initial:State.initial |> Fixpoint.exit in
  ()


let readonly_errors_for_define define =
  let () = populate_error_map define in
  (* TODO(T130377746): Read errors from the error map. *)
  []
