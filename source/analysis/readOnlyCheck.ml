(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre
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

module LocalErrorMap = struct
  type t = Error.t list Int.Table.t

  let empty () = Int.Table.create ()

  let set ~statement_key ~errors error_map = Int.Table.set error_map ~key:statement_key ~data:errors

  let append ~statement_key ~error error_map =
    Int.Table.add_multi error_map ~key:statement_key ~data:error


  let all_errors error_map = Int.Table.data error_map |> List.concat
end

module type Context = sig
  (* Where to store errors found during the fixpoint. `None` discards them. *)
  val error_map : LocalErrorMap.t option
end

module State (Context : Context) = struct
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


  let initial = Resolution.of_list []

  let forward_statement ~state ~statement:_ = state, []

  let forward ~statement_key state ~statement =
    let new_state, errors = forward_statement ~state ~statement in
    let () =
      let _ = Context.error_map >>| LocalErrorMap.set ~statement_key ~errors in
      ()
    in
    new_state


  let backward ~statement_key:_ _state ~statement:_ =
    failwith "Not implementing this for readonly analysis"
end

let readonly_errors_for_define define =
  let module Context = struct
    let error_map = Some (LocalErrorMap.empty ())
  end
  in
  let module State = State (Context) in
  let module Fixpoint = Fixpoint.Make (State) in
  let cfg = Node.value define |> Cfg.create in
  let _state = Fixpoint.forward ~cfg ~initial:State.initial |> Fixpoint.exit in
  Option.value_exn
    ~message:"no error map found in the analysis context"
    (Context.error_map >>| LocalErrorMap.all_errors >>| Error.deduplicate)
