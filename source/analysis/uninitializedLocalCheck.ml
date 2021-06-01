(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre
open Statement
open Expression
module Error = AnalysisError

let name = "UninitializedLocal"

module State = struct
  module InitializedVariables = Identifier.Set

  type t = {
    initialized_variables: InitializedVariables.t;
    uninitialized_usage: Identifier.t Node.t list;
  }

  (* TODO: implement *)
  let show _ = "unsupported"

  let pp format state = Format.fprintf format "%s" (show state)

  let initial ~define:{ Node.value = { Define.signature; _ }; _ } =
    {
      initialized_variables =
        signature.parameters |> List.map ~f:Parameter.name |> InitializedVariables.of_list;
      uninitialized_usage = [];
    }


  let errors ~qualifier ~define state =
    let emit_error { Node.value; location } =
      Error.create
        ~location:(Location.with_module ~qualifier location)
        ~kind:(Error.UnboundName value)
        ~define
    in
    List.map state.uninitialized_usage ~f:emit_error


  let less_or_equal ~left ~right =
    InitializedVariables.is_subset right.initialized_variables ~of_:left.initialized_variables


  let join left right =
    {
      initialized_variables =
        InitializedVariables.inter left.initialized_variables right.initialized_variables;
      uninitialized_usage = List.concat [left.uninitialized_usage; right.uninitialized_usage];
    }


  let widen ~previous ~next ~iteration:_ = join previous next

  let forward
      ~key:_
      ({ initialized_variables; uninitialized_usage } as state)
      ~statement:{ Node.value; _ }
    =
    match value with
    | Statement.Assign
        {
          Assign.value =
            { Node.value = Expression.Name (Name.Identifier indentifier_read); location };
          target = { Node.value = Expression.Name (Name.Identifier identifier_target); _ };
          _;
        } ->
        if InitializedVariables.mem initialized_variables indentifier_read then
          {
            initialized_variables = InitializedVariables.add initialized_variables identifier_target;
            uninitialized_usage;
          }
        else
          {
            initialized_variables = InitializedVariables.add initialized_variables identifier_target;
            uninitialized_usage = { Node.value = indentifier_read; location } :: uninitialized_usage;
          }
    | Statement.Return
        {
          expression = Some { Node.value = Expression.Name (Name.Identifier identifier); location };
          _;
        } ->
        if InitializedVariables.mem initialized_variables identifier then
          state
        else
          {
            initialized_variables;
            uninitialized_usage = { Node.value = identifier; location } :: uninitialized_usage;
          }
    | _ -> state


  let backward ~key:_ _ ~statement:_ = failwith "Not implemented"
end

let run
    ~configuration:_
    ~environment:_
    ~source:({ Source.source_path = { SourcePath.qualifier; _ }; _ } as source)
  =
  let check define =
    let module State = State in
    let module Fixpoint = Fixpoint.Make (State) in
    Fixpoint.forward ~cfg:(Cfg.create (Node.value define)) ~initial:(State.initial ~define)
    |> Fixpoint.exit
    >>| State.errors ~qualifier ~define
    |> Option.value ~default:[]
  in
  source |> Preprocessing.defines ~include_toplevels:true |> List.map ~f:check |> List.concat
