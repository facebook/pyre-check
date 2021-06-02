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

let identifiers { Node.value; location } =
  match value with
  | Expression.Name (Name.Identifier identifier) -> [{ Node.value = identifier; location }]
  | _ -> []


let extract_reads statement =
  match statement with
  | Statement.Assign { value = expression; _ } -> identifiers expression
  | Return { expression = Some expression; _ } -> identifiers expression
  | _ -> []


let extract_writes statement =
  ( match statement with
  | Statement.Assign { target = expression; _ } -> identifiers expression
  | _ -> [] )
  |> List.map ~f:Node.value


module State = struct
  module InitializedVariables = Identifier.Set

  type t = {
    initialized_variables: InitializedVariables.t;
    uninitialized_usage: Identifier.t Node.t list;
  }

  let show state =
    InitializedVariables.elements state.initialized_variables
    |> String.concat ~sep:", "
    |> Format.sprintf "[%s]"


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

  let forward ~key:_ { initialized_variables; uninitialized_usage } ~statement:{ Node.value; _ } =
    (* TODO: move uninitialized_usage out of fixpoint computation *)
    let is_uninitialized { Node.value = identifier; _ } =
      not (InitializedVariables.mem initialized_variables identifier)
    in
    {
      uninitialized_usage =
        extract_reads value |> List.filter ~f:is_uninitialized |> List.append uninitialized_usage;
      initialized_variables =
        extract_writes value
        |> InitializedVariables.of_list
        |> InitializedVariables.union initialized_variables;
    }


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
