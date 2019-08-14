(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Expression
open Statement
open Pyre
open CustomAnalysis
module Error = AnalysisError

module type Context = sig end

module State (Context : Context) = struct
  type t = {
    unused: Location.Reference.Set.t Identifier.Map.t;
    define: Define.t;
    nested_defines: t NestedDefines.t;
  }

  let show { unused; _ } = Map.keys unused |> String.concat ~sep:", "

  let pp format state = Format.fprintf format "%s" (show state)

  let initial ~define =
    { unused = Identifier.Map.empty; define; nested_defines = NestedDefines.initial }


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

  let forward
      ?key:_
      ({ unused; nested_defines; _ } as state)
      ~statement:({ Node.location; value } as statement)
    =
    (* Remove used names. *)
    let unused =
      let used_names = Visit.collect_base_identifiers statement |> List.map ~f:Node.value in
      List.fold used_names ~f:Map.remove ~init:unused
    in
    (* Add assignments to unused. *)
    let unused =
      match value with
      | Assign { target = { Node.value = Name (Name.Identifier identifier); _ }; _ } ->
          let update = function
            | Some existing -> Set.add existing location
            | None -> Location.Reference.Set.of_list [location]
          in
          Map.update unused identifier ~f:update
      | _ -> unused
    in
    let nested_defines = NestedDefines.update_nested_defines nested_defines ~statement ~state in
    { state with unused; nested_defines }


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
