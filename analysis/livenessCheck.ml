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
    define: Define.t Node.t;
    nested_defines: t NestedDefines.t;
  }

  let show { unused; _ } = Map.keys unused |> String.concat ~sep:", "

  let pp format state = Format.fprintf format "%s" (show state)

  let initial ~state:_ ~define =
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

  let nested_defines { nested_defines; _ } = nested_defines

  let errors { unused; define; _ } =
    let add_errors ~key ~data errors =
      let create_error location = Error.create ~location ~kind:(Error.DeadStore key) ~define in
      Set.to_list data |> List.map ~f:create_error |> fun new_errors -> new_errors @ errors
    in
    Map.fold unused ~init:[] ~f:add_errors


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
      let update_unused ~unused identifier =
        let update = function
          | Some existing -> Set.add existing location
          | None -> Location.Reference.Set.of_list [location]
        in
        Map.update unused identifier ~f:update
      in
      match value with
      | Assign { target; _ } ->
          let rec update_target unused = function
            | { Node.value = Name (Name.Identifier identifier); _ } ->
                update_unused ~unused identifier
            | { Node.value = List elements; _ } -> List.fold ~init:unused ~f:update_target elements
            | { Node.value = Starred (Starred.Once target); _ } -> update_target unused target
            | { Node.value = Tuple elements; _ } ->
                List.fold ~init:unused ~f:update_target elements
            | _ -> unused
          in
          update_target unused target
      | _ -> unused
    in
    let nested_defines = NestedDefines.update_nested_defines nested_defines ~statement ~state in
    { state with unused; nested_defines }


  let backward ?key:_ _ ~statement:_ = failwith "Not implemented"
end

let name = "Liveness"

let run ~configuration:_ ~global_resolution:_ ~source =
  let module Context = struct end in
  let module State = State (Context) in
  let module Fixpoint = Fixpoint.Make (State) in
  let rec check ~state define =
    let run_nested ~key ~data:{ NestedDefines.nested_define; state } errors =
      let result = check ~state:(Some state) { Node.location = key; value = nested_define } in
      result @ errors
    in
    Fixpoint.forward ~cfg:(Cfg.create (Node.value define)) ~initial:(State.initial ~state ~define)
    |> Fixpoint.exit
    >>| (fun state ->
          State.nested_defines state
          |> Map.fold ~f:run_nested ~init:[]
          |> fun result -> State.errors state @ result)
    |> Option.value ~default:[]
  in
  check ~state:None (Source.top_level_define_node source)
