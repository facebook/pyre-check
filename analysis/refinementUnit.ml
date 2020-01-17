(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast

type t = {
  base: Annotation.t option;
  attribute_refinements: t Identifier.Map.t;
}

let rec pp format { base; attribute_refinements } =
  let attribute_map_entry (identifier, refinement_unit) =
    Format.asprintf "%a -> %a" Identifier.pp identifier pp refinement_unit
  in
  ( match base with
  | Some base -> Format.fprintf format "[Base: %a; " Annotation.pp base
  | None -> Format.fprintf format "[Base: (); " );
  Map.to_alist attribute_refinements
  |> List.map ~f:attribute_map_entry
  |> String.concat ~sep:", "
  |> Format.fprintf format "Attributes: [%s]]"


let show = Format.asprintf "%a" pp

let create ?base ?(attribute_refinements = Identifier.Map.empty) () =
  { base; attribute_refinements }


let find = Identifier.Map.find

let add_attribute_refinement refinement_unit ~reference ~base =
  let rec add_attribute_refinement
      ({ attribute_refinements; _ } as refinement_unit)
      ~base
      ~attributes
    =
    match attributes with
    | [] -> { refinement_unit with base = Some base }
    | attribute :: attributes ->
        {
          refinement_unit with
          attribute_refinements =
            Map.set
              attribute_refinements
              ~key:attribute
              ~data:
                ( find attribute_refinements attribute
                |> Option.value ~default:(create ())
                |> add_attribute_refinement ~base ~attributes );
        }
  in
  add_attribute_refinement refinement_unit ~base ~attributes:(reference |> Reference.as_list)


let annotation refinement_unit ~reference =
  let rec annotation { base; attribute_refinements } ~attributes =
    match attributes with
    | [] -> base
    | attribute :: attributes -> (
        match find attribute_refinements attribute with
        | Some refinement_unit -> annotation refinement_unit ~attributes
        | None -> None )
  in
  annotation refinement_unit ~attributes:(reference |> Reference.as_list)


let base { base; _ } = base
