(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre
open Ast

module Type = AnalysisType


type t = Type.t Location.Table.t


let create () =
  Location.Table.create ()


let update lookup ~location ~annotation =
  Hashtbl.set lookup ~key:location ~data:annotation


let get_annotation lookup ~position =
  let location_contains_position
      {
        Location.start = { Location.column = start_column; line = start_line };
        stop = { Location.column = stop_column; line = stop_line };
        _;
      }
      { Location.column; line } =
    let start_ok = (start_line < line) || (start_line = line && start_column <= column) in
    let stop_ok = (stop_line > line) || (stop_line = line && stop_column >= column) in
    start_ok && stop_ok
  in
  let get_best_location position =
    Hashtbl.to_alist lookup
    |> List.filter ~f:(fun (key, _) -> location_contains_position key position)
    |> List.hd
    >>| fst
  in
  get_best_location position
  >>= (fun location ->
      Hashtbl.find lookup location
      >>| fun annotation -> (location, annotation))


let get_definition _lookup _position =
  None
