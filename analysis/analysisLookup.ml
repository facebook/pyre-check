(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Statement
open Pyre

module Cfg = AnalysisCfg
module Environment = AnalysisEnvironment
module Preprocessing = AnalysisPreprocessing
module Type = AnalysisType
module TypeResolutionSharedMemory = AnalysisTypeResolutionSharedMemory


type t = Type.t Location.Reference.Table.t


(** The result state of this visitor is ignored. We need two read-only
    pieces of information to build the location table: the types resolved for
    this statement, and a reference to the (mutable) location table to
    update. *)
module ExpressionVisitor = struct

  type t = Environment.Resolution.t * Type.t Location.Reference.Table.t

  let expression ((resolution, lookup) as state) expression =
    let lookup_of_arguments = function
      | { Node.value = Expression.Access access; _ } ->
          let check_single_access = function
            | Access.Call { Node.value = arguments; _ }  ->
                let check_argument
                    {
                      Argument.value = {
                        Node.location = value_location; _ } as value;
                      name
                    } =
                  let location =
                    match name with
                    | Some { Node.location = { Location.start; _ }; _ } ->
                        { value_location with Location.start }
                    | None ->
                        value_location
                  in
                  try
                    let annotation = Annotated.resolve ~resolution value in
                    if not (Type.is_unknown annotation) then
                      Location.Reference.Table.set lookup ~key:location ~data:annotation
                  with AnalysisTypeOrder.Untracked _ ->
                    (* If we cannot resolve the type of this
                       expression, ignore it silently. The
                       construction of the lookup table is not
                       critical. *)
                    ()
                in
                List.iter ~f:check_argument arguments
            | _ ->
                ()
          in
          List.iter ~f:check_single_access access
      | _ ->
          ()
    in

    (* T30816068: we need a better visitor interface that exposes Argument.name *)
    lookup_of_arguments expression;
    let { Node.location; _ } = expression in
    let () =
      try
        let annotation = Annotated.resolve ~resolution expression in
        if not (Type.is_unknown annotation) then
          Location.Reference.Table.set lookup ~key:location ~data:annotation
      with AnalysisTypeOrder.Untracked _ ->
        (* If we cannot resolve the type of this expression, ignore it
           silently. The construction of the lookup table is not
           critical. *)
        ()
    in
    state

  let statement state _ =
    state
end


module Visit = Visit.Make(ExpressionVisitor)


let create_of_source environment source =
  let open TypeResolutionSharedMemory in
  let location_lookup = Location.Reference.Table.create () in
  let walk_defines { Node.value = ({ Define.name = caller; _ } as define); _ } =
    let cfg = Cfg.create define in
    let annotation_lookup =
      let fold_annotations map { key; annotations } =
        Int.Map.set map ~key ~data:annotations
      in
      TypeResolutionSharedMemory.get caller
      >>| List.fold ~init:Int.Map.empty ~f:fold_annotations
      |> Option.value ~default:Int.Map.empty
    in
    let walk_cfg ~key:node_id ~data:cfg_node =
      let statements = Cfg.Node.statements cfg_node in
      let walk_statements statement_index statement =
        let annotations =
          Int.Map.find annotation_lookup ([%hash: int * int] (node_id, statement_index))
          |> Option.value ~default:[]
          |> Access.Map.of_alist_exn
        in
        let resolution = Environment.resolution environment ~annotations () in
        Visit.visit (resolution, location_lookup) (Source.create [statement])
        |> ignore
      in
      List.iteri statements ~f:walk_statements
    in
    Int.Table.iteri cfg ~f:walk_cfg
  in
  Preprocessing.defines source
  |> List.iter ~f:walk_defines;
  location_lookup


let get_annotation lookup ~position =
  let location_contains_position
      {
        Location.start = { Location.column = start_column; line = start_line };
        stop = { Location.column = stop_column; line = stop_line };
        _;
      }
      { Location.column; line } =
    let start_ok = (start_line < line) || (start_line = line && start_column <= column) in
    let stop_ok = (stop_line > line) || (stop_line = line && stop_column > column) in
    start_ok && stop_ok
  in
  let get_best_location position =
    let weight
        {
          Location.start = { Location.column = start_column; line = start_line };
          stop = { Location.column = stop_column; line = stop_line };
          _;
        } =
      (stop_line - start_line) * 1000 + stop_column - start_column
    in
    let instantiate_location (location, annotation) =
      Location.instantiate ~lookup:(fun hash -> Ast.AstSharedMemory.get_path ~hash) location,
      annotation
    in

    Hashtbl.to_alist lookup
    |> List.filter ~f:(fun (key, _) -> location_contains_position key position)
    |> List.min_elt ~compare:(fun (location_left, _) (location_right, _) ->
        (weight location_left) - (weight location_right))
    |> Option.map ~f:instantiate_location
  in
  get_best_location position


let get_all_annotations lookup =
  Location.Reference.Table.to_alist lookup


let get_definition _lookup _position =
  None
