(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open Statement

module Resolution = AnalysisResolution
module Type = AnalysisType


type t = Assign.t
[@@deriving compare, eq, sexp, show, hash]


let create assign =
  assign


let fold ~resolution ~initial ~f { Assign.target; value; _ } =
  value
  >>| (fun value ->
      let rec fold_simple_assign accumulator { Node.location; value } value_annotation =
        match value with
        | Access access ->
            f ~access:(Node.create ~location access) ~value_annotation accumulator
        | Tuple targets ->
            (* Recursively break down tuples such as x, y = z : Tuple[int, string] *)
            let parameters =
              match value_annotation with
              | Type.Tuple (Type.Bounded parameters) ->
                  parameters
              | Type.Tuple (Type.Unbounded parameter) ->
                  List.map ~f:(fun _ -> parameter) targets
              | _ ->
                  List.map ~f:(fun _ -> Type.Top) targets
            in
            if List.length targets = List.length parameters then
              List.fold2_exn ~init:accumulator ~f:fold_simple_assign targets parameters
            else
              accumulator
        | _ ->
            accumulator
      in
      begin
        match (Node.value target), (Node.value value) with
        (* Tuples of individual assignments *)
        | Tuple targets, Tuple values
          when List.length targets = List.length values ->
            List.map ~f:(Resolution.resolve resolution) values
            |> List.fold2_exn ~init:initial ~f:fold_simple_assign targets
        | _, _ ->
            fold_simple_assign initial target (Resolution.resolve resolution value)
      end)
  |> Option.value ~default:initial
