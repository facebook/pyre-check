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
      let rec fold_simple_assign accumulator target value_annotation =
        match Node.value target with
        | Access _ ->
            f ~target ~value_annotation accumulator
        | Tuple targets ->
            (* Recursively break down tuples such as x, y = z : Tuple[int, string] *)
            let parameters =
              match value_annotation with
              | Type.Tuple (Type.Bounded parameters) ->
                  parameters
              | Type.Tuple (Type.Unbounded parameter) ->
                  List.map ~f:(fun _ -> parameter) targets
              | Type.Parametric { Type.name; parameters = [parameter] }
                when Identifier.equal name (Identifier.create "list") ->
                  List.map ~f:(fun _ -> parameter) targets

              | _ ->
                  []
            in
            if List.length targets = List.length parameters then
              List.fold2_exn ~init:accumulator ~f:fold_simple_assign targets parameters
            else
              f ~target ~value_annotation accumulator
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
        | List targets, _ ->
            let annotation =
              Resolution.resolve resolution value
              |> Resolution.join resolution (Type.iterable Type.Bottom)
              |> Type.parameters
              |> function
              | [parameter] -> parameter
              | _ -> Type.Object
            in
            List.fold
              ~f:(fun accumulator assign -> fold_simple_assign accumulator assign annotation)
              ~init:initial
              targets
        | _, _ ->
            fold_simple_assign initial target (Resolution.resolve resolution value)
      end)
  |> Option.value ~default:initial
