(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast

let find_propagated_type_variables bases ~parse_annotation =
  let find_type_variables { Expression.Call.Argument.value; _ } =
    parse_annotation value |> Type.Variable.all_free_variables
  in
  let handle_deduplicated = function
    | [Type.Variable.ListVariadic variable] -> Type.Variable.Variadic.List.self_reference variable
    | deduplicated ->
        let to_unary = function
          | Type.Variable.Unary variable -> Some (Type.Variable variable)
          | _ -> None
        in
        List.map deduplicated ~f:to_unary
        |> Option.all
        |> Option.value ~default:[]
        |> fun concrete -> Type.OrderedTypes.Concrete concrete
  in
  List.concat_map ~f:find_type_variables bases
  |> List.dedup ~compare:Type.Variable.compare
  |> handle_deduplicated


let inferred_generic_base { Node.value = { ClassSummary.bases; _ }; _ } ~parse_annotation =
  let is_generic { Expression.Call.Argument.value; _ } =
    let primitive, _ = parse_annotation value |> Type.split in
    Type.is_generic_primitive primitive
  in
  if List.exists ~f:is_generic bases then
    []
  else
    let variables = find_propagated_type_variables bases ~parse_annotation in
    if Type.OrderedTypes.equal variables (Concrete []) then
      []
    else
      [
        {
          Expression.Call.Argument.name = None;
          value = Type.parametric "typing.Generic" variables |> Type.expression;
        };
      ]


let extends_placeholder_stub_class
    { Node.value = { ClassSummary.bases; _ }; _ }
    ~aliases
    ~from_empty_stub
  =
  let is_from_placeholder_stub { Expression.Call.Argument.value; _ } =
    let value = Expression.delocalize value in
    let parsed = Type.create ~aliases value in
    match parsed with
    | Type.Primitive primitive
    | Parametric { name = primitive; _ } ->
        Reference.create primitive |> fun reference -> from_empty_stub reference
    | _ -> false
  in
  List.exists bases ~f:is_from_placeholder_stub
