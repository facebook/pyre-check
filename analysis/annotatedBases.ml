(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre

let find_propagated_type_variables bases ~parse_annotation =
  let find_type_variables { Expression.Call.Argument.value; _ } =
    parse_annotation value |> Type.Variable.all_free_variables
  in
  List.concat_map ~f:find_type_variables bases
  |> List.dedup ~compare:Type.Variable.compare
  |> List.map ~f:Type.Variable.to_parameter


let inferred_generic_base { Node.value = { ClassSummary.bases; _ }; _ } ~parse_annotation =
  let is_generic { Expression.Call.Argument.value; _ } =
    let primitive, _ = parse_annotation value |> Type.split in
    Type.is_generic_primitive primitive
  in
  let extract_protocol_parameters { Expression.Call.Argument.value; _ } =
    let primitive, parameters = parse_annotation value |> Type.split in
    let is_protocol =
      primitive
      |> Type.primitive_name
      >>| String.equal "typing.Protocol"
      |> Option.value ~default:false
    in
    Option.some_if is_protocol parameters
  in
  if List.exists ~f:is_generic bases then
    []
  else
    let create variables =
      [
        {
          Expression.Call.Argument.name = None;
          value = Type.parametric "typing.Generic" variables |> Type.expression;
        };
      ]
    in
    match List.find_map bases ~f:extract_protocol_parameters with
    | Some parameters -> create parameters
    | None ->
        (* TODO:(T60673574) Ban propagating multiple type variables *)
        let variables = find_propagated_type_variables bases ~parse_annotation in
        if List.is_empty variables then
          []
        else
          create variables


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
