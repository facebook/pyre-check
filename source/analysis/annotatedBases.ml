(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre

let find_propagated_type_variables bases ~parse_annotation =
  let find_type_variables base_expression =
    parse_annotation base_expression |> Type.Variable.all_free_variables
  in
  List.concat_map ~f:find_type_variables bases
  |> List.dedup_and_sort ~compare:Type.Variable.compare
  |> List.map ~f:Type.Variable.to_parameter


let inferred_generic_base
    { Node.value = { ClassSummary.bases = { base_classes; metaclass; _ }; _ }; _ }
    ~parse_annotation
  =
  let bases =
    match metaclass with
    | Some metaclass -> metaclass :: base_classes
    | _ -> base_classes
  in
  let is_generic base_expression =
    let primitive, _ = parse_annotation base_expression |> Type.split in
    Type.is_generic_primitive primitive
  in
  let extract_protocol_parameters base_expression =
    let primitive, parameters = parse_annotation base_expression |> Type.split in
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
    let create variables = [Type.parametric "typing.Generic" variables |> Type.expression] in
    match List.find_map bases ~f:extract_protocol_parameters with
    | Some parameters -> create parameters
    | None ->
        (* TODO:(T60673574) Ban propagating multiple type variables *)
        let variables = find_propagated_type_variables bases ~parse_annotation in
        if List.is_empty variables then
          []
        else
          create variables


let base_is_from_placeholder_stub base_expression ~aliases ~from_empty_stub =
  let parsed = Expression.delocalize base_expression |> Type.create ~aliases in
  match parsed with
  | Type.Primitive primitive
  | Parametric { name = primitive; _ } ->
      Reference.create primitive |> fun reference -> from_empty_stub reference
  | _ -> false


let extends_placeholder_stub_class
    { Node.value = { ClassSummary.bases = { base_classes; metaclass; _ }; _ }; _ }
    ~aliases
    ~from_empty_stub
  =
  let metaclass_is_from_placeholder_stub =
    metaclass
    >>| base_is_from_placeholder_stub ~aliases ~from_empty_stub
    |> Option.value ~default:false
  in
  List.exists base_classes ~f:(base_is_from_placeholder_stub ~aliases ~from_empty_stub)
  || metaclass_is_from_placeholder_stub
