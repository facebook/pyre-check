(** Copyright (c) 2019-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the LICENSE file in the root
    directory of this source tree. *)

open Core
open Ast
open Expression
open Pyre
open Annotated.Class.Attribute

let is_named_tuple ~global_resolution ~annotation =
  GlobalResolution.less_or_equal global_resolution ~left:annotation ~right:Type.named_tuple


let field_annotations ~global_resolution annotation =
  let field_names attributes =
    let is_fields_attribute = function
      | { Node.value = { name = "_fields"; _ }; _ } -> true
      | _ -> false
    in
    match List.find ~f:is_fields_attribute attributes >>| Node.value with
    | Some { value = { Node.value = Tuple fields; _ }; _ } -> fields
    | _ -> []
  in
  let matching_annotation attributes field =
    let annotation_if_name_equals field { Node.value = { name; annotation; _ }; _ } =
      match Node.value field with
      | Expression.String { StringLiteral.value; _ } when String.equal name value -> Some annotation
      | _ -> None
    in
    List.find_map ~f:(annotation_if_name_equals field) attributes
  in
  annotation
  |> Option.some_if (is_named_tuple ~global_resolution ~annotation)
  >>= GlobalResolution.class_definition global_resolution
  >>| GlobalResolution.attributes ~resolution:global_resolution
  >>| fun attributes ->
  field_names attributes |> List.filter_map ~f:(matching_annotation attributes)
