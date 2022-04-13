(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre

let is_named_tuple ~global_resolution ~annotation =
  GlobalResolution.less_or_equal global_resolution ~left:annotation ~right:Type.named_tuple


let field_annotations ~global_resolution annotation =
  let class_name =
    annotation
    |> Option.some_if (is_named_tuple ~global_resolution ~annotation)
    >>| Type.split
    >>| fst
    >>= Type.primitive_name
  in
  match class_name with
  | Some class_name ->
      let field_names =
        GlobalResolution.class_summary global_resolution (Primitive class_name)
        >>| Node.value
        >>| fun summary -> ClassSummary.fields_tuple_value summary |> Option.value ~default:[]
      in
      let matching_annotation name =
        let attribute =
          GlobalResolution.attribute_from_class_name
            ~resolution:global_resolution
            ~name
            ~instantiated:(Primitive class_name)
            class_name
        in
        match attribute with
        | Some attribute when AnnotatedAttribute.defined attribute ->
            attribute |> AnnotatedAttribute.annotation |> Annotation.annotation |> Option.some
        | _ -> None
      in
      field_names >>| List.filter_map ~f:matching_annotation
  | None -> None
