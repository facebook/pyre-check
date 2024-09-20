(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Ast
open Pyre

let is_named_tuple ~global_resolution ~annotation =
  (not (Type.is_any annotation))
  && GlobalResolution.less_or_equal global_resolution ~left:annotation ~right:Type.named_tuple


let class_name ~global_resolution annotation =
  annotation
  |> Option.some_if (is_named_tuple ~global_resolution ~annotation)
  >>| Type.split
  >>| fst
  >>= Type.primitive_name


let field_names_from_class_name ~global_resolution class_name =
  let classes_to_check = class_name :: GlobalResolution.successors global_resolution class_name in
  List.find_map
    ~f:(fun class_name ->
      GlobalResolution.get_class_summary global_resolution class_name
      >>| Node.value
      >>= ClassSummary.fields_tuple_value)
    classes_to_check


let field_names ~global_resolution annotation =
  class_name ~global_resolution annotation >>= field_names_from_class_name ~global_resolution


let field_annotations ~global_resolution annotation =
  let class_name = class_name ~global_resolution annotation in
  match class_name with
  | Some class_name ->
      let field_names = field_names_from_class_name ~global_resolution class_name in
      let matching_annotation name =
        let attribute =
          GlobalResolution.attribute_from_class_name
            global_resolution
            ~name
            ~transitive:true
            ~type_for_lookup:(Primitive class_name)
            class_name
        in
        match attribute with
        | Some attribute when AnnotatedAttribute.defined attribute ->
            attribute |> AnnotatedAttribute.annotation |> TypeInfo.Unit.annotation |> Option.some
        | _ -> None
      in
      field_names >>| List.filter_map ~f:matching_annotation
  | None -> None
