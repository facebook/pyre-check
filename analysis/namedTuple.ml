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
  let class_name =
    annotation
    |> Option.some_if (is_named_tuple ~global_resolution ~annotation)
    >>| Type.split
    >>| fst
    >>= Type.primitive_name
  in
  match class_name with
  | Some class_name ->
      let field_names = function
        | { value = { Node.value = Tuple fields; _ }; _ } ->
            let name = function
              | { Node.value = Expression.String { StringLiteral.value; _ }; _ } -> Some value
              | _ -> None
            in
            List.filter_map fields ~f:name
        | _ -> []
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
            attribute |> Node.value |> fun { annotation; _ } -> annotation |> Option.some
        | _ -> None
      in
      GlobalResolution.attribute_from_class_name
        ~resolution:global_resolution
        ~name:"_fields"
        ~instantiated:(Primitive class_name)
        class_name
      >>= (fun attribute -> Option.some_if (AnnotatedAttribute.defined attribute) attribute)
      >>| Node.value
      >>| field_names
      >>| List.filter_map ~f:matching_annotation
  | None -> None
