(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Ast
open Expression
open Statement
module StatementAttribute = Attribute
module Attribute = AnnotatedAttribute

type t = ClassSummary.t Node.t [@@deriving compare, eq, sexp, show, hash]

let create definition = definition

let name { Node.value = { ClassSummary.name; _ }; _ } = name

let bases { Node.value = { ClassSummary.bases; _ }; _ } = bases

let annotation { Node.value = { ClassSummary.name; _ }; _ } = Type.Primitive (Reference.show name)

let fallback_attribute ~(resolution : Resolution.t) ~name class_name =
  let class_name_reference = Reference.create class_name in
  let compound_backup =
    let name =
      match name with
      | "__iadd__" -> Some "__add__"
      | "__isub__" -> Some "__sub__"
      | "__imul__" -> Some "__mul__"
      | "__imatmul__" -> Some "__matmul__"
      | "__itruediv__" -> Some "__truediv__"
      | "__ifloordiv__" -> Some "__floordiv__"
      | "__imod__" -> Some "__mod__"
      | "__idivmod__" -> Some "__divmod__"
      | "__ipow__" -> Some "__pow__"
      | "__ilshift__" -> Some "__lshift__"
      | "__irshift__" -> Some "__rshift__"
      | "__iand__" -> Some "__and__"
      | "__ixor__" -> Some "__xor__"
      | "__ior__" -> Some "__or__"
      | _ -> None
    in
    match name with
    | Some name ->
        GlobalResolution.attribute_from_class_name
          ~resolution:(Resolution.global_resolution resolution)
          class_name
          ~class_attributes:false
          ~transitive:true
          ~instantiated:(Type.Primitive class_name)
          ~name
    | _ -> None
  in
  let getitem_backup () =
    let fallback =
      GlobalResolution.attribute_from_class_name
        class_name
        ~class_attributes:false
        ~transitive:true
        ~resolution:(Resolution.global_resolution resolution)
        ~name:"__getattr__"
        ~instantiated:(Type.Primitive class_name)
    in
    match fallback with
    | Some fallback when AnnotatedAttribute.defined fallback -> (
        let annotation = fallback |> AnnotatedAttribute.annotation |> Annotation.annotation in
        match annotation with
        | Type.Callable ({ implementation; _ } as callable) ->
            let arguments =
              let name_argument =
                {
                  Call.Argument.name = None;
                  value =
                    {
                      Node.location = Location.any;
                      value = Expression.String (StringLiteral.create name);
                    };
                }
              in
              [name_argument]
            in
            let implementation =
              match
                GlobalResolution.signature_select
                  ~global_resolution:(Resolution.global_resolution resolution)
                  ~resolve:(Resolution.resolve_expression_to_type resolution)
                  ~arguments
                  ~callable
              with
              | AttributeResolution.Found { Type.Callable.implementation; _ } -> implementation
              | AttributeResolution.NotFound _ -> implementation
            in
            let return_annotation = Type.Callable.Overload.return_annotation implementation in
            Some
              (AnnotatedAttribute.create
                 ~annotation:return_annotation
                 ~original_annotation:return_annotation
                 ~abstract:false
                 ~async:false
                 ~class_attribute:false
                 ~defined:true
                 ~initialized:NotInitialized
                 ~name
                 ~parent:(Reference.show class_name_reference)
                 ~visibility:ReadWrite
                 ~property:false
                 ~static:false)
        | _ -> None )
    | _ -> None
  in
  match compound_backup with
  | Some backup when AnnotatedAttribute.defined backup -> Some backup
  | _ -> getitem_backup ()


let overrides class_name ~resolution ~name =
  let find_override parent =
    GlobalResolution.attribute_from_class_name
      ~transitive:false
      ~class_attributes:true
      ~name
      parent
      ~resolution
      ~instantiated:(Primitive class_name)
    >>= fun attribute -> Option.some_if (AnnotatedAttribute.defined attribute) attribute
  in
  GlobalResolution.successors class_name ~resolution |> List.find_map ~f:find_override


let has_abstract_base { Node.value = summary; _ } = ClassSummary.is_abstract summary
