(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre
open Expression
open Statement
module StatementAttribute = Attribute
module Callable = AnnotatedCallable
module Attribute = AnnotatedAttribute

type t = ClassSummary.t Node.t [@@deriving compare, eq, sexp, show, hash]

type global_resolution = GlobalResolution.t

let name_equal
    { Node.value = { ClassSummary.name = left; _ }; _ }
    { Node.value = { ClassSummary.name = right; _ }; _ }
  =
  Reference.equal left right


let create definition = definition

let name { Node.value = { ClassSummary.name; _ }; _ } = name

let bases { Node.value = { ClassSummary.bases; _ }; _ } = bases

let annotation { Node.value = { ClassSummary.name; _ }; _ } = Type.Primitive (Reference.show name)

let is_unit_test { Node.value; _ } = ClassSummary.is_unit_test value

let implicit_attributes { Node.value = { ClassSummary.attribute_components; _ }; _ } =
  Class.implicit_attributes attribute_components


let rec fallback_attribute ~(resolution : Resolution.t) ~name class_name =
  match
    GlobalResolution.summary_and_attribute_table
      class_name
      ~class_attributes:false
      ~transitive:true
      ~resolution:(Resolution.global_resolution resolution)
      ~instantiated:(Type.Primitive class_name)
      ~include_generated_attributes:true
  with
  | Some (({ Node.value = { ClassSummary.name = class_name_reference; _ }; _ } as summary), table)
    -> (
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
        | Some name -> AnnotatedAttribute.Table.lookup_name table name
        | _ -> None
      in
      let getitem_backup () =
        let fallback =
          GlobalResolution.attribute_from_class_name
            class_name
            ~class_attributes:true
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
                let location = AnnotatedAttribute.location fallback in
                let arguments =
                  let self_argument =
                    {
                      Call.Argument.name = None;
                      value = from_reference ~location class_name_reference;
                    }
                  in
                  let name_argument =
                    {
                      Call.Argument.name = None;
                      value =
                        { Node.location; value = Expression.String (StringLiteral.create name) };
                    }
                  in
                  [self_argument; name_argument]
                in
                let implementation =
                  match
                    GlobalResolution.signature_select
                      ~global_resolution:(Resolution.global_resolution resolution)
                      ~resolve:(Resolution.resolve resolution)
                      ~arguments
                      ~callable
                  with
                  | AttributeResolution.Found { Type.Callable.implementation; _ } -> implementation
                  | AttributeResolution.NotFound _ -> implementation
                in
                let return_annotation = Type.Callable.Overload.return_annotation implementation in
                Some
                  (GlobalResolution.create_attribute
                     ~resolution:(Resolution.global_resolution resolution)
                     ~parent:summary
                     {
                       Node.location;
                       value =
                         {
                           StatementAttribute.name;
                           kind =
                             Simple
                               {
                                 annotation = Some (Type.expression return_annotation);
                                 value = None;
                                 primitive = true;
                                 toplevel = true;
                                 frozen = false;
                                 implicit = false;
                               };
                         };
                     })
            | _ -> None )
        | _ -> None
      in
      match compound_backup with
      | Some backup when AnnotatedAttribute.defined backup -> Some backup
      | _ -> getitem_backup () )
  | None -> None


let has_explicit_constructor class_name ~resolution =
  match
    GlobalResolution.summary_and_attribute_table
      ~transitive:false
      ~class_attributes:false
      ~include_generated_attributes:true
      ?instantiated:None
      class_name
      ~resolution
  with
  | Some (summary, table) ->
      let in_test =
        let superclasses = GlobalResolution.superclasses ~resolution summary in
        List.exists ~f:is_unit_test (summary :: superclasses)
      in
      let mem name = AnnotatedAttribute.Table.lookup_name table name |> Option.is_some in
      mem "__init__"
      || mem "__new__"
      || in_test
         && ( mem "async_setUp"
            || mem "setUp"
            || mem "_setup"
            || mem "_async_setup"
            || mem "with_context" )
  | None -> false


let overrides definition ~resolution ~name =
  let find_override parent =
    let summary_and_attribute =
      GlobalResolution.summary_and_attribute_table
        ~transitive:false
        ~class_attributes:true
        parent
        ~resolution
        ~instantiated:(Type.Primitive parent)
        ~include_generated_attributes:true
      >>= fun (summary, table) ->
      AnnotatedAttribute.Table.lookup_name table name >>| fun attribute -> summary, attribute
    in
    match summary_and_attribute with
    | Some (summary, attribute) ->
        annotation definition
        |> (fun instantiated ->
             GlobalResolution.constraints ~target:summary definition ~resolution ~instantiated)
        |> (fun solution ->
             AnnotatedAttribute.instantiate
               ~constraints:(fun annotation ->
                 Some (TypeConstraints.Solution.instantiate solution annotation))
               attribute)
        |> Option.some
    | None -> None
  in
  GlobalResolution.successors definition ~resolution |> List.find_map ~f:find_override


let has_abstract_base { Node.value = summary; _ } = ClassSummary.is_abstract summary

let get_abstract_attributes ~resolution class_name =
  GlobalResolution.attributes
    ~transitive:true
    ~instantiated:(Type.Primitive class_name)
    class_name
    ~resolution
  >>| List.filter ~f:AnnotatedAttribute.abstract
  |> Option.value ~default:[]
