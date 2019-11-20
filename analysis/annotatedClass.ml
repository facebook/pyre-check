(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
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


let attribute_fold
    ?(transitive = false)
    ?(class_attributes = false)
    ?(include_generated_attributes = true)
    definition
    ~initial
    ~f
    ~resolution
  =
  GlobalResolution.attributes
    ~transitive
    ~class_attributes
    ~include_generated_attributes
    ~resolution
    definition
  |> List.fold ~init:initial ~f


let rec fallback_attribute
    ~(resolution : Resolution.t)
    ~name
    ({ Node.value = { ClassSummary.name = class_name; _ }; _ } as definition)
  =
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
        GlobalResolution.c_attribute
          definition
          ~class_attributes:false
          ~transitive:true
          ~resolution:(Resolution.global_resolution resolution)
          ~name
          ~instantiated:(annotation definition)
        |> Option.some
    | _ -> None
  in
  let getitem_backup () =
    let fallback =
      GlobalResolution.c_attribute
        definition
        ~class_attributes:true
        ~transitive:true
        ~resolution:(Resolution.global_resolution resolution)
        ~name:"__getattr__"
        ~instantiated:(annotation definition)
    in
    if AnnotatedAttribute.defined fallback then
      let annotation = fallback |> AnnotatedAttribute.annotation |> Annotation.annotation in
      match annotation with
      | Type.Callable ({ implementation; _ } as callable) ->
          let location = AnnotatedAttribute.location fallback in
          let arguments =
            let self_argument =
              { Call.Argument.name = None; value = from_reference ~location class_name }
            in
            let name_argument =
              {
                Call.Argument.name = None;
                value = { Node.location; value = Expression.String (StringLiteral.create name) };
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
               ~parent:definition
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
      | _ -> None
    else
      None
  in
  match compound_backup with
  | Some backup when AnnotatedAttribute.defined backup -> Some backup
  | _ -> getitem_backup ()


let has_explicit_constructor definition ~resolution =
  let table =
    GlobalResolution.attribute_table
      ~transitive:false
      ~class_attributes:false
      ~include_generated_attributes:true
      ?instantiated:None
      definition
      ~resolution
  in
  let in_test =
    let superclasses = GlobalResolution.superclasses ~resolution definition in
    List.exists ~f:is_unit_test (definition :: superclasses)
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


let overrides definition ~resolution ~name =
  let find_override parent =
    let potential_override =
      GlobalResolution.c_attribute
        ~transitive:false
        ~class_attributes:true
        parent
        ~resolution
        ~name
        ~instantiated:(annotation parent)
    in
    if AnnotatedAttribute.defined potential_override then
      annotation definition
      |> (fun instantiated ->
           GlobalResolution.constraints ~target:parent definition ~resolution ~instantiated)
      |> (fun solution ->
           AnnotatedAttribute.instantiate
             ~constraints:(fun annotation ->
               Some (TypeConstraints.Solution.instantiate solution annotation))
             potential_override)
      |> Option.some
    else
      None
  in
  GlobalResolution.superclasses definition ~resolution |> List.find_map ~f:find_override


let has_method ?transitive definition ~resolution ~name =
  GlobalResolution.c_attribute
    ?transitive
    definition
    ~resolution
    ~name
    ~instantiated:(annotation definition)
  |> AnnotatedAttribute.annotation
  |> Annotation.annotation
  |> Type.is_callable


let has_abstract_base { Node.value = summary; _ } = ClassSummary.is_abstract summary

let get_abstract_attributes ~resolution definition =
  let attributes =
    GlobalResolution.attributes
      ~transitive:true
      ~instantiated:(annotation definition)
      definition
      ~resolution
  in
  List.filter attributes ~f:AnnotatedAttribute.abstract
