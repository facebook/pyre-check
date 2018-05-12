(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open Statement

module Resolution = AnalysisResolution
module Type = AnalysisType

module Callable = AnnotatedCallable
module Class = AnnotatedClass
module Attribute = Class.Attribute
module Method = Class.Method


type t = Define.t
[@@deriving compare, eq, sexp, show, hash]


let create definition =
  definition


let create_toplevel statements =
  {
    Define.name = Expression.Access.create "$toplevel";
    parameters = [];
    body = statements;
    decorators = [];
    docstring = None;
    return_annotation = Some (Type.expression Type.none);
    async = false;
    generated = false;
    parent = None;
  }


let define annotated = annotated


let parameter_annotations { Define.parameters; _ } ~resolution =
  let element { Node.value = { Parameter.name; annotation; _ }; _ } =
    let annotation =
      (annotation
       >>| fun annotation -> Resolution.parse_annotation resolution annotation)
      |> Option.value ~default:Type.Top
    in
    name, annotation
  in
  List.map ~f:element parameters
  |> Identifier.Map.of_alist_exn


let parameter_annotations_positional { Define.parameters; _ } ~resolution =
  let element index { Node.value = { Parameter.annotation; _ }; _ } =
    let annotation =
      (annotation
       >>| fun annotation -> Resolution.parse_annotation resolution annotation)
      |> Option.value ~default:Type.Top
    in
    index, annotation
  in
  List.mapi ~f:element parameters
  |> Int.Map.of_alist_exn


let parent_definition { Define.parent; _ } ~resolution =
  match parent with
  | Some parent ->
      let annotation =
        Resolution.parse_annotation
          resolution
          (Node.create_with_default_location (Access parent))
      in
      Resolution.class_definition resolution annotation
      >>| Class.create
  | _ -> None


let method_definition define ~resolution =
  parent_definition define ~resolution
  >>| fun parent -> Class.Method.create ~define ~parent
