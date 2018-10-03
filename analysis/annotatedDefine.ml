(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Pyre
open Statement

module Callable = AnnotatedCallable
module Class = AnnotatedClass


type t = Define.t
[@@deriving compare, eq, sexp, show, hash]


let create definition =
  definition


let define annotated =
  annotated


let is_generator { Define.body; _ } =
  let module YieldVisit = Visit.Make(struct
      type t = bool

      let expression result expression =
        match result, expression with
        | true, _ -> true
        | false, { Node.value = Expression.Yield _; _ } -> true
        | false, _ -> false

      let statement result statement =
        match result, statement with
        | true, _ -> true
        | false, { Node.value = Statement.Yield _; _ } -> true
        | false, { Node.value = Statement.YieldFrom _; _ } -> true
        | false, _ -> false
    end)
  in
  YieldVisit.visit false (Source.create body)


let parameter_annotations { Define.parameters; _ } ~resolution =
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
        Access.expression parent
        |> Resolution.parse_annotation resolution
      in
      Resolution.class_definition resolution annotation
      >>| Class.create
  | _ -> None
