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


let parameter_annotations { Define.signature = { parameters; _ }; _ } ~resolution =
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


let parent_definition { Define.signature = { parent; _ }; _ } ~resolution =
  match parent with
  | Some parent ->
      let annotation = Resolution.parse_reference resolution parent in
      Resolution.class_definition resolution annotation
      >>| Class.create
  | _ -> None
