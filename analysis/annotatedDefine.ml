(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

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
      let parent_type = Type.Primitive (Reference.show parent) in
      Resolution.class_definition resolution parent_type
      >>| Class.create
  | _ -> None


let decorate
    ({
      Define.signature = {
        Define.decorators;
        parameters = original_parameters;
        _;
      } as signature;
      _;
    } as define)
    ~resolution =
  match decorators with
  | [] ->
      define
  | _ ->
      let { Type.Callable.parameters; annotation } =
        Callable.apply_decorators ~define ~resolution
      in
      let parameters =
        match parameters with
        | Defined parameters ->
            let convert parameter =
              match parameter with
              | Type.Callable.Parameter.Anonymous { annotation; _ } ->
                  (* This means it will be read back in as an anonymous *)
                  Ast.Parameter.create ~annotation:(Type.expression annotation) ~name:"__" ()
              | Type.Callable.Parameter.Named { name; annotation; _ } ->
                  Ast.Parameter.create ~annotation:(Type.expression annotation) ~name ()
              | Type.Callable.Parameter.Variable { name; annotation; _ } ->
                  Ast.Parameter.create
                    ~annotation:(Type.expression annotation)
                    ~name:("*" ^ name)
                    ()
              | Type.Callable.Parameter.Keywords { name; annotation; _ } ->
                  Ast.Parameter.create
                    ~annotation:(Type.expression annotation)
                    ~name:("**" ^ name)
                    ()
            in
            List.map parameters ~f:convert
        | ParameterVariadicTypeVariable _
        | Undefined ->
            original_parameters
      in
      let return_annotation =
        Some (Type.expression annotation)
      in
      { define with Define.signature = { signature with Define.parameters; return_annotation } }


let is_constructor definition ~resolution =
  match parent_definition definition ~resolution with
  | Some parent_class ->
      let in_test =
        let superclasses = Class.superclasses ~resolution parent_class in
        List.exists ~f:Class.is_unit_test (parent_class :: superclasses)
      in
      Define.is_constructor ~in_test definition
  | None ->
      false
