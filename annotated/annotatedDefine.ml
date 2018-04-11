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


let return_annotation ({ Define.return_annotation; async; _ } as define) ~resolution =
  let annotation =
    Option.value_map
      return_annotation
      ~f:(Resolution.parse_annotation resolution)
      ~default:Type.Top
  in
  if async then
    Type.awaitable annotation
  else
  if Define.is_coroutine define then
    begin
      match annotation with
      | Type.Parametric { Type.name; parameters = [_; _; return_annotation] }
        when Identifier.show name = "typing.Generator" ->
          Type.awaitable return_annotation
      | _ ->
          Type.Top
    end
  else
    annotation


let callable ({ Define.name; parameters; parent; _ } as define) ~resolution =
  let open Type.Callable in
  let parameter { Node.value = { Ast.Parameter.name; annotation; value }; _ } =
    let name = Identifier.show name in
    let access =
      String.lstrip ~drop:(function | '*' -> true | _ -> false) name
      |> Access.create
    in
    let annotation =
      annotation
      >>| Resolution.parse_annotation resolution
      |> Option.value ~default:Type.Top
    in
    if String.is_prefix ~prefix:"**" name then
      Parameter.Keywords { Parameter.name = access; annotation; default = false }
    else if String.is_prefix ~prefix:"*" name then
      Parameter.Variable { Parameter.name = access; annotation; default = false }
    else
      Parameter.Named { Parameter.name = access; annotation; default = Option.is_some value }
  in
  let name = parent >>| (fun parent -> parent @ name) |> Option.value ~default:name in
  {
    kind = Named name;
    overloads = [
      {
        annotation = return_annotation define ~resolution;
        parameters = Defined (List.map ~f:parameter parameters);
      }
    ];
    implicit_argument = Option.is_some parent && not (Define.is_static_method define);
  }


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


(* Given a callee f and an its argument at index index, evaluates to the parameter name the
 *  argument corresponds to. *)
let infer_argument_name { Define.parameters; _ } ~index ~argument =
  let parameter_names = List.map ~f:Parameter.name parameters in
  let star_index =
    List.find_mapi
      ~f:(fun index name ->
          if String.prefix (Identifier.show name) 1 = "*" then
            Some index
          else
            None)
      parameter_names
  in
  match argument.Argument.name, star_index with
  | None, None ->
      List.nth parameter_names index
  | None, Some star_index ->
      if star_index <= index then
        List.nth parameter_names star_index
      else
        List.nth parameter_names index
  | Some name, _ -> Some name


let apply_decorators define ~resolution =
  let return_annotation = return_annotation define ~resolution in
  match Define.has_decorator define "contextlib.contextmanager", return_annotation with
  | true, AnalysisType.Parametric { AnalysisType.name; parameters = [single_parameter] }
    when Identifier.show name = "typing.Iterator" ->
      {
        define with
        Define.return_annotation =
          Some
            (AnalysisType.Parametric {
                AnalysisType.name = Identifier.create "contextlib.GeneratorContextManager";
                parameters = [single_parameter];
              }
             |> Type.expression);
      }
  | _ ->
      define
