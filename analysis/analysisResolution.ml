(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Pyre
open Expression
open Statement

module Annotation = AnalysisAnnotation
module Type = AnalysisType
module TypeOrder = AnalysisTypeOrder


type global = Annotation.t Node.t
[@@deriving eq, show]


type t = {
  annotations: Annotation.t Access.Map.t;
  order: (module TypeOrder.Handler);

  resolve: resolution: t -> Expression.t -> Type.t;
  resolve_literal: resolution: t -> Expression.t -> Type.t;
  parse_annotation: Expression.t -> Type.t;

  global: Access.t -> global option;
  module_definition: Access.t -> Module.t option;
  class_definition: Type.t -> (Class.t Node.t) option;

  define: Statement.Define.t;
}


let create
    ~annotations
    ~order
    ~resolve
    ~resolve_literal
    ~parse_annotation
    ~global
    ~module_definition
    ~class_definition
    ~define =
  {
    annotations;
    define;
    order;
    resolve;
    resolve_literal;
    parse_annotation;
    global;
    module_definition;
    class_definition;
  }


let set_local ({ annotations; _ } as resolution) ~access ~annotation =
  { resolution with annotations = Map.set annotations ~key:access ~data:annotation }


let get_local { annotations; global; _ } ~access =
  match Map.find annotations access with
  | Some result ->
      Some result
  | _ ->
      let access =
        match access with
        | [Access.Identifier name] ->
            let name =
              Identifier.show_sanitized name
              |> Identifier.create
            in
            [Access.Identifier name]
        | _ ->
            access
      in
      global access
      >>| Node.value


let get_local_callable resolution ~access =
  get_local resolution ~access
  >>| Annotation.annotation
  >>= function
  | Type.Callable callable -> Some callable
  | _ -> None


let annotations { annotations; _ } =
  annotations


let with_annotations resolution ~annotations =
  { resolution with annotations }


let define { define; _ } =
  define


let with_define resolution ~define =
  { resolution with define }


let order { order; _ } =
  order


let resolve ({ resolve; _  } as resolution) =
  resolve ~resolution


let resolve_literal ({ resolve_literal; _  } as resolution) =
  resolve_literal ~resolution


let parse_annotation
    { parse_annotation; define; module_definition; _ }
    ({ Node.value; _ } as expression) =
  let expression =
    let is_local_access =
      match value with
      | Access [Access.Identifier name]
        when Identifier.show name |> String.is_prefix ~prefix:"$local_" -> true
      | _ -> false
    in
    let is_empty_stub =
      Expression.access expression
      |> fun access -> Module.from_empty_stub ~access ~module_definition
    in
    if is_local_access then
      match define with
      | { Define.name = scope; parent = None; _ }
      | { Define.parent = Some scope; _ } ->
          List.rev scope
          |> List.tl
          >>| List.rev
          >>| (fun qualifier -> Expression.delocalize expression ~qualifier)
          |> Option.value ~default:expression
    else if is_empty_stub then
      { expression with Node.value = Access (Access.create "typing.Any") }
    else
      expression
  in
  parse_annotation expression


let global { global; _ } =
  global


let module_definition { module_definition; _ } =
  module_definition


let class_definition { class_definition; _ } =
  class_definition


let less_or_equal { order; _ } =
  TypeOrder.less_or_equal order


let join { order; _ } =
  TypeOrder.join order


let meet { order; _ } =
  TypeOrder.meet order


let widen { order; _ } =
  TypeOrder.widen order
