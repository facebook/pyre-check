(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre

open Ast
open Statement


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

  parent: Access.t option;
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
    ?parent
    () =
  {
    annotations;
    order;
    resolve;
    resolve_literal;
    parse_annotation;
    global;
    module_definition;
    class_definition;
    parent;
  }


let set_local ({ annotations; _ } as resolution) ~access ~annotation =
  { resolution with annotations = Map.set annotations ~key:access ~data:annotation }


let get_local { annotations; global; _ } ~access =
  match Map.find annotations access with
  | Some ({ Annotation.annotation; _ } as result) when not (Type.equal annotation Type.Deleted) ->
      Some result
  | _ ->
      Access.delocalize access
      |> global
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


let parent { parent; _ } =
  parent


let with_parent resolution ~parent =
  { resolution with parent }


let order { order; _ } =
  order


let resolve ({ resolve; _  } as resolution) =
  resolve ~resolution


let resolve_literal ({ resolve_literal; _  } as resolution) =
  resolve_literal ~resolution


let parse_annotation { parse_annotation; module_definition; _ } expression =
  let expression =
    let is_local_access =
      Expression.show expression
      |> String.is_substring ~substring:"$local_"
    in
    if is_local_access then
      Expression.delocalize expression
    else
      expression
  in
  let parsed = parse_annotation expression in
  let constraints = function
    | Type.Primitive name ->
        let originates_from_empty_stub =
          Identifier.show name
          |> Access.create
          |> fun access -> Module.from_empty_stub ~access ~module_definition
        in
        if originates_from_empty_stub then
          Some Type.Object
        else
          None
    | _ ->
        None
  in
  Type.instantiate parsed ~constraints


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
