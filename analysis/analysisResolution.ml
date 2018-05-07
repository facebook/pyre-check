(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open Statement

module Annotation = AnalysisAnnotation
module Type = AnalysisType
module TypeOrder = AnalysisTypeOrder


type global = Annotation.t Node.t
[@@deriving eq, show]


type t = {
  annotations: Annotation.t Access.Map.t;
  define: Statement.Define.t option;
  order: (module TypeOrder.Handler);

  resolve: resolution: t -> Expression.t -> Type.t;
  resolve_literal: resolution: t -> Expression.t -> Type.t;
  parse_annotation: Expression.t -> Type.t;

  global: Access.t -> global option;

  module_definition: Access.t -> Module.t option;

  class_definition: Type.t -> (Class.t Node.t) option;
}


let create
    ~annotations
    ~order
    ~resolve
    ~resolve_literal
    ~parse_annotation
    ~global
    ~module_definition
    ~class_definition =
  {
    annotations;
    define = None;
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
  | Some resolved ->
      Some resolved
  | None ->
      global access
      >>| Node.value


let get_local_callable resolution ~access =
  get_local resolution ~access
  >>| Annotation.annotation
  >>= function
  | Type.Callable callable -> Some callable
  | _ -> None


let with_define resolution define =
  { resolution with define = Some define }


let with_annotations resolution ~annotations =
  { resolution with annotations }


let annotations { annotations; _ } =
  annotations


let define { define; _ } =
  define


let order { order; _ } =
  order


let resolve ({ resolve; _  } as resolution) =
  resolve ~resolution


let resolve_literal ({ resolve_literal; _  } as resolution) =
  resolve_literal ~resolution


let parse_annotation { parse_annotation; _ } =
  parse_annotation


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
