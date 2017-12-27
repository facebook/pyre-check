(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression
open Statement


type t = {
  annotations: Annotation.t Access.Map.t;
  order: (module TypeOrder.Reader);

  resolve: resolution: t -> Expression.t -> Type.t;
  parse_annotation: Expression.t -> Type.t;

  global: Access.t -> Annotation.t option;
  class_definition: Type.t -> (Statement.t Class.t) option;

  function_signature:
    Access.t
    -> Call.t
    -> Signature.argument list
    -> Signature.t list;
  method_signature:
    resolution: t
    -> Type.t
    -> Call.t
    -> Signature.argument list
    -> Signature.t list;
}


let create
    ~annotations
    ~order
    ~resolve
    ~parse_annotation
    ~global
    ~class_definition
    ~function_signature
    ~method_signature =
  {
    annotations;
    order;
    resolve;
    parse_annotation;
    global;
    class_definition;
    function_signature;
    method_signature;
  }


let with_annotations resolution annotations =
  { resolution with annotations }


let annotations { annotations; _ } =
  annotations


let order { order; _ } =
  order


let resolve ({ resolve; _  } as resolution) =
  resolve ~resolution


let parse_annotation { parse_annotation; _ } =
  parse_annotation


let global { global; _ } =
  global


let class_definition { class_definition; _ } =
  class_definition


let function_signature { function_signature; _ } =
  function_signature


let method_signature ({ method_signature; _ } as resolution) =
  method_signature ~resolution


let less_or_equal { order; _ } =
  TypeOrder.less_or_equal order


let join { order; _ } =
  TypeOrder.join order


let meet { order; _ } =
  TypeOrder.meet order


let widen { order; _ } =
  TypeOrder.widen order
