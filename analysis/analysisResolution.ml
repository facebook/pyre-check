(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression
open Statement

module Annotation = AnalysisAnnotation
module Signature = AnalysisSignature
module Type = AnalysisType
module TypeOrder = AnalysisTypeOrder


type global = {
  annotation: Annotation.t;
  location: Location.t;
}
[@@deriving show]


type t = {
  annotations: Annotation.t Access.Map.t;
  define: Statement.Define.t option;
  order: (module TypeOrder.Handler);

  resolve: resolution: t -> Expression.t -> Type.t;
  parse_annotation: Expression.t -> Type.t;

  global: Access.t -> global option;
  class_definition: Type.t -> (Class.t Node.t) option;

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
    define = None;
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


let with_define resolution define =
  { resolution with define = Some define }


let annotations { annotations; _ } =
  annotations


let define { define; _ } =
  define


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
