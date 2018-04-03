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

  is_module: Access.t -> bool;
  module_definition: Access.t -> Module.t option;

  function_definitions: Access.t -> (Define.t Node.t) list;
  class_definition: Type.t -> (Class.t Node.t) option;

  is_function: Access.t -> bool;
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
    ~resolve_literal
    ~parse_annotation
    ~global
    ~is_module
    ~module_definition
    ~function_definitions
    ~class_definition
    ~is_function
    ~function_signature
    ~method_signature =
  {
    annotations;
    define = None;
    order;
    resolve;
    resolve_literal;
    parse_annotation;
    global;
    is_module;
    module_definition;
    function_definitions;
    class_definition;
    is_function;
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


let resolve_literal ({ resolve_literal; _  } as resolution) =
  resolve_literal ~resolution


let parse_annotation { parse_annotation; _ } =
  parse_annotation


let global { global; _ } =
  global


let is_module { is_module; _ } =
  is_module


let module_definition { module_definition; _ } =
  module_definition


let function_definitions { function_definitions; _ } =
  function_definitions


let class_definition { class_definition; _ } =
  class_definition


let is_function { is_function; _ } =
  is_function


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
