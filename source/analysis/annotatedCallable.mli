(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Statement

type annotation_parser = {
  parse_annotation: Expression.expression Node.t -> Type.t;
  param_spec_from_vararg_annotations:
    args_annotation:Expression.t ->
    kwargs_annotation:Expression.t ->
    Type.Variable.ParamSpec.t option;
}

val return_annotation_without_applying_decorators
  :  signature:Define.Signature.t ->
  parser:annotation_parser ->
  Type.t

val create_overload_without_applying_decorators
  :  parser:annotation_parser ->
  variables:(Type.Primitive.t -> Type.Variable.t list option) ->
  Define.Signature.t ->
  Type.t Type.Callable.overload
