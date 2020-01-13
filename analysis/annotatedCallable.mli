(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Statement

type annotation_parser = {
  parse_annotation: Expression.expression Node.t -> Type.t;
  parse_as_concatenation:
    Expression.t ->
    (Type.t Type.OrderedTypes.Concatenation.Middle.t, Type.t) Type.OrderedTypes.Concatenation.t
    option;
  parse_as_parameter_specification_instance_annotation:
    variable_parameter_annotation:Expression.t ->
    keywords_parameter_annotation:Expression.t ->
    Type.Variable.Variadic.Parameters.t option;
}

val return_annotation_without_applying_decorators
  :  signature:Define.Signature.t ->
  parser:annotation_parser ->
  Type.t

val create_overload_without_applying_decorators
  :  parser:annotation_parser ->
  Define.Signature.t Node.t ->
  Type.t Type.Callable.overload
