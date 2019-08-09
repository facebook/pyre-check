(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Expression

type mismatch = {
  actual: Type.t;
  actual_expression: Expression.t;
  expected: Type.t;
  name: Identifier.t option;
  position: int;
}
[@@deriving eq, show, compare]

type invalid_argument = {
  expression: Expression.t;
  annotation: Type.t;
}
[@@deriving eq, show, compare]

type missing_argument =
  | Named of Identifier.t
  | Anonymous of int
[@@deriving eq, show, compare, sexp, hash]

type mismatch_with_list_variadic_type_variable =
  | NotDefiniteTuple of invalid_argument
  | CantConcatenate of Type.OrderedTypes.t list
  | ConstraintFailure of Type.OrderedTypes.t
[@@deriving compare, eq, show, sexp, hash]

type reason =
  | AbstractClassInstantiation of Reference.t
  | CallingParameterVariadicTypeVariable
  | InvalidKeywordArgument of invalid_argument Node.t
  | InvalidVariableArgument of invalid_argument Node.t
  | Mismatch of mismatch Node.t
  | MismatchWithListVariadicTypeVariable of
      Type.OrderedTypes.t * mismatch_with_list_variadic_type_variable
  | MissingArgument of missing_argument
  | MutuallyRecursiveTypeVariables
  | ProtocolInstantiation of Reference.t
  | TooManyArguments of {
      expected: int;
      provided: int;
    }
  | UnexpectedKeyword of Identifier.t
[@@deriving eq, show, compare]

type closest = {
  callable: Type.Callable.t;
  reason: reason option;
}
[@@deriving eq, show]

type t =
  | Found of Type.Callable.t
  | NotFound of closest
[@@deriving eq, show]

val select
  :  resolution:Resolution.t ->
  arguments:Call.Argument.t list ->
  callable:Type.Callable.t ->
  t
