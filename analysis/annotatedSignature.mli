(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression


type mismatch = {
  actual: Type.t;
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

type reason =
  | InvalidKeywordArgument of invalid_argument Node.t
  | InvalidVariableArgument of invalid_argument Node.t
  | Mismatch of mismatch Node.t
  | MissingArgument of Identifier.t
  | MutuallyRecursiveTypeVariables
  | TooManyArguments of { expected: int; provided: int }
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
  :  resolution: Resolution.t
  -> arguments: Argument.t list
  -> callable: Type.Callable.t
  -> t
