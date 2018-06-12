(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression

module Resolution = AnalysisResolution
module Type = AnalysisType


type too_many_arguments = {
  expected: int;
  provided: int;
}
[@@deriving eq, show]


type mismatch = {
  actual: Type.t;
  expected: Type.t;
  name: Identifier.t option;
  position: int;
}
[@@deriving eq, show]

type reason =
  | Mismatch of mismatch Node.t
  | MissingArgument of Access.t
  | TooManyArguments of too_many_arguments
[@@deriving eq, show]

type found = {
  callable: Type.Callable.t;
  constraints: Type.t Type.Map.t;
}
[@@deriving eq, show]

type closest = {
  rank: int;
  callable: Type.Callable.t;
  reason: reason option;
}
[@@deriving eq, show]

type t =
  | Found of found
  | NotFound of closest
[@@deriving eq, show]

val select
  :  arguments: Argument.t list
  -> resolution: Resolution.t
  -> callable: Type.Callable.t
  -> t

(* Calls on objects can determine their type. E.g. `[].append(1)` will determine the list to be of
   type `List[int]`. *)
val determine
  :  t
  -> resolution: Resolution.t
  -> annotation: Type.t
  -> Type.t option
