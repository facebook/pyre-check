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

type reason =
  | Mismatch of mismatch Node.t
  | MissingArgument of Access.t
  | TooManyArguments of { expected: int; provided: int }
  | TypedDictionaryAccessWithNonLiteral of string list
  | TypedDictionaryMissingKey of { typed_dictionary_name: Identifier.t; missing_key: string }
  | UnexpectedKeyword of Identifier.t
[@@deriving eq, show, compare]

type found = {
  callable: Type.Callable.t;
  constraints: Type.t Type.Map.t;
}
[@@deriving eq, show]

type closest = {
  callable: Type.Callable.t;
  reason: reason option;
}
[@@deriving eq, show]

type t =
  | Found of found
  | NotFound of closest
[@@deriving eq, show]

val select
  :  resolution: Resolution.t
  -> arguments: Argument.t list
  -> callable: Type.Callable.t
  -> t

(* Calls on objects can determine their type. E.g. `[].append(1)` will determine the list to be of
   type `List[int]`. *)
val determine
  :  t
  -> resolution: Resolution.t
  -> annotation: Type.t
  -> Type.t option
