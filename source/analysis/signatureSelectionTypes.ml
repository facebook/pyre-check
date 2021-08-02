(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast

type mismatch = {
  actual: Type.t;
  expected: Type.t;
  name: Identifier.t option;
  position: int;
}
[@@deriving eq, show, sexp, compare]

type invalid_argument = {
  expression: Expression.t option;
  annotation: Type.t;
}
[@@deriving compare, eq, show, sexp, hash]

type missing_argument =
  | Named of Identifier.t
  | PositionalOnly of int
[@@deriving eq, show, compare, sexp, hash]

type mismatch_with_tuple_variadic_type_variable =
  | NotBoundedTuple of invalid_argument
  | CannotConcatenate of Type.OrderedTypes.t list
  | ConstraintFailure of Type.OrderedTypes.t
[@@deriving compare, eq, show, sexp, hash]

type mismatch_reason =
  | Mismatch of mismatch Node.t
  | MismatchWithTupleVariadicTypeVariable of {
      variable: Type.OrderedTypes.t;
      mismatch: mismatch_with_tuple_variadic_type_variable;
    }
[@@deriving eq, show, sexp, compare]

type reason =
  | AbstractClassInstantiation of {
      class_name: Reference.t;
      abstract_methods: string list;
    }
  | CallingParameterVariadicTypeVariable
  | InvalidKeywordArgument of invalid_argument Node.t
  | InvalidVariableArgument of invalid_argument Node.t
  | Mismatches of mismatch_reason list
  | MissingArgument of missing_argument
  | MutuallyRecursiveTypeVariables
  | ProtocolInstantiation of Reference.t
  | TooManyArguments of {
      expected: int;
      provided: int;
    }
  | TypedDictionaryInitializationError of
      WeakenMutableLiterals.typed_dictionary_mismatch Node.t list
  | UnexpectedKeyword of Identifier.t
[@@deriving eq, show, sexp, compare]

type closest = {
  closest_return_annotation: Type.t;
  reason: reason option;
}
[@@deriving show, sexp]

let equal_closest (left : closest) (right : closest) =
  (* Ignore rank. *)
  Type.equal left.closest_return_annotation right.closest_return_annotation
  && Option.equal equal_reason left.reason right.reason


type sig_t =
  | Found of { selected_return_annotation: Type.t }
  | NotFound of closest
[@@deriving eq, show, sexp]
