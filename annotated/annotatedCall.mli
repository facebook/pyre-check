(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression
open Statement

module Annotation = AnalysisAnnotation
module Resolution = AnalysisResolution
module Signature = AnalysisSignature
module Type = AnalysisType

module Assign = AnnotatedAssign
module Class = AnnotatedClass
module Attribute = Class.Attribute
module Method = Class.Method
module Define = AnnotatedDefine


type kind =
  | Function
  | Method
[@@deriving compare, eq, sexp, show, hash]

type t
[@@deriving compare, eq, sexp, show, hash]

val name_equal: t -> t -> bool

val create: kind: kind -> Call.t -> t

val call: t -> Call.t
val name: t -> Expression.t

val arguments: t -> (Expression.t Argument.t) list
val with_arguments: t -> (Expression.t Argument.t) list -> t
val insert_implicit_arguments: t -> callee: Signature.t option -> location: Location.t -> t

(* Some calls are redirected to method calls, e.g. `repr(x)` will call
   `x.__repr__()`. *)
type redirect = {
  access: Access.t;
  call: Access.t;
}
val redirect: t -> redirect option

(* Some calls have a backup in case the original call raises `NotImplemented`. *)
val backup: t -> t option

val argument_annotations
  :  t
  -> resolution: Resolution.t
  -> Signature.argument list

val check_parameters
  :  resolution: Resolution.t
  -> check_parameter: (
      argument: Expression.t Argument.t
      -> position: int
      -> offset: int
      -> location: Location.t
      -> name: Identifier.t
      -> actual: Type.t
      -> expected: Type.t
      -> 'error option)
  -> add_error: ('accumulator -> 'error -> 'accumulator)
  -> init: 'accumulator
  -> t
  -> Signature.t
  -> 'accumulator

type closest = {
  rank: int;
  callable: Type.Callable.t;
}
[@@deriving eq, show]

type overload =
  | Found of Type.Callable.t
  | NotFound of closest
[@@deriving eq, show]

val overload
  :  t
  -> resolution: Resolution.t
  -> callable: Type.Callable.t
  -> overload
