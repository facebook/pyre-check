(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression
open Statement

module Annotation = AnalysisAnnotation
module Resolution = AnalysisResolution
module Type = AnalysisType

module Assign = AnnotatedAssign
module Class = AnnotatedClass
module Attribute = Class.Attribute
module Method = Class.Method
module Define = AnnotatedDefine


module Signature : sig
  include module type of struct include AnalysisSignature end
end

module Call : sig
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

  (* Some calls are redirected to method calls, e.g. `repr(x)` will call
     `x.__repr__()`. *)
  type redirect = {
    access: Access.t;
    call: Access.t;
  }
  val redirect: t -> redirect option

  (* Some calls have a backup in case the original call raises `NotImplemented`. *)
  val backup: t -> t option

  (* In some cases python magically calls a backup method in case a call fails. *)
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
end

module Access: sig
  type t
  [@@deriving compare, eq, sexp, show, hash]

  val create: Access.t -> t

  module Element: sig
    type call = {
      location: Location.t;
      call: Call.t;
      callee: Signature.t option
    }

    type method_call = {
      location: Location.t;
      access: Access.t;
      annotation: Annotation.t;
      call: Call.t;
      callee: Signature.t option;
      backup: (Call.t * Signature.t) option;
    }

    type t =
      | Call of call
      | Attribute of Attribute.t
      | Method of method_call
      | Value
  end

  val fold
    :  resolution: Resolution.t
    -> initial: 'accumulator
    -> f:
         ('accumulator
          -> annotations: Annotation.t Access.Map.t
          -> resolved: Annotation.t
          -> element: Element.t
          -> 'accumulator)
    -> t
    -> 'accumulator

  val last_element: resolution: Resolution.t -> t -> Element.t
end

val resolve
  :  resolution: Resolution.t
  -> Expression.t
  -> Type.t
