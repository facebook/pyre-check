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
module Call = AnnotatedCall


module Signature : sig
  include module type of struct include AnalysisSignature end
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
