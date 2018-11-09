(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression

module Class = AnnotatedClass
module Attribute = Class.Attribute
module Method = Class.Method
module Define = AnnotatedDefine


type t
[@@deriving compare, eq, sexp, show, hash]

val create: Access.t -> t

module Element: sig
  type origin =
    | Instance of Attribute.t
    | Module of Access.t
  [@@deriving show]

  type attribute = {
    attribute: Access.t;
    origin: origin;
    defined: bool;
  }
  [@@deriving show]

  type signature = {
    signature: AnnotatedSignature.t;
    arguments: Argument.t list;
  }
  [@@deriving show]

  type t =
    | Signature of signature
    | Attribute of attribute
    | Value
  [@@deriving show]
end

val fold
  :  resolution: Resolution.t
  -> initial: 'accumulator
  -> f:
       ('accumulator
        -> resolution: Resolution.t
        -> resolved: Annotation.t
        -> element: Element.t
        -> lead: Access.t
        -> 'accumulator)
  -> t
  -> 'accumulator

val last_element: resolution: Resolution.t -> t -> Element.t
