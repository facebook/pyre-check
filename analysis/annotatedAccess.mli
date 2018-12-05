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

type origin =
  | Instance of Attribute.t
  | Module of Access.t
[@@deriving show]

type element =
  | Signature of {
      signature: AnnotatedSignature.t;
      callees: Type.Callable.t list;
      arguments: Argument.t list;
    }
  | Attribute of { attribute: Access.t; origin: origin; defined: bool }
  | NotCallable of Type.t
  | Value
[@@deriving show]

val fold
  :  resolution: Resolution.t
  -> initial: 'accumulator
  -> f:
       ('accumulator
        -> resolution: Resolution.t
        -> resolved: Annotation.t
        -> element: element
        -> lead: Access.t
        -> 'accumulator)
  -> t
  -> 'accumulator

val last_element: resolution: Resolution.t -> t -> element
